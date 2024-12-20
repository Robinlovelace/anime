mod overlap;
pub mod structs;
use crate::{
    overlap::*, overlap_range, solve_no_x_overlap, solve_no_y_overlap, structs::*, x_range,
    y_range, TarLine,
};
use geo::{BoundingRect, EuclideanDistance, EuclideanLength};
use rstar::primitives::{CachedEnvelope, GeomWithData};
use std::{cell::OnceCell, collections::BTreeMap};

/// R* Tree for source geometries
pub type SourceTree = rstar::RTree<GeomWithData<CachedEnvelope<geo_types::Line>, (usize, f64)>>;

/// R* Tree for target geometries
pub type TargetTree = rstar::RTree<GeomWithData<CachedEnvelope<TarLine>, (usize, f64)>>;

/// Approximate Network Matching, Integration, and Enrichment
///
/// This struct contains all of the information needed to perform
/// and store the results of the ANIME algorithm.
///
/// The `source_tree` and `target_tree` are used to perform the
/// partial matching based on the `distance_tolerance` and
/// `angle_tolerance`. The results of the matching
/// are stored in the `BTreeMap`.
///
/// The lengths, represented as `Vec<f64>` are required for the
/// integration of attributes.
pub struct Anime {
    pub distance_tolerance: f64,
    pub angle_tolerance: f64,
    pub source_tree: SourceTree,
    pub source_lens: Vec<f64>,
    pub target_tree: TargetTree,
    pub target_lens: Vec<f64>,
    pub matches: OnceCell<BTreeMap<i32, Vec<(i32, f64)>>>,
}

impl Anime {
    /// Load source and target `LineString` geometries
    ///
    /// This creates two R* Trees using cached envelopes for each component
    /// line in a LineString. In addition to the envelope, the slope and
    /// index of the LineString is stored.
    pub fn load_geometries(
        source: impl Iterator<Item = geo_types::LineString>,
        target: impl Iterator<Item = geo_types::LineString>,
        distance_tolerance: f64,
        angle_tolerance: f64,
    ) -> Self {
        let mut source_lens = Vec::new();
        let mut target_lens = Vec::new();
        let source_tree = create_source_rtree(source, &mut source_lens);
        let target_tree = create_target_rtree(target, &mut target_lens, distance_tolerance);
        Self {
            distance_tolerance,
            angle_tolerance,
            source_tree,
            source_lens,
            target_tree,
            target_lens,
            matches: OnceCell::new(),
        }
    }

    pub fn find_matches(&mut self) -> Result<&mut Anime, BTreeMap<i32, Vec<(i32, f64)>>> {
        let mut matches: BTreeMap<i32, Vec<(i32, f64)>> = BTreeMap::new();
        let candidates = self
            .source_tree
            .intersection_candidates_with_other_tree(&self.target_tree);

        let crs_type = crate::CrsType::Projected;
        candidates.for_each(|(cx, cy)| {
            let xbb = cx.geom().bounding_rect();
            let ybb = cy.geom().0.bounding_rect();

            // extract cached slopes and index positions
            let (i, x_slope) = cx.data;
            let (j, y_slope) = cy.data;

            // convert calculated slopes to degrees
            let x_deg = x_slope.atan().to_degrees();
            let y_deg = y_slope.atan().to_degrees();

            // compare slopes:
            let is_tolerant = (x_deg - y_deg).abs() < self.angle_tolerance;

            // if the slopes are within tolerance then we check for overlap
            if is_tolerant {
                let xx_range = x_range(&xbb);
                let xy_range = x_range(&ybb);
                let x_overlap = overlap_range(xx_range, xy_range);
                let y_overlap = overlap_range(y_range(&xbb), y_range(&ybb));

                // if theres overlap then we do a distance based check
                // following, check that they're within distance tolerance,
                // if so, calculate the shared length
                if x_overlap.is_some() || y_overlap.is_some() {
                    // calculate the distance from the line segment
                    // if its within our threshold we include it;
                    let d = cy.geom().distance(&cx.geom());

                    // if distance is less than or equal to tolerance, add the key
                    if d <= self.distance_tolerance {
                        let shared_len = if x_slope.atan().to_degrees() <= 45.0 {
                            if x_overlap.is_some() {
                                let (p1, p2) =
                                    solve_no_y_overlap(x_overlap.unwrap(), &cx.geom(), &x_slope);

                                p1.euclidean_distance(&p2)
                            } else {
                                0.0
                            }
                        } else {
                            if y_overlap.is_some() {
                                let (p1, p2) =
                                    solve_no_x_overlap(y_overlap.unwrap(), &cx.geom(), &x_slope);
                                p1.euclidean_distance(&p2)
                            } else {
                                0.0
                            }
                        };
                        // add 1 for R indexing
                        // ensures that no duplicates are inserted. Creates a new empty vector is needed
                        let entry = matches.entry((i + 1) as i32).or_insert_with(Vec::new);
                        let j_plus_one = (j + 1) as i32;

                        if let Some(tuple) = entry.iter_mut().find(|(x, _)| *x == j_plus_one) {
                            tuple.1 += shared_len;
                        } else {
                            entry.extend(std::iter::once((j_plus_one, shared_len)));
                        }
                    }
                }
            }
        });
        self.matches.set(matches)?;
        Ok(self)
    }
}

fn create_source_rtree(
    x: impl Iterator<Item = geo_types::LineString>,
    source_lens: &mut Vec<f64>,
) -> SourceTree {
    let to_insert = x
        .enumerate()
        .flat_map(|(i, xi)| {
            let xi_len = xi.euclidean_length();
            source_lens.push(xi_len);
            let components = xi
                .lines()
                .map(|li| {
                    let slope = li.slope();
                    let env = CachedEnvelope::new(li);
                    GeomWithData::new(env, (i, slope))
                })
                .collect::<Vec<GeomWithData<_, _>>>();
            components
        })
        .collect::<Vec<_>>();

    rstar::RTree::bulk_load(to_insert)
}

fn create_target_rtree(
    y: impl Iterator<Item = geo_types::LineString>,
    target_lens: &mut Vec<f64>,
    dist: f64,
) -> TargetTree {
    let to_insert = y
        .enumerate()
        .flat_map(|(i, yi)| {
            let yi_len = yi.euclidean_length();
            target_lens.push(yi_len);
            let components = yi
                .lines()
                .map(|li| {
                    let tl = TarLine(li, dist);
                    let slope = li.slope();
                    let env = CachedEnvelope::new(tl);
                    GeomWithData::new(env, (i, slope))
                })
                .collect::<Vec<GeomWithData<_, _>>>();
            components
        })
        .collect::<Vec<_>>();

    rstar::RTree::bulk_load(to_insert)
}
