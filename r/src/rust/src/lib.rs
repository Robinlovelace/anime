use arrow::{
    array::{ArrayData, make_array},
    datatypes::Field,
    error::ArrowError
};
use arrow_extendr::from::FromArrowRobj;
use geoarrow::{algorithm::geo::EuclideanLength, array::LineStringArray, GeometryArrayTrait};
use extendr_api::prelude::*;
use itertools::Itertools;
use std::result::Result;

pub type ErrGeoArrowRobj = ArrowError;

// wrapper functions around R functions to make converting from
// nanoarrow-geoarrow easier
pub fn infer_geoarrow_schema(robj: &Robj) -> Result<Robj, Error> {
    R!("geoarrow::infer_geoarrow_schema")
        .expect("`geoarrow` must be installed")
        .as_function()
        .expect("`infer_geoarrow_schema()` must be available")
        .call(pairlist!(robj))
}

pub fn as_data_type(robj: &Robj) -> Result<Robj, Error> {
    R!("arrow::as_data_type")
    .expect("`arrow` must be installed")
    .as_function()
    .expect("`as_data_type()` must be available")
    .call(pairlist!(robj))
}

pub fn new_field(robj: &Robj, name: &str) -> Result<Robj, Error> {
    R!("arrow::field")
        .expect("`arrow` must be installed")
        .as_function()
        .expect("`new_field()` must be available")
        .call(pairlist!(name, robj))
}

fn read_geoarrow_r(robj: Robj) -> Result<
    std::sync::Arc<dyn GeometryArrayTrait>, 
    geoarrow::error::GeoArrowError
>  {

    // extract datatype from R object
    let narrow_data_type = infer_geoarrow_schema(&robj).unwrap();
    let arrow_dt = as_data_type(&narrow_data_type).unwrap();

    // create and extract field 
    let field = new_field(&arrow_dt, "geometry").unwrap();
    let field = Field::from_arrow_robj(&field).unwrap();

    // extract array data
    let x = make_array(ArrayData::from_arrow_robj(&robj).unwrap());

    // create geoarrow array
    geoarrow::array::from_arrow_array(&x, &field)
    // ?.unwrap();
    // l?et ga = ga.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();
}

#[extendr]  
fn rnet_match_two_trees(x: Robj, y: Robj, distance_tolerance: f64, angle_tolerance: f64, is_projected: bool) -> Robj {

    let crs_type = match is_projected {
        true => rnetmatch::CrsType::Projected,
        false => unimplemented!("Geographic CRS not yet supported.")
    };

    let x = read_geoarrow_r(x).unwrap();
    let y = read_geoarrow_r(y).unwrap();

    let x = x.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();
    let y = y.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();

    let res = rnetmatch::find_candidates(
        x.iter_geo_values(), 
        y.iter_geo_values(), 
        distance_tolerance, 
        angle_tolerance, 
        crs_type
    );

    let (ks, js, shared_lens): (Vec<_>, Vec<_>, Vec<_>) = res
        .into_iter()
        .flat_map(|(k, v)| {
            let (j, shared_len): (Vec<_>, Vec<_>) = v.into_iter().unzip();
            let ks = vec![k; j.len()];
            ks.into_iter()
                .zip(j.into_iter())
                .zip(shared_len.into_iter())
                .map(|((k, j), shared_len)| (k, j, shared_len))
        })
        .multiunzip();

    data_frame!(i = ks, j = js, shared_len = shared_lens)

}

#[extendr]
fn rnet_match_two_trees_hashmap(x: Robj, y: Robj, distance_tolerance: f64, angle_tolerance: f64, is_projected: bool) -> Robj {

    let crs_type = match is_projected {
        true => rnetmatch::CrsType::Projected,
        false => unimplemented!("Geographic CRS not yet supported.")
    };

    let x = read_geoarrow_r(x).unwrap();
    let y = read_geoarrow_r(y).unwrap();

    let x = x.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();
    let y = y.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();

    let x_len = x.len();
    let mut res_list = List::new(x_len);
    
    let mut res = rnetmatch::find_candidates_hm(
        x.iter_geo_values(), 
        y.iter_geo_values(), 
        distance_tolerance, 
        angle_tolerance, 
        crs_type
    );

    for i in 0..x_len {
        let ri = res.remove(&((i + 1) as i32));
        match ri {
            Some(idx) => {
                let (source_id, shared_len): (Vec<_>, Vec<_>) = idx.into_iter().unzip();
                let _ = res_list.set_elt(i, list!(source_id = source_id, shared_len  = shared_len).into_robj());
            }
            None => {
                let _  = res_list.set_elt(i, list!(source_id = Integers::new(0), shared_len  = Doubles::new(0)).into_robj());
            },
        }
    }

    res_list.into_robj()
}


#[extendr]  
fn rnet_match_one_tree(x: Robj, y: Robj, distance_tolerance: f64, angle_tolerance: f64, is_projected: bool) -> Robj {

    let crs_type = match is_projected {
        true => rnetmatch::CrsType::Projected,
        false => unimplemented!("Geographic CRS not yet supported.")
    };

    let x = read_geoarrow_r(x).unwrap();
    let y = read_geoarrow_r(y).unwrap();

    let x = x.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();
    let y = y.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();

    let res = rnetmatch::find_candidates_one_tree(
        x.iter_geo_values(), 
        y.iter_geo_values(), 
        distance_tolerance, 
        angle_tolerance,
        crs_type
    );

    let (ks, js, shared_lens): (Vec<_>, Vec<_>, Vec<_>) = res
        .into_iter()
        .flat_map(|(k, v)| {
            let (j, shared_len): (Vec<_>, Vec<_>) = v.into_iter().unzip();
            let ks = vec![k; j.len()];
            ks.into_iter()
                .zip(j.into_iter())
                .zip(shared_len.into_iter())
                .map(|((k, j), shared_len)| (k, j, shared_len))
        })
        .multiunzip();

    data_frame!(i = ks, j = js, shared_len = shared_lens)

}

#[extendr]
fn rnet_match_vec(x: Robj, y: Robj, distance_tolerance: f64, angle_tolerance: f64, is_projected: bool) -> Robj {

    let crs_type = match is_projected {
        true => rnetmatch::CrsType::Projected,
        false => unimplemented!("Geographic CRS not yet supported.")
    };

    let x = read_geoarrow_r(x).unwrap();
    let y = read_geoarrow_r(y).unwrap();
    let n_x = x.len();

    let x = x.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();
    let y = y.as_any().downcast_ref::<LineStringArray<i32>>().unwrap().clone();

    let res = rnetmatch::find_candidates_vec(
        x.iter_geo_values(), 
        y.iter_geo_values(), 
        n_x,
        distance_tolerance, 
        angle_tolerance,
        crs_type
    );

    let (ks, js, shared_lens): (Vec<_>, Vec<_>, Vec<_>) = 
    res
        .into_iter()
        .enumerate()
        .flat_map(|(k, v)| {
            let (mut j, mut shared_len): (Vec<_>, Vec<_>) = v.into_iter().unzip();
            
            if j.len() == 0 {
                j = vec![Rint::na().inner()];
                shared_len = vec![Rfloat::na().inner()];
            }

            let ks = vec![(k + 1) as i32; j.len()];
            ks.into_iter()
                .zip(j.into_iter())
                .zip(shared_len.into_iter())
                .map(|((k, j), shared_len)| (k, j, shared_len))
        })
            .multiunzip();

    data_frame!(target_id = ks, target_id = js, shared_len = shared_lens)

}


// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod rnetmatch;
    fn rnet_match_one_tree;
    fn rnet_match_two_trees;
    fn rnet_match_two_trees_hashmap;
    fn rnet_match_vec;
}


