use geo_types::{Line, Point, Rect};
use std::ops::Range;

// TODO for handling geographic CRS
// calculate the distance from the top left to the bottom left corners
pub(crate) fn x_range(rect: &Rect) -> Range<f64> {
    rect.min().x..rect.max().x
}

// TODO for handling geographic CRS
// calculate the distance from the top left to the top right corners
pub(crate) fn y_range(rect: &Rect) -> Range<f64> {
    rect.min().y..rect.max().y
}
pub(crate) fn overlap_range(r1: Range<f64>, r2: Range<f64>) -> Option<Range<f64>> {
    if r1.end < r2.start || r2.end < r1.start {
        None
    } else {
        Some(r1.start.max(r2.start)..r1.end.min(r2.end))
    }
}

// When x range is known but y range is not, we need to solve for start and end points
// of the line segment
pub(crate) fn solve_no_y_overlap(x_overlap: Range<f64>, x: &Line, slope: &f64) -> (Point, Point) {
    let (known_x, known_y) = x.points().0.x_y();
    let b = known_y - (slope * known_x); // Corrected calculation of b

    let y1 = (slope * x_overlap.start) + b;
    let y2 = (slope * x_overlap.end) + b;
    let p1 = Point::new(x_overlap.start, y1);
    let p2 = Point::new(x_overlap.end, y2);
    (p1, p2)
}

pub(crate) fn solve_no_x_overlap(y_overlap: Range<f64>, x: &Line, slope: &f64) -> (Point, Point) {
    let (known_x, known_y) = x.points().0.x_y();
    let b = known_y - (slope * known_x); // Corrected calculation of b

    // create bindings to x vars that will be set in if statement
    let x1;
    let x2;

    // handle undefined slope
    if slope.is_infinite() || slope.is_nan() {
        // Assign a constant value to x1 and x2
        x1 = known_x;
        x2 = known_x;
    } else {
        x1 = (y_overlap.start - b) / slope;
        x2 = (y_overlap.end - b) / slope;
    }
    let p1 = Point::new(x1, y_overlap.start);
    let p2 = Point::new(x2, y_overlap.end);
    (p1, p2)
}
