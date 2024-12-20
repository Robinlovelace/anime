use anime::Anime;
use arrow::{
    array::{make_array, ArrayData},
    datatypes::Field,
    error::ArrowError,
};
use arrow_extendr::from::FromArrowRobj;
use extendr_api::prelude::*;
use geoarrow::{
    array::LineStringArray, trait_::GeometryArrayAccessor,
    GeometryArrayTrait,
};
pub type ErrGeoArrowRobj = ArrowError;

// wrapper functions around R functions to make converting from
// nanoarrow-geoarrow easier
pub fn infer_geoarrow_schema(robj: &Robj) -> Result<Robj> {
    R!("geoarrow::infer_geoarrow_schema")
        .expect("`geoarrow` must be installed")
        .as_function()
        .expect("`infer_geoarrow_schema()` must be available")
        .call(pairlist!(robj))
}

pub fn as_data_type(robj: &Robj) -> Result<Robj> {
    R!("arrow::as_data_type")
        .expect("`arrow` must be installed")
        .as_function()
        .expect("`as_data_type()` must be available")
        .call(pairlist!(robj))
}

pub fn new_field(robj: &Robj, name: &str) -> Result<Robj> {
    R!("arrow::field")
        .expect("`arrow` must be installed")
        .as_function()
        .expect("`new_field()` must be available")
        .call(pairlist!(name, robj))
}

fn read_geoarrow_r(
    robj: Robj,
) -> Result<std::sync::Arc<dyn GeometryArrayTrait>> {
    // extract datatype from R object
    let narrow_data_type = infer_geoarrow_schema(&robj).unwrap();
    let arrow_dt = as_data_type(&narrow_data_type).unwrap();

    // create and extract field
    let field = new_field(&arrow_dt, "geometry").unwrap();
    let field = Field::from_arrow_robj(&field).unwrap();

    // extract array data
    let x = make_array(ArrayData::from_arrow_robj(&robj).unwrap());

    // create geoarrow array
    let res = geoarrow::array::from_arrow_array(&x, &field).map_err(|e| e.to_string())?;
    Ok(res)
}


#[extendr]
fn init_anime(
    source: Robj,
    target: Robj,
    distance_tolerance: f64,
    angle_tolerance: f64,
) -> ExternalPtr<anime::Anime> {
    let source = read_geoarrow_r(source).unwrap();
    let target = read_geoarrow_r(target).unwrap();
    let source = source
        .as_any()
        .downcast_ref::<LineStringArray<i32, 2>>()
        .unwrap()
        .clone();
    let target = target
        .as_any()
        .downcast_ref::<LineStringArray<i32, 2>>()
        .unwrap()
        .clone();
    let mut anime = anime::Anime::load_geometries(
        source.iter_geo_values(),
        target.iter_geo_values(),
        distance_tolerance,
        angle_tolerance,
    );

    anime.find_matches().unwrap();

    ExternalPtr::new(anime)
}

#[extendr]
fn interpolate_extensive_(source_var: &[f64], anime: ExternalPtr<Anime>) -> Doubles {
    let Ok(res) = anime
        .interpolate_extensive(source_var) else {
            throw_r_error("Failed to perform extensive interpolation")
        };
    Doubles::from_values(res)
}


#[extendr]
fn interpolate_intensive_(source_var: &[f64], anime: ExternalPtr<Anime>) -> Doubles{
    let Ok(res) = anime
    .interpolate_intensive(source_var) else {
        throw_r_error("Failed to perform intensive interpolation")
    };
    Doubles::from_values(res)
}

#[derive(IntoDataFrameRow)]
struct MatchRow {
    target_id: i32,
    source_id: i32,
    shared_len: f64
}

#[extendr]
fn get_matches_(anime: ExternalPtr<Anime>) -> Robj {
    let inner = anime.matches.get().unwrap();
    let all_items = inner.into_iter().flat_map(|(idx, cands)| {
        cands.into_iter().map(|ci| {
            MatchRow {
                target_id: *idx,
                source_id: ci.index,
                shared_len: ci.shared_len   
            }
        })
    }).collect::<Vec<_>>();
    let df = Dataframe::try_from_values(all_items).unwrap();
    df.into()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod anime;
    fn init_anime;
    fn interpolate_extensive_;
    fn interpolate_intensive_;
    fn get_matches_;
}
