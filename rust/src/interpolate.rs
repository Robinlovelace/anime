use crate::{Anime, AnimeError};

/// Intensive or Extensive Interpolation
///
/// Intensive interpolation weights the attribute by the
/// shared length divided by the length of the source geometry.
///
/// Extensive interpolation weights the attribute by the shared
/// length divided by the length of the target geometry.
pub enum Tensive {
    In,
    Ex,
}

impl Anime {
    /// Perform numeric attribute interpolation
    pub fn interpolate(&self, var: &[f64], tensive: Tensive) -> Result<Vec<f64>, AnimeError> {
        match tensive {
            Tensive::In => self.interpolate_intensive(var),
            Tensive::Ex => self.interpolate_extensive(var),
        }
    }

    pub(crate) fn interpolate_extensive(&self, var: &[f64]) -> Result<Vec<f64>, AnimeError> {
        let nv = var.len();
        let n_tar = self.target_lens.len();

        eprintln!("var len: {:?}\nsource_lens: {:?}", nv, n_tar);

        if nv != n_tar {
            return Err(AnimeError::IncorrectLength);
        }

        // shared length divided by the y length
        let matches = self.matches.get().ok_or(AnimeError::MatchesNotFound)?;

        let minfo = matches
            .iter()
            .map(|(_, matches)| {
                matches.iter().fold(0.0, |acc, mi| {
                    dbg!(&mi);
                    let tar_idx = mi.index as usize;
                    let wt = mi.shared_len / self.target_lens.get(tar_idx).unwrap();
                    let v = var[tar_idx] * wt;
                    acc + v
                })
            })
            .collect::<Vec<f64>>();

        Ok(minfo)
    }

    pub(crate) fn interpolate_intensive(&self, var: &[f64]) -> Result<Vec<f64>, AnimeError> {
        let nv = var.len();
        let n_tar = self.source_lens.len();

        if nv != n_tar {
            return Err(AnimeError::IncorrectLength);
        }

        // shared length divided by the y length
        let matches = self.matches.get().ok_or(AnimeError::MatchesNotFound)?;

        let minfo = matches
            .iter()
            .map(|(i, matches)| {
                let i = *i as usize;
                let source_len_i = self.source_lens.get(i).unwrap();
                matches.iter().fold(0.0, |acc, mi| {
                    let tar_idx = mi.index as usize;
                    let wt = mi.shared_len / source_len_i;
                    let v = var[tar_idx] * wt;
                    acc + v
                })
            })
            .collect::<Vec<f64>>();

        Ok(minfo)
    }
}

// rnet_aggregate_extensive <- function(
//     x, y, matches, ...,
//     y_len = as.numeric(sf::st_length(y))
//   ) {
//   # capture variables
//   vars <- rlang::ensyms(...)
//   # get var-names
//   var_names <- vapply(vars, rlang::as_string, character(1))
//   # TODO validate variables are in y before subsetting
//   # extract j index
//   j <- matches$j
//   # subset vars by j to get ij pairs
//   ij <- rlang::set_names(lapply(var_names, \(.x) y[[.x]][j]), var_names)
//   # combine into 1 df
//   dplyr::bind_cols(matches, ij) |>
//     dplyr::mutate(
//       wt = shared_len / as.numeric(y_len[j])
//     ) |>
//     dplyr::group_by(i) |>
//     dplyr::summarise(dplyr::across(
//       -all_of(c("j", "shared_len", "wt")),
//       ~ sum(.x * wt, na.rm = TRUE)
//     ))
// }
