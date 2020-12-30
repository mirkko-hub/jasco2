#' convert_absorbance
#'
#' given a tibble containing a column 'Absorbance', the concentration of a chemical
#'   of which the Absorbance is measured is calculated and attached to the tibble.
#'   Userprovided coefficients return the calculated concentration in corresponding
#'   units - If the coefficient is one of the chemicals provided in 'chemical_specs.R'
#'   the calculated concentration is stated in mol/l .
#'
#' @param data a tibble with at least one column 'Absorbance'.
#' @param coef either the name of any chemical found in `chemicals_specs` or a
#' user-provided appropriate absorption coefficient (a named vector, eg. `setNames(7282, "chemical1")`)
#' @keywords jasco
#' @importFrom checkmate assert check_tibble check_names check_count check_character check_subset check_number assert_subset
#' @importFrom magrittr %>% %$%
#' @importFrom rlang !! :=
#' @importFrom dplyr mutate filter
#' @export
#' @examples
#' 
#' # this is a typical output from jasco_tibble
#' jasco_df
#' 
#' jasco_df %>%
#'   convert_absorbance(., "NADH")
#'   
#' jasco_df %>%
#'   convert_absorbance(., coef = setNames(7282, "chemical XYZ"))

convert_absorbance <- function(data, coef) {
  
  assert(
    check_tibble(data),
    check_names(names(data), must.include = "Absorbance"),
    combine = "and"
  )
  
  
  assert(
    check_character(coef, len = 1),
    assert(
      check_count(coef, positive = TRUE),
      check_character(names(coef), len = 1),
      combine = "and"
    ),
    combine = "or"
  )
  
  # # For whatever reason this does not work, first assertion has to be a simple one layered check,
  # # additional check is included in further processing.
  # assert(
  #   assert(
  #     check_character(coef, len = 1),
  #     check_subset(coef, choices = chemicals_specs$Chemical),
  #     combine = "and"
  #   ),
  #   assert(
  #     check_count(coef, positive = TRUE),
  #     check_character(names(coef), len = 1),
  #     combine = "and"
  #   ),
  #   combine = "or"
  # )
  
  if (is.character(coef) == TRUE) {
    assert_subset(coef, choices = chemicals_specs$Chemical)
    coef_value <- filter(chemicals_specs, Chemical == !! coef) %$% coef
    coef_name <- filter(chemicals_specs, Chemical == !! coef) %$% Chemical
  }
  
  else {
    coef_value <- coef
    coef_name <- names(coef)
  }

  data %>% mutate(., !! coef_name := (Absorbance / coef_value))
}


