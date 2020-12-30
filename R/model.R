#' jasco_lm
#'
#' This is a wrapper for lm and a helper function for jasco_extract_lm.
#' 
#' For a given tibble, this function allows to specify response variable and predictor
#' in order to retrieve the coefficients of the linear model.
#'
#' @param response unquoted name of the response variable found in the tibble
#' @param predictor unquoted name of the response variable found in the tibb
#' @param data a tibblele
#' @keywords jasco
#' @importFrom rlang !! := ensym expr
#' @importFrom stats lm
#' @export
#' @examples
#' jasco_lm(tibble::tibble(a = 1:10, b = seq(10, 100, 10)), response = b, predictor = a)

jasco_lm <- function(df, response, predictor, ...){
  response <- ensym(response)
  predictor <- ensym(predictor)
  lm(expr(!!response ~ !!predictor), df)
}

#' jasco_extract_lm
#'
#' This is a wrapper for lm and a helper function for jasco_extract_lm.
#' 
#' For a given tibble, this function allows to ...
#'
#' @param data a tibble with spectrophotometer data, eg. output from jasco_tibble
#' @param response unquoted name of the response variable found in the tibble
#' @param predictor unquoted name of the predictor variable found in the tibble
#' @keywords jasco
#' @importFrom magrittr %>% 
#' @importFrom dplyr mutate
#' @importFrom rlang !! enquo
#' @importFrom tidyr nest
#' @importFrom purrr map map_dbl
#' @importFrom broom glance tidy augment
#' @return tibble, that includes parameters and statistics of the fitted linear 
#'   model using broom::glance, broom::tidy and broom::augment, nested per experiment.
#'   Metadata stated per experiment is maintained.
#' @export
#' @examples
#' 
#' # this is a typical output from jasco_tibble
#' jasco_df
#' 
#' jasco_df %>%
#'   jasco_extract_lm(., response = Absorbance, predictor = Time_s, min = 75, max = 175)
#'
#' # Conversion from Absorbance to NADH concentration (mol/l) is optional
#' jasco_df %>%
#'   convert_absorbance(., "NADH") %>%
#'   jasco_extract_lm(., response = NADH, predictor = Time_s, min = 75, max = 175)

jasco_extract_lm <- function(data, response, predictor, min, max, ...){
  response <- enquo(response)
  predictor <- enquo(predictor)
  
  data <- data %>%
    filter(., !!predictor < max & !!predictor > min) %>%
    nest(c(!!predictor, !!response, Absorbance))

  data %>%
    mutate(
      mod = map(data, jasco_lm, predictor = !!predictor, response = !!response),
      glance = map(mod, glance),
      tidy = map(mod, tidy),
      augment = map(mod, augment),
      rsq = glance %>% map_dbl("r.squared")
  )
}


#' jasco_get_turnover
#'
#' sentence here
#' 
#' For a given tibble, this function allows to ...
#'
#' @param .data a tibble, minimal column `rate_avg`
#' @param .protein_uM final protein concentration in assay
#' @param .unit current time scale; one of h = hour, min = minutes, sec = seconds, msec = milliseconds
#' @param .factor coupling factor; eg. for ATPase assay NADH decline is monitored:
#' each oxidized NADH molecule reports the hydrolysis of one molecule of ATP; hence, coupling factor is `-1`
#' @keywords jasco
#' @importFrom magrittr %>% %$%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter
#' @importFrom rlang !! enquo
#' @export
#' @examples
#' jasco_get_turnover()

jasco_get_turnover <- function(.rate, .protein_uM, .unit, .factor) {
  unit <- enquo(.unit)
  
  time <- tibble(unit = c("h", "min", "sec", "msec"),
                 calc = c(1/60, 1, 60, 3600 ))
  calc <- time %>% filter(unit == !!unit) %$% calc
  (((.rate / (.protein_uM * 1e-06)) * calc) * .factor)
}


#' jasco_extract_params
#'
#' extracts turnover of ATP molecules per protein (min^-1). For input, see examples or README.
#' 
#' @param .data a tibble, minimal column `rate_avg`
#' @param .bgcorr optional background correction (subtraction); NADH oxidation in absence of protein
#' @param .protein_uM final protein concentration in assay
#' @param .unit current time scale; one of h = hour, min = minutes, sec = seconds, msec = milliseconds
#' @param .factor coupling factor; eg. for ATPase assay NADH decline is monitored:
#' each oxidized NADH molecule reports the hydrolysis of one molecule of ATP; hence, coupling factor is `-1`
#' @param .group_vars grouping variable for summary report (eg. 'Group' - for grouping replicates)
#' @keywords jasco
#' @importFrom checkmate assert check_logical check_double check_choice check_character
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr mutate
#' @importFrom rlang !! !!! enquo enquos quo_name
#' @importFrom tidyr nest unnest
#' @importFrom dplyr group_by summarise
#' @importFrom purrr map map_dbl
#' @importFrom broom glance tidy augment
#' @return list of two tibbles; 'data' and 'summary'. 'data' contains all columns,
#'   including model statistics for evaluation. 'summary' is a grouped summary of
#'   turnover(min^-1) ('turnover_avg') and sd ('turnover_sd')
#' @export
#' @examples
#' 
#' # typical output from jasco_tibble()
#' jasco_df
#' 
#' jasco_df %>%
#'   convert_absorbance(., "NADH") %>%
#'   jasco_extract_lm(., response = NADH, predictor = Time_s, min = 75, max = 175) %>%
#'   jasco_extract_params(., .bgcorr = FALSE, .protein_uM = 6, .unit = "sec", .factor = -1, Group)

jasco_extract_params <- function(.data, .bgcorr, .protein_uM, .unit, .factor, ...){
  
  assert(
    check_names(names(.data), must.include = "tidy"),
    check_names(.data %$% tidy %>% extract2(1) %>% names(), must.include = "estimate"),
    check_logical(.bgcorr, len = 1, any.missing = FALSE),
    check_double(.protein_uM, lower = 0.000001, any.missing = FALSE, len = 1),
    check_choice(.unit, c("h", "min", "sec", "msec")),
    check_character(.unit, any.missing = FALSE, len = 1),
    check_double(.factor, len = 1, any.missing = FALSE),
    combine = "and"
  )
  
  group_vars <- enquos(..., .named = TRUE)

  df <- .data %>%
    unnest(., tidy) %>%
    filter(., term != "(Intercept)") %>%
    mutate(rate = estimate)
  
  if (.bgcorr == TRUE) {
    blank_id <- df[str_which(df$Group, pattern = regex("blank", ignore_case = T)), ] %$% Exp_ID
    blank_rate <- df %>% filter(., Exp_ID %in% blank_id) %>% summarise(., rate = mean(rate)) %$% rate
    df <- df %>% filter(! Exp_ID %in% blank_id) %>% mutate(., rate = rate - blank_rate)
  }
  
  df <- df %>% mutate(turnover = rate %>% map_dbl(., jasco_get_turnover, .protein_uM, .unit, .factor))
  
  # Modify the names of the list of quoted dots
  names(group_vars) <- paste0("groups_", names(group_vars))
  
  df_summary <- df %>%
    group_by(!!!group_vars) %>%  # Unquote-splice as usual
    summarise(.,
              rate_avg = mean(rate),
              rate_sd = sd(rate),
              turnover_avg = mean(turnover),
              turnover_sd = sd(turnover))
  
  list(data = df,
       summary = df_summary)
  
}

# The end -----------------------------------------------------------------

# jasco_get_turnover2 <- function(.data, .protein_uM, .unit, .factor) {
#   unit <- enquo(.unit)
#   
#   time <- tibble(unit = c("h", "min", "sec", "msec"),
#                  calc = c(1/60, 1, 60, 3600 ))
#   calc <- time %>% filter(unit == !!unit) %$% calc
#   .data %>% mutate(., `turnover_min^-1` = ((rate_avg / (.protein_uM * 1e-06)) * calc) * .factor,
#                    turnover_sd = ((rate_sd / (.protein_uM * 1e-06)) * calc) * .factor)
# }
# 
# jasco_get_turnover3 <- function(.data, .protein_uM, .unit, .factor) {
#   unit <- enquo(.unit)
#   
#   time <- tibble(unit = c("h", "min", "sec", "msec"),
#                  calc = c(1/60, 1, 60, 3600 ))
#   calc <- time %>% filter(unit == !!unit) %$% calc
#   .data %>% mutate(., `turnover_min^-1` = ((rate / (.protein_uM * 1e-06)) * calc) * .factor)
# }


# # hadley example!
# grouped_mean2 <- function(.data, .summary_var, ...) {
#   summary_var <- enquo(.summary_var)
#   
#   # Quote the dots with default names
#   group_vars <- enquos(..., .named = TRUE)
#   
#   summary_nm <- as_label(summary_var)
#   summary_nm <- paste0("avg_", summary_nm)
#   
#   # Modify the names of the list of quoted dots
#   names(group_vars) <- paste0("groups_", names(group_vars))
#   
#   .data %>%
#     group_by(!!!group_vars) %>%  # Unquote-splice as usual
#     summarise(!!summary_nm := mean(!!summary_var))
# }
