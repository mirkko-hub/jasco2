
#' Typical output from jasco_tibble()
#'
#'A dataset containing NADH absorbance traces as function of time for two proteins
#'  ("A", "B") in three conditions with 4 replicates, each. In addition four 
#'  control reactions are included. The variables are as follows:
#'
#' @format A data frame with 8127 rows and 10 variables:
#' \describe{
#'   \item{Set_ID}{time stamp revealing date-time of record}
#'   \item{Intrument}{Instruments name}
#'   \item{Exp_ID}{ID of the experiment, one per timecourse experiment}
#'   \item{Time_s}{time in s, start of individual experiment to end of experiment (0--300)}
#'   \item{Absorbance}{Absorbance as function of time per experiment, values higher 
#'     than 2 are less reliable}
#'   \item{Group}{one group per replicate}
#'   \item{FLAG}{1 or 0, flagged '1' experiments can be removed from analysis}
#'   \item{Note}{any note per experiment}
#'   \item{Label}{some label per group}
#'   \item{Protein}{Protein examined in assay}
#' }
"jasco_df"