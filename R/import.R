# Import files from JASCO Corp., V-560, Rev. 1.00 -------------------------

#' jasco_read
#'
#' This is a function for easy import of data from the Spectrophotometer (JASCO Corp., V-560, Rev. 1.00)
#' 
#' Output is a tidy tibble containing a Set_ID (date-time of record), an Exp_ID (basename of provided file),
#' the instrument name and the actual data, which is a Absorbance reading `Absorbance` as a function of time `Time_s`.
#' 
#' @param file full path to .txt file
#' @keywords jasco
#' @importFrom magrittr %>%
#' @importFrom readr read_delim cols col_double col_character
#' @importFrom dplyr select mutate
#' @importFrom tidyr unite spread
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_sub
#' @importFrom tidyselect everything
#' @export
#' @examples
#' file <- system.file("extdata", "1.txt" , package = "jasco2", mustWork = TRUE)
#' jasco_read(file)

jasco_read <- function(file) {
  data_body <- read_delim(file,
                          delim = "\t",
                          col_names = c("Time_s", "Absorbance"),
                          col_types = cols(
                            Time_s = col_double(),
                            Absorbance = col_double()),
                          skip = 18)
  
  data_header <- read_delim(file,
                            delim = "\t",
                            col_names = c("Parameter", "Value"),
                            col_types = cols(
                              Parameter = col_character(),
                              Value = col_character()),
                            n_max = 17) %>%
    spread(., Parameter, value = Value) %>%
    select(DATE, TIME, `SPECTROMETER/DATA SYSTEM`) %>%
    unite("Set_ID", DATE, TIME, remove = FALSE)
  
  data_body %>%
    mutate(Set_ID = ymd_hms(data_header$Set_ID),
           Exp_ID = basename(file) %>% str_sub(., end = -5) %>% as.integer(),
           Instrument = data_header$`SPECTROMETER/DATA SYSTEM`) %>%
    select(., Set_ID, Instrument, Exp_ID, everything())
}


#' jasco_tibble
#'
#' This is a function for easy import of data from the Spectrophotometer (JASCO Corp., V-560, Rev. 1.00)
#' 
#' Output is a tidy tibble containing a Set_ID (date-time of record), an Exp_ID (basename of provided file),
#' the instrument name and the actual data, which is a Absorbance reading `Absorbance` as a function of time `Time_s`.
#' If 'filenames' is a character vector of multiple files, individual tibbles per file are bound by row.
#' See further options below.
#'
#' @param filenames a character vector of file pathes to Jasco .txt files;
#' each element contains the full path to one file. Can be generated
#' e.g. by using the command 'list.files(path, pattern = "*.txt", full.names = T)'
#' @param design a tibble with columns 'Exp_ID', 'Group' and optional 'FLAG'. 'Exp_ID'
#' matches 'Group' to files, 'Group' specifies replicates, e.g. by letters. If rmblank = T
#' rows are removed that match the pattern 'regex("blank", ignore_case = T)' in the 'Group' column.
#' FLAG can be used to exclude files from the analysis, that carry a FLAG - (everything that is not 'NA')
#' by setting 'rmflag = T'.
#' @param treatment a tibble that supplements 'Group' with content. In addition to 'Group'
#' it may contain as many columns as you wish! (e.g. buffer conditions, salt, pH, protein concentration, ...)
#' @param rmflag default to F; if T removes flagged values from tibble (see param 'design')
#' @param rmblank default to F; if T removes rows matching the pattern 'regex("blank", ignore_case = T)' in 'Group'.
#' @keywords jasco
#' @importFrom magrittr %>%   
#' @importFrom checkmate assert_logical assert_tibble assert_names assert_true
#' @importFrom stringr str_count regex str_which
#' @importFrom purrr map_dfr
#' @importFrom dplyr left_join anti_join filter select distinct arrange
#' 
#' @export
#' @examples
#' design <- read_csv(system.file("extdata", "design.csv", package = "jasco2", mustWork = TRUE))
#' treatment <- read_csv(system.file("extdata", "treatment.csv", package = "jasco2", mustWork = TRUE))
#' files <- system.file("extdata", paste0(1:28, ".txt") , package = "jasco2", mustWork = TRUE)
#' jasco_tibble(filenames = files, design, treatment, rmflag = FALSE, rmblank = TRUE)

jasco_tibble <- function(filenames, design, treatment, rmflag = F, rmblank = F) {
  
  # stop if rmflag not logical
  assert_logical(rmflag, len = 1)
  assert_logical(rmblank, len = 1)
  
  # stop if design is != F, but design is either no tibble or a tibble w/o Exp_ID
  if (!missing(design)) {
    assert_tibble(design)
    assert_names(names(design), must.include = c("Exp_ID"))
  }
  
  # stop if rmflag == T but no design with column 'Exp_ID' and 'FLAG' given
  if (rmflag == T) {
    assert_tibble(design)
    assert_names(names(design), must.include = c("Exp_ID", "FLAG"))
  }
  
  # stop if rmblank == T but no design with column 'Group', 'Exp_ID' given
  if (rmblank == T) {
    assert_tibble(design)
    assert_names(names(design), must.include = c("Exp_ID", "Group"))
    assert_true(str_count(design$Group, pattern = regex("blank", ignore_case = T)) %>%
                  sum() > 0)
  }
  
  # stop if treatment given w/o column 'Group' or no design given with column 'Group' and Exp_ID
  if (!missing(treatment)) {
    assert_tibble(treatment)
    assert_names(names(treatment), must.include = c("Group"))
    assert_tibble(design)
    assert_names(names(design), must.include = c("Exp_ID", "Group"))
  }
  
  df_long <- purrr::map_dfr(filenames, jasco_read)
  
  if (!missing(design)) {
    
    df_long <- df_long %>% left_join(., design, by = "Exp_ID")
  }
  
  if (!missing(treatment)) {
    
    df_long <- df_long %>% left_join(., treatment, by = "Group")
  }
  
  if (rmflag == T) {
    
    rm_flag <- df_long %>% filter(FLAG == 1) %>% select(Exp_ID) %>% distinct()
    df_long <- df_long %>% anti_join(., rm_flag, by = "Exp_ID")
  }
  
  if (rmblank == T) {
    
    rm_blank <- df_long %>% select(-Time_s, -Absorbance) %>% distinct()
    rm_blank <- rm_blank[str_which(rm_blank$Group, pattern = regex("blank", ignore_case = T)), ] %>%
      select(Exp_ID)
    df_long <- df_long %>% anti_join(., rm_blank, by = "Exp_ID")
  }
  
  df_long <- df_long %>% arrange(., Exp_ID)
  df_long
}

