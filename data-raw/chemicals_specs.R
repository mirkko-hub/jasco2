library(tidyverse)

chemicals_specs <- tibble(Chemical = c("NADH", "ATP"), coef = c(6220, 15400), lambda = c(340, 260))
use_data(chemicals_specs, internal = TRUE, overwrite = TRUE)
