source("packrat/init.R")

suppressPackageStartupMessages({
  library(conflicted)
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(ggthemes)
  library(testthat)
})

conflict_prefer("not", "magrittr") # use magrittr::not over testthat::not
conflict_prefer("filter", "dplyr") # use dplyr::filter over stats::filter
conflict_prefer("discard", "purrr") # use purrr::discard over scales::discard
list <- rlang::list2 # a better list()
c <- vctrs::vec_c # a stricter c() that doesn't do double duty as unname()
