# Copyright (C) 2020 Martin Edström

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

install_missing_packages <- function() {
    installed <- rownames(installed.packages())
    recommends <- c(
      # normally included with R (the recommended distribution)
      "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster", "codetools",
      "lattice", "mgcv", "nlme", "nnet", "rpart", "spatial", "survival")
    custom <- c(
      "conflicted", "testthat", "tidyverse", "lubridate", "ggthemes")
    missing <- setdiff(c(recommends, custom), installed)

    if (length(missing) > 0) {
      message("Installing: ", paste(missing, collapse = ", "))
      install.packages(missing)
    }
}

if (any(grepl("docker", suppressWarnings(try(readLines("/proc/self/cgroup"),
                                             silent = TRUE))))) {
  message("Am in Docker container, so installing packages system-wide.")
  install_missing_packages()
} else if (!file.exists("packrat/init.R")) {
  warning("No packrat directory! When you load packages, ",
          "they will probably be from your system-wide installation. ",
          "Run install_missing_packages() if this is fine with you, otherwise ",
          "\n    install.packages(\"packrat\"); packrat::init() \n",
          "to keep it directory-local, and try again.")
  return()
} else {
  source("packrat/init.R")
  install_missing_packages()
}

suppressPackageStartupMessages({
  library(conflicted)
  library(tidyverse)
  library(lubridate)
  library(ggthemes)
  library(testthat)
})

conflict_prefer("not", "magrittr") # use magrittr::not over testthat::not
conflict_prefer("filter", "dplyr") # use dplyr::filter over stats::filter
conflict_prefer("discard", "purrr") # use purrr::discard over scales::discard
