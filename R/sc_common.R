# Copyright (C) 2020 Martin Edström

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

source("packrat/init.R")

suppressPackageStartupMessages({
  library(conflicted)
  library(dtplyr)
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
