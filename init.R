# Copyright (C) 2021 Martin Edström

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

# ------------------------------------------------------------------------

source("renv/activate.R")

# On a fresh install, this will take time because it installs packages. If
# there are problems, of course you can forgo renv and use your system-wide R
# packages. Just comment this out and call install.packages() as needed.
renv::hydrate()

# Idempotent -- the first time the R process sources us, this blurb will be a
# big cause of any delay. After that it's instant because there's nothing to
# do.
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
})
