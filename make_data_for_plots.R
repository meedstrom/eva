# Copyright (C) 2020-2021 Martin Edström

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


# On a fresh install, this will take time because it installs packages. If
# there are problems, of course you can forgo renv and use your system-wide R
# packages. Just comment this out and call install.packages() as needed.
source("renv/activate.R")

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

# Projected daily weight loss. Sobering to see how slow the result...
daily_change <- -0.1

if (!dir.exists("/tmp/secretary"))
  dir.create("/tmp/secretary")

wt <- read_tsv("/home/kept/Self_data/weight.tsv",
               col_names = c("posted", "weighed", "weight_kg")) %>%
  mutate(weighed = as_date(weighed)) %>%
  select(-posted) %>%
  write_delim("/tmp/secretary/weight.dat", col_names = FALSE)

projected_wt <-
  tibble(weight_kg = seq(from = tail(wt$weight_kg, 1),
                         to = 60,
                         by = daily_change)) %>%
  mutate(weighed = today() + days(row_number())) %>%
  select(weighed, weight_kg) %>%
  write_delim("/tmp/secretary/weight_projected.dat", col_names = FALSE)

# --------------

mood <- read_tsv("/home/kept/Self_data/mood.tsv",
                 col_names = c("posted", "time", "mood_score", "mood_desc")) %>%
  mutate(time = as_datetime(time)) %>%
  drop_na(mood_score) %>%
  select(-posted) %>%
  write_delim("/tmp/secretary/mood.dat", col_names = FALSE)

mood$mood_desc %>%
  write_lines("/tmp/secretary/mood_desc.txt")