# generate_an_ods.R
# Copyright (C) 2021-2023 Martin Edström

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

###############################################################################
# Compile Ledger data into a typical sort of finance spreadsheet.
#
# The script can be run on a command line as follows.
#     Rscript generate_an_ods.R input.ledger output.ods EUR
#
# The optional third argument is recommended if you have multiple
# commodities, it merges them into one using ledger's -HX flag.

source("renv/activate.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

prog_exists <- function(x) {
  if (Sys.which(x) == "") FALSE else TRUE
}

if (!prog_exists("ssconvert"))
  stop("Install gnumeric for the ssconvert binary!")
if (!prog_exists("ledger"))
  stop("Install ledger!")

args <- commandArgs(TRUE)
inpath <- args[[1]]
outpath <- args[[2]]
currency <- if(length(args) >= 3) args[[3]]

# for manual testing
if (interactive()) {
  inpath <- "/home/kept/Journal/Finances/l.ledger"
  # inpath <- "2021.ledger"
  outpath <- "/tmp/secretary/output.ods"
  currency <- "SEK"
  # currency <- NULL
}

# Ask Ledger to print out a CSV for us to mangle
system2("ledger",
        c("-f", inpath, "csv", "-M", if (!is.null(currency)) "-HX", currency),
        stdout = "tmp_full.csv")

raw <- read_delim("tmp_full.csv",
                  delim = ",",
                  escape_backslash = TRUE,
                  escape_double = FALSE,
                  col_names = FALSE
                  ) %>%
  select(X1, X4, X6) %>%
  set_names(c("posted", "account", "amount")) %>%
  # treat [virtual] postings as real
  mutate(account = str_remove_all(account, "\\[|\\]")) %>%
  mutate(category = str_replace(account, "^(.*?:.*?):.*$", "\\1")) %>%
  group_by(category, posted) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(supercategory = str_replace(category, "^(.*?):.*$", "\\1"))

all_categs <- raw %>%
  pivot_wider(names_from = posted, names_sort = TRUE, values_from = amount) %>%
  arrange(category)

sums <- all_categs %>%
  group_by(supercategory) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(category = supercategory)

savings_deg <- raw %>%
  filter(str_detect(supercategory, "Income|Expenses")) %>%
  group_by(supercategory, posted) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = supercategory, values_from = amount) %>%
  mutate(Income = -Income) %>%
  transmute(posted, savings_percent = round((Income - Expenses) / Income * 100)) %>%
  pivot_wider(names_from = posted, values_from = savings_percent) %>%
  mutate(category = "Percent savings") %>%
  select(category, everything())

merged <- bind_rows(sums, tibble(.rows = 1), savings_deg, tibble(.rows = 1), all_categs)

years <- sort(unique(year(raw$posted)))

filenames <- map_chr(
  as.character(years),
  function (year) {
    filename <- str_glue("tmp_{year}.csv")
    merged %>%
      select(category, starts_with(year)) %>%
      rename_with(.cols = -"category", ~ format.Date(.x, "%b")) %>%
      write_excel_csv(filename, na = "")
    filename
  })

# Combine CSVs into an ODS spreadsheet
if (length(filenames) == 1) {
  system2("ssconvert", c(filenames, outpath))
} else {
  system2("ssconvert", c("--merge-to", outpath, rev(filenames)))
}

file.remove(list.files(pattern = "tmp_.*.csv"))

# Open the spreadsheet
if (interactive()) {
  prog <- case_when(prog_exists("gnumeric") ~ "gnumeric",
                    prog_exists("soffice") ~ "soffice",
                    prog_exists("open") ~ "open",
                    prog_exists("xdg-open") ~ "xdg-open")
  if(is.na(prog)) stop("no spreadsheet program found")
  system2(prog, outpath)
}
