source("packrat/init.R")
## packrat::init()

my_packages <- c(
  # normally distributed with R
  ## "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster", "codetools",
  ## "lattice", "mgcv", "nlme", "nnet", "rpart", "spatial", "survival",
  # my essentials
  "tidyverse", "conflicted", # "languageserver", "styler", "lintr",
  # for this project
  "lubridate", "patchwork", "usethis", "testthat"
)
missing <- setdiff(my_packages, rownames(installed.packages()))
install.packages(missing, dependencies = T, source = T)

## install.packages("tidyverse", source = T)

## # invisible(lapply(my_packages, library, character.only = TRUE))
## lapply(my_packages, function(x)
##   suppressPackageStartupMessages(library(x, character.only = TRUE)))

suppressPackageStartupMessages({
  library(conflicted)
  library(tidyverse)
  library(lubridate)
  library(patchwork)
})

as_datetime("2020-12-12 10:00:00 +0100")
as_datetime("2020-12-12T10:00:00+0100", tz = "+0100")

conflict_prefer("not", "magrittr") # use magrittr::not over testthat::not
conflict_prefer("filter", "dplyr") # use dplyr::filter over stats::filter
conflict_prefer("discard", "purrr") # use purrr::discard over scales::discard
list <- rlang::list2 # a better list()

packrat::init()
packrat::status()
packrat::restore()
packrat::bundle()
packrat::install()
packrat::unused_packages()


# It should not be necessary to average for plots, we can have multiple datapoints per day.
## mood %>%
##   group_by(date) %>%
##   summarise(mood_score = mean(mood_score)) %>%
##   full_join(wt, by = "date")

## merged <- full_join(mood, wt) %>%
##   arrange(date)

read_csv("/home/kept/Self_data/sleep.csv", col_names = c("date", "time", "sleep_hrs")) %>%
  mutate(sleep_hrs = as.period(sleep_hrs)) %>%
  mutate(sleep_block = interval(end = time, start = time - hour(sleep_hrs)))

# so what do we model?

# first out is activity, an unobserved categorical, though we can have a small
# set of true observations to start off with, and grow that over time by asking
# the user to confirm a whole day at once.

# see here a total bullshit, just to get you thinking
activity ~ weight + mood_score + sleep_hrs

# we actually arent gonna use weight, mood, sleep for predicting activity,
# probably, they are for the user's own purposes kinda. for now. well i can
# definitely use sleep and maybe mood, but later.

# bigger question: how do we use data from different datasets in a model? we do
# have continuous information on sleep so we could represent it as a boolean
# variable in any dataset: in other words, just add a column, don't add any
# rows.

# but say a measurement on mood was taken at 11:34 and we want to model
# soemthing at 11:37. we have probably a huge dataset of buffer-changes, and if
# there is a "mood" variable, it will mostly be NAs. Or even all NA, since no
# buffer change will coincide exactly with the measurement.


d <- read_csv("/home/kept/Self_data/buffers.csv",
              col_names = c("uuid", "buf_name", "file", "mode", "created")) %>%
  mutate(created = as_datetime(created))
d

d2 <- read_csv("/home/kept/Self_data/buffer-focus.csv", col_names = c("focused_in", "uuid")) %>%
  mutate(focused_in = as_datetime(focused_in)) %>%
  mutate(activity = factor("Unknown",
                           levels = c("Work", "Study", "Sleep", "Downtime", "Unknown"))) %>%
  mutate(focused_block = interval(start = focused_in, end = lead(focused_in))) %>%
  left_join(d)
d2 %>% print(width = 200)

# This is also an interesting data view.
nest(d2, -uuid)

# We're at first going to go purely by priors: the data are irrelevant, they do
# not influence the eoutcome. We're just making guesses based on the data, which
# is why we need the data, but it's not a statistical model per se without some
# samples of "activity".
d2 %>%
  lm(activity ~ focused_in)
