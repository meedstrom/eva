source("sc_common.R")
## packrat::init()

my_packages <- c(
  # normally distributed with R
  ## "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster", "codetools",
  ## "lattice", "mgcv", "nlme", "nnet", "rpart", "spatial", "survival",
  # my essentials
  "tidyverse", "conflicted", # "languageserver", "styler", "lintr",
  # for this project
  "lubridate", "patchwork", "usethis", "testthat", "ggthemes"
)
missing <- setdiff(my_packages, rownames(installed.packages()))
install.packages(missing, dependencies = T, source = T)

fast_strptime("2020-12-12 10:00", "%Y-%m-%d %H:%M")
parse_date_time("2020-12-12 10:00", "YmdHM")
parse_date_time2("2020-12-12 10:00", "YmdHM", tz = "UTC+1")
as_datetime("2020-12-12 10:00", format = "%Y-%m-%d %H:%M" tz = "+0100")
as_datetime("2020-12-12 10:00:00 +0100")
as_datetime("2020-12-12T10:00:00+0100", tz = "+0100")


# It should not be necessary to average for plots, we can have multiple datapoints per day.
## mood %>%
##   group_by(date) %>%
##   summarise(mood_score = mean(mood_score)) %>%
##   full_join(wt, by = "date")

## merged <- full_join(mood, wt) %>%
##   arrange(date)


%>%
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

d$buf_name

# This is where 80% of the magic happens. When unsure, just don't classify,
# leave a NA. Also, the categories don't need to map to your clock categories.
# Ideally the categories here are slighly more granular. Imagine you change the
# clock categories later, these should remain meaningful to the modelling
# machine because they carve reality at the joints. If you have time to kill,
# see https://www.greaterwrong.com/tag/thingspace for inspiration. The category
# names (after the tilde ~) are just suggestive and meaningless. Consider
# giving them truly meaningless names, like "fnord" or "1", "2"... so you don't
# fool yourself.
d2 <- d %>%
  mutate(buf_categ = case_when(
    str_detect(buf_name, "help|describe") ~ "help",
    str_detect(buf_name, "Agenda|Org") ~ "org",
    str_detect(buf_name, "\\*eww") ~ "browsing",
    str_detect(buf_name, "\\*EXWM Firefox") ~ "browsing",
    str_detect(buf_name, "\\*EXWM Blender") ~ "fnord",
    str_detect(buf_name, "\\*timer-list|\\*Warnings|\\*Elint") ~ "emacs",
    str_detect(file, "\\.org$") ~ "org",
    str_detect(file, "\\.el$") ~ "emacs",
    str_detect(file, "\\.csv$") ~ "coding-or-studying",
    str_detect(file, "\\.tsv$") ~ "coding-or-studying",
    str_detect(file, "stats.org$") ~ "studying",
    str_detect(file, "/home/kept/Emacs/conf-vanilla") ~ "emacs-yak-shaving",
    str_detect(file, "/home/kept/Emacs/conf-doom") ~ "emacs-yak-shaving",
    str_detect(file, "/home/kept/Emacs/conf-common") ~ "emacs-yak-shaving",
    str_detect(file, "/home/kept/Emacs") ~ "emacs",
    str_detect(file, "/home/kept/Code") ~ "coding",
    str_detect(file, "/home/kept/Guix") ~ "OS",
    str_detect(file, "/home/kept/Dotfiles") ~ "OS",
    str_detect(file, "/home/kept/Private dotfiles") ~ "OS",
    str_detect(file, "/home/kept/Coursework") ~ "studying",
    str_detect(file, "/home/kept/Flashcards") ~ "studying",
    str_detect(file, "/home/kept/Diary") ~ "org",
    str_detect(file, "/home/kept/Journal") ~ "org",
    str_detect(file, "/home/me/bin") ~ "coding",
    str_detect(file, "/home/me/\\.") ~ "OS",
    str_detect(mode, "emacs-lisp-mode|lisp") ~ "emacs",
    str_detect(mode, "^org") ~ "org",
    str_detect(mode, "ess") ~ "coding"
  ))

# TODO: Make the intervals align as tested by int_aligns(). I want to be able
#       to ask what buffer was active at a specific time and not risk hitting
#       one of the gaps. This can be done either by including the subsecond
#       intervals we deleted, or extending their neighbors.
d3 <- read_csv("/home/kept/Self_data/buffer-focus.csv",
         col_names = c("focused_in", "uuid")) %>%
  mutate(focused_in = as_datetime(focused_in)) %>%
  mutate(activity = factor("Unknown",
                           levels = c("Work", "Study", "Sleep", "Downtime", "Unknown"))) %>%
  mutate(focused_block = interval(start = focused_in, end = lead(focused_in) - seconds(1))) %>%
  filter(int_length(focused_block) > 0) %>%
  left_join(d2)

test_that("there's no overlap", {
  block <- as_tibble_col(d3$focused_block)
  overlap <- map2(block, lead(block), int_overlaps)
  overlap_without_last_na <- unlist(unname(overlap)) %>% head(-1)
  expect_false(
    any(overlap_without_last_na)
  )
})

d3 %>%
  mutate()

d3 %>% print(width = 200)

# This is also an interesting data view.
nest(d3, -uuid)

# We're at first going to go purely by priors: the data are irrelevant, they do
# not influence the eoutcome. We're just making guesses based on the data, which
# is why we need the data, but it's not a statistical model per se without some
# samples of "activity".
lm(activity ~ focused_in, data = d2)
