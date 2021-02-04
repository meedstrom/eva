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

source("common.R")
#theme_set(theme_solarized(light = FALSE))
theme_set(theme_hc(style = "darkunica"))

WIDTH <- 10
WIDTH <- as.numeric(commandArgs(TRUE)[[1]])
BY <- -0.1
BY <- as.numeric(commandArgs(TRUE)[[2]])

#wt <- read_csv("/home/kept/Self_data/weight.csv", col_names = c("date", "weight_kg"))

wt <- read_tsv("/home/kept/Self_data/weight.tsv",
               col_names = c("posted", "weight_kg")) %>%
  mutate(posted = as_datetime(posted)) %>%
  mutate(weighed = as_date(posted))

projected_wt <-
  tibble(weight_kg = seq(from = 85, to = 59, by = BY)) %>%
  mutate(weighed = today() + days(row_number())) %>%
  mutate(note = if_else(mday(weighed) == 1, weight_kg, NULL)) # for geom_text

ggplot(as_tibble(wt), aes(weighed, weight_kg)) +
  ylim(75, 86) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b", # %B for full month name
               limits = c(ymd("2020-05-15"), today() + months(2))) +
  labs(y = NULL, x = NULL) +
  geom_point() +
  geom_point(data = projected_wt, color = "blue") +
  geom_vline(xintercept = ymd("2020-06-01") + months(0:12), color = "#555555") +
  geom_text(data = projected_wt, aes(weighed, weight_kg, label = note))

ggsave("/tmp/secretary/sc_plot1.png",
       height = 5,
       width = WIDTH,
       units = "cm",
       dpi = 96)

mood <- read_tsv("/home/kept/Self_data/mood.tsv",
                 col_names = c("time", "mood_score", "mood_desc")) %>%
  mutate(time = as_datetime(time)) %>%
  drop_na(mood_score)
mood

ggplot(mood, aes(time, mood_score, label = mood_desc)) +
  scale_x_datetime(date_breaks = "2 days",
                   ## date_labels = "%B",
                   limits = c(now() - days(7),
                              now() + days(1))) +
  ylim(1, 5) +
  labs(y = NULL, x = NULL) +
  geom_point() +
  geom_text(vjust = 2)

ggsave("/tmp/secretary/sc_mood.png",
       height = 5,
       width = WIDTH,
       units = "cm",
       dpi = 96)

#combined <- plot1 / plot2
#ggsave(combined, "")
