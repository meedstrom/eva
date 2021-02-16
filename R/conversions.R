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


# just useful snippets I've used with modification as I was changing my mind
# about data formats

# TODO: make them more useful, like a series of "how do I..."

try(source("renv/activate.R"), silent = TRUE)

# TODO: use datetime, not just date, so we can make multiple samples in a day
mood <- read_tsv("/home/kept/Self_data/mood.tsv",
                 col_names = c("time", "mood_score", "mood_desc")) %>%
  mutate(time = parse_date_time2(time, "zYmdHM")) %>%
  mutate(unix = map_int(time, ~ as.integer(difftime(.x, origin, units = "secs")))) %>%
  select(unix, time, mood_score, mood_desc)

write_tsv(mood, "/home/kept/Self_data/mood2.tsv")
mood

difftime(today(), origin, units = "secs")
as_datetime(origin) - today()
origin()
system.time(today())
mood

fast_strptime("2020-12-12 10:00", "%Y-%m-%d %H:%M")
parse_date_time("2020-12-12 10:00", "YmdHM")
parse_date_time2("2020-12-12 10:00", "YmdHM", tz = "UTC+1")
as_datetime("2020-12-12 10:00", format = "%Y-%m-%d %H:%M", tz = "+0100")
as_datetime("2020-12-12 10:00:00 +0100")
as_datetime("2020-12-12T10:00:00+0100", tz = "+0100")


  mutate(date = parse_date_time2("2020-12-12 10:00 +0100", "YmdHMz"))


read_csv("/home/kept/Self_data/sleep.csv", col_names = c("date", "time", "sleep_hrs")) %>%
  mutate(sleep_hrs = as.period(sleep_hrs)) %>%
  mutate(sleep_block = interval(end = time, start = time - hour(sleep_hrs)))

mood <- read_tsv("/home/kept/Self_data/mood.tsv",
                 col_names = c("time", "mood_score", "mood_desc")) %>%
  mutate(time = parse_date_time2(time, "zYmdHM")) %>%
  mutate(unix = map_int(time, ~ as.integer(difftime(.x, origin, units = "secs")))) %>%
  select(unix, time, mood_score, mood_desc)

read_csv("/home/kept/Self_data/weight2.csv", col_names = c("time", "weight_kg")) %>%
  mutate(time = parse_date_time2(time, "zYmdHM")) %>%
  mutate(unix = map_int(time, ~ as.integer(difftime(.x, origin, units = "secs")))) %>%
  select(unix, time, weight_kg) %>%
  mutate(time = str_glue("{year(time)}-{month(time, label = TRUE, abbr = TRUE)}-{str_pad(day(time), 2, pad = 0)} 08:00")) %>%
  write_tsv("/home/kept/Self_data/weight.tsv")


read_csv("/home/kept/Self_data/idle.csv", col_names = c("time", "idle_minutes")) %>%
  mutate(time = as_datetime(time)) %>%
  mutate(time = str_c(as.character(time), " +0000")) %>%
  write_tsv("/home/kept/Self_data/idle.tsv")

write_tsv("/home/kept/Self_data/idle.tsv")


read_tsv("/home/kept/Self_data/mood.tsv",
         col_names = c("unix", "time", "mood_score", "mood_desc")) %>%
  mutate(time = as_datetime(unix) + hours(1)) %>%
  mutate(time = str_c(as.character(time), " +0100")) %>%
  select(-unix)
  write_tsv("/home/kept/Self_data/mood2.tsv")


read_tsv("/home/kept/Self_data/weight.tsv",
         col_names = c("unix", "time", "weight_kg")) %>%
  mutate(time = as_datetime(unix) + hours(1)) %>%
  mutate(time = str_c(as.character(time), " +0100")) %>%
  select(-unix) %>%
  write_tsv("/home/kept/Self_data/weight.tsv")

read_csv("/home/kept/Self_data/sleep.csv",
         col_names = c("posted", "time_awoken", "sleep_minutes")) %>%
  mutate(posted = as_datetime(posted) + hours(1)) %>%
  mutate("date_awoken" = date(posted)) %>%
  select(posted, date_awoken, time_awoken, sleep_minutes) %>%
  write_tsv("/home/kept/Self_data/sleep.tsv")
