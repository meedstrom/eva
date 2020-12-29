# just useful snippets I've used with modification as I was changing my mind
# about data formats

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
  select(-unix) %>%
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
