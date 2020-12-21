# daily_plot.R
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
source("sc.R")

wt <- read_csv("/home/kept/Self_data/weight.csv", col_names = c("date", "weight_kg"))
mood <- read_csv("/home/kept/Self_data/mood.csv", col_names = c("date", "mood_desc", "mood_score"))

plot1 <- ggplot(wt, aes(date, weight_kg)) +
  ylim(70, 86) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               limits = c(ymd("2020-05-01"), today() + months(1))) +
  geom_point()

plot2 <- ggplot(mood, aes(date, mood_score)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               limits = c(ymd("2020-05-01"), today() + months(1))) +
  geom_point()



combined <- plot1 / plot2
ggsave(combined, "")
