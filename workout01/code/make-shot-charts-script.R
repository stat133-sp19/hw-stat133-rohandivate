# Title: Shot Charts Generation Script
# Description: This script uses the coordinates of the shot data to create a graph of shot locations on the NBA court for each player.
# Inputs: This script takes in the five csv files of shots data from the five GSW players. It also takes in an image of an nba court.
# Outputs: This script outputs 5 images of shot charts for the five players and a pdf and png version of a combined chart with all players.


library("jpeg")
library("grid")
library("ggplot2")

court_file <- "../images/nba-court.jpg"

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()

green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

klay_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

ggsave(filename = "../images/andre-iguodala-shot-chart.pdf", plot = iguodala_shot_chart, width = 6.5, height = 5)
ggsave(filename = "../images/draymond-green-shot-chart.pdf", plot = green_shot_chart, width = 6.5, height = 5)
ggsave(filename = "../images/kevin-durant-shot-chart.pdf", plot = durant_shot_chart, width = 6.5, height = 5)
ggsave(filename = "../images/klay-thompson-shot-chart.pdf", plot = klay_shot_chart, width = 6.5, height = 5)
ggsave(filename = "../images/stephen-curry-shot-chart.pdf", plot = curry_shot_chart, width = 6.5, height = 5)

curry$name = "Stephen Curry"

green$name = "Draymond Green"

durant$name = "Kevin Durant"

thompson$name = "Klay Thompson"

iguodala$name = "Andre Iguodala"

combined <- rbind(curry, thompson, durant, green, iguodala)

combined_shot_chart <- ggplot(data = combined) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap(~name)

ggsave(filename = "../images/gsw-shot-charts.pdf", plot = combined_shot_chart, width = 8, height = 7)

ggsave(filename = "../images/gsw-shot-charts.png", plot = combined_shot_chart, width = 8, height = 7)
