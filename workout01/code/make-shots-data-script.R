# Title: Shots Data Manipulation Script
# Description: This script loads shots data from each of the GSW players and adds a name column and minute column to their dataset. It also computes summaries of the data and combines the five datasets of the NBA players into one dataset.
# Inputs: This script takes in the five csv files of shots data from the five GSW players.
# Outputs: This script outputs a summary of each player's data and outputs a csv file with the combined data. It also outputs the summary of the combined data.

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)

curry$name = "Stephen Curry"

green$name = "Draymond Green"

durant$name = "Kevin Durant"

thompson$name = "Klay Thompson"

iguodala$name = "Andre Iguodala"

curry$shot_made_flag[curry$shot_made_flag == 'n'] = 'shot_no'
curry$shot_made_flag[curry$shot_made_flag == 'y'] = 'shot_yes'


green$shot_made_flag[green$shot_made_flag == 'n'] = 'shot_no'
green$shot_made_flag[green$shot_made_flag == 'y'] = 'shot_yes'


iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] = 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] = 'shot_yes'


durant$shot_made_flag[durant$shot_made_flag == 'n'] = 'shot_no'
durant$shot_made_flag[durant$shot_made_flag == 'y'] = 'shot_yes'


thompson$shot_made_flag[thompson$shot_made_flag == 'n'] = 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] = 'shot_yes'



curry$minute = 12*curry$period - curry$minutes_remaining
green$minute = 12*green$period - green$minutes_remaining
durant$minute = 12*durant$period - durant$minutes_remaining
thompson$minute = 12*thompson$period - thompson$minutes_remaining
iguodala$minute = 12*iguodala$period - iguodala$minutes_remaining

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

combined <- rbind(curry, thompson, durant, green, iguodala)

write.csv(
  x = combined, # R object to be exported
  file = '../data/shots-data.csv'  # file path
)

sink(file = '../output/shots-data-summary.txt')
summary(combined)
sink()

