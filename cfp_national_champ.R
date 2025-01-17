#import dataset
library(readxl)
OSU_v_Notre_Dame <- read_excel("OSU v Notre Dame.xlsx")
View(OSU_v_Notre_Dame)
# my dataset shows the 4 previous games for Ohio State and Notre Dame with stats on each game
#change name of dataset to something easier to read
natty <- OSU_v_Notre_Dame
#dplyr for pipe operator
library(dplyr)
osu_stats <- natty %>% filter(Teams == "Ohio State")
nd_stats <- natty %>% filter(Teams == "Notre Dame")
nd_stats <- natty %>% filter(Teams == "Notre Dame")
#combine both team stats with "natty_combined"
natty_combined <- natty %>% bind_rows(osu_stats, nd_stats)
#offense scoring model
score_model_off <- lm(pointsscored ~ (offpassyd + offpassydpp + offrushyd + offrushypp -  turnovers - offmissedfg) / offposstime, data = natty_combined)
summary(score_model_off)

#score model defense
score_model_def <- lm(pointsallowed ~ (defpassallowed + defpassydperatt + defrushallowed + defrushydpp + offavgpunt - offmissedfg) / offposstime , data = natty_combined)
score_model_def <- lm(pointsallowed ~ (defpassallowed + defpassydperatt + defrushallowed + defrushydpp + offavgpunt - offmissedfg) / offposstime , data = natty_combined)
summary(score_model_def)

#now that we have 2 models for offense and defense we can create a "new game" that takes into account for comparing team stats against each other
colnames(natty_combined)

team1_avg <- natty_combined %>%
  filter(Teams == "Ohio State") %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
#we want to elevate that error messgae
team1_avg <- natty_combined %>%
  filter(Teams == "Ohio State") %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
team2_avg <- natty_combined %>%
  filter(Teams == "Notre Dame") %>%  summarise(across(where(is.numeric), mean, na.rm = TRUE))
#now that we have both avg we can find that "new game" data
new_game_off <- data.frame (
  offpassyd = c(team1_avg$offpassyd, team2_avg$offpassyd),
  offpassydpp = c(team1_avg$offpassydpp, team2_avg$offpassydpp),
  offrushyd = c(team1_avg$offrushyd, team2_avg$offrushyd),
  offrushypp = c(team1_avg$offrushypp, team2_avg$offrushypp),
  turnovers = c(team1_avg$turnovers, team2_avg$turnovers),
  offmissedfg = c(team1_avg$offmissedfg, team2_avg$offmissedfg),
  offposstime = c(team1_avg$offposstime, team2_avg$offposstime), offavgpunt = c(team1_avg$offavgpunt, team2_avg$offavgpunt)
)
summary(new_game_off)

new_game_def <- data.frame(defpassallowed = c(team1_avg$defpassallowed, team2_avg$defpassallowed), defpassydperatt = c(team1_avg$defpassydperatt, team2_avg$defpassydperatt), defrushallowed = c(team1_avg$defrushallowed, team2_avg$defrushallowed), defrushydpp = c(team1_avg$defrushydpp, team2_avg$defrushydpp), offavgpunt = c(team1_avg$offavgpunt, team2_avg$offavgpunt), offmissedfg = c(team1_avg$offmissedfg, team2_avg$offmissedfg), offposstime = c(team1_avg$offposstime, team2_avg$offposstime))
predicted_offense <- predict(score_model_off, newdata = new_game_off, type = "response")
predicted_defense <- predict(score_model_def, newdata = new_game_def, type = "response")
team1_score <- predicted_offense[1] - predicted_defense[2]
team2_score <- predicted_offense[2] - predicted_defense[1]
cat("Predicted Score:\n")
cat("Team 1 (Ohio State):", round(team1_score, 2), "\n")
cat("Team 2 (Notre Dame):", round(team2_score, 2), "\n")




