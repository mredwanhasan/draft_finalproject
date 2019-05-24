library(dplyr)
library(tidyverse)
library(kableExtra)
library(ggrepel)
library(ggpubr)
library(readxl)
library(here)
library(maps)
library(dygraphs)
library(PerformanceAnalytics)
library(corrplot)
#library(Hmisc)
library(rpart)
library(randomForest)

column_types <- rep("guess", 51)
column_types[c(8,12,14:21,26:29,33:35,43,44,47:49)] <- "numeric"
Seasons_Stats <- read_excel(path = here("data/Seasons_Stats.xlsx"), sheet = 1, col_types = column_types)
Champions_Info <- read_excel(path = here("data/Champions_Info.xlsx"), sheet = 1)
League_Averages <- read_excel(path = here("data/League_Averages.xlsx"), sheet = 1)
Player_Data <- read_excel(path = here("data/Player_Data.xlsx"), sheet = 1)
Players <- read_excel(path = here("data/Players.xlsx"), sheet = 1)

as_tibble(Champions_Info)
as_tibble(League_Averages)
as_tibble(Player_Data)
as_tibble(Players)
as_tibble(Seasons_Stats)

Seasons_Stats$Pos[Seasons_Stats$Pos == "C-F"] <- "C"
Seasons_Stats$Pos[Seasons_Stats$Pos == "C-PF"] <- "C"
Seasons_Stats$Pos[Seasons_Stats$Pos == "C-SF"] <- "C"
Seasons_Stats$Pos[Seasons_Stats$Pos == "F"] <- "PF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "F-C"] <- "PF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "F-G"] <- "SF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "G"] <- "SG"
Seasons_Stats$Pos[Seasons_Stats$Pos == "G-F"] <- "SF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "PF"] <- "PF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "PF-C"] <- "PF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "PF-SF"] <- "PF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "PG-SF"] <- "PG"
Seasons_Stats$Pos[Seasons_Stats$Pos == "PG-SG"] <- "PG"
Seasons_Stats$Pos[Seasons_Stats$Pos == "SF-PF"] <- "SF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "SF-PG"] <- "PG"
Seasons_Stats$Pos[Seasons_Stats$Pos == "SF-SG"] <- "SF"
Seasons_Stats$Pos[Seasons_Stats$Pos == "SG-PF"] <- "SG"
Seasons_Stats$Pos[Seasons_Stats$Pos == "SG-PG"] <- "SG"
Seasons_Stats$Pos[Seasons_Stats$Pos == "SG-SF"] <- "SG"



# Here I tried to mutate a new column by calculating percentage of players per state. This is arrangd in descending order #
players_per_birthstate = Players %>% count(birth_state) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    


# A plot to visualize the previous line of code #
players_per_birthstate %>% slice(2:11) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(birth_state, -percent_players), y = percent_players, fill = birth_state), stat = "identity") + 
  ggtitle("Total player representation by birth state") + xlab("State") + ylab("Percentage of players")

# Here we calculate the mean height and weight per birth_state. You can also rearrange it to see #
mean_height_weight_per_birthstate = Players %>% group_by(birth_state) %>% summarize(mean_height_per_state = mean(height, na.rm = TRUE),
                                                mean_weight_per_state = mean(weight, na.rm = TRUE)) 


# Made variables by selecting certain columns #
playerdata_1 = Player_Data %>% select(year_start, year_end, position, name)
seasonstats_1 = Seasons_Stats %>% select(Year, Player, Age, Tm)
players_1 = Players %>% select(height, weight, Player)

# Here we are displaying the height density of players in the NBA #
players_1 %>% ggplot(aes(height, y= ..density..)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(fill = "lightgreen", alpha = 0.50) + 
  geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), linetype = "dashed", color = "red", size = 1) +
  ggtitle("Player height density distribution") + xlab("Height") + ylab("Player count") 

# Here we are displaying the weight density of players in the NBA #
players_1 %>% ggplot(aes(weight, y= ..density..)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(fill = "lightblue", alpha = 0.50) + 
  geom_vline(aes(xintercept = mean(weight, na.rm = TRUE)), linetype = "dashed", color = "red", size = 1) +
  ggtitle("Player weight density distribution") + ylab("Player count")

# This is where we join different tables. 
playerdata_1 %>% left_join(Players, by = c("name" = "Player")) %>% left_join(seasonstats_1, by = c("name" = "Player"))
combined_1 = players_1 %>% left_join(seasonstats_1, by = "Player")

## New variable of players by college
players_per_college = Player_Data %>% count(college) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    

### Visualize Total Player representation by University ###
players_per_college %>% slice(2:11) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(college, -percent_players), y = percent_players, fill = n), stat = "identity") + 
  ggtitle("Total player representation by University") + xlab("College") + ylab("Percentage of players")

### Evolution of height and weight over time ### 

Players_3 <- Players %>% select(Player, height, weight)
seasonstats_2 <- Seasons_Stats %>% select(Year, Player)
combined_3 <- full_join(Players_3, seasonstats_2, by = "Player")

### Evolution of height and weight over time for birth year ### ---> Not sure if this is relevant or even needed anylonger

Players %>% group_by(born) %>% summarize(mean_height_birthyear = mean(height, na.rm = TRUE),
                                         mean_weight_birthyear = mean(weight, na.rm = TRUE)) %>% 
ggplot() + 
  geom_line(aes(born, mean_height_birthyear, colour = "Height in cm", linetype = "dashed")) +  
  geom_line(aes(born, mean_weight_birthyear, colour = "Weight in kgs", linetype = "dashed")) + 
  labs(x = "Birth year", y = "") + ggtitle("Height and Weight change over the years in NBA")


### Evolution of height and weight over time by NBA game years ###

height_ts <- combined_3 %>% group_by(Year) %>% summarize(mean_height_playingyear = mean(height, na.rm = TRUE))
height_ts <- ts(height_ts[,-1], start = 1950, frequency = 1)
height_ts %>% dygraph(main = "Height change in NBA over 50 years") %>% dyRangeSelector() %>% 
  dyAxis("y", label = "Average height in CM", valueRange = c(191, 201)) %>% dyOptions(stackedGraph = TRUE)  

weight_ts <- combined_3 %>% group_by(Year) %>% summarize(mean_weight_playingyear = mean(weight, na.rm = TRUE))
weight_ts <- ts(weight_ts[,-1], start = 1950, frequency = 1)
weight_ts %>% dygraph(main = "Weight change in NBA over 50 years") %>% dyRangeSelector() %>% 
  dyAxis("y", label = "Average weight in KG", valueRange = c(86, 102)) %>% dyOptions(stackedGraph = TRUE)  


### Evolution of Field Goal % over the years ### ---> What's the purpose of this? What does it tell us?
Seasons_Stats %>% group_by(Year) %>% summarize(fieldgoal_mean_percentage = mean(`FG%`, na.rm = TRUE)) %>%
  ggplot(aes(Year, fieldgoal_mean_percentage)) + 
  geom_line(color = "red", size = 1) + 
  scale_x_continuous(breaks = seq(1950, 2018, 10)) + 
  geom_point() +
  ggtitle("Field goal probability : For both 2 points and 3 points") + xlab("Years") + ylab("Average field goal probability")


### Evolution of Average number of Assists over the years ###
Seasons_Stats %>% group_by(Year) %>% summarize(mean_assists = mean(AST, na.rm = TRUE)) %>%
  ggplot(aes(Year, mean_assists)) + 
  geom_line(color = "purple", size = 1) + 
  scale_x_continuous(breaks = seq(1950, 2018, 10)) + 
  geom_point() +
  ggtitle("Average number of assists over the years") + xlab("Years") + ylab("Average assists")


### Evolution of average number of personal fould over the years ###
Seasons_Stats %>% group_by(Year) %>% summarize(mean_personal_fouls = mean(PF, na.rm = TRUE)) %>%
  ggplot(aes(Year, mean_personal_fouls)) + 
  geom_line(color = "blue", size = 1) + scale_x_continuous(breaks = seq(1950, 2018, 10)) + geom_point() +
  ggtitle("Average number of personal fouls over the years") + xlab("Years") + ylab("Average personal fouls")

### Attempt to get player height, weight by position ###
Players_2 <- Players %>% select(Player, height, weight)
Seasonal_data_1 <- Seasons_Stats %>% select(Player, Pos)
combined_2 <- Players_2 %>% left_join(Seasonal_data_1, by = "Player")

combined_2 %>% group_by(Pos) %>% summarize(mean_height_per_position = mean(height, na.rm = TRUE),
                                                    mean_weight_per_position = mean(weight, na.rm = TRUE)) %>% filter(!is.na(Pos)) %>%
  ggplot() + geom_bar(mapping = aes(x = reorder(Pos, -mean_height_per_position), 
                                    y = mean_height_per_position, fill = mean_weight_per_position), stat = "identity") +
  ggtitle("Player height per position") + ylab("Height") + xlab("Positions")

### Evolution of 2point shooting VS 3point shooting (starting from 1980) ###
points_ts <- Seasons_Stats %>% group_by(Year) %>% filter(Year > 1979) %>% summarize(mean_2point = mean(`2P`, na.rm = TRUE),
                                                                                    mean_3point = mean(`3P`, na.rm = TRUE))
points_ts <- ts(points_ts[,-1], start = 1980, frequency = 1)
points_ts %>% dygraph(main = "Point scoring change in NBA over 40 years") %>% dyRangeSelector() %>% 
  dyAxis("y", label = "Average points scored", valueRange = c(0, 250)) %>% dyOptions(stackedGraph = TRUE)  


### Weight distribution around the world ### ---> Need to get the map for the USA only
Players %>% group_by(birth_state) %>% summarize(mean_weight_world = mean(weight, na.rm = TRUE)) %>% 
  ggplot(aes(map_id=birth_state)) +
  geom_map(mapping=aes(fill=mean_weight_world), map=map_data("world")) + 
  expand_limits(x=c(-130, 180), y=c(-60,85)) + 
  ggtitle("Weight distribution over the world") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Longitude", y="Latitude", fill= "height") +
  scale_fill_gradient(low="cyan", high="darkred")

### Weight distribution around the world ### ---> Need to get the map for the USA only
Players %>% group_by(birth_state) %>% summarize(mean_height_world = mean(height, na.rm = TRUE)) %>% 
  ggplot(aes(map_id=birth_state)) +
  geom_map(mapping=aes(fill=mean_height_world), map=map_data("world")) + 
  expand_limits(x=c(-130, 180), y=c(-60,85)) + 
  ggtitle("Height distribution over the world") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Longitude", y="Latitude", fill= "height") +
  scale_fill_gradient(low="cyan", high="darkred")


#### Visualizing how game has evolved in terms of sizes of players, teams, games
Team_Player <- Seasons_Stats %>%
  group_by(Year) %>%
  summarise(Number_of_Players = n_distinct(Player),
            Number_of_Teams = n_distinct(Tm),
            Number_of_Games = max(G),
            Players_per_Team = round(Number_of_Players/Number_of_Teams, 2)) 

### Evolution of Number of NBA Teams per year in League ---> Look into these codes and see if we can modify something
Team_Player %>%
  kable(escape = FALSE, align='c', caption = "Evolving of players, teams and games by the years") %>%
  kable_styling("striped", full_width = T) %>%
  column_spec(1, bold = T) %>%
  scroll_box(width = "100%", height = "500px")

Team_Player %>%
  ggplot() +
  geom_line(aes(Year, Number_of_Teams, linetype = "Trend line")) +
  ggtitle("Change in the Number of NBA Teams by Year") +
  geom_hline(aes(yintercept = mean(Team_Player$Number_of_Teams ), linetype = "Average line"),
             col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2017, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1), guide = guide_legend(reverse = TRUE)) +
  ylab("Number of Teams") + theme(legend.position="bottom")

### Selecting the relevant columns and visualizing the plyers by generations ###

Players %>% group_by(born, Player) %>%
  mutate(Player_Generation = ifelse(born <= 1945, "Silent Generation",
                                    ifelse(born %in% 1946:1964, "Baby Boomers",
                                           ifelse(born %in% 1965:1976, "Generation X",
                                                  ifelse(born %in% 1977:1995, "Generation Y",
                                                         "Generation Z"))))) %>%
  ggplot(aes(born, y= ..count.., colour=Player_Generation, fill=Player_Generation)) +
  geom_density(alpha=0.30) +
  ggtitle("Number of players per generations over the years") + ylab("Player count") + xlab("Birth year") +
  scale_x_continuous(breaks = seq(1910, 2018, 20)) 

### Number of seasons played and how many players played in each of them ###

Player_Data %>% select(name, year_start, year_end, position) %>% 
  mutate(seasons_played = year_end - year_start) %>% 
  ggplot(aes(seasons_played)) +
  geom_bar(aes(fill = ..count..)) + scale_x_continuous(breaks = seq(0, 20, 1)) +
  ggtitle("Number of players and how many seaons they play") +xlab("Number of seasons played") + ylab("Number of players")

### The most popular/common positions in NBA ### ---> Need to reorder it
Seasons_Stats %>% select(Player, Age, Pos) %>% filter(!is.na(Pos)) %>%
  ggplot(aes(Pos)) + geom_bar(aes(fill = Pos)) +
  ggtitle("Number of players per position") + ylab("Number of players")

## -------------------------------------------Model selection----------------------------------------------------- ##


player_winshare <- Seasons_Stats %>% group_by(Player) %>% summarise(WS = sum(WS)) %>%
  mutate(Player = fct_reorder(Player, WS)) %>% arrange(desc(WS)) %>% slice(1:15)

ggplot(player_winshare, aes(WS, Player)) +
  geom_segment(aes(x = 0, y = Player, xend = WS, yend = Player), color = "thistle") +
  geom_point() + ggtitle("Highest win share by players")

# 24,10,11,16:21,28,29,35,38,42,50,51 --> all the variables we might be interested in.

corr_data <- Seasons_Stats %>% filter(Year > 1979) %>% select(c(24,10,11,16:21,28,29,35,38,42,50,51))

colnames(corr_data) <- c("WS","PER","TS_pcent","TRB_pcent","AST_pcent","STL_pcent","BLK_pcent","TOV_pcent",
                         "USG_pcent","BPM","VORP","threeP_pcent","twoP_pcent","FT_pcent","PF","PTS")


### !!! Need to figure a way to use something else, cuz its package creates a conflict with DPLYR !!! ###
corr_data %>% as.matrix() %>% rcorr()


### Running the multiple regression model with stepwise selection ###
trial_model_lm <- lm(WS ~ ., data = corr_data)
summary(trial_model_lm)
step(trial_model_lm)

trial_model_lm_edited <- lm(WS ~. -TRB_pcent, data = corr_data)
summary(trial_model_lm_edited)

### Doing the random forest here ###
corr_data <- na.omit(corr_data)
trial_model_rf <-randomForest(WS~., data=corr_data, ntree=500)
varImpPlot(trial_model_rf)

trial_moodel_rf_pred <- predict(trial_mode_rf, newdata=Test.JapanSample2)
confusionMatrix(RF.Prediction2, Test.JapanSample2$Return)
par(mfrow=c(1,1))
varImpPlot(trial_model_rf)

## Doing the different degree of polynomial model ##
poly_model_try_one <- lm(WS ~ I(PER^4) +I(TS_pcent^4) + I(TRB_pcent^4) + I(AST_pcent^4) + I(STL_pcent^4) + I(BLK_pcent^4) + 
                       I(TOV_pcent^4) + I(USG_pcent^4) + I(BPM^4) + I(VORP^4) + I(threeP_pcent^4) + 
                       I(twoP_pcent^4) + I(FT_pcent^4) + I(PF^4) + I(PTS^4), data = corr_data)

summary(poly_model_try_one)
step(poly_model_try_one)


poly_model_try_two <- lm(WS ~ I(PER^2) +I(TS_pcent^2) + I(TRB_pcent^2) + I(AST_pcent^2) + I(STL_pcent^2) + I(BLK_pcent^2) + 
                           I(TOV_pcent^2) + I(USG_pcent^2) + I(BPM^2) + I(VORP^2) + I(threeP_pcent^2) + 
                           I(twoP_pcent^2) + I(FT_pcent^2) + I(PF^2) + I(PTS^2), data = corr_data)


summary(poly_model_try_two)
step(poly_model_try_two)

## Perfomring the cross validation ##
corr_data_random <- corr_data[sample(1:nrow(corr_data)),]
# create an empty list that will be the test sets
Test.List <- list() 
# create an empty list that will be the train sets
Train.List <- list() 
# set counter to 0
counter <- 0

for (i in 1:9){
  index <- counter + c(1:(floor(nrow(corr_data_random) /10)))
  # the row numbers that will be in the test set
  Test.List[[i]] <- corr_data_random[index, ] # the test set nbr i
  Train.List[[i]] <- corr_data_random[-index, ] # the train set nbr i
  counter <- counter + (floor(nrow(corr_data_random) /10))
}

Index.Test <- counter + c(1:(nrow(corr_data)-(floor(nrow(corr_data) /10)*9)))
Test.List[[10]] <- corr_data_random[Index.Test,] # the test set nbr 10
Train.List[[10]] <- corr_data_random[-Index.Test,] # the train set nbr 10

CV.RF.Acc <- numeric(10)
CV.LM.Acc <- numeric(10)
CV.Poly.Acc <- numeric(10)

for (i in 1:10){

  CV.RF.Model <- randomForest(WS~.-TRB_pcent, data=Train.List[[i]]) # CV for the random forest
  CV.LM.Model <- lm(WS~., data=Train.List[[i]])
  CV_poly_model_one <- lm(WS ~ I(PER^8) +I(TS_pcent^8) + I(TRB_pcent^8) + I(AST_pcent^8) + I(STL_pcent^8) + I(BLK_pcent^8) + 
                             I(TOV_pcent^8) + I(USG_pcent^8) + I(BPM^8) + I(VORP^8) + I(threeP_pcent^8) + 
                             I(twoP_pcent^8) + I(FT_pcent^8) + I(PF^8) + I(PTS^8), data = Train.List[[i]])
  
  CV.RF.Prediction <- predict(CV.RF.Model, newdata=Test.List[[i]], type="class")
  CV.LM.Prediction <- predict(CV.LM.Model, newdata=Test.List[[i]])
  CV_poly_model_one_pred <- predict(CV_poly_model_one, data = Test.List[[i]])
  
  
  CV.RF.Acc[i] <- sqrt(mean((CV.RF.Prediction - Test.List[[i]]$WS)^2, na.rm=TRUE)) 
  CV.LM.Acc[i] <- sqrt(mean((CV.LM.Prediction - Test.List[[i]]$WS)^2, na.rm=TRUE))
  CV.Poly.Acc[i] <- sqrt(mean((CV_poly_model_one_pred - Test.List[[i]]$WS)^2, na.rm=TRUE))
}

mean_poly <- mean(CV.Poly.Acc)
sd_poly <- sd(CV.Poly.Acc)
mean_RandomForest <- mean(CV.RF.Acc)
mean_MultipleReg <- mean(CV.LM.Acc)
stdev_RandomForest <- sd(CV.RF.Acc)
stdev_MultipleReg <- sd(CV.LM.Acc)


model_results_dataframe <- data.frame("Model" = c("Random forest", "Random forest", "Multiple Regression", "Multiple Regression", 
                                                  "Polynomial", "Polynomial"),
                                      "Result_type" = c("Mean RMSE", "Standard Deviation", 
                                                        "Mean RMSE", "Standard Deviation",
                                                        "Mean RMSE", "Standard Deviation"),
                                      "Results" = c(mean_RandomForest, stdev_RandomForest, mean_MultipleReg, stdev_MultipleReg,
                                                    mean_poly, sd_poly))
                            

model_results_dataframe %>% ggplot(aes(factor(Result_type), Results, fill = Model)) + 
  geom_bar(stat="identity", position = "dodge") + labs(x = "Result type", y = " ") + ggtitle("Model comparison") +
  scale_fill_brewer(palette = "Set1")


corr_data_ts <- Seasons_Stats %>% filter(Year > 1979) %>% select(c(2,3,24,10,11,16:21,28,29,35,38,42,50,51))

corr_data_ts %>% group_by(Year, Player) %>% filter(Year > 2013) %>% summarize(avg.WS = mean(WS, na.rm = TRUE))

trial_ts <- ts(corr_data_ts, start = 1980, frequency = 1)
