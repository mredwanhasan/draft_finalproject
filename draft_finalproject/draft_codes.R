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
library(Hmisc)
library(rpart)

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
  ggtitle("Total player representation by birth state") + xlab("State")

# Here we calculate the mean height and weight per birth_state. You can also rearrange it to see #
mean_height_weight_per_birthstate = Players %>% group_by(birth_state) %>% summarize(mean_height_per_state = mean(height, na.rm = TRUE),
                                                mean_weight_per_state = mean(weight, na.rm = TRUE)) 


# Made variables by selecting certain columns #
playerdata_1 = Player_Data %>% select(year_start, year_end, position, name)
seasonstats_1 = Seasons_Stats %>% select(Year, Player, Age, Tm)
players_1 = Players %>% select(height, weight, Player)

# Here we are displaying the height density of players in the NBA #
players_1 %>% ggplot(aes(height, y= ..count..)) + 
  geom_density(fill = "lightgreen", alpha = 0.50) + geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), linetype = "dashed") +
  ggtitle("Player height density distribution") + ylab("Player count")

# Here we are displaying the weight density of players in the NBA #
players_1 %>% ggplot(aes(weight, y= ..count..)) + 
  geom_density(fill = "lightblue", alpha = 0.50) + geom_vline(aes(xintercept = mean(weight, na.rm = TRUE)), linetype = "dashed") +
  ggtitle("Player weight density distribution") + ylab("Player count")

# This is where we join different tables. 
playerdata_1 %>% left_join(Players, by = c("name" = "Player")) %>% left_join(seasonstats_1, by = c("name" = "Player"))
combined_1 = players_1 %>% left_join(seasonstats_1, by = "Player")

## New variable of players by college
players_per_college = Player_Data %>% count(college) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    

### Visualize Total Player representation by University ###
players_per_college %>% slice(2:11) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(college, -percent_players), y = percent_players, fill = college), stat = "identity") + 
  ggtitle("Total player representation by University") + xlab("College") 

### Evolution of height and weight over time ### --> This is okay but should combine with year from season stats and not birth year ###

Players_3 <- Players %>% select(Player, height, weight)
seasonstats_2 <- Seasons_Stats %>% select(Year, Player)
combined_3 <- full_join(Players_3, seasonstats_2, by = "Player")

### Evolution of height and weight over time for birth year ###

Players %>% group_by(born) %>% summarize(mean_height_birthyear = mean(height, na.rm = TRUE),
                                         mean_weight_birthyear = mean(weight, na.rm = TRUE)) %>% 
ggplot() + 
  geom_line(aes(born, mean_height_birthyear, colour = "Height in cm", linetype = "dashed")) +  
  geom_line(aes(born, mean_weight_birthyear, colour = "Weight in kgs", linetype = "dashed")) + 
  labs(x = "Birth year", y = "") + ggtitle("Height and Weight change over the years in NBA")

### Evolution of height and weight over time by NBA game years ###

height_ts <- combined_3 %>% group_by(Year) %>% summarize(mean_height_playingyear = mean(height, na.rm = TRUE))
height_ts <- ts(height_ts, start = 1950, frequency = 1)
dygraph(height_ts) %>% dyRangeSelector() %>% dyAxis("y", label = "Average height", valueRange = c(191, 201))

weight_ts <- combined_3 %>% group_by(Year) %>% summarize(mean_weight_playingyear = mean(weight, na.rm = TRUE))
weight_ts <- ts(weight_ts, start = 1950, frequency = 1)
dygraph(weight_ts) %>% dyRangeSelector() %>% dyAxis("y", label = "Average weight", valueRange = c(86, 99))


### Evolution of Field Goal % over the years ###
Seasons_Stats %>% group_by(Year) %>% summarize(fieldgoal_mean_percentage = mean(`FG%`, na.rm = TRUE)) %>%
  ggplot(aes(Year, fieldgoal_mean_percentage)) + 
  geom_line(color = "red", size = 1) + scale_x_continuous(breaks = seq(1950, 2018, 10)) + geom_point() +
  ggtitle("Field goal % : For both 2 points and 3 points")

### Evolution of Average number of Assists over the years ###
Seasons_Stats %>% group_by(Year) %>% summarize(mean_assists = mean(AST, na.rm = TRUE)) %>%
  ggplot(aes(Year, mean_assists)) + 
  geom_line(color = "purple", size = 1) + scale_x_continuous(breaks = seq(1950, 2018, 10)) +
  ggtitle("Average number of assists over the years")

### Evolution of average number of personal fould over the years ###
Seasons_Stats %>% group_by(Year) %>% summarize(mean_personal_fouls = mean(PF, na.rm = TRUE)) %>%
  ggplot(aes(Year, mean_personal_fouls)) + 
  geom_line(color = "blue", size = 1) + scale_x_continuous(breaks = seq(1950, 2018, 10)) +
  ggtitle("Average number of personal fouls over the years")

### Attempt to get player height, weight by position ###
Players_2 <- Players %>% select(Player, height, weight)
Seasonal_data_1 <- Seasons_Stats %>% select(Player, Pos)
combined_2 <- Players_2 %>% left_join(Seasonal_data_1, by = "Player")

combined_2 %>% group_by(Pos) %>% summarize(mean_height_per_position = mean(height, na.rm = TRUE),
                                                    mean_weight_per_position = mean(weight, na.rm = TRUE)) %>%
  ggplot() + geom_bar(mapping = aes(x = reorder(Pos, -mean_height_per_position), 
                                    y = mean_height_per_position, fill = mean_weight_per_position), stat = "identity") +
  ggtitle("Player height per position") + ylab("Height") + xlab("Positions")

### Evolution of 2point shooting VS 3point shooting (starting from 1980) ###
Seasons_Stats %>% group_by(Year) %>% summarize(mean_2point = mean(`2P`, na.rm = TRUE),
                                               mean_3point = mean(`3P`, na.rm = TRUE)) %>%
  filter(Year > 1979) %>%
  ggplot() + 
  geom_line(aes(Year, mean_2point, colour = "2 point score", linetype = "dashed")) +  
  geom_line(aes(Year, mean_3point, colour = "3 point score", linetype = "dashed")) + 
  scale_x_continuous(breaks = seq(1950, 2018, 10)) +
  labs(x = "NBA year", y = "Points scored") + ggtitle("Change in 2 point and 3 point score change over the years")

### Weight distribution around the world ###
Players %>% group_by(birth_state) %>% summarize(mean_weight_world = mean(weight, na.rm = TRUE)) %>% 
  ggplot(aes(map_id=birth_state)) +
  geom_map(mapping=aes(fill=mean_weight_world), map=map_data("world")) + 
  expand_limits(x=c(-130, 180), y=c(-60,85)) + 
  ggtitle("Weight distribution over the world") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Longitude", y="Latitude", fill= "height") +
  scale_fill_gradient(low="cyan", high="darkred")

### Weight distribution around the world ###
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

### Evolution of Number of NBA Teams per year in League
Team_Player %>%
  kable(escape = FALSE, align='c', caption = "Evolving of players, teams and games by the") %>%
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
  ggtitle("Number of players per generations over the years") + ylab("Count") +
  scale_x_continuous(breaks = seq(1910, 2018, 20)) 

### Number of seasons played and how many players played in each of them ###

Player_Data %>% select(name, year_start, year_end, position) %>% 
  mutate(seasons_played = year_end - year_start) %>% 
  ggplot(aes(seasons_played)) +
  geom_bar(aes(fill = ..count..)) + scale_x_continuous(breaks = seq(0, 20, 1)) +
  ggtitle("Number of players and how many seaons they play") + ylab("Number of players")

### The most popular/common positions in NBA ### Need to reorder it !!!
Player_Data %>% select(name, year_start, year_end, position) %>% filter(!is.na(position)) %>%
  mutate(seasons_played = year_end - year_start) %>% arrange(seasons_played) %>% 
  ggplot(aes(position)) + geom_bar(aes(fill = position)) +
  ggtitle("Number of players per position") + ylab("Number of players")


## -------------------------------------------Model selection----------------------------------------------------- ##


player_winshare <- Seasons_Stats %>% group_by(Player) %>% summarise(WS = sum(WS)) %>%
  mutate(Player = fct_reorder(Player, WS)) %>% arrange(desc(WS)) %>% slice(1:15)

ggplot(player_winshare, aes(WS, Player)) +
  geom_segment(aes(x = 0, y = Player, xend = WS, yend = Player), color = "thistle") +
  geom_point() + ggtitle("Highest win share by players")

# 24,10,11,16:21,28,29,35,38,42,50,51 --> all the variables we might be interested in.

corr_data <- Seasons_Stats %>% filter(Year > 1979) %>% select(c(24,10,11,16:21,28,29,35,38,42,50,51))
# corr_data <- Seasons_Stats[, c(24,10,11,16:21,28,29,35,38,42,50,51 )]

corr_data %>% as.matrix() %>% rcorr()



trial_model <- lm(WS ~ PER+`TS%`+`TRB%`+`AST%`+`STL%`+`BLK%`+`TOV%`+`USG%`+ BPM + VORP +`3P%`+`2P%`+`FT%`+ PF + PTS, data = corr_data)
summary(trial_model)
step(trial_model)
confint(trial_model)



trial_model_1 <- rpart(WS ~ PER+`TS%`+`TRB%`+`AST%`+`STL%`+`BLK%`+`TOV%`+`USG%`+ BPM + VORP +`3P%`+`2P%`+`FT%`+ PF + PTS,
             method="anova", data=corr_data)
summary(trial_model_1)
plotcp(trial_model_1)

plot(trial_model_1, uniform=TRUE, 
     main="Regression Tree for Win Share ")
text(trial_model_1, use.n=TRUE, all=TRUE, cex=.8)

