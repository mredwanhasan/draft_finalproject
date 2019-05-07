library(dplyr)
library(tidyverse)
library(kableExtra)
library(ggrepel)
library(ggpubr)

as_tibble(Champions_Info)
as_tibble(League_Averages)
as_tibble(Player_Data)
as_tibble(Players)
as_tibble(Seasons_Stats)

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
players_1 %>% ggplot(aes(height)) + 
  geom_density(fill = "lightgreen", alpha = 0.50) + geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), linetype = "dashed") +
  ggtitle("End-to-end density distribution of player height")

# Here we are displaying the weight density of players in the NBA #
players_1 %>% ggplot(aes(weight)) + 
  geom_density(fill = "lightblue", alpha = 0.50) + geom_vline(aes(xintercept = mean(weight, na.rm = TRUE)), linetype = "dashed") +
  ggtitle("End-to-end density distribution of player weight")

# This is where we join different tables. 
playerdata_1 %>% left_join(Players, by = c("name" = "Player")) %>% left_join(seasonstats_1, by = c("name" = "Player"))
combined_1 = players_1 %>% left_join(seasonstats_1, by = "Player")

## New variable of players by college
players_per_college = Player_Data %>% count(college) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    

# Visualize Total Player representation by University
players_per_college %>% slice(2:11) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(college, -percent_players), y = percent_players, fill = college), stat = "identity") + 
  ggtitle("Total player representation by University") + xlab("College") 

### Still need to fix this code !!! ###
ggplot(combined_1, aes(Year)) + 
  geom_line(aes(combined_1 = height, colour = "Height")) + 
  geom_line(aes(combined_1 = weight, colour = "Weight"))


#### Visualizing how game has evolved in terms of sizes of players, teams, games
Team_Player <- Seasons_Stats %>%
  group_by(Year) %>%
  summarise(nPlayers = n_distinct(Player),
            nTeams = n_distinct(Tm),
            nGames = max(G),
            Players_per_Team = round(nPlayers/nTeams, 2)) 

### Evolution of Number of NBA Teams per year in League
Team_Player %>%
  kable(escape = FALSE, align='c', caption = "Players, Teams and Games") %>%
  kable_styling("striped", full_width = T) %>%
  column_spec(1, bold = T) %>%
  scroll_box(width = "100%", height = "500px")

Team_Player %>%
  ggplot() +
  geom_line(aes(Year, nTeams, linetype = "Trend line")) +
  ggtitle("Number of NBA Teams by Year") +
  geom_hline(aes(yintercept = mean(Team_Player$nTeams), linetype = "Average line"),
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
  ggplot(aes(born, y=..count.., colour=Player_Generation, fill=Player_Generation)) +
  geom_density(alpha=0.30) +
  ggtitle("Number of players per generations over the years") + ylab("Count") +
  scale_x_continuous(breaks = seq(1910, 2018, 20)) 

### Number of seasons played and how many players played in eahc of them ###

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

