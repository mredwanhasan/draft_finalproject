library(dplyr)
library(tidyverse)
library(kableExtra)
library(ggrepel)

as_tibble(Champions_Info)
as_tibble(League_Averages)
as_tibble(Player_Data)
as_tibble(Players)
as_tibble(Seasons_Stats)

# Here I tried to mutate a new column by calculating percentage of players per state. This is arrangd in descending order)
a = Players %>% count(birth_state) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    

# A plot to visualize the previous line of code 
a %>% slice(2:11) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(birth_state, -percent_players), y = percent_players, fill = birth_state), stat = "identity") + 
  ggtitle("Total player representation by birth state") + xlab("State")

# Here we calculate the mean height and weight per birth_state. You can also rearrange it to see.
d = Players %>% group_by(birth_state) %>% summarize(mean_height_per_state = mean(height, na.rm = TRUE),
                                                mean_weight_per_state = mean(weight, na.rm = TRUE)) %>%
                                                arrange(desc(mean_weight_per_state))
# Made variables by selecting certain columns
b = Player_Data %>% select(year_start, year_end, position, name)
c = Seasons_Stats %>% select(Year, Player, Age, Tm)
e = Players %>% select(height, weight, Player)

e %>% ggplot(aes(height)) + 
  geom_density(fill = "lightgreen") + geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), linetype = "dashed") +
  ggtitle("End-to-end density distribution of player height")

# Here we are 
e %>% ggplot(aes(weight)) + 
  geom_density(fill = "lightblue") + geom_vline(aes(xintercept = mean(weight, na.rm = TRUE)), linetype = "dashed") +
  ggtitle("End-to-end density distribution of player weight")



# This is where we join different tables. 
b %>% left_join(Players, by = c("name" = "Player")) %>% left_join(c, by = c("name" = "Player"))
b %>% left_join(Players, by = c("name" = "Player")) %>% left_join(c, by = c("name" = "Player"))
y=e %>% left_join(c, by = "Player")
y

## New variable of players by college
f = Player_Data %>% count(college) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    

# Visualize Total Player representation by University
f %>% slice(2:11) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(college, -percent_players), y = percent_players, fill = college), stat = "identity") + 
  ggtitle("Total player representation by University") + xlab("College") 

library(ggpubr)
#ggscatter(y, x = "Year", y = "weight", 
          #add = "reg.line", conf.int = TRUE, 
          #cor.coef = TRUE, cor.method = "pearson",
          #xlab = "Year", ylab = "Weight")

#ggscatter(y, x = "Year", y = "height", 
          #add = "reg.line", conf.int = TRUE, 
          #cor.coef = TRUE, cor.method = "pearson",
          #xlab = "Year", ylab = "Height")

ggplot(y, aes(Year)) + 
  geom_line(aes(y = height, colour = "Height")) + 
  geom_line(aes(y = weight, colour = "Weight"))
