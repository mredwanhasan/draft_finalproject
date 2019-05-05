install.packages("ggrepel")
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
  ggtitle("Total player representation by state") + xlab("State")

# Here we calculate the mean height and weight per birth_state. You can also rearrange it to see.
Players %>% group_by(birth_state) %>% summarize(mean_height_per_state = mean(height, na.rm = TRUE),
                                                mean_weight_per_state = mean(weight, na.rm = TRUE))

# Made variables by selecting certain columns
b = Player_Data %>% select(year_start, year_end, position, name)
c = Seasons_Stats %>% select(Player, Age, Tm)

# This is where we join different tables. 
b %>% left_join(Players, by = c("name" = "Player")) %>% left_join(c, by = c("name" = "Player"))
