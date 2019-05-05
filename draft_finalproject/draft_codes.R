library(dplyr)
library(tidyverse)
library(kableExtra)

as_tibble(Champions_Info)
as_tibble(League_Averages)
as_tibble(Player_Data)
as_tibble(Players)
as_tibble(Seasons_Stats)

Player_Data %>% select(-height) %>% filter(year_start > 1990, year_end < 2000)
a = Players %>% count(birth_state) %>% arrange(desc(n)) %>% mutate(percent_players = (n/sum(n)*100))                    

a %>% slice(2:11) %>% ggplot() + geom_bar(mapping = aes(x = birth_state, y = percent_players), stat = "identity", fill = ) + 
  ggtitle("Total player representation by state")

 