library(tidyverse)
library(introdataviz)



sport <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')


sport %>% filter(sports %in% c("Golf","Archery","Fencing" )) %>% select(exp_men, exp_women, sports) %>%
  group_by(sports) %>% summarise(m = sum(exp_men, na.rm = T), w = sum(exp_women, na.rm = T)) %>%
pivot_longer(cols = c(m,w))-> test


sport %>% filter(sports %in% c("Golf","Archery","Fencing", "Gymnastics" )) %>% select(exp_men, exp_women, sports) %>%
  pivot_longer(cols = c(exp_men,exp_women)) -> test2

test2 %>% drop_na() %>% ggplot(aes(x = sports, y = log(value), fill = name)) + geom_split_violin() 



ggsave(path = "D:/r_mess_around/tidy-tuesday/2022-03-29","sports.png")
