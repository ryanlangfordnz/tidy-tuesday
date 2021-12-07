library(tidytuesdayR)
library(tidyverse)
library(maps)
library(ggsci)
library(sysfonts)
library(showtext)

#Fonts
showtext_auto()
font_add_google("Fira Sans")
font1 <- "Fira Sans"


#Loads in the matches
matches <-
  matches <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv"
  )


#get world map
world <- map_data("world")

#gets the team that has won the most per ground
winners <- matches %>%
  group_by(ground) %>%
  count(ground, winner) %>%
  slice(which.max(n)) %>%
  rename("name" = "ground")


#merges the winners dataframe with the cities, and eliminates any duplicate city names
winners_cities <-
  world.cities %>% filter(name %in% matches$ground) %>% left_join(test, by = "name") %>% distinct(name, .keep_all =  TRUE)

#plotting
ggplot() +
  geom_map(
    data = world,
    map = world,
    aes(long, lat, map_id = region),
    fill = "#2B2542",
    size = 0.1
  ) + geom_point(data = winners_cities,
                 aes(long, lat, size = n, col = winner),
                 alpha = 0.9) + theme_void()  +
  scale_color_aaas(name = "Most Common Winner") +
  scale_size(range = c(0, 5)) +
  labs(
    title = str_to_title("Cricket Grounds and the Team with the most wins there"),
    subtitle = str_to_title("size relates to absolute number of wins at a location")
  ) +
  theme(
    plot.background = element_rect(fill = "black"),
    legend.text = element_text(color = "lightgray"),
    legend.title = element_text(color = "lightgray"),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(color = "lightgray"),
    text = element_text(family = font1, size =15),
    plot.subtitle  = element_text(color = "lightgray"),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
  ) + guides(size = "none",
             color = guide_legend(override.aes = list(size = 6)))



ggsave("cricket_map.png", width = 7, height = 4)
  