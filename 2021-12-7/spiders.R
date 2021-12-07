library(tidyverse)
library(maps)
library(ggsci)
library(sysfonts)
library(showtext)

#Fonts
showtext_auto()
font_add_google("Fira Sans")
font1 <- "Fira Sans"



#get world map
world <- map_data("world")

#get spider data
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')


#Get spider gount by country
county_count <- spiders %>% group_by(distribution) %>% summarise(count = n())

#join spider count onto country data
world <- world %>% left_join(county_count, by = c("region" = "distribution"))


#plotting
ggplot() +
  geom_map(
    data = world,
    map = world,
    aes(long, lat, map_id = region, fill = log(n)),
    size = 0.1
  )  + scale_fill_viridis_c(option = "plasma",breaks = c(1,2,3,4,5,6,7,8)) + theme_void() + 
  ggtitle("Number of Spider Species by Country", subtitle = "Log scale to avoid the massive amount of spiders in China distorting the distribution") + theme(
    text = element_text(family = font1, size =15),
    legend.position = "bottom"
  ) + labs(
    fill = "Log(Distinct Spider Species)",
    caption = "#TidyTuesday Week 50 | Data: World Spider Database"
  ) 


ggsave("spider_map_large.png", width = 10, height = 8)
