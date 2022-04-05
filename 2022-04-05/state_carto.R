library(tidyverse)
library(cartogram)
library(transformr)
library(tigris)
library(scales)
library(viridis)
library(gganimate)

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')



test <- news_orgs %>% group_by(state) %>% summarise(n = n())



states <- states(cb = F) %>% shift_geometry()

states <- states %>% left_join(test, by = c("STUSPS" = "state")) %>% drop_na()

states <- states[order(states$GEOID), ]

states["scaled"] <- rescale(states$n, to = c(1, 100))



start <- states %>% filter(GEOID < 60 ) %>% drop_na() %>% ggplot(aes(fill = scaled)) + geom_sf() + coord_sf(crs = 5070, datum = NA)

state_cart <- cartogram_cont(states, "scaled", itermax = 7)




ggplot() + geom_sf(data = state_cart, aes(fill = scaled)) + scale_fill_viridis() + coord_sf(crs = 5070, datum = NA)


morph <- tween_sf(states,
                  state_cart,
                  ease = 'linear',
                  id = GEOID,
                  nframes = 30)
# 
# plt <-
#   ggplot(morph) + 
#   geom_sf(aes(geometry = geometry, fill = scaled)) + 
#   coord_sf(crs = 5070, datum = NA) +
#   transition_states(.frame) + 
#   scale_fill_viridis(breaks = c(25,50,75), name = "Relative %") + 
#   theme_void() +
#   labs(title = "Cartogram of relative amount of sex", subtitle = "Data from memory", caption = "God I'm so bored") + 
#   theme(
#     text = element_text(family = "Decima WE"), 
#     plot.title = element_text(vjust = -75,face = "bold", size = 12),
#     plot.subtitle = element_text(vjust = -75, size = 12)
#   )



plt <-
  ggplot(morph) +
  geom_sf(aes(geometry = geometry, fill = scaled)) +
  coord_sf(crs = 5070, datum = NA) +
  transition_states(.frame)
 




animate(plt, rewind = TRUE)


