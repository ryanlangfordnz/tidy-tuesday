library(tidyverse)
library(ggstream)
library(ggpubr)
library(showtext)
library(sysfonts)

showtext_auto()
font_add_google("Source Sans Pro")
font1 <- "Source Sans Pro"


babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


overall <- babynames %>%
  pivot_wider(id_cols = c(year,name), values_from = n, names_from = sex) %>%
  drop_na() %>%
  mutate(
    inverse = 1 - (abs(M - F) / (M + F)),
    overall = inverse * (M + F)
  ) %>%
  group_by(name) %>%
  summarise(n = sum(overall))

names <- overall %>% slice_max(order_by = n,n = 10) %>% pull(var = 1)


plot_graph = function(baby_name){
  babynames %>% filter(name == baby_name) %>% ggplot() + 
    geom_stream(aes(x=year, y = n, fill = sex), type = "mirror") +
    scale_fill_manual(values = c("#ed968c","#88a0dc"))+
    theme_void() +
    theme(legend.position = "none")+
    ggtitle(baby_name)+
    theme(
      plot.title = element_text(color="#0f252f", size=100, family = font1, hjust = 0.04)
    )
}


plots <- lapply(names, plot_graph)



ggarrange(plotlist = plots, ncol = 1) +
  theme(plot.margin = margin(7, 0, 5, 0, "cm"),
        plot.background = element_rect(fill="#fbf7f0", color="#fbf7f0")) +
  draw_text(text="Baby Names",family=font1, size=300, x=0.375, y=1.2, color="#0f252f")+
  draw_text(text="Streams indicate amount of children given",family=font1, size=100, x =0.035, hjust = 0, y=1.10, color="#0f252f") +
  draw_text(text="that name each year, by assigned gender",family=font1, size=100, x =0.035, hjust = 0, y=1.06, color="#0f252f") +
  draw_text(text="Names chosen for M/F ratio, with a popularity scaling factor",family=font1, size=60, x =0.03, hjust = 0, y=-0.1, color="#0f252f")+
  draw_text(text="Source: babynames R package · Graphic: Ryan Langford",family=font1, size=60, x =0.03, hjust = 0, y=-0.15, color="#0f252f")+
  draw_text(text="1880",family=font1, size=100, x =0.04, hjust = 0, y=-0.01, color="black")+
  draw_text(text="2017",family=font1, size=100, x =0.95, hjust = 1, y=-0.01, color="black")+
  draw_text(text = "MALE", family = font1, size = 100, x = 0.3, , y = -0.04, color = "#88a0dc") + 
  draw_text(text = "FEMALE", family = font1, size = 100, x = 0.7, , y = -0.04, color = "#ed968c")





ggsave("baby_names.png", height=15, width=10)
