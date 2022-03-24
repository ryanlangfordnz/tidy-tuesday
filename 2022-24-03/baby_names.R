library(tidyverse)
library(ggstream)
library(ggpubr)
library(showtext)
library(sysfonts)

showtext_auto()
font_add_google("Roboto")
font1 <- "Roboto"


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


plot_graph = function(baby_name){
  babynames %>% filter(name == baby_name) %>% ggplot() + 
    geom_stream(aes(x=year, y = n, fill = sex), type = "mirror") +
    scale_fill_manual(values = c("#ed968c","#88a0dc"))+
    theme_void() +
    theme_void()+
    theme(legend.position = "none")
}


plots <- lapply(names, plot_graph)


ggarrange(plotlist = plots,nrow = 10, ncol = 1) + 
  theme(plot.background = element_rect(fill="#fbf7f0", color="#fbf7f0"),
        plot.margin = margin(5, 0, 3.5, 0, "cm")) +
  draw_text(text="Streams show number of children given that name each year by assigned gender.",
            family=font1, size=55, x=0.0, y=1,hjust = 0,  color="#4f2217") +
  draw_text(text = "Twitter: @BlakeRobMills | Source: World Spider Database | GitHub: BlakeRMills", 
            x=0.5, y=-0.11, color="#4f2217", size=50, family=font1, fontface="bold") +
  draw_text(text = "Baby Names", x=0.0, y=1.12,hjust=0,vjust = 0.5 , fontface="bold", size=300, family=font1, color="#4f2217") 


ggsave("Spiders.png", height=15, width=10)
