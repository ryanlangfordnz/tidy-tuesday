library(tidyverse)

library(sysfonts)
library(showtext)

#font
showtext_auto()
font_add_google("Fira Sans")
font1 <- "Fira Sans"


#reading the data in
bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')


#data cleaning
mod_cran <- cran
mod_cran$version <- gsub("-","",mod_cran$version)
mod_cran$version <- gsub("[0-9]+\\.[0-9]+(\\.)", "", mod_cran$version)
mod_cran$version <- as.numeric(mod_cran$version)
mod_cran$date <- as.Date(mod_cran$date)


#getting only the  5 packages with the most versions
updated <- mod_cran %>% group_by(package) %>% filter(version > 0) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(5)

#Gets the plot and filters out anything where they use the year as a package number
plot <- mod_cran %>% filter(version < 5) %>% filter(package %in% updated$package) %>% na.omit()  %>% ggplot(aes(x= date, y = version, col = package)) + geom_point() + geom_smooth()

final <- plot + theme(
  text = element_text(family = font1, size = 35, color = "#CCCCCC"),
  panel.background = element_rect(color="#363C48", fill="#363C48"),
   plot.background = element_rect(color="#363C48", fill="#363C48"),
  legend.background = element_rect(color = "#363C48", fill = "#363C48"),
  legend.key = element_rect(color = "#363C48", fill = "#363C48"),
  legend.key.size = unit(3,"line"),
  axis.text = element_text(color = "#CCCCCC"),
  panel.grid = element_line(color = "#CCCCCC")
  
) + labs(
 title = "Version Number by Date",
 subtitle = "Of the five most updated packages on CRAN",
 caption = "Data: R. Flight",
 y = "Version #",
 x = "Date", 
 color = "Package"
) 

final


ggsave("Version_plot.png", width = 8 , height = 4)
         