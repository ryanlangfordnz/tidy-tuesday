#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggstream)
library(babynames)


baby_data <- data.frame(babynames)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Baby Names"),  # Add a title panel
  sidebarLayout(  # Make the layout a sidebarLayout
    sidebarPanel(
      textInput(inputId = "baby_name", 
                label = "Enter a name here", "")
    ),  # Inside the sidebarLayout, add a sidebarPanel
    mainPanel(
      plotOutput("stream_plot")
    )  # Inside the sidebarLayout, add a mainPanel
  
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    renderPlot(
    babynames %>% filter(name == str_to_title(input$baby_name)) %>% ggplot() + 
    geom_stream(aes(x=year, y = n, fill = sex), type = "mirror") +
    scale_fill_manual(values = c("#ed968c","#88a0dc"))+
    #theme_void() +
    theme(legend.position = "none")+
    ggtitle(input$baby_name)) -> output$stream_plot
   
}

# Run the application 
shinyApp(ui = ui, server = server)
