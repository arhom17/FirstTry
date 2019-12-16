#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(ggformula)
library(png)
library(ggpubr)
library(grid)

#################################################################################

ui <- fluidPage(
    titlePanel("Long Balls"),

    sidebarLayout(
        sidebarPanel(selectInput("cin", "Selection:", choices = c("Successful", "Unsuccessful", "Both"))),

        mainPanel(
           plotOutput("longBallPlot"))
        )
)
#################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
    
        output$longBallPlot <- renderPlot({
            
            if(input$cin == 'Successful'){
                longBallClean %>% filter(LongBallSuccess == 1) %>% gf_point(longbally ~ longballx, color = "Blue", width = 1, height = 1) + scale_fill_continuous(guide = FALSE) + annotation_custom(rasterGrob(img, width = unit(0.82, "npc"),height = unit(1.1,"npc")),-Inf, Inf, -Inf, Inf) + lims(x=c(-0.6,4.6), y = c(0,6))
            }
            
            else if(input$cin == 'Unsuccessful'){
                longBallClean %>% filter(LongBallSuccess == 2) %>% gf_point(longbally ~ longballx, color = "Red", width = 1, height = 1) + scale_fill_continuous(guide = FALSE) + annotation_custom(rasterGrob(img, width = unit(0.82, "npc"),height = unit(1.1,"npc")),-Inf, Inf, -Inf, Inf) + lims(x=c(-0.6,4.6), y = c(0,6))
            }
            
            else if(input$cin == 'Both'){
                longBallClean %>% gf_point(longbally ~ longballx, color =~ LongBallSuccess, width = 1, height = 1) + scale_color_continuous(low = "blue", high = "red") + scale_fill_continuous(guide = FALSE) + annotation_custom(rasterGrob(img, width = unit(0.82, "npc"),height = unit(1.1,"npc")),-Inf, Inf, -Inf, Inf) + lims(x=c(-0.6,4.6), y = c(0,6))
            }
            
            })
    
}

#################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
