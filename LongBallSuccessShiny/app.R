#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(ggformula)
library(png)
library(ggpubr)
library(grid)
library(gridExtra)

ui <- fluidPage(
    titlePanel("Long Balls"),

    sidebarLayout(
        sidebarPanel(selectInput("cin", "Selection:", choices = c("Successful", "Unsuccessful", "Both"))),

        mainPanel(
            
            tabsetPanel(type = "tabs", tabPanel("Location", h4("Long Ball Location", plotOutput("longBallPlot1", width = "75%"))), tabPanel("Time", h4("Long Ball Time", plotOutput("longBallPlot2")))),
            
            dataTableOutput("longBallPlot4"),
            
            dataTableOutput("longBallPlot3")
               
        )
    )
)

server <- function(input, output) {
    
        output$longBallPlot1 <- renderPlot({
            
            if(input$cin == 'Successful'){
                longBallClean %>% filter(LongBallSuccess == 1) %>% gf_point(longbally ~ longballx, color =~ LongBallSuccess, width = 1, height = 1) + scale_color_continuous(low = "blue", high = "blue") + scale_fill_continuous(guide = FALSE) + annotation_custom(rasterGrob(img, width = unit(0.82, "npc"),height = unit(1.1,"npc")),-Inf, Inf, -Inf, Inf) + lims(x=c(-0.6,4.6), y = c(0,6))
            }
            
            else if(input$cin == 'Unsuccessful'){
                longBallClean %>% filter(LongBallSuccess == 2) %>% gf_point(longbally ~ longballx, color =~ LongBallSuccess, width = 1, height = 1) + scale_color_continuous(low = "red", high = "red") + scale_fill_continuous(guide = FALSE) + annotation_custom(rasterGrob(img, width = unit(0.82, "npc"),height = unit(1.1,"npc")),-Inf, Inf, -Inf, Inf) + lims(x=c(-0.6,4.6), y = c(0,6))
            }
            
            else if(input$cin == 'Both'){
                longBallClean %>% gf_point(longbally ~ longballx, color =~ LongBallSuccess, width = 1, height = 1) + scale_color_continuous(low = "blue", high = "red") + scale_fill_continuous(guide = FALSE) + annotation_custom(rasterGrob(img, width = unit(0.82, "npc"),height = unit(1.1,"npc")),-Inf, Inf, -Inf, Inf) + lims(x=c(-0.6,4.6), y = c(0,6))
            }
            
            })
        
        output$longBallPlot2 <- renderPlot({
            
            
            if(input$cin == 'Successful'){
                longBallClean %>% filter(LongBallSuccess == 1) %>% mutate(LongBall = as_factor(LongBall), LongBall = fct_lump_min(LongBall, min = 1)) %>% filter(LongBallTimeStamp <= 95) %>%
                    mutate(LongBallSuccess = as_factor(LongBallSuccess),
                           LongBallSuccess = fct_recode(LongBallSuccess,"Successful"="1", "Unsuccessful"="2")) %>% gf_histogram(~LongBallTimeStamp, fill =~ LongBallSuccess) + scale_fill_manual(values = c("Blue", "Blue"))
                }
            
            else if(input$cin == 'Unsuccessful'){
                longBallClean %>% filter(LongBallSuccess == 2) %>% mutate(LongBall = as_factor(LongBall), LongBall = fct_lump_min(LongBall, min = 1)) %>% filter(LongBallTimeStamp <= 95) %>%
                    mutate(LongBallSuccess = as_factor(LongBallSuccess),
                           LongBallSuccess = fct_recode(LongBallSuccess,"Successful"="1", "Unsuccessful"="2")) %>% gf_histogram(~LongBallTimeStamp, fill =~ LongBallSuccess) + scale_fill_manual(values = c("Red", "Red"))
                }
            
            else if(input$cin == 'Both'){
                longBallClean %>% mutate(LongBall = as_factor(LongBall), LongBall = fct_lump_min(LongBall, min = 1)) %>% filter(LongBallTimeStamp <= 95) %>%
                    mutate(LongBallSuccess = as_factor(LongBallSuccess),
                           LongBallSuccess = fct_recode(LongBallSuccess,"Successful"="1", "Unsuccessful"="2")) %>% gf_histogram(~LongBallTimeStamp, fill =~ LongBallSuccess) + scale_fill_manual(values = c("Blue", "Red"))
                }
            
        })
        
        
        output$longBallPlot4 <- renderDataTable({
            
            longBallClean %>%  group_by(LongBall, LongBallSuccess) %>% 
                count() %>% spread(LongBallSuccess, n) %>% 
                mutate( `1` = replace_na(`1`,0),
                        `2` = replace_na(`2`,0)) %>%
                mutate(Tries = `1`+`2`,
                       SuccessRate = `1`/Tries ) %>%
                rename(PracticeJerseyNumber = LongBall,
                       Successes = `1`,
                       Fails = `2`) %>%
                arrange(desc(Successes, SuccessRate)) 
            
        })
        
        output$longBallPlot3 <- renderDataTable({
            
            longBallClean %>%  group_by(LongBallSuccess) %>% 
                count() %>% spread(LongBallSuccess, n) %>% mutate(Tries = `1`+`2`,
                                                                  SuccessRate = `1`/Tries) %>% rename(Successes = `1`, Fails = `2`)
            
        })
            
    
}

shinyApp(ui = ui, server = server)
