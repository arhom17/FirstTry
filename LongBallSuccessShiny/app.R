#    http://shiny.rstudio.com/

library(shiny)

ui <- fluidPage(
    titlePanel("Long Balls"),

    sidebarLayout(
        sidebarPanel(selectInput("cin", "Selection:", choices = c("Successful", "Unsuccessful", "Both"))),

        mainPanel(
           plotOutput("longBallPlot"))
        )
)

server <- function(input, output) {
    
        output$longBallPlot <- renderPlot({
            
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
    
}

shinyApp(ui = ui, server = server)
