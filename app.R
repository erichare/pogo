library(shiny)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(ggplot2)

pogo <- gs_key("1eMIur0WMbAf13HSEZsrxvF-ZInUIVte8UMyOnN9v5Is")
attackers <- pogo %>% 
    gs_read(ws = 1) %>%
    select(Pokemon, `Fast Attack`, `Charged Attack`, `Primary Type`, `Secondary Type`, `Move Rating`, `Raid Boss I`, `Raid Boss II`, `Raid Boss III`)
ourpoke <- pogo %>% 
    gs_read(ws = 2) %>%
    select(Trainer, Pokemon, `Fast Attack`, `Charged Attack`, CP, IV = `IV (%)`)

joined_attackers <- ourpoke %>%
    left_join(attackers)

type_colors <- c("blue", "darkblue", "grey", "peru", "lightgreen", "yellow", "darkred", "orangered1", "darkgreen", "bisque3", "lightblue", "purple", "indianred2", "lightsteelblue1")
names(type_colors) <- c("Water", "Dragon", "Normal", "Rock", "Bug", "Electric", "Fighting", "Fire", "Grass", "Ground", "Ice", "Poison", "Psychic", "Steel")

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    titlePanel("Pokemon Go Attackers"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("trainer", "Trainer", choices = unique(joined_attackers$Trainer)),
            conditionalPanel(condition = "input.tabs1 == 'Raid Counters'",
                             selectInput("boss", "Raid Boss", choices = sort(unique(c(joined_attackers$`Raid Boss I`,
                                                                                      joined_attackers$`Raid Boss II`,
                                                                                      joined_attackers$`Raid Boss III`))),
                                         selected = "Machamp")
            )
        ),
        
        mainPanel(
            tabsetPanel(id = "tabs1",
                tabPanel("Attack Team",
                         h4("Attack Team Plot"),
                         plotOutput("type_coverage"),
                         hr(),
                         h4("Attack Team Data"),
                         dataTableOutput("attack")
                ),
                tabPanel("Raid Counters",
                         dataTableOutput("counters")
                )
            )
        )
    )
)

server <- function(input, output) {
    
    mydat <- reactive({
        joined_attackers %>%
            filter(Trainer == input$trainer)
    })
    
    output$type_coverage <- renderPlot({
        missings <- setdiff(names(type_colors), unique(mydat()$`Primary Type`))
            
        type_plot <- mydat() %>%
            mutate(`Primary Type` = factor(`Primary Type`, levels = c(names(sort(table(`Primary Type`), decreasing = TRUE)), missings)))
        ggplot(data = type_plot, aes(x = `Primary Type`, fill = `Primary Type`)) +
            geom_bar() +
            scale_x_discrete(drop = FALSE) +
            theme_bw() +
            scale_fill_manual(values = type_colors[match(levels(type_plot$`Primary Type`), names(type_colors))]) +
            theme(legend.position = "off") +
            ggtitle(paste0(input$trainer, "'s Attack Team by Primary Type")) +
            xlab("Type") +
            ylab("Count")
    })
    
    output$attack <- renderDataTable({
        return(mydat())
    })
    
    output$counters <- renderDataTable({
        mydat() %>%
            filter(`Raid Boss I` == input$boss | `Raid Boss II` == input$boss | `Raid Boss III` == input$boss) %>%
            select(Trainer:`Move Rating`)
    })
        
}

shinyApp(ui = ui, server = server)
