library(shiny)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

raid_bosses <- read_csv("data/raid_counters.csv") %>%
    mutate(Type = factor(Type, levels = c("Supreme", "Good", "Glass", "Tank")))

type_colors <- c("blue", "darkblue", "grey", "peru", "lightgreen", "yellow", "darkred", "orangered1", "darkgreen", "bisque3", "lightblue", "purple", "indianred2", "lightsteelblue1", "black", "slategray2", "purple4", "pink")
names(type_colors) <- c("Water", "Dragon", "Normal", "Rock", "Bug", "Electric", "Fighting", "Fire", "Grass", "Ground", "Ice", "Poison", "Psychic", "Steel", "Dark", "Flying", "Ghost", "Fairy")

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    titlePanel("Pokemon Go Attackers"),
    
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = "input.tabs1 == 'Attack Team' || input.tabs1 == 'Raid Counters'",
                             selectInput("trainer", "Trainer", choices = "AB", selected = "AB") 
            ),
            conditionalPanel(condition = "input.tabs1 == 'Attack Team'",
                             selectInput("type", "Type", choices = c("Primary Type", "Secondary Type", "Primary and Secondary Type"))
            ),
            conditionalPanel(condition = "input.tabs1 == 'Raid Counters'",
                             selectInput("boss", "Raid Boss", choices = sort(unique(raid_bosses$Boss)),
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
                         h4("Your Counters"),
                         dataTableOutput("counters"),
                         hr(),
                         h4("All Possible Counters"),
                         dataTableOutput("possible_counters")
                ),
                tabPanel("User Statistics",
                         h4("Statistics by Trainer"),
                         dataTableOutput("stats")
                )
            )
        )
    )
)

server <- function(input, output, session) {

    observe({
        updateSelectInput(session, "trainer", choices = unique(sort(joined_attackers()$Trainer)))
    })
    
    joined_attackers <- reactive({
        pogo <- gs_key("1eMIur0WMbAf13HSEZsrxvF-ZInUIVte8UMyOnN9v5Is")
        
        attackers <- pogo %>% 
            gs_read(ws = 1) %>%
            select(Pokemon, `Fast Attack`, `Charged Attack`, `Primary Type`, `Secondary Type`, `Move Rating`)
        ourpoke <- pogo %>% 
            gs_read(ws = 2) %>%
            select(Trainer, Pokemon, `Fast Attack`, `Charged Attack`, CP, IV = `IV (%)`)
        
        ourpoke %>%
            left_join(attackers)
    })
    
    mydat <- reactive({
        joined_attackers() %>%
            filter(Trainer == input$trainer) %>%
            arrange(desc(CP))
    })
    
    output$type_coverage <- renderPlot({
        if (input$type == "Primary and Secondary Type") {
            missings <- setdiff(names(type_colors), unique(c(mydat()$`Primary Type`, mydat()$`Secondary Type`)))
            
            type_plot <- mydat() %>%
                gather(key = Which, value = Type, 7:8) %>%
                mutate(Type = factor(Type, levels = c(names(sort(table(Type), decreasing = TRUE)), missings))) %>%
                filter(!is.na(Type))
        } else if (input$type == "Primary Type") {
            missings <- setdiff(names(type_colors), unique(mydat()$`Primary Type`))
            
            type_plot <- mydat() %>%
                mutate(Type = factor(`Primary Type`, levels = c(names(sort(table(`Primary Type`), decreasing = TRUE)), missings)))
        } else if (input$type == "Secondary Type") {
            missings <- setdiff(names(type_colors), unique(mydat()$`Secondary Type`))
            
            type_plot <- mydat() %>%
                mutate(Type = factor(`Secondary Type`, levels = c(names(sort(table(`Secondary Type`), decreasing = TRUE)), missings))) %>%
                filter(!is.na(Type))
        }
        ggplot(data = type_plot, aes(x = Type, fill = Type)) +
            geom_bar() +
            scale_x_discrete(drop = FALSE) +
            theme_bw() +
            scale_fill_manual(values = type_colors[match(levels(type_plot$Type), names(type_colors))]) +
            theme(legend.position = "off") +
            ggtitle(paste0(input$trainer, "'s Attack Team by ", input$type)) +
            xlab("Type") +
            ylab("Count")
    })
    
    output$attack <- renderDataTable({
        return(mydat())
    })
    
    output$counters <- renderDataTable({
        mydat() %>%
            left_join(raid_bosses, by = c("Pokemon" = "Counter", "Fast Attack" = "Fast Attack", "Charged Attack" = "Charged Attack")) %>%
            filter(Boss == input$boss) %>%
            arrange(Type) %>%
            rename(`Counter Type` = Type) %>%
            select(-Boss)
    })
    
    output$possible_counters <- renderDataTable({
        raid_bosses %>%
            filter(Boss == input$boss) %>%
            arrange(Type) %>%
            rename(`Counter Type` = Type) %>%
            select(-Boss)
    })
    
    output$stats <- renderDataTable({
        joined_attackers() %>%
            group_by(Trainer) %>%
            summarise(`Avg CP` = round(mean(CP)),
                      `Avg IV` = round(mean(IV)),
                      `Most Common Pokemon` = head(names(sort(table(Pokemon), decreasing = TRUE)), n = 1),
                      `Most Common Fast` = head(names(sort(table(`Fast Attack`), decreasing = TRUE)), n = 1),
                      `Most Common Charged` = head(names(sort(table(`Charged Attack`), decreasing = TRUE)), n = 1),
                      `Most Common Type` = head(names(sort(table(`Primary Type`), decreasing = TRUE)), n = 1))
    })
}

shinyApp(ui = ui, server = server)
