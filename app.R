library(shiny)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(shinycssloaders)

raid_bosses <- read_csv("data/raid_counters.csv") %>%
    mutate(Type = factor(Type, levels = c("Supreme", "Good", "Glass", "Tank")))
pokes <- read_csv("data/poke_stats.csv")
attackers <- read_csv("data/poke_grades.csv")
moves <- read_csv("data/poke_moves.csv")

type_colors <- c("blue", "darkblue", "grey", "peru", "lightgreen", "yellow", "pink", "darkred", "orangered1", "purple3", "darkgreen", "bisque3", "lightblue", "purple", "indianred2", "lightsteelblue1", "black", "slategray2", "purple4", "pink")
names(type_colors) <- c("Water", "Dragon", "Normal", "Rock", "Bug", "Electric", "Fairy", "Fighting", "Fire", "Ghost", "Grass", "Ground", "Ice", "Poison", "Psychic", "Steel", "Dark", "Flying", "Ghost", "Fairy")

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    titlePanel("Pokemon Go Attackers"),
    
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = "input.tabs1 == 'Attack Team' || input.tabs1 == 'Raid Counters'",
                             selectInput("trainer", "Trainer", choices = "ERH", selected = "ERH")
            ),
            conditionalPanel(condition = "input.tabs1 == 'Attack Team'",
                             selectInput("type", "Type", choices = c("Move Type", "Quick Move Type", "Charge Move Type", "Pokemon Type"))
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
                         withSpinner(plotOutput("type_coverage")),
                         hr(),
                         h4("Attack Team Data"),
                         withSpinner(dataTableOutput("attack"))
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
                ),
                tabPanel("Pokemon Statistics",
                         h4("Statistics by Pokemon"),
                         dataTableOutput("poke_stats")
                ),
                tabPanel("Move Statistics",
                         h4("Statistics by Move"),
                         dataTableOutput("move_stats")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    observe({
        updateSelectInput(session, "trainer", choices = unique(sort(joined_attackers()$Trainer)), selected = input$trainer)
    })
    
    joined_attackers <- reactive({
        pogo <- gs_key("1eMIur0WMbAf13HSEZsrxvF-ZInUIVte8UMyOnN9v5Is")
    
        ourpoke <- pogo %>% 
            gs_read(ws = 2) %>%
            rename(Quick = `Fast Attack`, Charge = `Charged Attack`, LVL = Level, `ATK IV` = `Attack IV`, 
                   `DEF IV` = `Defense IV`, `STA IV` = `Stamina IV`) %>%
            select(Trainer:`STA IV`)
        
        ourpoke %>%
            left_join(attackers, by = c("Pokemon" = "Name", "Quick" = "Quick", "Charge" = "Charge"))
    })

    mydat <- reactive({
        joined_attackers() %>%
            filter(Trainer == input$trainer) %>%
            arrange(desc(CP))
    })
    
    output$type_coverage <- renderPlot({
        result <- mydat() %>%
            left_join(select(moves, Name, `Quick Type` = `Pokemon Type`), by = c("Quick" = "Name")) %>%
            left_join(select(moves, Name, `Charge Type` = `Pokemon Type`), by = c("Charge" = "Name")) %>%
            left_join(select(pokes, Name, `First Pokemon Type` = `First Type`), by = c("Pokemon" = "Name")) %>%
            left_join(select(pokes, Name, `Second Pokemon Type` = `Second Type`), by = c("Pokemon" = "Name"))
        
        if (input$type == "Move Type") {
            plot_data <- result %>%
                select(Pokemon, `Quick Type`, `Charge Type`) %>%
                gather(key = Which, value = Type, 2:3)
        } else if (input$type == "Quick Move Type") {
            plot_data <- result %>%
                select(Pokemon, Type = `Quick Type`)
        } else if (input$type == "Charge Move Type") {
            plot_data <- result %>%
                select(Pokemon, Type = `Charge Type`)
        } else if (input$type == "Pokemon Type") {
            plot_data <- result %>%
                select(Pokemon, `First Pokemon Type`, `Second Pokemon Type`) %>%
                gather(key = Which, value = Type, 2:3)
        }
        
        missings <- setdiff(names(type_colors), unique(plot_data$Type))
        plot_data <- plot_data %>%
            filter(!is.na(Type)) %>%
            mutate(Type = factor(Type, levels = c(names(sort(table(Type), decreasing = TRUE)), missings)))
      
        ggplot(data = plot_data, aes(x = Type, fill = Type)) +
            geom_bar() +
            scale_x_discrete(drop = FALSE) +
            theme_bw() +
            scale_fill_manual(values = type_colors[match(levels(plot_data$Type), names(type_colors))]) +
            theme(legend.position = "off") +
            ggtitle(paste0(input$trainer, "'s Attack Team by ", input$type)) +
            xlab("Type") +
            ylab("Count")
    })
    
    output$attack <- renderDataTable({
        return(mydat() %>%
                   select(-Trainer))
    })
    
    output$counters <- renderDataTable({
        mydat() %>%
            left_join(raid_bosses, by = c("Pokemon" = "Counter", "Quick" = "Fast Attack", "Charge" = "Charged Attack")) %>%
            filter(Boss == input$boss) %>%
            arrange(Type) %>%
            select(`Counter Type` = Type, everything()) %>%
            select(-Boss, -Trainer)
    })
    
    output$possible_counters <- renderDataTable({
        raid_bosses %>%
            filter(Boss == input$boss) %>%
            arrange(Type) %>%
            select(`Counter Type` = Type, everything()) %>%
            select(-Boss)
    })
    
    output$stats <- renderDataTable({
        joined_attackers() %>%
            group_by(Trainer) %>%
            summarise(`Avg CP` = round(mean(CP)),
                      `Avg IV` = round(mean(`IV (%)`)),
                      `Most Common Pokemon` = head(names(sort(table(Pokemon), decreasing = TRUE)), n = 1),
                      `Most Common Quick` = head(names(sort(table(Quick), decreasing = TRUE)), n = 1),
                      `Most Common Charge` = head(names(sort(table(Charge), decreasing = TRUE)), n = 1))
    })
        
    output$poke_stats <- renderDataTable({
        return(pokes)
    })
    
    output$move_stats <- renderDataTable({
        return(moves)
    })
}

shinyApp(ui = ui, server = server)
