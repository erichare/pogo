library(shiny)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

raid_bosses <- read_csv("data/raid_counters.csv") %>%
    mutate(Type = factor(Type, levels = c("Supreme", "Good", "Glass", "Tank")))

type_colors <- c("blue", "darkblue", "grey", "peru", "lightgreen", "yellow", "darkred", "orangered1", "darkgreen", "bisque3", "lightblue", "purple", "indianred2", "lightsteelblue1", "black", "slategray2")
names(type_colors) <- c("Water", "Dragon", "Normal", "Rock", "Bug", "Electric", "Fighting", "Fire", "Grass", "Ground", "Ice", "Poison", "Psychic", "Steel", "Dark", "Flying")

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
            ),
            conditionalPanel(condition = "input.tabs1 == 'Catch Rates'",
                             h4("Instructions"),
                             HTML("Enter the throw parameters below.<br><br>For the Circle Radius, a value of 1 means the circle was as big as possible. An average nice throw has a radius of 0.85, and average great throw has a radius of 0.5, and an average excellent throw has a radius of 0.15<br><br>For the badges, choose each of the two types. For instance, against Articuno, if you have the Gold flying badge but the Silver ice badge, select Gold for the first and Silver for the second."),
                             hr(),
                             h4("Multipliers"),
                             numericInput("rate", "Base Catch Rate (Articuno = .03, Lugia = .02)", value = .03, step = .01),
                             sliderInput("radius", "Circle Radius", min = 0, max = 1, value = 1),
                             checkboxInput("curve", "Curve Ball"),
                             selectInput("berry", "Berry", choices = c("None" = 1, "Razz Berry" = 1.5, "Golden Razz Berry" = 2.5)),
                             selectInput("medal1", "Badge 1", choices = c("None" = 1, "Bronze" = 1.1, "Silver" = 1.2, "Gold" = 1.3)),
                             selectInput("medal2", "Badge 2", choices = c("None" = 1, "Bronze" = 1.1, "Silver" = 1.2, "Gold" = 1.3))
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
                tabPanel("Catch Rates",
                         h4("The Simulation"),
                         h5(textOutput("prob")),
                         h5(textOutput("prob2")),
                         hr(),
                         plotOutput("cumprob"),
                         hr(),
                         h4("The Formula"),
                         h3(withMathJax("$$\\text{Probability} = 1 - \\left(1 - \\frac{BCR}{2 \\times CPM}\\right)^{\\text{Multipliers}}$$"))
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
            filter(Trainer == input$trainer)
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
    
    catchprob <- reactive({
        curvemult <- ifelse(input$curve, 1.7, 1)
        radmult <- 2 - input$radius
        berrymult <- as.numeric(input$berry)
        medalmult <- (as.numeric(input$medal1) + as.numeric(input$medal2)) / 2
        cpm <- 0.79030001
        
        result <- 1 - (1 - input$rate /  (2 * cpm))^(curvemult * radmult * berrymult * medalmult)
        
        return(result)
    })
    
    output$prob <- renderText({
        return(paste("With a single premiere ball, your chances of catching the legendary are", paste0(round(100 * catchprob(), digits = 2), "%")))
    })
    
    output$prob2 <- renderText({
        return(paste("With seven premiere balls, your chances of catching the legendary are", paste0(round(100 * (1 - (1 - catchprob())^7), digits = 2), "%")))
    })
    
    output$cumprob <- renderPlot({
        cumprobs <- 1 - (1 - catchprob())^(1:25)
        
        mydf <- data.frame(Balls = 1:25, CatchProb = cumprobs, Text = rep("", 25), stringsAsFactors = FALSE)
        mydf$Text[c(1, 7)] <- paste0(round(100 * cumprobs[c(1, 7)], digits = 2), "%")
        
        ggplot(data = mydf, aes(x = Balls, y = CatchProb, colour = CatchProb)) +
            geom_point(size = 2.5) +
            geom_line() +
            geom_text(aes(label = Text, vjust = 1, hjust = -0.2)) +
            theme_bw() +
            ggtitle("Probability of Catching the Legendary Within the Given Number of Balls") +
            geom_hline(yintercept = .5) +
            xlab("Number of Balls Thrown") +
            ylab("Catch Probability") +
            scale_y_continuous(breaks = seq(0, 1, by = .1), labels = seq(0, 1, by = .1), limits = c(0, 1)) +
            scale_x_continuous(breaks = 0:25, minor_breaks = NULL) +
            scale_color_gradient(low = "#FF0000", high = "#00FF00", limits = c(0, 1))
    })
}

shinyApp(ui = ui, server = server)
