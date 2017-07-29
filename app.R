library(shiny)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)
library(readr)

# url <- "https://pokemongo.gamepress.gg/raid-boss-counters"
# my_nodes <- url %>%
#     read_html() %>%
#     html_nodes(css = ".raid-boss-pokemon-title a , #block-views-block-raid-boss-counters-block-1 .field--type-string") %>%
#     html_text()
# 
# my_quick_moves <- url %>%
#     read_html() %>%
#     html_nodes(css = ".raid-pokemon-quick-move a , .raid-pokemon+ th") %>%
#     html_text()
# 
# my_charge_moves <- url %>%
#     read_html() %>%
#     html_nodes(css = ".raid-pokemon-charge-move a , th+ th") %>%
#     html_text()
# 
# mylist <- list()
# for (i in 6:length(my_nodes)) {
#     if (str_count(my_nodes[i], "Supreme Counters") == 1 || str_count(my_nodes[i], "Supreme Counter") == 1) {
#         boss <- my_nodes[i - 1]
#         if (length(mylist) > 1) mylist <- mylist[1:(length(mylist) - 1)]
#     }
#     
#     if (str_count(my_nodes[i], "Counters") == 1 || str_count(my_nodes[i], "Counter") == 1) {
#         type <- strsplit(my_nodes[i], " ")[[1]][1]
#     } else {
#         poke <- my_nodes[i]
#         mylist[[length(mylist) + 1]] <- c(boss, poke, type)
#     }
# }
# 
# mydf <- as.data.frame(do.call(rbind, mylist), stringsAsFactors = FALSE)
# names(mydf) <- c("Boss", "Counter", "Type")
# 
# pokemon_ind <- 0
# mylist_quick <- list()
# for (i in 1:length(my_quick_moves)) {
#     if (my_quick_moves[i] == "Quick Move") {
#         pokemon_ind <- pokemon_ind + 1
#     } else {
#         poke_list <- c(unlist(mydf[pokemon_ind,]), Quick = my_quick_moves[i])
#         mylist_quick[[length(mylist_quick) + 1]] <- poke_list
#     }
# }
# 
# mydf_quick <- as.data.frame(do.call(rbind, mylist_quick), stringsAsFactors = FALSE)
# names(mydf_quick) <- c("Boss", "Counter", "Type", "Fast Attack")
# 
# pokemon_ind <- 0
# mylist_charge <- list()
# for (i in 1:length(my_charge_moves)) {
#     if (my_charge_moves[i] == "Charge Move") {
#         pokemon_ind <- pokemon_ind + 1
#     } else {
#         poke_list <- c(unlist(mydf[pokemon_ind,]), Charge = my_charge_moves[i])
#         mylist_charge[[length(mylist_charge) + 1]] <- poke_list
#     }
# }
# 
# mydf_charge <- as.data.frame(do.call(rbind, mylist_charge), stringsAsFactors = FALSE)
# names(mydf_charge) <- c("Boss", "Counter", "Type", "Charged Attack")
# 
# mydf_merge <- full_join(mydf_quick, mydf_charge) %>%
#     filter(!duplicated(.))
# 
# write.csv(mydf_merge, file = "raid_counters.csv", row.names = FALSE)

pogo <- gs_key("1eMIur0WMbAf13HSEZsrxvF-ZInUIVte8UMyOnN9v5Is")

attackers <- pogo %>% 
    gs_read(ws = 1) %>%
    select(Pokemon, `Fast Attack`, `Charged Attack`, `Primary Type`, `Secondary Type`, `Move Rating`)
ourpoke <- pogo %>% 
    gs_read(ws = 2) %>%
    select(Trainer, Pokemon, `Fast Attack`, `Charged Attack`, CP, IV = `IV (%)`)

joined_attackers <- ourpoke %>%
    left_join(attackers)

raid_bosses <- read_csv("raid_counters.csv") %>%
    mutate(Type = factor(Type, levels = c("Supreme", "Good", "Glass", "Tank")))

type_colors <- c("blue", "darkblue", "grey", "peru", "lightgreen", "yellow", "darkred", "orangered1", "darkgreen", "bisque3", "lightblue", "purple", "indianred2", "lightsteelblue1", "black")
names(type_colors) <- c("Water", "Dragon", "Normal", "Rock", "Bug", "Electric", "Fighting", "Fire", "Grass", "Ground", "Ice", "Poison", "Psychic", "Steel", "Dark")

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    titlePanel("Pokemon Go Attackers"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("trainer", "Trainer", choices = unique(joined_attackers$Trainer)),
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
        
}

shinyApp(ui = ui, server = server)
