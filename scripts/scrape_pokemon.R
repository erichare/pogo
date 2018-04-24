library(rvest)
library(dplyr)
library(tidyr)
library(readr)

poke_inds <- 1:386
base_url <- "https://pokemongo.gamepress.gg/pokemon/"
urls <- paste0(base_url, poke_inds)

pokemon_data <- lapply(urls, function(url) {
    cat("Processing", url, "\n")
    
    mylist <- NULL
    try({
        full_html <- url %>%
            read_html()
        
        pokemon_name <- full_html %>%
            html_nodes("#block-gamepressbase-page-title .field--label-hidden") %>%
            html_text()
        
        pokemon_type <- full_html %>%
            html_nodes(".field--name-field-pokemon-type img") %>%
            html_attr("src") %>%
            strsplit("/") %>%
            sapply(function(x) { x[length(x)] })
        pokemon_type <- tools::toTitleCase(gsub(".gif", "", pokemon_type))
        
        pokemon_stats <- full_html %>%
            html_nodes("#evolution-requirements td , .header-stats") %>%
            html_text()
        pokemon_stats <- pokemon_stats[grep("\n|%", pokemon_stats)]
        pokemon_stats <- as.numeric(gsub("[^0-9\\.]", "", pokemon_stats))
        
        pokemon_stats_names <- full_html %>%
            html_nodes(".header-stats") %>%
            html_text()
        pokemon_stats_names[1:3] <- c("Attack", "Defense", "Stamina")
        
        names(pokemon_stats) <- pokemon_stats_names
        
        pokemon_moves <- full_html %>%
            html_nodes(".primary-move-title a span") %>%
            html_text()
        
        pokemon_move_grades <- full_html %>%
            html_table(fill = TRUE)
        pokemon_move_grades <- pokemon_move_grades[[1]]
        if ("Quick" %in% names(pokemon_move_grades)) {
            pokemon_move_grades$Quick <- sapply(strsplit(pokemon_move_grades$Quick, "\n"), `[`, 1)
            pokemon_move_grades$Charge <- sapply(strsplit(pokemon_move_grades$Charge, "\n"), `[`, 1)
        } else {
            pokemon_move_grades <- NULL
        }
        
        Sys.sleep(1)
        
        mylist <- list(name = pokemon_name, 
                       type = pokemon_type, 
                       stats = pokemon_stats, 
                       moves = pokemon_moves,
                       grades = pokemon_move_grades)
    })
    
    return(mylist)
})

all_moves <- sort(unique(unlist(sapply(pokemon_data, function(x) { x$moves }))))

move_data <- lapply(all_moves, function(move) {
    move_proc <- gsub(" ", "-", tolower(move))
    base_url <- "https://pokemongo.gamepress.gg/pokemon-move/"
    
    url <- paste0(base_url, move_proc)
    
    cat("Processing", url, "\n")
    result <- url %>%
        read_html() %>%
        html_table()
    result <- as.data.frame(t(result[[1]]))
    names(result) <- unlist(result[1,])
    result <- result[-1,]
    rownames(result) <- NULL
    
    Sys.sleep(1)
    
    return(result)
})

fast_moves <- move_data[sapply(move_data, `[`, 1, 1) == "Fast Move"]
charge_moves <- move_data[sapply(move_data, `[`, 1, 1) == "Charge Move"]

fast_move_df <- plyr:::rbind.fill(fast_moves) %>%
    cbind(Name = all_moves[sapply(move_data, `[`, 1, 1) == "Fast Move"]) %>%
    mutate_all(funs(as.character)) %>%
    select(Name, everything())
fast_move_df$`Damage Window` <- as.numeric(gsub(" seconds", "", fast_move_df$`Damage Window`))
fast_move_df$`Move Cooldown` <- as.numeric(gsub(" seconds", "", fast_move_df$`Move Cooldown`))

fast_move_df <- fast_move_df %>%
    mutate_at(vars(`Base Power`, `Energy Delta`, `Damage Per Second`, `Energy Per Second`), funs(as.numeric)) %>%
    arrange(Name) %>%
    select(-`Move Type`) %>%
    rename(Type = `Pokemon Type`)

charge_move_df <- plyr:::rbind.fill(charge_moves) %>%
    cbind(Name = all_moves[sapply(move_data, `[`, 1, 1) == "Charge Move"]) %>%
    mutate_all(funs(as.character)) %>%
    select(Name, everything())
charge_move_df$`Damage Window` <- as.numeric(gsub(" seconds", "", charge_move_df$`Damage Window`))
charge_move_df$`Move Cooldown` <- as.numeric(gsub(" seconds", "", charge_move_df$`Move Cooldown`))
charge_move_df$`Charge Energy` <- as.numeric(gsub(" Energy", "", charge_move_df$`Charge Energy`))

charge_move_df <- charge_move_df %>%
    mutate_at(vars(`Base Power`, `Damage per Energy`, `Damage Per Second`, `DPE*DPS`), funs(as.numeric)) %>%
    arrange(Name) %>%
    select(-`Move Type`) %>%
    rename(Type = `Pokemon Type`)

write_csv(fast_move_df, "data/poke_fast_moves.csv")
write_csv(charge_move_df, "data/poke_charge_moves.csv")

all_grades <- lapply(pokemon_data, function(x) {
    if (is.null(x$grades)) return(NULL)
    
    cbind(Name = x$name, x$grades)
})
grades_df <- plyr:::rbind.fill(all_grades) %>%
    mutate_all(funs(as.character)) %>%
    replace_na(list(ATK = "X", DEF = "X"))

write_csv(grades_df, "data/poke_grades.csv")

all_stats <- lapply(pokemon_data, function(x) {
    cbind(Name = x$name, `First Type` = x$type[1], `Second Type` = x$type[2], as.data.frame(t(x$stats)))
})
stats_df <- plyr:::rbind.fill(all_stats) %>%
    mutate_all(funs(as.character)) %>%
    mutate_at(vars(Attack:`Capture Rate`), funs(as.numeric)) %>%
    mutate_at(vars(`Female Ratio`, `Male Ratio`, `Flee Rate`, `Capture Rate`), function(x) x / 100)

write_csv(stats_df, "data/poke_stats.csv")
