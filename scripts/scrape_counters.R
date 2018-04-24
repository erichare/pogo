library(rvest)
library(dplyr)
library(stringr)

base_url <- "https://pokemongo.gamepress.gg"
url <- file.path(base_url, "raid-boss-counters")

system("phantomjs generate_html.js")

raid_bosses <- read_html("full_counters.html") %>%
    html_nodes(".no-weather a")
raid_bosses_url <- html_attr(raid_bosses, "href")
raid_bosses_names <- gsub("  \n\n\n ", "", html_text(raid_bosses))

mydf <- data.frame(name = raid_bosses_names, url = paste0(base_url, raid_bosses_url))

myres <- apply(mydf, 1, function(x) {
    cat(x[1], "\n")
    
    thepage <- x[2] %>%
        read_html()
    
    counters <- thepage %>%
        html_nodes(css = "#block-gamepressbase-content .field--label-hidden.field__item , #block-gamepressbase-content h2") %>%
        html_text()
    
    fast_moves <- thepage %>%
        html_nodes(css = ".raid-pokemon-quick-move a , .raid-pokemon+ th") %>%
        html_text()
    
    charge_moves <- thepage %>%
        html_nodes(css = ".paragraph--view-mode--default th+ th , .raid-pokemon-charge-move a") %>%
        html_text()
    
    myvec <- gsub("\n", "", counters)
    myvec <- myvec[grep("VS |Vs ", myvec, invert = TRUE)]
    
    mylist <- list()
    for (i in 1:length(myvec)) {
        if (str_count(myvec[i], "Counters") == 1 || str_count(myvec[i], "Counter") == 1) {
            type <- strsplit(myvec[i], " ")[[1]][1]
        } else {
            poke <- myvec[i]
            mylist[[length(mylist) + 1]] <- c(x[1], poke, type)
        }
    }
    
    newdf <- as.data.frame(do.call(rbind, mylist), stringsAsFactors = FALSE)
    names(newdf) <- c("Boss", "Counter", "Type")
    
    pokemon_ind <- 0
    mylist_quick <- list()
    for (i in 1:length(fast_moves)) {
        if (fast_moves[i] == "Fast Move") {
            pokemon_ind <- pokemon_ind + 1
        } else {
            poke_list <- c(unlist(newdf[pokemon_ind,]), Quick = fast_moves[i])
            mylist_quick[[length(mylist_quick) + 1]] <- poke_list
        }
    }
    
    mydf_quick <- as.data.frame(do.call(rbind, mylist_quick), stringsAsFactors = FALSE)
    names(mydf_quick) <- c("Boss", "Counter", "Type", "Fast Attack")
    
    pokemon_ind <- 0
    mylist_charge <- list()
    for (i in 1:length(charge_moves)) {
        if (charge_moves[i] == "Charge Move") {
            pokemon_ind <- pokemon_ind + 1
        } else {
            poke_list <- c(unlist(newdf[pokemon_ind,]), Charge = charge_moves[i])
            mylist_charge[[length(mylist_charge) + 1]] <- poke_list
        }
    }
    
    mydf_charge <- as.data.frame(do.call(rbind, mylist_charge), stringsAsFactors = FALSE)
    names(mydf_charge) <- c("Boss", "Counter", "Type", "Charged Attack")
    
    mydf_merge <- full_join(mydf_quick, mydf_charge) %>%
        filter(!duplicated(.))
    
    return(mydf_merge)
})

final_scrape <- do.call(rbind, myres)
rownames(final_scrape) <- NULL

write.csv(final_scrape, file = "data/raid_counters.csv", row.names = FALSE)
