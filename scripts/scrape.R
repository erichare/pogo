library(rvest)
library(dplyr)
library(stringr)

url <- "https://pokemongo.gamepress.gg/raid-boss-counters"
my_nodes <- url %>%
    read_html() %>%
    html_nodes(css = ".raid-boss-pokemon-title a , #block-views-block-raid-boss-counters-block-1 .field--type-string") %>%
    html_text()

my_quick_moves <- url %>%
    read_html() %>%
    html_nodes(css = ".raid-pokemon-quick-move a , .raid-pokemon+ th") %>%
    html_text()

my_charge_moves <- url %>%
    read_html() %>%
    html_nodes(css = ".raid-pokemon-charge-move a , th+ th") %>%
    html_text()

mylist <- list()
for (i in 6:length(my_nodes)) {
    if (str_count(my_nodes[i], "Supreme Counters") == 1 || str_count(my_nodes[i], "Supreme Counter") == 1) {
        boss <- my_nodes[i - 1]
        if (length(mylist) > 1) mylist <- mylist[1:(length(mylist) - 1)]
    }

    if (str_count(my_nodes[i], "Counters") == 1 || str_count(my_nodes[i], "Counter") == 1) {
        type <- strsplit(my_nodes[i], " ")[[1]][1]
    } else {
        poke <- my_nodes[i]
        mylist[[length(mylist) + 1]] <- c(boss, poke, type)
    }
}

mydf <- as.data.frame(do.call(rbind, mylist), stringsAsFactors = FALSE)
names(mydf) <- c("Boss", "Counter", "Type")

pokemon_ind <- 0
mylist_quick <- list()
for (i in 1:length(my_quick_moves)) {
    if (my_quick_moves[i] == "Quick Move") {
        pokemon_ind <- pokemon_ind + 1
    } else {
        poke_list <- c(unlist(mydf[pokemon_ind,]), Quick = my_quick_moves[i])
        mylist_quick[[length(mylist_quick) + 1]] <- poke_list
    }
}

mydf_quick <- as.data.frame(do.call(rbind, mylist_quick), stringsAsFactors = FALSE)
names(mydf_quick) <- c("Boss", "Counter", "Type", "Fast Attack")

pokemon_ind <- 0
mylist_charge <- list()
for (i in 1:length(my_charge_moves)) {
    if (my_charge_moves[i] == "Charge Move") {
        pokemon_ind <- pokemon_ind + 1
    } else {
        poke_list <- c(unlist(mydf[pokemon_ind,]), Charge = my_charge_moves[i])
        mylist_charge[[length(mylist_charge) + 1]] <- poke_list
    }
}

mydf_charge <- as.data.frame(do.call(rbind, mylist_charge), stringsAsFactors = FALSE)
names(mydf_charge) <- c("Boss", "Counter", "Type", "Charged Attack")

mydf_merge <- full_join(mydf_quick, mydf_charge) %>%
    filter(!duplicated(.))

write.csv(mydf_merge, file = "raid_counters.csv", row.names = FALSE)
