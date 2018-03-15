library(rvest)
library(dplyr)
library(readr)

url <- "https://pokemondb.net/move/all"
poke_moves <- url %>%
    read_html() %>%
    html_table()
poke_moves <- poke_moves[[1]]
poke_moves <- poke_moves %>%
    select(Name, Type)

write_csv(poke_moves, "data/poke_moves.csv")
