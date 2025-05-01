library(tidyverse)
library(rvest)

z <- rio::import("https://github.com/eugene100hickey/ATU-2025/blob/main/rexams/data/excel-example.xlsx?raw=true", 
            which = "skillpad")

imdb_url <- "https://en.wikipedia.org/wiki/List_of_current_cardinals"
imdb_url <- "https://www.boxofficemojo.com/year/world/2021/"
imdb_url <- "https://www.atu.ie/research/research-centres/heritage-research-group"
imbd_url <- "https://www.imdb.com/search/title/?title_type=feature"
imdb <- read_html(imdb_url)
imdb_xml <- "td:nth-child(3) > a"
imdb_xml <- "td:nth-child(6) > a"
imdb_xml <- ".mojo-field-type-percent~ .mojo-field-type-percent"
imdb_xml <- "a"
imdb_nodes <- imdb %>% html_nodes(imdb_xml) %>% html_text()
imdb_nodes |> table()


z <- rio::import("https://raw.githubusercontent.com/eugene100hickey/ATU-2023/main/rexams/data/university-rankings-kaggle.csv") |> 
  filter(country == "Turkey")
 

z <- rio::import("https://raw.githubusercontent.com/eugene100hickey/ATU-2023/main/rexams/data/Methane_final.csv") |> 
  filter(region == "Russia & Caspian", type == "Agriculture")


# Wed Apr 30 10:39:16 2025 ------------------------------

year <- 2000:2024 |> 
  sample(1)
new_url <- glue::glue("https://www.boxofficemojo.com/year/world/{year}/")

w <- read_html(new_url)

css <- ".mojo-field-type-percent:nth-child(5)"

data_html <- html_nodes(w, css)
my_data <- html_text2(data_html) |> 
  str_remove_all("%") |> 
  as.numeric() |> 
  mean(na.rm = T) |> 
  round(1)

