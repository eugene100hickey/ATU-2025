library(tidyverse)
library(pdftools)
library(tabulizer)
library(rvest)


z <- "https://www.ipcc.ch/site/assets/uploads/2017/09/WG1AR5_AnnexII_FINAL.pdf"
z1 <- extract_tables(z)
z2 <- extract_tables(z, pages = 10, output = "data.frame", skip = 0)

row_index <- which(z1[[1]] == "Applicants-March" | z1[[1]] == "Total Applications")
column_index <- which(z1[[1]][row_index,] == "Applicants-March" | z1[[1]][row_index,] == "Total Applications")
z1[[1]][row_index, column_index+1] |>
  str_remove_all(",") |>
  as.numeric()

# Fri Apr 28 11:33:59 2023 ------------------------------

new_url <- "https://en.wikipedia.org/wiki/2020%E2%80%9321_Premier_League"
w <- read_html(new_url)
css <- "tr:nth-child(11) .infobox-data a"

data_html <- html_nodes(w, css)
html_text2(data_html)

