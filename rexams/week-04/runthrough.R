library(tidyverse)
library(rvest)
library(rentrez)
library(seqinr)
library(rio)
library(googlesheets4)
library(rnoaa)
library(tabulizer)
library(tidyquant)

z <- import("https://github.com/eugene100hickey/ATU-2025/blob/main/rexams/data/excel-example.xlsx?raw=true", 
            which = "brexit")

my_url <- "https://www.boxofficemojo.com/year/world/2007/"

bom <- read_html(my_url)
my_css <-  ".mojo-field-type-percent:nth-child(5)"
my_page <- html_elements(bom, my_css)
html_text2(my_page) |> 
str_remove_all("%") |> 
  as.numeric() |> 
  sum(na.rm = T) |> 
  round(1)

z <- rio::import("https://raw.githubusercontent.com/eugene100hickey/ATU-2023/main/rexams/data/university-rankings-kaggle.csv") |> 
  filter(country == "Ukraine")
 

z <- rio::import("https://raw.githubusercontent.com/eugene100hickey/ATU-2023/main/rexams/data/Methane_final.csv") |> 
  filter(region == "Russia & Caspian", type == "Energy")

sum(z$emissions)

Rabies <- entrez_search(db="nuccore", term="MK981888", retmax=40)
my_downloaded_sequence <- entrez_fetch(db="nuccore", id=Rabies$ids[1], rettype="fasta")
write(my_downloaded_sequence, "some-file-name.fasta", sep="\n")
z <- read.fasta(file = "some-file-name.fasta")[[1]]


google_link <- "https://docs.google.com/spreadsheets/d/1Jrr9I-GcGiusqkgRJQsQ3UFmsoXet3wsH3r1HQZZyms/edit?usp=sharing"
z <- read_sheet(google_link, sheet = "brca")
mean(z$texture_worst, na.rm = T)

z <- ncdc_stations(locationid = "FIPS:EI")$data
z1 <- ncdc(stationid = "GHCND:EI000003953", 
           datasetid = "GHCND", 
           startdate = "1963-10-01", 
           enddate = "1964-10-01",
           datatypeid = "PRCP",
           limit = 500)

z <- extract_tables("https://raw.githubusercontent.com/eugene100hickey/cao-pdf/master/data/DN-2016.pdf",
                    output = "data.frame")
z[[1]]$final |> mean(na.rm = T)


z <- tq_index("DOW")
z1 <- tq_get("KO")
z2 <- z1 |> 
  filter(date > "2018-01-02",
         date < "2023-04-13")
mean(z2$close)
