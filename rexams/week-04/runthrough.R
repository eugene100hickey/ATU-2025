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
            which = "skillpad")

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
  filter(country == "Indonesia")
 

z <- rio::import("https://raw.githubusercontent.com/eugene100hickey/ATU-2023/main/rexams/data/Methane_final.csv") |> 
  filter(region == "Europe", type == "Energy")

sum(z$emissions)

Japanese_encephalitis_virus <- entrez_search(db="nuccore", term="L48961", retmax=40)
my_downloaded_sequence <- entrez_fetch(db="nuccore", id=Japanese_encephalitis_virus$ids[1], rettype="fasta")
write(my_downloaded_sequence, "some-file-name.fasta", sep="\n")
z <- read.fasta(file = "some-file-name.fasta")[[1]]


google_link <- "https://docs.google.com/spreadsheets/d/1Jrr9I-GcGiusqkgRJQsQ3UFmsoXet3wsH3r1HQZZyms/edit?usp=sharing"
z <- read_sheet(google_link, sheet = "co2")
mean(z$co2, na.rm = T)

z <- ncdc_stations(locationid = "FIPS:EI")$data
z1 <- ncdc(stationid = "GHCND:EIM00003967", 
           datasetid = "GHCND", 
           startdate = "2008-02-01", 
           enddate = "2009-02-01",
           datatypeid = "PRCP",
           limit = 500)

z <- extract_tables("https://raw.githubusercontent.com/eugene100hickey/cao-pdf/master/data/DN-2019.pdf",
                    output = "data.frame")
z[[1]]$final |> mean(na.rm = T)


z <- tq_index("DOW")
z1 <- tq_get("NKE")
z2 <- z1 |> 
  filter(date > "2020-01-02",
         date < "2023-04-13")
mean(z2$close)
