library(tidyverse)
library(rio)
library(here)
library(tabulizer)
library(rtoot)
library(eurostat)
library(rentrez)
library(seqinr)
library(rvest)

import(system.file("extdata", package = "dslabs", "olive.csv"))

import("https://github.com/rfordatascience/tidytuesday/raw/refs/heads/main/data/2025/2025-01-21/exped_tidy.csv")

import(here(.libPaths()[1], "readxl", "extdata", "datasets.xls"), which = "quakes")

here(system.file(package="tabulizer"), "examples", "data.pdf") |> extract_tables(output = "data.frame") |> pluck(1)

organ_transplant <- "https://nhsbtdbe.blob.core.windows.net/umbraco-assets-corp/18494/centre-specific-activity-report.pdf"
extract_tables(organ_transplant, output = "data.frame", pages = 1) |> pluck(1) |> gt::gt()

auth_setup("mastodon.social", "public")
z <- get_timeline_hashtag(hashtag = "eurovision", instance = "mastodon.social", limit = 20) 
(table <- datatable(z[, "content"], escape = FALSE) )


#### workshop ####

z <- import("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv")

system.file(package = "readxl", "extdata") |> list.files()
z <- import(system.file(package = "readxl", "extdata", "deaths.xlsx"), which = "other", skip = 4)

z <- import("https://docs.google.com/spreadsheets/d/1_Qfq64QDWsd-6pgODmk7wfeMBguegqQs-yHg326x6-s/edit?usp=sharing")

z <- extract_tables("https://assets.gov.ie/static/documents/school-self-evaluation-guidelines-2016-2020-primary.pdf", 
                    output = "data.frame")[[6]]


diabetes <- search_eurostat("diabetes")
z <- get_eurostat(id = diabetes$code[1])

z |> filter(duration == "Y_LT1",
            geo == "SI", 
            TIME_PERIOD == "2019-01-01", 
            sex == "T",
            age == "Y_GE75", 
            isced11 == "ED3_4")

z <- entrez_search(db = "nuccore", term = "brca1")

my_url <- "https://en.wikipedia.org/wiki/2023%E2%80%9324_Premier_League"
my_selector <- "tr:nth-child(11) .infobox-data a"
my_html <- read_html(my_url)
z <- html_elements(my_html, my_selector)
html_text2(z)


# Fri May  2 18:07:42 2025 ------------------------------

education <- search_eurostat("education")
my_feature <- get_eurostat_dic("indic_md")
z <- get_eurostat(id = "G25103")

z |> filter(duration == "Y_LT1",
            geo == "SI", 
            TIME_PERIOD == "2019-01-01", 
            sex == "T",
            age == "Y_GE75", 
            isced11 == "ED3_4")


#####
# (@) Install the `eurostat` package. Search for datasets on *diabetes* using the `search_eurostat()` function. Get the *eurostat* dataset on screening of diabetes by sex, age, and education from 2014-2019. Decode the column values using `get_eurostat_dic()` (e.g. `get_eurostat_dic("hlthcare")`). Find the incidence of diabetes/cardiovascular that has been detected for [*less than one year*]{.underline} in [*Slovenia*]{.underline} in [*2019*]{.underline} for people of [*all*]{.underline} genders who are [*older than 75*]{.underline} with [*Upper secondary and post-secondary non-tertiary education (levels 3 and 4)*]{.underline}. __[74, 87.4, 77.2]__
#####