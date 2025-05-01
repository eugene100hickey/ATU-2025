library(tidyverse)
library(readxl)

z_file <- "../ATU-2025-private/data/chris/s1m10339-chris.txt"
x_file <-  "../ATU-2025-private/data/chris/coh_sample.xlsx"

z <- read.delim(z_file) |> 
  janitor::clean_names()

indices <- which(z$comment == "MM")

z1 <- z[indices[1]:indices[2],] |> 
  as_tibble() |> 
  filter(value != "MM")

z2 <- z$comment |> as.numeric()
z3 <- z[is.na(z2),]
z4 <- z[!is.na(z2),] |> as.numeric()


zq <- tibble(x = z$comment[141:20141] |> as.numeric(), y =  z$comment[20146:40146] |> as.numeric())

zq |> ggplot(aes(x, y)) + geom_point()
