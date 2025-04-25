brain_gene_expression <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/ABAData?raw=true") |> 
  filter(hgnc_symbol %in% c("FGF6", "OR52W1"))

brain_gene_expression |> 
  group_by(hgnc_symbol, age) |> 
  summarise(my_mean = mean(signal)) |> 
  pivot_wider(names_from = hgnc_symbol, values_from = my_mean)


gene_data <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/ABA-adult?raw=true")
brain_data <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/brain-area-code?raw=true")
gene_data <- gene_data |> 
  left_join(brain_data) |> 
  filter(brain_area == "Pons")
mean(gene_data$signal)


brain_gene_expression_wide <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/ABAData-wide?raw=true") |> 
  select(structure, SHE, ALKBH6, N6AMT1, MYH7B, FAM179A, ATG7, TTC32, AHNAK, ALDH6A1, DNAJC21)

brain_long <- brain_gene_expression_wide |> 
  pivot_longer(cols = -structure, names_to = "my_gene", values_to = "my_signal")


library(tidyverse)
library(dslabs)

my_number <- 35
set.seed(my_number)
falling <- rfalling_object(n = 1000)
falling <- falling |> 
  mutate(my_difference = abs(observed_distance - distance))


glimpse(gapminder)
gapminder |> 
  filter(year == 2012) |> 
  group_by(continent) |> 
  summarise(cont_pop = sum(population)/1e6,
            my_life = mean(life_expectancy)) |> 
  ungroup()



format(as.Date("2023-09-19"), "%Y %m %d")



glimpse(murders)
murders |> 
  mutate(pop_ratio = total / population) |> 
  filter(region == "South") |> 
  arrange(desc(pop_ratio))


state_areas <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/state-areas?raw=true")
state_regions <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/state-regions?raw=true")
glimpse(state_areas)
glimpse(state_regions)
z <- state_regions |> 
  left_join(state_areas, by = join_by("state" == "state_name"))  |> 
  filter(region == "Mountain")
sum(z$area)


library(tidyverse)
library(dslabs)
head(us_contagious_diseases)

z <- us_contagious_diseases |> 
  filter(disease == "Pertussis", year == 2001)
sum(z$count)