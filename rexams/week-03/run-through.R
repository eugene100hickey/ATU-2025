library(tidyverse)

brain_gene_expression <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/ABAData?raw=true") |> 
  filter(hgnc_symbol %in% c("DNAJC21", "PLCB2"))

brain_gene_expression |> 
  group_by(hgnc_symbol, age) |> 
  summarise(my_mean = mean(signal)) |> 
  pivot_wider(names_from = hgnc_symbol, values_from = my_mean)

gene_data <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/ABA-adult?raw=true")
brain_data <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/brain-area-code?raw=true")
gene_data <- gene_data |> 
  left_join(brain_data) |> 
  filter(brain_area == "Cerebral Cortex")
mean(gene_data$signal)


brain_gene_expression_wide <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/ABAData-wide?raw=true") |> 
  select(structure, ECHDC2, COPB1, SHE, PLCB2, LIN7A, DNAJC21, PPP1R13L, MAML1, ZDHHC20, CDCA8)

brain_long <- brain_gene_expression_wide |> 
  pivot_longer(cols = -structure, names_to = "my_gene", values_to = "my_signal")
mean(brain_long$my_signal)


library(tidyverse)
library(dslabs)

my_number <- 4
set.seed(my_number)
falling <- rfalling_object(n = 1000)
falling <- falling |> 
  mutate(my_difference = abs(observed_distance - distance))
sum(falling$my_difference)


glimpse(gapminder)
gapminder |> 
  filter(year == 1998) |> 
  group_by(continent) |> 
  summarise(cont_pop = sum(population)/1e6,
            my_life = mean(life_expectancy)) |> 
  ungroup()



format(as.Date("2025-09-19"), "%y %d %m")



glimpse(murders)
murders |> 
  mutate(pop_ratio = total / population) |> 
  filter(region == "Northeast") |> 
  arrange(desc(pop_ratio))


state_areas <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/state-areas?raw=true")
state_regions <- read_rds("https://github.com/eugene100hickey/ATU-2023/blob/main/rexams/data/state-regions?raw=true")
glimpse(state_areas)
glimpse(state_regions)
z <- state_regions |> 
  left_join(state_areas, by = join_by("state" == "state_name"))  |> 
  filter(region == "East North Central")
sum(z$area)


library(tidyverse)
library(dslabs)
head(us_contagious_diseases)

z <- us_contagious_diseases |> 
  filter(disease == "Polio", year == 1936)
sum(z$count)
