# https://github.com/rfordatascience/tidytuesday
# https://github.com/doehm/tidytues/blob/main/scripts/2025/15%20-%20penguins/penguins.R

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggsankey)
library(ggbeeswarm)
library(cropcircles)

make_caption <- function(accent, bg, data) {
  
  if(length(accent) != 3) {
    accent <- rep(accent[1], 3)
  }
  
  github <- glue("<span style='font-family:fa-brands; color:{accent[1]}'>&#xf09b;</span>")
  bluesky <- glue("<span style='font-family:fa-brands; color:{accent[2]}'>&#xe671;</span>")
  linkedin <- glue("<span style='font-family:fa-brands; color:{accent[3]}'>&#xf08c;</span>")
  
  twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
  threads <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xe618;</span>")
  mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
  floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
  
  space <- glue("<span style='color:{bg};font-size:1px'>''</span>")
  space2 <- glue("<span style='color:{bg}'>-</span>") # can't believe I'm doing this
  
  glue("
       {github} {space} eugene100hickey {space2}
       {mastodon} {space} @eugene100hickey {space2}
       {linkedin} {space} eugene100hickey
       ")
}

# 💾 load data ---------------------------------------------------------------

penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-15/penguins.csv')
penguins_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-15/penguins_raw.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "assets/fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
accent <- txt

bg1 <- "white"
bg2 <- "grey85"

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')

font_add_google("Raleway", "raleway", regular.wt = 200)
ft <- "raleway"
showtext_auto()

img_url <- "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png"

pal <- c(Chinstrap = '#c25dca', Gentoo = '#0f7473', Adelie = '#ff7f03')

# 🚙 functions ---------------------------------------------------------------



# 🤼 wrangle -----------------------------------------------------------------

df <- penguins |>
  select(species, bill_len, bill_dep, flipper_len, body_mass) |>
  mutate(body_mass = body_mass/1000) |>
  drop_na() |>
  pivot_longer(-species) |>
  mutate(
    name = case_when(
      name == "bill_len" ~ "Bill length",
      name == "bill_dep" ~ "Bill depth",
      name == "flipper_len" ~ "Flipper length",
      name == "body_mass" ~ "Body mass"
    )
  )

df_means <- df |>
  group_by(species, name) |>
  summarise(
    mean = mean(value)
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)


# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_boxplot(aes(value, species, colour = species, fill = species), alpha = 0.2, width = 0.5) +
  geom_beeswarm(aes(value, species, colour = species), size = 4, alpha = 0.5, cex = 1.5) +
  geom_richtext(aes(mean, species, label = round(mean, 1), colour = species), df_means, family = ft, size = 12, fill = bg, label.r = unit(0.3, "lines")) +
  facet_wrap(~name, scales = "free_x") +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    title = glue('Palmer Penguins'),,
    subtitle = glue('<img src="{img_url}" height = 140>'),
    caption = caption,
    colour = "Species"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_markdown(size = 128, hjust = 0.5, margin = margin(b = 0)),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none",
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0.25, "cm"),
    strip.background = element_rect(fill = txt),
    strip.text = element_text(colour = bg, margin = margin(t = 5, b = 5), size = 48),
    panel.background = element_rect(fill = grid::radialGradient(c(bg1, bg2)), colour = NA),
    legend.margin = margin(b = 20)
  )
