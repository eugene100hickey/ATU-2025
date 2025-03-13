library(tidyverse)
library(pdftools)
library(ggokabeito)   # Colorblind-friendly color palette
library(showtext)
library(ggrepel)
library(here)
library(gt)
library(pdftools)


directory <- "C:\\Users\\ehickey\\OneDrive - Technological University Dublin\\Desktop\\Academic\\Hector\\fizzics\\fizzics\\_posts\\2021-12-06-university-choices\\data"

font_add("Fuzzy Bubbles", regular = here(directory, "../fonts/FuzzyBubbles-Regular.ttf"))
showtext_auto()

theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 28, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 32),
          axis.title = element_text(face = "bold", size = 36),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 24))
}

years <- c("07", "08", "09", "10", "13", 14:19 )
cao_points_year <- function(year) {
  cao_pdf <- glue::glue("http://www2.cao.ie/points/lvl8_{year}.pdf")
  z <- pdf_text(cao_pdf) %>%
    str_split("\n") %>%
    unlist()
  z <- z[!str_detect(z, "^ ") & z != "" & !str_detect(z, "Course Code") & str_count(z, "  +") == 3] %>% # gets rid of non-data rows
    str_split("  +") %>% # splits rows based on runs of several spaces
    unlist() %>%
    str_remove("#") %>%
    str_remove("\\*") # deletes some annoying characters
  z <- tibble(year = glue::glue("20{year}"),
              code = z[c(T, F, F, F)],
              course = z[c(F, T, F, F)],
              final = z[c(F, F, T, F)],
              medium = z[c(F, F, F, T)])
  # original list made bunches of four elements that together described a course. This tibble winds them up to one row
  z
}

z <- map_df(years, cao_points_year) %>%
  mutate(year = as.numeric(year))

z21 <- readxl::read_excel(here(directory, "CAOPointsCharts2021.xlsx"),
                          sheet = "EOS_2021", skip = 10) %>%
  janitor::clean_names() %>%
  filter(course_level == 8) %>%
  mutate(year = 2021) %>%
  select(year,
         code = course_code,
         course = course_title,
         final = eos_points,
         medium = eos_midpoints) %>%
  mutate(year = 2021,
         medium = as.character(medium))
z20 <- readxl::read_excel(here(directory,"CAOPointsCharts2020.xlsx"),
                          sheet = "PointsCharts2020_V2", skip = 9) %>%
  janitor::clean_names() %>%
  filter(level == 8) %>%
  mutate(year = 2020) %>%
  select(year,
         code = course_code2,
         course = course_title,
         final = eos,
         medium = eos_mid_point) %>%
  mutate(year = 2020)

z <- bind_rows(z, z20, z21)

z1 <- z %>%
  select(code, course) %>%
  group_by(code) %>%
  mutate(last_name = if_else(row_number() == n(), 1 ,0)) %>%
  filter(last_name == 1) %>%
  select(code, last_name = course)

z <- z %>%
  left_join(z1) %>%
  mutate(final = as.numeric(final),
         medium = as.numeric(medium),
         label = if_else(year == max(year),
                         as.character(code), NA_character_)) |>
  mutate(code1 = str_sub(code, 1, 2))


courses <- z1 %>% filter(str_detect(last_name, "Physics"),
                         !str_detect(last_name, "Agri")) %>%
  pull(code)


max_years <- z %>%  filter(code %in% courses) %>%
  group_by(code) %>%
  summarize(year_max = max(year)) |>
  arrange(desc(year_max))

courses <- max_years$code[10:18]

z %>%  filter(code %in% courses) %>%
  left_join(max_years) %>%
  mutate(last_name = str_replace(last_name, "Environmental Science", "EnvSci.")) %>%
  mutate(label = ifelse(year == year_max, code, "")) %>%
  mutate(code = glue::glue("{code}: {abbreviate(last_name, 20)}")) %>%
  ggplot(aes(year, final, colour = code, group = code)) +
  geom_line(size = 2) +
  geom_point(size = 5) +
  geom_label_repel(aes(label = label),
                   nudge_x = 0.2,
                   size = 12,
                   na.rm = TRUE,
                   show.legend = F) +
  scale_color_okabe_ito() +
  scale_y_continuous(breaks = seq(100, 700, by = 50)) +
  labs(y = "Final Points", x = "") +
  theme_clean() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(col=guide_legend(nrow=3))


length <- min(c(50, nrow(z2)))

z2[1:length,] |>
  gt()
