library(tidyverse)
library(rsdmx)
library(scales)
library(zoo)
library(viridis)

# ---- Parameters ----

idbank_codes <- c("010567012", "010567056")
min_date <- as.Date("1998-01-01")
max_date <- as.Date("2022-01-01")

# ---- Build URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import and transform data ----

figure7 <- url |>
  readSDMX() |>
  as_tibble() |>
  transmute(
    TITLE_EN = str_extract(TITLE_EN, "Paris|Metropolitan France"),
    TITLE_EN = factor(TITLE_EN,
                      levels = c("Paris", "Metropolitan France"),
                      labels = c("Paris", "Metropolitan France")),
    date = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  filter(date >= min_date, date <= max_date) |>
  group_by(TITLE_EN) |>
  arrange(date) |>
  mutate(OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]) |>
  ungroup()

# ---- Graph ----

ggplot(figure7, aes(x = date, y = OBS_VALUE, color = TITLE_EN)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = viridis(3)[1:2]
  ) +
  scale_x_date(
    breaks = seq(1998, 2025, 2) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(
    breaks = seq(50, 7000, by = 50)
  ) +
  labs(
    x = NULL,
    y = "Price Index of Existing Housing"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.25, 0.85),
    legend.title = element_blank()
  )

# ---- Export ----

ggsave("figure7.png", width = 7.5, height = 4.2)
ggsave("figure7.pdf", width = 7.5, height = 4.2, device = cairo_pdf)

