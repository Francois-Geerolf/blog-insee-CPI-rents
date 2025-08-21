library(tidyverse)
library(rsdmx)
library(scales)
library(viridis)

# ---- Parameters ----

idbank_codes <- c("001759970", "001763530")
min_date <- as.Date("1990-01-01")
max_date <- as.Date("2021-01-01")

# ---- Build URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import and transform data ----

figure6 <- url |>
  readSDMX() |>
  as_tibble() |>
  transmute(
    TITLE_FR = str_remove(TITLE_FR, "All households"),
    TITLE_FR = str_extract(TITLE_FR, "Actual rents|Overall"),
    TITLE_FR = factor(TITLE_FR,
                      levels = c("Actual rents", "Overall"),
                      labels = c("Actual rents", "Overall CPI (IPC)")),
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  filter(date >= min_date, date <= max_date) |>
  group_by(TITLE_FR) |>
  arrange(date) |>
  mutate(OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]) |>
  ungroup()

# ---- Graph ----

ggplot(figure6, aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = viridis(3)[1:2]
  ) +
  scale_x_date(
    breaks = seq(1990, 2021, 5) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(
    breaks = seq(80, 200, 5),
    labels = dollar_format(accuracy = 1, prefix = "")
  ) +
  labs(
    x = NULL,
    y = "Price Index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.7, 0.2),
    legend.title = element_blank()
  )

# ---- Export graph ----

ggsave("figure6.png", width = 7.5, height = 4.2)
ggsave("figure6.pdf", width = 7.5, height = 4.2, device = cairo_pdf)

