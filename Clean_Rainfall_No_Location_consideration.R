library(tidyverse)
library(dplyr)
library(tidyr)
library(broom)
#Read in all rain data
folder_path <- "Data/Rain/2-10-2026/"
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
get_rain_name <- function(filename) {
  name <- basename(filename)
  name <- str_remove(name, " Rain.*")
  return(name)}
rain_data <- files %>%
  set_names(map_chr(., get_rain_name)) %>%
  map(read_csv)
rain_data <- map_dfr(files, ~ read_csv(.x) %>%
  mutate(Name = get_rain_name(.x)))
rain_data$Date<-as.Date(rain_data$Date,format="%Y-%m-%d (%a)")

#Fill in dates for missing days of data (ex when the battery dies)
rain_data_full <- rain_data%>%
  group_by(Name) %>%
  complete(Date = seq(min(Date), as.Date("2026-02-10"), by = "day") ) %>%
  ungroup()

rain_wide <- rain_data_full %>%
  select(Date, Name, `Rain (in)`) %>%
  pivot_wider(names_from = Name, values_from = `Rain (in)`)

rain_wide_JLDP<-rain_wide[,1:18]

rain_filtered <- rain_wide_JLDP %>%
  filter(rowSums(across(-Date, ~ .x > 0.01), na.rm = TRUE) > 0)

cor_mat <- rain_filtered %>%
  select(-Date) %>%
  cor(use = "pairwise.complete.obs")

cor_long <- as_tibble(cor_mat, rownames = "station_x") %>%
  pivot_longer(-station_x, names_to = "station_y", values_to = "r")

cor_long <- cor_long %>%
  filter(station_x != station_y)

top3_cor <- cor_long %>%
  group_by(station_x) %>%
  slice_max(order_by = r, n = 3, with_ties = FALSE) %>%
  ungroup()

write.csv(top3_cor,"Data/processed/Best_Match_Stations_dataset_no_distance_restriction.csv")

#Now getting regression equations
pairs_all <- expand_grid(
  station_x = names(rain_wide_JLDP)[-1],
  station_y = names(rain_wide_JLDP)[-1]) %>%
  filter(station_x != station_y)

rain_filtered <- rain_wide_JLDP %>%
  filter(rowSums(across(-Date, ~ .x > 0.01), na.rm = TRUE) > 0)

regression_table <- pairs_all %>%
  mutate(model = map2(station_x, station_y, ~ {
    x <- rain_filtered[[.x]]
    y <- rain_filtered[[.y]]
    df_pair <- tibble(x = x, y = y) %>% drop_na()
    if (nrow(df_pair) < 5) return(NULL)
    lm(y ~ x, data = df_pair)})) %>%
  filter(!map_lgl(model, is.null)) %>%
  mutate(slope = map_dbl(model, ~ coef(.x)[2]),
    intercept = map_dbl(model, ~ coef(.x)[1]),
    r = map2_dbl(station_x, station_y,
                 ~ cor(rain_filtered[[.x]], rain_filtered[[.y]],
                       use = "complete.obs")),
    r2 = map_dbl(model, ~ summary(.x)$r.squared),
    p_value = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    equation = paste0( "y = ",
      round(slope, 3), "x + ", round(intercept, 3))) %>%
      select(-model)

write.csv(regression_table,"Data/processed/Rainy_day_regression_all.csv")












