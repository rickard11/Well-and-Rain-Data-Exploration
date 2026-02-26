#well data cleaning and automation
library(tidyverse)
library(dplyr)
library(ggplot2)
#Read in all well data
getwd()
folder_path <- "Data/well/2-11-2026/"
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
get_well_name <- function(filename) {
  name <- basename(filename)
  name <- str_remove(name, "_well.*")
  return(name)}

well_data <- files %>%
  set_names(map_chr(., get_well_name)) %>%
  map(read_csv)

well_data <- map_dfr(files, ~ read_csv(.x) %>%
                       mutate(Name = get_well_name(.x)))


