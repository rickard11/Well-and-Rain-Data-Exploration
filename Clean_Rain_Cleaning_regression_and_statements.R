# inport data
library(tidyverse)
library(dplyr)
library(tidyr)
library(broom)
library(writexl)
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

#Transform each item out of the list to a date format
df_list <- purrr::map(rain_data, ~ dplyr::mutate(.x, Date = as.Date(Date, format = "%Y-%m-%d (%a)")))
#Transorm Date to a sequenced list
df_list_filled <- map(df_list, ~ .x %>%
         complete(Date = seq(min(Date, na.rm = TRUE),as.Date("2026-02-10"), by = "day")))

#Remove spaces in Names for DF creation
names(df_list_filled) <- gsub(" ", "_", names(df_list_filled))

#Change name of rain column to include site name
df_list_filled <- imap(df_list_filled, ~ {.x %>%
    rename(!!paste0(.y, "_Rain_in") := `Rain (in)`)})

################################################################################
################################################################################
#Regression functions
#Identify top 3 correlations and complete regression for each site.
rain_data <- map_dfr(files, ~ read_csv(.x) %>%
                       mutate(Name = get_rain_name(.x)))
rain_wide <- rain_data %>%
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
bm<-top3_cor

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

eq<-regression_table

#Merge correlation coefficients and regression table
both<-merge(bm,eq,by=c("station_x","station_y"),all.x=TRUE,all.y=FALSE)
both<-both[,c(1,2,6,9)]

################################################################################
################################################################################
#seperate array to individuala data frames
list2env(df_list_filled, envir = .GlobalEnv)

#Escondido 2
JLDP_Escondido_2 <- JLDP_Escondido_2 %>% 
  left_join(JLDP_Escondido_3 %>% select(Date,JLDP_Escondido_3_Rain_in),by="Date")
JLDP_Escondido_2 <- JLDP_Escondido_2 %>% 
  left_join(JLDP_Escondido_5_new %>% select(Date,JLDP_Escondido_5_new_Rain_in),by="Date")
JLDP_Escondido_2 <- JLDP_Escondido_2 %>% 
  left_join(JLDP_Tinta_3 %>% select(Date,JLDP_Tinta_3_Rain_in),by="Date")

JLDP_Escondido_2$E3_reg<-0.915*(JLDP_Escondido_2$JLDP_Escondido_3_Rain_in)
JLDP_Escondido_2$E5_reg<-0.936*(JLDP_Escondido_2$JLDP_Escondido_5_new_Rain_in)
JLDP_Escondido_2$T3_reg<-1.167*(JLDP_Escondido_2$JLDP_Tinta_3_Rain_in)

#Escondido 2 Impute Statements
JLDP_Escondido_2$dat_miss <-!is.na(JLDP_Escondido_2$JLDP_Escondido_2_Rain_in) &
  JLDP_Escondido_2$JLDP_Escondido_2_Rain_in == 0 &
  (replace_na(JLDP_Escondido_2$JLDP_Escondido_3_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Escondido_2$JLDP_Escondido_5_new_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Escondido_2$JLDP_Tinta_3_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Escondido_2$dat_NA<-is.na(JLDP_Escondido_2$JLDP_Escondido_2_Rain_in) # if NA or missing data
JLDP_Escondido_2$Imputed<-JLDP_Escondido_2$dat_NA==TRUE|JLDP_Escondido_2$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Escondido_2$rain_clean <-ifelse(JLDP_Escondido_2$Imputed == FALSE,
        JLDP_Escondido_2$JLDP_Escondido_2_Rain_in,
        coalesce(JLDP_Escondido_2$E3_reg,JLDP_Escondido_2$E5_reg,JLDP_Escondido_2$T3_reg))

#Escondido 3
JLDP_Escondido_3 <- JLDP_Escondido_3 %>% 
  left_join(JLDP_Tinta_4 %>% select(Date,JLDP_Tinta_4_Rain_in),by="Date")
JLDP_Escondido_3 <- JLDP_Escondido_3 %>% 
  left_join(JLDP_Escondido_5_new %>% select(Date,JLDP_Escondido_5_new_Rain_in),by="Date")
JLDP_Escondido_3 <- JLDP_Escondido_3 %>% 
  left_join(JLDP_Tinta_5 %>% select(Date, JLDP_Tinta_5_Rain_in), by = "Date")

JLDP_Escondido_3$E5_reg<-1.019*(JLDP_Escondido_3$JLDP_Escondido_5_new_Rain_in)
JLDP_Escondido_3$T5_reg<-0.946*(JLDP_Escondido_3$JLDP_Tinta_5_Rain_in)
JLDP_Escondido_3$T4_reg<-1.069*(JLDP_Escondido_3$JLDP_Tinta_4_Rain_in)

#Esscondido 3 Impute Statements
JLDP_Escondido_3$dat_miss <-!is.na(JLDP_Escondido_3$JLDP_Escondido_3_Rain_in) &
  JLDP_Escondido_3$JLDP_Escondido_3_Rain_in == 0 &
  (replace_na(JLDP_Escondido_3$JLDP_Escondido_5_new_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Escondido_3$JLDP_Tinta_5_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Escondido_3$JLDP_Tinta_4_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Escondido_3$dat_NA<-is.na(JLDP_Escondido_3$JLDP_Escondido_3_Rain_in) # if NA or missing data
JLDP_Escondido_3$Imputed<-JLDP_Escondido_3$dat_NA==TRUE|JLDP_Escondido_3$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Escondido_3$rain_clean <-ifelse(JLDP_Escondido_3$Imputed == FALSE,
      JLDP_Escondido_3$JLDP_Escondido_3_Rain_in,
      coalesce(JLDP_Escondido_3$E5_reg,JLDP_Escondido_3$T5_reg,JLDP_Escondido_3$T4_reg))

#Escondido 5
JLDP_Escondido_5_new <- JLDP_Escondido_5_new %>% 
  left_join(JLDP_Tinta_4 %>% select(Date,JLDP_Tinta_4_Rain_in),by="Date")
JLDP_Escondido_5_new <- JLDP_Escondido_5_new %>% 
  left_join(JLDP_Escondido_3 %>% select(Date,JLDP_Escondido_3_Rain_in),by="Date")
JLDP_Escondido_5_new <- JLDP_Escondido_5_new %>% 
  left_join(JLDP_Tinta_10 %>% select(Date, JLDP_Tinta_10_Rain_in), by = "Date")

JLDP_Escondido_5_new$E3_reg<-0.967*(JLDP_Escondido_5_new$JLDP_Escondido_3_Rain_in)
JLDP_Escondido_5_new$T10_reg<-0.81*(JLDP_Escondido_5_new$JLDP_Tinta_10_Rain_in)
JLDP_Escondido_5_new$T4_reg<-1.03*(JLDP_Escondido_5_new$JLDP_Tinta_4_Rain_in)

#Escondido 5 Impute Statements
JLDP_Escondido_5_new$dat_miss <-!is.na(JLDP_Escondido_5_new$JLDP_Escondido_5_new_Rain_in) &
  JLDP_Escondido_5_new$JLDP_Escondido_5_new_Rain_in == 0 &
  (replace_na(JLDP_Escondido_5_new$JLDP_Escondido_3_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Escondido_5_new$JLDP_Tinta_10_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Escondido_5_new$JLDP_Tinta_4_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Escondido_5_new$dat_NA<-is.na(JLDP_Escondido_5_new$JLDP_Escondido_5_new_Rain_in) # if NA or missing data
JLDP_Escondido_5_new$Imputed<-JLDP_Escondido_5_new$dat_NA==TRUE|JLDP_Escondido_5_new$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Escondido_5_new$rain_clean <-ifelse(JLDP_Escondido_5_new$Imputed == FALSE,
      JLDP_Escondido_5_new$JLDP_Escondido_5_new_Rain_in,
      coalesce(JLDP_Escondido_5_new$E3_reg,JLDP_Escondido_5_new$T10_reg,JLDP_Escondido_5_new$T4_reg))

#Gaspar 1
JLDP_Gaspar_1 <- JLDP_Gaspar_1 %>% 
  left_join(JLDP_Tinta_1 %>% select(Date,JLDP_Tinta_1_Rain_in),by="Date")

JLDP_Gaspar_1$T1_reg<-1.18*(JLDP_Gaspar_1$JLDP_Tinta_1_Rain_in)

#Gaspar_1 Impute Statements
JLDP_Gaspar_1$dat_miss <-!is.na(JLDP_Gaspar_1$JLDP_Gaspar_1_Rain_in) &
  JLDP_Gaspar_1$JLDP_Gaspar_1_Rain_in == 0 &
  (replace_na(JLDP_Gaspar_1$JLDP_Tinta_1_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Gaspar_1$dat_NA<-is.na(JLDP_Gaspar_1$JLDP_Gaspar_1_Rain_in) # if NA or missing data
JLDP_Gaspar_1$Imputed<-JLDP_Gaspar_1$dat_NA==TRUE|JLDP_Gaspar_1$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Gaspar_1$rain_clean <-ifelse(JLDP_Gaspar_1$Imputed == FALSE,
             JLDP_Gaspar_1$JLDP_Gaspar_1_Rain_in,
             JLDP_Gaspar_1$T1_reg)

#Jalama Vaqueros
JLDP_Jalama_Vaqueros <- JLDP_Jalama_Vaqueros %>% 
  left_join(JLDP_Tinta_10 %>% select(Date,JLDP_Tinta_10_Rain_in),by="Date")
JLDP_Jalama_Vaqueros <- JLDP_Jalama_Vaqueros %>% 
  left_join(JLDP_Escondido_5_new %>% select(Date,JLDP_Escondido_5_new_Rain_in),by="Date")
JLDP_Jalama_Vaqueros <- JLDP_Jalama_Vaqueros %>% 
  left_join(JLDP_Tinta_5 %>% select(Date, JLDP_Tinta_5_Rain_in), by = "Date")

JLDP_Jalama_Vaqueros$T10_reg<-0.903*(JLDP_Jalama_Vaqueros$JLDP_Tinta_10_Rain_in)
JLDP_Jalama_Vaqueros$E5_reg<-0.706*(JLDP_Jalama_Vaqueros$JLDP_Escondido_5_new_Rain_in)
JLDP_Jalama_Vaqueros$T5_reg<-0.717*(JLDP_Jalama_Vaqueros$JLDP_Tinta_5_Rain_in)

#Jalama Vaqueros Impute Statements
JLDP_Jalama_Vaqueros$dat_miss <-!is.na(JLDP_Jalama_Vaqueros$JLDP_Jalama_Vaqueros_Rain_in) &
  JLDP_Jalama_Vaqueros$JLDP_Jalama_Vaqueros_Rain_in == 0 &
  (replace_na(JLDP_Jalama_Vaqueros$JLDP_Tinta_10_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Jalama_Vaqueros$JLDP_Escondido_5_new_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Jalama_Vaqueros$JLDP_Tinta_5_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Jalama_Vaqueros$dat_NA<-is.na(JLDP_Jalama_Vaqueros$JLDP_Jalama_Vaqueros_Rain_in) # if NA or missing data
JLDP_Jalama_Vaqueros$Imputed<-JLDP_Jalama_Vaqueros$dat_NA==TRUE|JLDP_Jalama_Vaqueros$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Jalama_Vaqueros$rain_clean <-ifelse(JLDP_Jalama_Vaqueros$Imputed == FALSE,
         JLDP_Jalama_Vaqueros$JLDP_Jalama_Vaqueros_Rain_in,
         coalesce(JLDP_Jalama_Vaqueros$T10_reg,JLDP_Jalama_Vaqueros$E5_reg,JLDP_Jalama_Vaqueros$T5_reg))
#Oaks 1
JLDP_Oaks_1 <- JLDP_Oaks_1 %>% 
  left_join(JLDP_Tinta_10 %>% select(Date,JLDP_Tinta_10_Rain_in),by="Date")
JLDP_Oaks_1 <- JLDP_Oaks_1 %>% 
  left_join(JLDP_Escondido_3 %>% select(Date,JLDP_Escondido_3_Rain_in),by="Date")
JLDP_Oaks_1 <- JLDP_Oaks_1 %>% 
  left_join(JLDP_Escondido_2 %>% select(Date, JLDP_Escondido_2_Rain_in), by = "Date")

JLDP_Oaks_1$E2_reg<-1.122*(JLDP_Oaks_1$JLDP_Escondido_2_Rain_in)
JLDP_Oaks_1$E3_reg<-1.055*(JLDP_Oaks_1$JLDP_Escondido_3_Rain_in)
JLDP_Oaks_1$T10_reg<-0.903*(JLDP_Oaks_1$JLDP_Tinta_10_Rain_in)

#Oaks 1 Impute Statements
JLDP_Oaks_1$dat_miss <-!is.na(JLDP_Oaks_1$JLDP_Oaks_1_Rain_in) &
  JLDP_Oaks_1$JLDP_Oaks_1_Rain_in == 0 &
  (replace_na(JLDP_Oaks_1$JLDP_Escondido_2_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Oaks_1$JLDP_Escondido_3_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Oaks_1$JLDP_Tinta_10_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Oaks_1$dat_NA<-is.na(JLDP_Oaks_1$JLDP_Oaks_1_Rain_in) # if NA or missing data
JLDP_Oaks_1$Imputed<-JLDP_Oaks_1$dat_NA==TRUE|JLDP_Oaks_1$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Oaks_1$rain_clean <-ifelse(JLDP_Oaks_1$Imputed == FALSE,
           JLDP_Oaks_1$JLDP_Oaks_1_Rain_in,
           coalesce(JLDP_Oaks_1$E2_reg,JLDP_Oaks_1$E3_reg,JLDP_Oaks_1$T10_reg))

#Oaks 3B
JLDP_Oaks_3B <- JLDP_Oaks_3B %>% 
  left_join(JLDP_Tinta_10 %>% select(Date,JLDP_Tinta_10_Rain_in),by="Date")
JLDP_Oaks_3B <- JLDP_Oaks_3B %>% 
  left_join(JLDP_Escondido_5_new %>% select(Date,JLDP_Escondido_5_new_Rain_in),by="Date")
JLDP_Oaks_3B <- JLDP_Oaks_3B %>% 
  left_join(JLDP_Oaks_5 %>% select(Date, JLDP_Oaks_5_Rain_in), by = "Date")

JLDP_Oaks_3B$E5_reg<-1.023*(JLDP_Oaks_3B$JLDP_Escondido_5_new_Rain_in)
JLDP_Oaks_3B$O5_reg<-1.008*(JLDP_Oaks_3B$JLDP_Oaks_5_Rain_in)
JLDP_Oaks_3B$T10_reg<-0.907*(JLDP_Oaks_3B$JLDP_Tinta_10_Rain_in)

#Oaks 3 Impute Statements
JLDP_Oaks_3B$dat_miss <-!is.na(JLDP_Oaks_3B$JLDP_Oaks_3B_Rain_in) &
  JLDP_Oaks_3B$JLDP_Oaks_3B_Rain_in == 0 &
  (replace_na(JLDP_Oaks_3B$JLDP_Escondido_5_new_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Oaks_3B$JLDP_Oaks_5_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Oaks_3B$JLDP_Tinta_10_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Oaks_3B$dat_NA<-is.na(JLDP_Oaks_3B$JLDP_Oaks_3B_Rain_in) # if NA or missing data
JLDP_Oaks_3B$Imputed<-JLDP_Oaks_3B$dat_NA==TRUE|JLDP_Oaks_3B$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Oaks_3B$rain_clean <-ifelse(JLDP_Oaks_3B$Imputed == FALSE,
            JLDP_Oaks_3B$JLDP_Oaks_3B_Rain_in,
            coalesce(JLDP_Oaks_3B$E5_reg,JLDP_Oaks_3B$O5_reg,JLDP_Oaks_3B$T10_reg))
#Oaks 5
JLDP_Oaks_5 <- JLDP_Oaks_5 %>% 
  left_join(JLDP_Oaks_3B %>% select(Date,JLDP_Oaks_3B_Rain_in),by="Date")
JLDP_Oaks_5 <- JLDP_Oaks_5 %>% 
  left_join(JLDP_Escondido_3 %>% select(Date,JLDP_Escondido_3_Rain_in),by="Date")
JLDP_Oaks_5 <- JLDP_Oaks_5 %>% 
  left_join(JLDP_Escondido_2 %>% select(Date, JLDP_Escondido_2_Rain_in), by = "Date")

JLDP_Oaks_5$E2_reg<-0.993*(JLDP_Oaks_5$JLDP_Escondido_2_Rain_in)
JLDP_Oaks_5$E3_reg<-0.898*(JLDP_Oaks_5$JLDP_Escondido_3_Rain_in)
JLDP_Oaks_5$O3B_reg<-0.957*(JLDP_Oaks_5$JLDP_Oaks_3B_Rain_in)

#Oaks 5 Impute Statements
JLDP_Oaks_5$dat_miss <-!is.na(JLDP_Oaks_5$JLDP_Oaks_5_Rain_in) &
  JLDP_Oaks_5$JLDP_Oaks_5_Rain_in == 0 &
  (replace_na(JLDP_Oaks_5$JLDP_Escondido_2_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Oaks_5$JLDP_Escondido_3_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Oaks_5$JLDP_Oaks_3B_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Oaks_5$dat_NA<-is.na(JLDP_Oaks_5$JLDP_Oaks_5_Rain_in) # if NA or missing data
JLDP_Oaks_5$Imputed<-JLDP_Oaks_5$dat_NA==TRUE|JLDP_Oaks_5$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Oaks_5$rain_clean <-ifelse(JLDP_Oaks_5$Imputed == FALSE,
            JLDP_Oaks_5$JLDP_Oaks_5_Rain_in,
            coalesce(JLDP_Oaks_5$E2_reg,JLDP_Oaks_5$E3_reg,JLDP_Oaks_5$O3B_reg))

#Quail
JLDP_Quail_Canyon <- JLDP_Quail_Canyon %>% 
  left_join(JLDP_Tinta_10 %>% select(Date,JLDP_Tinta_10_Rain_in),by="Date")
JLDP_Quail_Canyon <- JLDP_Quail_Canyon %>% 
  left_join(JLDP_Oaks_1 %>% select(Date,JLDP_Oaks_1_Rain_in),by="Date")
JLDP_Quail_Canyon <- JLDP_Quail_Canyon%>% 
  left_join(JLDP_Escondido_2 %>% select(Date, JLDP_Escondido_2_Rain_in), by = "Date")

JLDP_Quail_Canyon$E2_reg<-1.44*(JLDP_Quail_Canyon$JLDP_Escondido_2_Rain_in)
JLDP_Quail_Canyon$O1_reg<-1.166*(JLDP_Quail_Canyon$JLDP_Oaks_1_Rain_in)
JLDP_Quail_Canyon$T10_reg<-1.173*(JLDP_Quail_Canyon$JLDP_Tinta_10_Rain_in)

#Quail Canyon Impute Statements
JLDP_Quail_Canyon$dat_miss <-!is.na(JLDP_Quail_Canyon$JLDP_Quail_Canyon_Rain_in) &
  JLDP_Quail_Canyon$JLDP_Quail_Canyon_Rain_in == 0 &
  (replace_na(JLDP_Quail_Canyon$JLDP_Escondido_2_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Quail_Canyon$JLDP_Oaks_1_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Quail_Canyon$JLDP_Tinta_10_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Quail_Canyon$dat_NA<-is.na(JLDP_Quail_Canyon$JLDP_Quail_Canyon_Rain_in) # if NA or missing data
JLDP_Quail_Canyon$Imputed<-JLDP_Quail_Canyon$dat_NA==TRUE|JLDP_Quail_Canyon$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Quail_Canyon$rain_clean <-ifelse(JLDP_Quail_Canyon$Imputed == FALSE,
        JLDP_Quail_Canyon$JLDP_Quail_Canyon_Rain_in,
        coalesce(JLDP_Quail_Canyon$E2_reg,JLDP_Quail_Canyon$O1_reg,JLDP_Quail_Canyon$T10_reg))
#Quarry
JLDP_Quarry_1 <- JLDP_Quarry_1 %>% 
  left_join(JLDP_Tinta_4 %>% select(Date,JLDP_Tinta_4_Rain_in),by="Date")
JLDP_Quarry_1 <- JLDP_Quarry_1 %>% 
  left_join(JLDP_Tinta_5 %>% select(Date,JLDP_Tinta_5_Rain_in),by="Date")
JLDP_Quarry_1 <- JLDP_Quarry_1 %>% 
  left_join(JLDP_Tinta_6 %>% select(Date, JLDP_Tinta_6_Rain_in), by = "Date")

JLDP_Quarry_1$T4_reg<-1.181*(JLDP_Quarry_1$JLDP_Tinta_4_Rain_in)
JLDP_Quarry_1$T5_reg<-1.157*(JLDP_Quarry_1$JLDP_Tinta_5_Rain_in)
JLDP_Quarry_1$T6_reg<-1.119*(JLDP_Quarry_1$JLDP_Tinta_6_Rain_in)

#Quarry 1 Impute Statements
JLDP_Quarry_1$dat_miss <-!is.na(JLDP_Quarry_1$JLDP_Quarry_1_Rain_in) &
  JLDP_Quarry_1$JLDP_Quarry_1_Rain_in == 0 &
  (replace_na(JLDP_Quarry_1$JLDP_Tinta_4_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Quarry_1$JLDP_Tinta_5_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Quarry_1$JLDP_Tinta_6_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Quarry_1$dat_NA<-is.na(JLDP_Quarry_1$JLDP_Quarry_1_Rain_in) # if NA or missing data
JLDP_Quarry_1$Imputed<-JLDP_Quarry_1$dat_NA==TRUE|JLDP_Quarry_1$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Quarry_1$rain_clean <-ifelse(JLDP_Quarry_1$Imputed == FALSE,
           JLDP_Quarry_1$JLDP_Quarry_1_Rain_in,
           coalesce(JLDP_Quarry_1$T4_reg,JLDP_Quarry_1$T5_reg,JLDP_Quarry_1$T6_reg))
#Tinta 10
JLDP_Tinta_10 <- JLDP_Tinta_10 %>% 
  left_join(JLDP_Tinta_4 %>% select(Date,JLDP_Tinta_4_Rain_in),by="Date")
JLDP_Tinta_10 <- JLDP_Tinta_10 %>% 
  left_join(JLDP_Escondido_5_new %>% select(Date,JLDP_Escondido_5_new_Rain_in),by="Date")
JLDP_Tinta_10 <- JLDP_Tinta_10 %>% 
  left_join(JLDP_Jalama_Vaqueros %>% select(Date, JLDP_Jalama_Vaqueros_Rain_in), by = "Date")

JLDP_Tinta_10$E5_reg<-1.223*(JLDP_Tinta_10$JLDP_Escondido_5_new_Rain_in)
JLDP_Tinta_10$JV_reg<-1.082*(JLDP_Tinta_10$JLDP_Jalama_Vaqueros_Rain_in)
JLDP_Tinta_10$T4_reg<-1.29*(JLDP_Tinta_10$JLDP_Tinta_4_Rain_in)

# Tinta 10 Impute Statements
JLDP_Tinta_10$dat_miss <-!is.na(JLDP_Tinta_10$JLDP_Tinta_10_Rain_in) &
  JLDP_Tinta_10$JLDP_Tinta_10_Rain_in == 0 &
  (replace_na(JLDP_Tinta_10$JLDP_Tinta_4_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_10$JLDP_Escondido_5_new_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_10$JLDP_Jalama_Vaqueros_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Tinta_10$dat_NA<-is.na(JLDP_Tinta_10$JLDP_Tinta_10_Rain_in) # if NA or missing data
JLDP_Tinta_10$Imputed<-JLDP_Tinta_10$dat_NA==TRUE|JLDP_Tinta_10$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Tinta_10$rain_clean <-ifelse(JLDP_Tinta_10$Imputed == FALSE,
             JLDP_Tinta_10$JLDP_Tinta_10_Rain_in,
             coalesce(JLDP_Tinta_10$T4_reg,JLDP_Tinta_10$E5_reg,JLDP_Tinta_10$JV_reg))

#Tinta 3
JLDP_Tinta_3 <- JLDP_Tinta_3 %>% 
  left_join(JLDP_Escondido_2 %>% select(Date,JLDP_Escondido_2_Rain_in),by="Date")
JLDP_Tinta_3 <- JLDP_Tinta_3 %>% 
  left_join(JLDP_Escondido_3 %>% select(Date,JLDP_Escondido_3_Rain_in),by="Date")
JLDP_Tinta_3<- JLDP_Tinta_3 %>% 
  left_join(JLDP_Tinta_10 %>% select(Date, JLDP_Tinta_10_Rain_in), by = "Date")

JLDP_Tinta_3$E2_reg<-0.804*(JLDP_Tinta_3$JLDP_Escondido_2_Rain_in)
JLDP_Tinta_3$E3_reg<-0.784*(JLDP_Tinta_3$JLDP_Escondido_3_Rain_in)
JLDP_Tinta_3$T10_reg<-0.615*(JLDP_Tinta_3$JLDP_Tinta_10_Rain_in)

#Tinta 3 Impute Statements
JLDP_Tinta_3$dat_miss <-!is.na(JLDP_Tinta_3$JLDP_Tinta_3_Rain_in) &
  JLDP_Tinta_3$JLDP_Tinta_3_Rain_in == 0 &
  (replace_na(JLDP_Tinta_3$JLDP_Escondido_2_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_3$JLDP_Escondido_3_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_3$JLDP_Tinta_10_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Tinta_3$dat_NA<-is.na(JLDP_Tinta_3$JLDP_Tinta_3_Rain_in) # if NA or missing data
JLDP_Tinta_3$Imputed<-JLDP_Tinta_3$dat_NA==TRUE|JLDP_Tinta_3$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Tinta_3$rain_clean <-ifelse(JLDP_Tinta_3$Imputed == FALSE,
            JLDP_Tinta_3$JLDP_Tinta_3_Rain_in,
            coalesce(JLDP_Tinta_3$E2_reg,JLDP_Tinta_3$E3_reg,JLDP_Tinta_3$T10_reg))

#Tinta 4
JLDP_Tinta_4 <- JLDP_Tinta_4 %>% 
  left_join(JLDP_Escondido_5_new %>% select(Date,JLDP_Escondido_5_new_Rain_in),by="Date")
JLDP_Tinta_4 <- JLDP_Tinta_4 %>% 
  left_join(JLDP_Escondido_3 %>% select(Date,JLDP_Escondido_3_Rain_in),by="Date")
JLDP_Tinta_4<- JLDP_Tinta_4 %>% 
  left_join(JLDP_Tinta_5 %>% select(Date, JLDP_Tinta_5_Rain_in), by = "Date")

JLDP_Tinta_4$E3_reg<-0.925*(JLDP_Tinta_4$JLDP_Escondido_3_Rain_in)
JLDP_Tinta_4$E5_reg<-0.955*(JLDP_Tinta_4$JLDP_Escondido_5_new_Rain_in)
JLDP_Tinta_4$T5_reg<-0.976*(JLDP_Tinta_4$JLDP_Tinta_5_Rain_in)

#If Tinta 4 Impute Statements
JLDP_Tinta_4$dat_miss <-!is.na(JLDP_Tinta_4$JLDP_Tinta_4_Rain_in) &
  JLDP_Tinta_4$JLDP_Tinta_4_Rain_in == 0 &
  (replace_na(JLDP_Tinta_4$JLDP_Escondido_3_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_4$JLDP_Escondido_5_new_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_4$JLDP_Tinta_5_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Tinta_4$dat_NA<-is.na(JLDP_Tinta_4$JLDP_Tinta_4_Rain_in) # if NA or missing data
JLDP_Tinta_4$Imputed<-JLDP_Tinta_4$dat_NA==TRUE|JLDP_Tinta_4$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Tinta_4$rain_clean <-ifelse(JLDP_Tinta_4$Imputed == FALSE,
         JLDP_Tinta_4$JLDP_Tinta_4_Rain_in,
         coalesce(JLDP_Tinta_4$E3_reg,JLDP_Tinta_4$E5_reg,JLDP_Tinta_4$T5_reg))

#Tinta 5
JLDP_Tinta_5 <- JLDP_Tinta_5 %>% 
  left_join(JLDP_Quarry_1 %>% select(Date,JLDP_Quarry_1_Rain_in),by="Date")
JLDP_Tinta_5 <- JLDP_Tinta_5 %>% 
  left_join(JLDP_Oaks_3B %>% select(Date,JLDP_Oaks_3B_Rain_in),by="Date")
JLDP_Tinta_5<- JLDP_Tinta_5 %>% 
  left_join(JLDP_Tinta_4 %>% select(Date, JLDP_Tinta_4_Rain_in), by = "Date")

JLDP_Tinta_5$O3B_reg<-0.922*(JLDP_Tinta_5$JLDP_Oaks_3B_Rain_in)
JLDP_Tinta_5$Q1_reg<-0.835*(JLDP_Tinta_5$JLDP_Quarry_1_Rain_in)
JLDP_Tinta_5$T4_reg<-1.019*(JLDP_Tinta_5$JLDP_Tinta_4_Rain_in)

#Tinta 5 Impute Statements
JLDP_Tinta_5$dat_miss <-!is.na(JLDP_Tinta_5$JLDP_Tinta_5_Rain_in) &
  JLDP_Tinta_5$JLDP_Tinta_5_Rain_in == 0 &
  (replace_na(JLDP_Tinta_5$JLDP_Oaks_3B_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_5$JLDP_Quarry_1_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_5$JLDP_Tinta_4_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Tinta_5$dat_NA<-is.na(JLDP_Tinta_5$JLDP_Tinta_5_Rain_in) # if NA or missing data
JLDP_Tinta_5$Imputed<-JLDP_Tinta_5$dat_NA==TRUE|JLDP_Tinta_5$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Tinta_5$rain_clean <-ifelse(JLDP_Tinta_5$Imputed == FALSE,
      JLDP_Tinta_5$JLDP_Tinta_5_Rain_in,
      coalesce(JLDP_Tinta_5$O3B_reg,JLDP_Tinta_5$Q1_reg,JLDP_Tinta_5$T4_reg))

#Tinta 6
JLDP_Tinta_6 <- JLDP_Tinta_6 %>% 
  left_join(JLDP_Quarry_1 %>% select(Date,JLDP_Quarry_1_Rain_in),by="Date")
JLDP_Tinta_6 <- JLDP_Tinta_6 %>% 
  left_join(JLDP_Oaks_3B%>% select(Date,JLDP_Oaks_3B_Rain_in),by="Date")
JLDP_Tinta_6<- JLDP_Tinta_6 %>% 
  left_join(JLDP_Tinta_4 %>% select(Date, JLDP_Tinta_4_Rain_in), by = "Date")

JLDP_Tinta_6$O3B_reg<-0.942*(JLDP_Tinta_6$JLDP_Oaks_3B_Rain_in)
JLDP_Tinta_6$Q1_reg<-0.877*(JLDP_Tinta_6$JLDP_Quarry_1_Rain_in)
JLDP_Tinta_6$T4_reg<-1.053*(JLDP_Tinta_6$JLDP_Tinta_4_Rain_in)

#If Tinta 6 Impute Statements
JLDP_Tinta_6$dat_miss <-!is.na(JLDP_Tinta_6$JLDP_Tinta_6_Rain_in) &
  JLDP_Tinta_6$JLDP_Tinta_6_Rain_in == 0 &
  (replace_na(JLDP_Tinta_6$JLDP_Oaks_3B_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_6$JLDP_Quarry_1_Rain_in > 0.25, FALSE) |
     replace_na(JLDP_Tinta_6$JLDP_Tinta_4_Rain_in > 0.25, FALSE)) #If 0, but other sites have rain
JLDP_Tinta_6$dat_NA<-is.na(JLDP_Tinta_6$JLDP_Tinta_6_Rain_in) # if NA or missing data
JLDP_Tinta_6$Imputed<-JLDP_Tinta_6$dat_NA==TRUE|JLDP_Tinta_6$dat_miss==TRUE #If either are true

#run regression equation for each
JLDP_Tinta_6$rain_clean <-ifelse(JLDP_Tinta_6$Imputed == FALSE,
       JLDP_Tinta_6$JLDP_Tinta_6_Rain_in,
       coalesce(JLDP_Tinta_6$O3B_reg,JLDP_Tinta_6$Q1_reg,JLDP_Tinta_6$T4_reg))

#Wood Canyon
JLDP_Wood_Canyon <- JLDP_Wood_Canyon %>% 
  left_join(JLDP_Escondido_2 %>% select(Date,JLDP_Escondido_2_Rain_in),by="Date")
JLDP_Wood_Canyon <- JLDP_Wood_Canyon %>% 
  left_join(JLDP_Oaks_1 %>% select(Date,JLDP_Oaks_1_Rain_in),by="Date")
JLDP_Wood_Canyon<- JLDP_Wood_Canyon %>% 
  left_join(JLDP_Tinta_10 %>% select(Date, JLDP_Tinta_10_Rain_in), by = "Date")

JLDP_Wood_Canyon$E2_reg<-0.723*(JLDP_Wood_Canyon$JLDP_Escondido_2_Rain_in)
JLDP_Wood_Canyon$O1_reg<-0.898*(JLDP_Wood_Canyon$JLDP_Oaks_1_Rain_in)
JLDP_Wood_Canyon$T10_reg<-0.578*(JLDP_Wood_Canyon$JLDP_Tinta_10_Rain_in)

#If wood canyon is 0 and E2,O1 or T10 are>0.25 then true else false
JLDP_Wood_Canyon$dat_miss <-
  !is.na(JLDP_Wood_Canyon$JLDP_Wood_Canyon_Rain_in) &
  JLDP_Wood_Canyon$JLDP_Wood_Canyon_Rain_in == 0 &
  (replace_na(JLDP_Wood_Canyon$JLDP_Escondido_2_Rain_in > 0.25, FALSE) |
      replace_na(JLDP_Wood_Canyon$JLDP_Oaks_1_Rain_in > 0.25, FALSE) |
      replace_na(JLDP_Wood_Canyon$JLDP_Tinta_10_Rain_in > 0.25, FALSE))

#If Wood Canyon is NA or blank then True else false
JLDP_Wood_Canyon$dat_NA<-is.na(JLDP_Wood_Canyon$JLDP_Wood_Canyon_Rain_in)
#If either are TRUE impute from E2_reg, unless NA then impute from O1_reg,unless NA then impute from T10_reg
JLDP_Wood_Canyon$Imputed<-JLDP_Wood_Canyon$dat_NA==TRUE|JLDP_Wood_Canyon$dat_miss==TRUE

#run regression equation for each
JLDP_Wood_Canyon$rain_clean <-ifelse(JLDP_Wood_Canyon$Imputed == FALSE,
    JLDP_Wood_Canyon$JLDP_Wood_Canyon_Rain_in,
    coalesce(JLDP_Wood_Canyon$E2_reg,
      JLDP_Wood_Canyon$O1_reg,
      JLDP_Wood_Canyon$T10_reg))


# Export data
my_list <- list("E2" = JLDP_Escondido_2, "E3" = JLDP_Escondido_3,
                "E5" = JLDP_Escondido_5_new, "G1" = JLDP_Gaspar_1,
                "JV" = JLDP_Jalama_Vaqueros, "O1" = JLDP_Oaks_1,
                "O3B" = JLDP_Oaks_3B, "O5" = JLDP_Oaks_5,
                "QC" = JLDP_Quail_Canyon, "Q1" = JLDP_Quarry_1,
                "T10" = JLDP_Tinta_10, "T3" = JLDP_Tinta_3,
                "T4" = JLDP_Tinta_4, "T5" = JLDP_Tinta_5,
                "T6" = JLDP_Tinta_6, "WC" = JLDP_Wood_Canyon)
write_xlsx(my_list, path = "JLDP_Rain_imputing_Clean_R_export.xlsx")
