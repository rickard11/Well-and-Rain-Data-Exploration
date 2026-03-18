library(tidyverse)
library(dplyr)
library(sf)
library(tidyr)
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
#download well coord and merge with rain data
loc<-read.csv("Data/JLDP_well_location.csv")
rain_site_data<-left_join(rain_data,loc,by="Name")

JLDP_rain_site_data <- rain_site_data[grepl("JLDP", rain_site_data$Name), ]




#Transform csv to a location compatable datatype and compute the nearest neighbors
rain_sf <- JLDP_rain_site_data %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(3310)   # California Albers (meters)

stations <- rain_sf %>%
  group_by(Name) %>%
  slice(1) %>%
  ungroup()

# distance matrix (in meters)
dist_mat <- st_distance(stations)

# convert to numeric matrix
dist_mat <- as.matrix(dist_mat)

# set diagonal to NA so a station isn't its own neighbor
diag(dist_mat) <- NA

# get k nearest neighbors for each station (currently set to 3, can change in future)
neighbors <- lapply(1:nrow(dist_mat), function(i) {
  order(dist_mat[i, ])[1:3]})

#Convert from numbered sites to named site (list of neighbors is still numeric)
names(neighbors) <- stations$Name

rain_flagged <- JLDP_rain_site_data %>%
  group_by(Date) %>%
  mutate(flag_zero_suspicious = sapply(1:n(), function(i) {
      # define the current station
      this_site <- Name[i]
      # skip if rain is not zero
      if (`Rain (in)`[i] != 0) return(FALSE)
      # get neighbor indices
      neighbor_indices <- neighbors[[this_site]]
      neighbor_names <- stations$Name[neighbor_indices]
      # check if any neighbor has rain > 0.25
      any(`Rain (in)`[Name %in% neighbor_names] > 0.25, na.rm = TRUE) }) ) %>%
  ungroup()


#Fill in dates for missing days of data (ex when the battery dies)
JLDP_rain_site_data <- JLDP_rain_site_data %>%
  group_by(Name) %>%
  complete(Date = seq(min(Date), as.Date("2026-02-10"), by = "day") ) %>%
  ungroup()

rain_flagged <- rain_flagged %>%
  mutate( flag_zero_suspicious = ifelse(is.na(flag_zero_suspicious),
    TRUE,flag_zero_suspicious))

rain_wide <- JLDP_rain_site_data %>%
  select(Date, Name, `Rain (in)`) %>%
  pivot_wider(names_from = Name, values_from = `Rain (in)`)

cor_mat <- rain_wide %>%
  select(-Date) %>%
  cor(use = "pairwise.complete.obs")
#write.csv(cor_mat,"Data/processed/Rainfall_correlatioon_matrix.csv")

get_best_neighbor <- function(station, neighbors, cor_mat) {
  neighs <- neighbors[[station]]
  
  # convert indices → names if needed
  if (is.numeric(neighs)) {
    neighs <- colnames(cor_mat)[neighs]
  }
  
  # get correlations
  cors <- cor_mat[station, neighs]
  
  # remove NA
  cors <- cors[!is.na(cors)]
  if (length(cors) == 0) return(NA)
  # return highest correlation neighbor
  names(which.max(cors))
}

stations <- colnames(cor_mat)

donor_table <- tibble(
  station = stations,
  best_neighbor = map_chr(stations, ~ get_best_neighbor(.x, neighbors, cor_mat)))


fill_with_regression <- function(df, donor_table) {
  df_filled <- df
  for (i in seq_len(nrow(donor_table))) {
    s <- donor_table$station[i]
    n <- donor_table$best_neighbor[i]
    if (is.na(n)) next
    
    x <- df[[n]]
    y <- df[[s]]
    
    ok <- !is.na(x) & !is.na(y)
    
    if (sum(ok) < 5) next  # not enough data
    
    fit <- lm(y ~ x1 + x2)
    
    missing_idx <- is.na(y) & !is.na(x)
    
    df_filled[[s]][missing_idx] <- predict(
      fit,
      newdata = data.frame(x = x[missing_idx])
    )
  }
  
  df_filled
}


rain_filled <- fill_with_regression(rain_wide, donor_table)
































#Not sure this is neccessary- it will loose the significance of the flags.


##Now transform to wide
rain_wide <- rain_flagged %>%
  select(Date, Name, `Rain (in)`) %>%
  pivot_wider(names_from = Name, values_from = `Rain (in)`)
rain_wide<- rain_wide %>%
  filter( if_any(-Date, ~ . > 0)) #Remove 0 days because they through off regression
colnames(rain_wide) <- make.names(colnames(rain_wide)) #removing spaces from column names

#Correlation matrix to decide 2 best sites to impute data from for each site
cor_matrix <- cor(rain_wide[,-1], use = "pairwise.complete.obs")

top_donors <- lapply(colnames(cor_matrix), function(site) {
  cor_vals <- cor_matrix[site, ]
  cor_vals <- cor_vals[names(cor_vals) != site]   # remove self
  ranked <- sort(cor_vals, decreasing = TRUE)
  names(ranked)[1:3] })
names(top_donors) <- colnames(cor_matrix)

top_donors[["JLDP.Wood.Canyon"]] #prove it works

#Getting the regression equation for all the donor sites
models <- list()

for (target in names(top_donors)) {
  donors <- top_donors[[target]]
  models[[target]] <- list()
  for (donor in donors) {
    formula <- as.formula(paste(target, "~", donor))
    models[[target]][[donor]] <- lm(
      formula,data = rain_wide)
  }
}

#Now actually imputing the rainfall data for 0 and NA values
rain_imputed <- rain_flagged %>%
  mutate(
    rain_imputed = `Rain (in)`,
    imputed_flag = FALSE,
    donor_used = NA_character_)

rain_imputed$Name <- make.names(rain_imputed$Name)

for (i in seq_len(nrow(rain_imputed))) {
  site  <- rain_imputed$Name[i]
  value <- rain_imputed$`Rain (in)`[i]
  flag  <- rain_imputed$flag_zero_suspicious[i]
  # Only impute if NA or flagged
  if (is.na(value) || flag) {
    donors <- top_donors[[site]]
    for (donor in donors) {
      row_index <- match(rain_imputed$Date[i], rain_wide$Date)
      
      if (!is.na(row_index)) {
        donor_value <- rain_wide[row_index, donor]
      } else {
        donor_value <- NA
      }
      if (!is.na(donor_value)) {
        
        # CASE 1: donor reports 0 rain → keep 0
        if (donor_value == 0) {
          
          rain_imputed$rain_imputed[i] <- 0
          rain_imputed$imputed_flag[i] <- TRUE
          rain_imputed$donor_used[i]   <- paste0(donor, "_zero")
          
          break
        }
        
        # CASE 2: donor has rain → use regression
        if (donor_value > 0 && !is.null(models[[site]][[donor]])) {
          
          model <- models[[site]][[donor]]
          
          newdata <- setNames(
            data.frame(donor_value),
            donor
          )
          
          predicted <- predict(model, newdata = newdata)
          
          # Prevent negative predictions (important!)
          predicted <- max(predicted, 0)
          
          rain_imputed$rain_imputed[i] <- predicted
          rain_imputed$imputed_flag[i] <- TRUE
          rain_imputed$donor_used[i]   <- donor
          
          break
        }}
  }}}


rain_imputed$rain_imputed[rain_imputed$rain_imputed < 0] <- 0 # removing any negative values

############################################################
#Adding manual corrections for days that report NA values
#See JLDP_rain_imputed_2_25_2026_manual in data folder for reference
#1/4/2026 was only day with significant rain of the NA day up until 2/10/2026


manual_condition <- rain_imputed$Date == as.Date("2026-01-04") &
  is.na(rain_imputed$rain_imputed)

rain_imputed$rain_imputed[manual_condition] <- 0.3
rain_imputed$donor_used[manual_condition]   <- "Manual-impute"
rain_imputed$imputed_flag[manual_condition] <- TRUE

rain_imputed %>%
  filter(Date == as.Date("2026-01-04"))

#Need to go back and make sure there are NA values for dates when rain samplers went out.
#write.csv(rain_imputed,"Data/processed/JLDP_rain_imputed_2_25_2026.csv")












