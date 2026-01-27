#librarys and functions
library(GGally)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
##Function for Plots of Ranchbot regression
panel_lm_stats <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  fit <- lm(y ~ x)
  r  <- cor(x, y, use = "complete.obs")
  r2 <- summary(fit)$r.squared
  p  <- summary(fit)$coefficients[2, 4]
  sd_x <- sd(x, na.rm = TRUE)
  sd_y <- sd(y, na.rm = TRUE)
  ggplot(data = data, mapping = mapping) +geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    annotate("text",x = min(x, na.rm = TRUE),y = max(y, na.rm = TRUE),
    hjust = 0,vjust = 1,size = 3,label = paste0(
    "r = ", round(r, 2),"\nR² = ", round(r2, 2),
    "\np = ", signif(p, 3), "\nSD(x) = ", round(sd_x, 2),
    "\nSD(y) = ", round(sd_y, 2))) +theme_minimal()}
#Function for flagging data an sd away
flag_all_station_pairs <- function(df, stats_table, n_sd = 2) {
  flag_vec <- rep(NA_character_, nrow(df))
  for (i in seq_len(nrow(stats_table))) {
    sx <- stats_table$station_x[i]
    sy <- stats_table$station_y[i]
    sd_ref <- stats_table$sd_reg[i]
    if (!(sx %in% names(df) && sy %in% names(df))) next
    x <- df[[sx]]
    y <- df[[sy]]
    ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 3) next
    fit <- lm(y ~ x, subset = ok)
    yhat <- predict(fit, newdata = data.frame(x = x))
    resid <- y - yhat
    hit <- !is.na(resid) & abs(resid) > n_sd * sd_ref
    code <- paste0(sx, "sd", sy)
    flag_vec[hit] <- ifelse(
      is.na(flag_vec[hit]),
      code,
      paste(flag_vec[hit], code, sep = ";")) }  
  flag_vec}

#function for plotting flagged data
plot_flagged_regression <- function(df, s1, s2, lims) {
  flag_code <- paste0(s1, "sd", s2)
  df$flag_pair <- grepl(flag_code, df$Flag)
  ggplot(df, aes(x = .data[[s1]], y = .data[[s2]])) +
    geom_point(aes(color = flag_pair), alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_color_manual(
      values = c("FALSE" = "grey60", "TRUE" = "red"),
      labels = c("FALSE" = "OK", "TRUE" = flag_code)
    ) +
    coord_equal(xlim = lims, ylim = lims) +
    labs(x = s1, y = s2, color = "QC Flag") +
    theme_minimal()
}
#Downloading Ranchbot data
E5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/Escondido_5_Daily_Corrected_Jan_2026.csv")
E5<-E5[,c(1,6)]
E3<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Escondido 3 Rain Gauge Rain Gauge 20231102-20251201.csv")
E2<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Escondido 2 Rain Gauge Rain Gauge 20231102-20251201.csv")
G1<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Gaspar 1 Rain Gauge Rain Gauge 20231102-20251201.csv")
JV<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Jalama Vaqueros Rain Gauge Rain Gauge 20231102-20251201.csv")
O1<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Oaks 1 Rain Gauge Rain Gauge 20231102-20251201.csv")
O3<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Oaks 3B Rain Gauge Rain Gauge 20231102-20251201.csv")
O4<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Oaks 4 Rain Gauge Rain Gauge 20231102-20251201.csv")
O5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Oaks 5 Rain Gauge Rain Gauge 20231102-20251201.csv")
quail<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Quail Canyon Rain Gauge Rain Gauge 20231102-20251201.csv")
quarry<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Quarry 1 Rain Gauge Rain Gauge 20231102-20251201.csv")
T1<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Tinta 1 Rain Gauge Rain Gauge 20231102-20251201.csv")
T3<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Tinta 3  Rain Gauge Rain Gauge 20231102-20251201.csv")
T4<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Tinta 4 Rain Gauge Rain Gauge 20231102-20251201.csv")
T5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Tinta 5 Rain Gauge Rain Gauge 20231102-20251201.csv")
T6<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Tinta 6 Rain Gauge Rain Gauge 20231102-20251201.csv")
wood<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/JLDP Wood Canyon Rain Gauge Rain Gauge 20231102-20251201.csv")

#Format and Merge Ranchbot files before full merge
colnames(E5)[2]<-"E5"
colnames(E3)[2]<-"E3"
colnames(E2)[2]<-"E2"
colnames(G1)[2]<-"G1"
colnames(JV)[2]<-"JV"
colnames(O1)[2]<-"O1"
colnames(O3)[2]<-"O3"
colnames(O4)[2]<-"O4"
colnames(O5)[2]<-"O5"
colnames(quail)[2]<-"quail"
colnames(quarry)[2]<-"quarry"
colnames(T1)[2]<-"T1"
colnames(T3)[2]<-"T3"
colnames(T4)[2]<-"T4"
colnames(T5)[2]<-"T5"
colnames(T6)[2]<-"T6"
colnames(wood)[2]<-"wood"

#Change escondido to dates because E5 is different
E5$Date<-as.Date(E5$Date,format="%m/%d/%Y")
E2$Date<-as.Date(E2$Date,format="%Y-%m-%d (%a)")
E3$Date<-as.Date(E3$Date,format="%Y-%m-%d (%a)")

#################################################################################################################
##########################################################################################################
#####Data Analysis
##Escondido

#Break into sections to efficiently plot regression
df_list <- list(E2,E3,E5) 
Escondido <- df_list %>% reduce(full_join, by = "Date")
Estats_all_clean <- Escondido[Escondido$E2>0|Escondido$E3>0|Escondido$E5>0,]
Estats<-Escondido[,2:4] #remove date column
# Escondido Linear regression plots
E<-ggpairs(Estats,upper = "blank",diag  = "blank",
           lower = list(continuous = panel_lm_stats))
#ggsave(plot=E,"figures/escondido_linear.png")
#Escondido Linear regression table
Estats_clean<-Estats[with(Estats,(Estats$E2>0|Estats$E3>0|Estats$E5>0) %in% TRUE),]
pairs <- combn(names(Estats_clean), 2, simplify = FALSE)
Estats_table <- map_dfr(pairs, \(p) {
  x <- Estats_clean[[p[1]]]
  y <- Estats_clean[[p[2]]]
  fit <- lm(y ~ x)
  tibble(
    station_x = p[1],
    station_y = p[2],
    r  = cor(x, y, use = "complete.obs"),
    r2 = summary(fit)$r.squared,
    p_value = summary(fit)$coefficients[2, 4],
    sd_reg = summary(fit)$sigma)})
#Flag
Estats_all$Flag <- flag_all_station_pairs(
  df = Estats_all,
  stats_table = Estats_table,
  n_sd = 5)
Estats_all_clean$Flag <- flag_all_station_pairs(
  df = Estats_all,
  stats_table = Estats_table,
  n_sd = 5)
#Plot with flagged data highlighted
station_cols <- setdiff(names(Estats_all), c("Date", "Flag", "X"))
lims <- range(
  unlist(Estats_all[station_cols]),
  na.rm = TRUE)
stations <- c("E2", "E3", "E5")  # or names(Estats)
pairs <- combn(stations, 2, simplify = FALSE)
plots <- lapply(pairs, \(p) {
  plot_flagged_regression(Estats_all_clean, p[1], p[2], lims)})
wrap_plots(plots, ncol = 2)
#write.csv(Estats_all,"Data/processed/Escondido_rain_auto_flag.csv")

###########################################################################################################
###### Oaks
df_list <- list(O1,O3,O4,O5,wood,quail) 
oaks <- df_list %>% reduce(full_join, by = "Date")
Ostats_all <- oaks
Ostats<-oaks[,2:7] #remove date column

# oaks Linear regression plots

panel_lm_stats <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  fit <- lm(y ~ x)
  r  <- cor(x, y, use = "complete.obs")
  r2 <- summary(fit)$r.squared
  p  <- summary(fit)$coefficients[2, 4]
  sd_x <- sd(x, na.rm = TRUE)
  sd_y <- sd(y, na.rm = TRUE)
  ggplot(data = data, mapping = mapping) +geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +theme_minimal()}
O<-ggpairs( Ostats,upper = "blank",diag  = "blank",
  lower = list(continuous = panel_lm_stats))
#ggsave(plot=O,"figures/Oaks_linear.png")
#Oaks Linear regression table
Ostats_clean<-Ostats[Ostats$O1>0|Ostats$O3>0|Ostats$O4>0|
               Ostats$O5>0|Ostats$wood>0|Ostats$quail>0,]
Ostats_clean <- Ostats[with(Ostats,(O1>0|O3>0|O4>0|O5>0|wood>0|quail>0) %in% TRUE),]
pairs <- combn(names(Ostats_clean), 2, simplify = FALSE)
Ostats_table <- map_dfr(pairs, \(p) {
  x <- Ostats_clean[[p[1]]]
  y <- Ostats_clean[[p[2]]]
  fit <- lm(y ~ x)
  tibble(
    station_x = p[1],
    station_y = p[2],
    r  = cor(x, y, use = "complete.obs"),
    r2 = summary(fit)$r.squared,
    p_value = summary(fit)$coefficients[2, 4],
    sd_reg = summary(fit)$sigma)})

Ostats_all$Flag <- flag_all_station_pairs(
  df =Ostats_all,
  stats_table = Ostats_table,
  n_sd = 5)
#write.csv(Ostats_all,"Data/processed/Oaks_rain_auto_flag.csv")

stations <- c("O1", "O3", "O4","O5","wood","quail")  # or names(Estats)
pairs <- combn(stations, 2, simplify = FALSE)

plots <- lapply(pairs, \(p) {
  plot_flagged_regression(Ostats_all, p[1], p[2],lims)
})
wrap_plots(plots, ncol = 3)

#Tinta
df_list <- list(T3,T5,T6,quarry,JV) 
Tinta <- df_list %>% reduce(full_join, by = "Date")
Tstats_all <- Tinta
Tstats<-Tinta[,2:6] #remove date column

# Tinta Linear regression plots
Ti<-ggpairs(
  Tstats,
  upper = "blank",
  diag  = "blank",
  lower = list(continuous = panel_lm_stats))
#ggsave(plot=Ti,"figures/Tinta_linear_full.png")
pairs <- combn(names(Tstats), 2, simplify = FALSE)
Tstats_table <- map_dfr(pairs, \(p) {
  x <- Tstats[[p[1]]]
  y <- Tstats[[p[2]]]
  fit <- lm(y ~ x)
  tibble(station_x = p[1],station_y = p[2],
         r  = cor(x, y, use = "complete.obs"),
         r2 = summary(fit)$r.squared,
         p_value = summary(fit)$coefficients[2, 4],
         sd_x = sd(x, na.rm = TRUE),
         sd_y = sd(y, na.rm = TRUE)
  )})
Tstats_all$Flag <- flag_all_station_pairs(
  df = Tstats_all,
  stats_table = Tstats_table,
  n_sd = 2)
#write.csv(Tstats_all,"Data/processed/Tinta_rain_auto_flag.csv")

stations <- c("T3", "T5", "T6","quarry")  # or names(Estats)
pairs <- combn(stations, 2, simplify = FALSE)
plots <- lapply(pairs, \(p) {
  plot_flagged_regression(Tstats_all, p[1], p[2])
})
wrap_plots(plots, ncol = 3)



#Merge tables into One 
df_list <- list(Estats,Ostats,Tstats) 
Rain_stats <- rbind(Tstats_table,Ostats_table,Estats_table)
#write.csv(Rain_stats,"Data/processed/Rain_linear_regression_all.csv")

strongest_per_station <- lapply(stations, function(st) {
  rows <- Rain_stats %>%
    filter(station_x == st | station_y == st)
  rows[which.max(abs(rows$r)), ]
}) %>%
  bind_rows()
#write.csv(strongest_per_station,"Data/processed/Rain_top_linear_regression.csv")

Tstats_table
Rain_stats<-as.data.frame(Rain_stats)







###########3try
station_cols <- setdiff(names(Ostats_all), c("Date", "Flag"))

lims <- range(
  unlist(Ostats_all[station_cols]),
  na.rm = TRUE
)

plot_flagged_regression <- function(df, s1, s2, lims) {
  
  flag_code <- paste0(s1, "sd", s2)
  df$flag_pair <- grepl(flag_code, df$Flag)
  
  ggplot(df, aes(x = .data[[s1]], y = .data[[s2]])) +
    geom_point(aes(color = flag_pair), alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_color_manual(
      values = c("FALSE" = "grey60", "TRUE" = "red"),
      labels = c("FALSE" = "OK", "TRUE" = flag_code)
    ) +
    coord_equal(xlim = lims, ylim = lims) +
    labs(x = s1, y = s2, color = "QC Flag") +
    theme_minimal()
}

plots <- lapply(pairs, \(p) {
  plot_flagged_regression(Tstats_all, p[1], p[2], lims)
})

wrap_plots(plots, ncol = 3)+ geom_abline(intercept = 0, slope = 1, linetype = "dashed")


