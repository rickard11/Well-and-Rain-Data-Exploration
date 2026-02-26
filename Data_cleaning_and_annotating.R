#functions
#Remove first entries of ranchbot wells since they are usually still out of water when started
remove_last_n <- function(df, n = 3) { 
  df[seq_len(nrow(df) - n), ]}

#apply corrections
apply_corrections <- function(df, corrections) {
  df <- df %>% mutate(dtw_corrected = ft..below.ground.)
  for (i in seq_len(nrow(corrections))) {
    start <- corrections$start_date[i]
    end   <- corrections$end_date[i]
    delta <- corrections$correction_ft[i]
    df <- df %>%
      mutate(dtw_corrected = if_else(
        Date >= start & (is.na(end) | Date <= end),
        dtw_corrected + delta,dtw_corrected))
  }
  df
}

## Escondido 3 
E3_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Escondido 3 Well - 20240101-20260210.csv")
E3_well <- remove_last_n(E3_well, n=3)
E3_well$Date.and.Time<-as.POSIXct(E3_well$Date.and.Time)
E3_well$Date<-as.Date(E3_well$Date.and.Time)

#Other data flagging will be done with daily data
E3_daily<-aggregate(ft..below.ground.~Date,E3_well,FUN=mean)

#should make this into a function?
df_flagged <- E3_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
    flag_big_jump = abs(dtw_change) > 1)

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-07-25")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(+1.75),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
    aes(y = ft..below.ground.),color = "red",size = 2) +
  labs(y = "Depth to Water (ft)",
    title = "Well DTW with QA Flags and Corrections")

df_corrected
write.csv(df_corrected,"Data/well/Corrected/JLDP Escondido 3_well_daily_corrected_02_10_2026.csv")

################################################################################
## Escondido 2
E2_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Escondido 2 Well - 20240101-20260211.csv")
tail(E2_well)
E2_well <- remove_last_n(E2_well, n=3)
E2_well$Date.and.Time<-as.POSIXct(E2_well$Date.and.Time)
E2_well$Date<-as.Date(E2_well$Date.and.Time)

#Other data flagging will be done with daily data
E2_daily<-aggregate(ft..below.ground.~Date,E2_well,FUN=mean)

#should make this into a function?
df_flagged <- E2_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.33)

ggplot(E2_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-07-23")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(+0.63),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")


write.csv(df_corrected,"Data/well/Corrected/JLDP Escondido 2_well_daily_corrected_02_10_2026.csv")

################################################################################
## Oaks1
O1_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Oaks 1 Well - 20240101-20260211.csv")
tail(O1_well)
O1_well[5700:5737,]
O1_well <- remove_last_n(O1_well, n=7)
O1_well$Date.and.Time<-as.POSIXct(O1_well$Date.and.Time)
O1_well$Date<-as.Date(O1_well$Date.and.Time)

#Other data flagging will be done with daily data
O1_daily<-aggregate(ft..below.ground.~Date,O1_well,FUN=mean)

#removing 2 days with faulty data.
O1_daily <- O1_daily %>%
  filter(!(Date >= as.Date("2025-08-11") & Date <= as.Date("2025-08-12")))

ggplot(O1_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(O1_daily,"Data/well/Corrected/JLDP Oaks 1_well_daily_corrected_02_10_2026.csv")

################################################################################
## Oaks 3
O3_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Oaks 3 B Well - 20240101-20260211.csv")
tail(O3_well)
O3_well$Date.and.Time<-as.POSIXct(O3_well$Date.and.Time)
O3_well$Date<-as.Date(O3_well$Date.and.Time)

#Other data flagging will be done with daily data
O3_daily<-aggregate(ft..below.ground.~Date,O3_well,FUN=mean)

ggplot(O3_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(O3_daily,"Data/well/Corrected/JLDP Oaks 3_well_daily_corrected_02_10_2026.csv")

################################################################################
## Oaks 5
O5_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Oaks 5 Well - 20240101-20260211.csv")
tail(O5_well)
O5_well <- remove_last_n(O5_well, n=5)
O5_well$Date.and.Time<-as.POSIXct(O5_well$Date.and.Time)
O5_well$Date<-as.Date(O5_well$Date.and.Time)

#Other data flagging will be done with daily data
O5_daily<-aggregate(ft..below.ground.~Date,O5_well,FUN=mean)

#should make this into a function?
df_flagged <- O5_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.33)

ggplot(O5_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-02-27")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(-1.01),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/JLDP Oaks 5_well_daily_corrected_02_10_2026.csv")

################################################################################
## Gaspar 1
G1_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Gaspar 1 Well - 20240101-20260211.csv")
tail(G1_well)
G1_well <- remove_last_n(G1_well, n=2)
G1_well$Date.and.Time<-as.POSIXct(G1_well$Date.and.Time)
G1_well$Date<-as.Date(G1_well$Date.and.Time)

#Other data flagging will be done with daily data
G1_daily<-aggregate(ft..below.ground.~Date,G1_well,FUN=mean)

#should make this into a function?
df_flagged <- G1_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.33)

ggplot(G1_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(G1_daily,"Data/well/Corrected/JLDP Gaspar 1_well_daily_corrected_02_10_2026.csv")

################################################################################
## Jalama Vaqueros
JV_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Jalama vaqueros Well - 20230101-20260211.csv")
tail(JV_well)
JV_well[2950:3012,]
JV_well <- remove_last_n(JV_well, n=7)
JV_well$Date.and.Time<-as.POSIXct(JV_well$Date.and.Time)
JV_well$Date<-as.Date(JV_well$Date.and.Time)

#Other data flagging will be done with daily data
JV_daily<-aggregate(ft..below.ground.~Date,JV_well,FUN=mean)
#removing 2 days with faulty data.
JV_daily <- JV_daily %>%
  filter(!(Date >= as.Date("2024-03-16") & Date <= as.Date("2024-03-17")))

#should make this into a function?
df_flagged <- JV_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 1)

ggplot(JV_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-04-16")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(-2.5),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/JLDP Jalama Vaqueros_well_daily_corrected_02_10_2026.csv")

################################################################################
## Quail Canyon
Quail_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Quail Canyon Well - 20240107-20260211.csv")
tail(Quail_well)
Quail_well <- remove_last_n(Quail_well, n=1)
Quail_well$Date.and.Time<-as.POSIXct(Quail_well$Date.and.Time)
Quail_well$Date<-as.Date(Quail_well$Date.and.Time)

#Other data flagging will be done with daily data
Quail_daily<-aggregate(ft..below.ground.~Date,Quail_well,FUN=mean)
Quail_daily <- Quail_daily %>%
  filter(!(Date >= as.Date("2025-09-18") & Date <= as.Date("2025-09-19")))

#should make this into a function?
df_flagged <- Quail_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(Quail_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-09-19")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(+2),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/JLDP Quail Canyon_well_daily_corrected_02_10_2026.csv")

################################################################################
## Quarry 1
Q1_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Quarry 1 Well - 20240107-20260211.csv")
tail(Q1_well)
Q1_well <- remove_last_n(Q1_well, n=1)
Q1_well$Date.and.Time<-as.POSIXct(Q1_well$Date.and.Time)
Q1_well$Date<-as.Date(Q1_well$Date.and.Time)

#Other data flagging will be done with daily data
Q1_daily<-aggregate(ft..below.ground.~Date,Q1_well,FUN=mean)

#should make this into a function?
df_flagged <- Q1_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(Q1_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(Q1_daily,"Data/well/Corrected/JLDP Quarry 1_well_daily_corrected_02_10_2026.csv")

################################################################################
## Tinta 1
T1_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 1 Well - 20240107-20260211.csv")
tail(T1_well)
T1_well$Date.and.Time<-as.POSIXct(T1_well$Date.and.Time)
T1_well$Date<-as.Date(T1_well$Date.and.Time)

#Other data flagging will be done with daily data
T1_daily<-aggregate(ft..below.ground.~Date,T1_well,FUN=mean)
T1_daily <- T1_daily %>%
  filter(!(Date >= as.Date("2025-09-18") & Date <= as.Date("2025-09-19")))

#should make this into a function?
df_flagged <- T1_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(T1_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(T1_daily,"Data/well/Corrected/JLDP Tinta 1_well_daily_corrected_02_10_2026.csv")

################################################################################
## Tinta 10
T10_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 10 Well - 20240101-20260211.csv")
tail(T10_well)
#T10_well <- remove_last_n(T10_well, n=1)
T10_well$Date.and.Time<-as.POSIXct(T10_well$Date.and.Time)
T10_well$Date<-as.Date(T10_well$Date.and.Time)

#Other data flagging will be done with daily data
T10_daily<-aggregate(ft..below.ground.~Date,T10_well,FUN=mean)
T10_daily <- T10_daily %>%
  filter(!(Date >= as.Date("2024-05-05") & Date <= as.Date("2024-09-20")))

#should make this into a function?
df_flagged <- T10_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(T10_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2024-01-01")),
  end_date   = as.Date(c("2024-06-27")),  # NA = apply to end of dataset
  correction_ft = c(-40.2),
  reason = c("Sensor offset can't get good manual measurement"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/JLDP Tinta 10_well_daily_corrected_02_10_2026.csv")

################################################################################
## Tinta 11B
T11_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 11B Well - 20240107-20260211.csv")
tail(T11_well)
#T11_well <- remove_last_n(T11_well, n=1)
T11_well$Date.and.Time<-as.POSIXct(T11_well$Date.and.Time)
T11_well$Date<-as.Date(T11_well$Date.and.Time)

#Other data flagging will be done with daily data
T11_daily<-aggregate(ft..below.ground.~Date,T11_well,FUN=mean)
T11_daily <- T11_daily %>%
  filter(!(Date >= as.Date("2025-09-18") & Date <= as.Date("2025-09-19")))

#should make this into a function?
df_flagged <- T11_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.2)

ggplot(T11_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(T11_daily,"Data/well/Corrected/JLDP Tinta 11B_well_daily_corrected_02_10_2026.csv")

################################################################################
## Titna 3
T3_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 3 Well - 20231101-20260211.csv")
tail(T3_well)
T3_well$Date.and.Time<-as.POSIXct(T3_well$Date.and.Time)
T3_well$Date<-as.Date(T3_well$Date.and.Time)

#Other data flagging will be done with daily data
T3_daily<-aggregate(ft..below.ground.~Date,T3_well,FUN=mean)
T3_daily <- T3_daily %>%
  filter(!(Date >= as.Date("2025-09-18") & Date <= as.Date("2025-09-19")))

#should make this into a function?
df_flagged <- T3_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(T3_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(T3_daily,"Data/well/Corrected/JLDP Tinta 3_well_daily_corrected_02_10_2026.csv")

################################################################################
## Tinta 4
T4_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 4 Well - 20240107-20260211.csv")
tail(T4_well)
T4_well[1800:1867,]
T4_well <- remove_last_n(T4_well, n=9)
T4_well$Date.and.Time<-as.POSIXct(T4_well$Date.and.Time)
T4_well$Date<-as.Date(T4_well$Date.and.Time)

#Other data flagging will be done with daily data
T4_daily<-aggregate(ft..below.ground.~Date,T4_well,FUN=mean)
T4_daily <- T4_daily %>%
  filter(!(Date >= as.Date("2025-09-18") & Date <= as.Date("2025-09-19")))

#should make this into a function?
df_flagged <- T4_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(T4_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(T4_daily,"Data/well/Corrected/JLDP Tinta 4_well_daily_corrected_02_10_2026.csv")

################################################################################
## Tinta 5
T5_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 5 Well - 20240101-20260211.csv")
tail(T5_well)
#T5_well <- remove_last_n(T5_well, n=1)
T5_well$Date.and.Time<-as.POSIXct(T5_well$Date.and.Time)
T5_well$Date<-as.Date(T5_well$Date.and.Time)

#Other data flagging will be done with daily data
T5_daily<-aggregate(ft..below.ground.~Date,T5_well,FUN=mean)
T5_daily <- T5_daily %>%
  filter(!(Date >= as.Date("2024-05-25") & Date <= as.Date("2024-05-26")))

#should make this into a function?
df_flagged <- T5_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.9)

ggplot(T5_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(T5_daily,"Data/well/Corrected/JLDP Tinta 5_well_daily_corrected_02_10_2026.csv")

################################################################################
## Tinta 6
T6_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Tinta 6 Well - 20240101-20260211.csv")
tail(T6_well)
T6_well <- remove_last_n(T6_well, n=1)
T6_well$Date.and.Time<-as.POSIXct(T6_well$Date.and.Time)
T6_well$Date<-as.Date(T6_well$Date.and.Time)

#Other data flagging will be done with daily data
T6_daily<-aggregate(ft..below.ground.~Date,T6_well,FUN=mean)

#should make this into a function?
df_flagged <- T6_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(T6_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")


write.csv(T6_daily,"Data/well/Corrected/JLDP Tinta 6_well_daily_corrected_02_10_2026.csv")

################################################################################
## Wood Canyon
Wood_well<-read.csv("Data/well/2-11-2026/JLDP/JLDP Wood Canyon Well - 20240101-20260211.csv")
tail(Wood_well)
Wood_well$Date.and.Time<-as.POSIXct(Wood_well$Date.and.Time)
Wood_well$Date<-as.Date(Wood_well$Date.and.Time)

#Other data flagging will be done with daily data
Wood_daily<-aggregate(ft..below.ground.~Date,Wood_well,FUN=mean)

#should make this into a function?
df_flagged <- Wood_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.25)

ggplot(Wood_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-09-18")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(+0.7),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/JLDP Wood Canyon_well_daily_corrected_02_10_2026.csv")

################################################################################
## Other Calgro
################################################################################

################################################################################
## Cooper
Coop_well<-read.csv("Data/well/2-11-2026/Other Calgro/Las Piletas Cooper plus - 20240107-20260211.csv")
tail(Coop_well)
Coop_well[6100:6220,]
Coop_well <- remove_last_n(Coop_well, n=7)
Coop_well$Date.and.Time<-as.POSIXct(Coop_well$Date.and.Time)
Coop_well$Date<-as.Date(Coop_well$Date.and.Time)

#Other data flagging will be done with daily data
Coop_daily<-aggregate(ft..below.ground.~Date,Coop_well,FUN=mean)
Coop_daily <- Coop_daily %>%
  filter(!(Date >= as.Date("2025-04-02") & Date <= as.Date("2025-05-30")))

#should make this into a function?
df_flagged <- Coop_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(Coop_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2024-12-12")),
  end_date   = as.Date(c("2025-04-01")),  # NA = apply to end of dataset
  correction_ft = c(+18),
  reason = c("Inital Sensor misprogramming"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/Las Piletas Cooper plus_well_daily_corrected_02_10_2026.csv")

################################################################################
## NE Corner
NE_well<-read.csv("Data/well/2-11-2026/Other Calgro/Las Piletas NE Corner - 20240107-20260211.csv")
tail(NE_well)
NE_well <- remove_last_n(NE_well, n=9)
NE_well$Date.and.Time<-as.POSIXct(NE_well$Date.and.Time)
NE_well$Date<-as.Date(NE_well$Date.and.Time)

#Other data flagging will be done with daily data
NE_daily<-aggregate(ft..below.ground.~Date,NE_well,FUN=mean)

#should make this into a function?
df_flagged <- NE_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(NE_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-09-19")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(+2),
  reason = c("Sensor offset after maintenance"))

write.csv(NE_daily,"Data/well/Corrected/Las Piletas NE Corner_well_daily_corrected_02_10_2026.csv")

################################################################################
## Windmill
Windmill_well<-read.csv("Data/well/2-11-2026/Other Calgro/Las Piletas Windmill - 20240105-20260212.csv")
tail(Windmill_well)
Windmill_well <- remove_last_n(Windmill_well, n=9)
Windmill_well$Date.and.Time<-as.POSIXct(Windmill_well$Date.and.Time)
Windmill_well$Date<-as.Date(Windmill_well$Date.and.Time)

#Other data flagging will be done with daily data
Windmill_daily<-aggregate(ft..below.ground.~Date,Windmill_well,FUN=mean)

#should make this into a function?
df_flagged <- Windmill_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(Windmill_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(Windmill_daily,"Data/well/Corrected/Las Piletas Windmill_well_daily_corrected_02_10_2026.csv")

################################################################################
## Parks Creek
PC_well<-read.csv("Data/well/2-11-2026/Other Calgro/Parks Creek Well - 20240101-20260211.csv")
tail(PC_well)
PC_well <- remove_last_n(PC_well, n=1)
PC_well$Date.and.Time<-as.POSIXct(PC_well$Date.and.Time)
PC_well$Date<-as.Date(PC_well$Date.and.Time)

#Other data flagging will be done with daily data
PC_daily<-aggregate(ft..below.ground.~Date,PC_well,FUN=mean)

#should make this into a function?
df_flagged <- PC_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(PC_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(PC_daily,"Data/well/Corrected/Parks Creek_well_daily_corrected_02_10_2026.csv")

################################################################################
## Pixley
Pixley_well<-read.csv("Data/well/2-11-2026/Other Calgro/Pixley Tulare Capinero Creek Well - 20240107-20260211.csv")
tail(Pixley_well)
Pixley_well <- remove_last_n(Pixley_well, n=10)
Pixley_well$Date.and.Time<-as.POSIXct(Pixley_well$Date.and.Time)
Pixley_well$Date<-as.Date(Pixley_well$Date.and.Time)

#Other data flagging will be done with daily data
Pixley_daily<-aggregate(ft..below.ground.~Date,Pixley_well,FUN=mean)

#should make this into a function?
df_flagged <- Pixley_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(Pixley_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(Pixley_daily,"Data/well/Corrected/Pixley Tulare Capinero Creek_well_daily_corrected_02_10_2026.csv")

################################################################################
## B and C Canyon
BC_well<-read.csv("Data/well/2-11-2026/Other Calgro/Randall B and C Ranch Well - 20240101-20260211.csv")
tail(BC_well)
BC_well$Date.and.Time<-as.POSIXct(BC_well$Date.and.Time)
BC_well$Date<-as.Date(BC_well$Date.and.Time)

#Other data flagging will be done with daily data
BC_daily<-aggregate(ft..below.ground.~Date,BC_well,FUN=mean)

#should make this into a function?
df_flagged <- BC_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(BC_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(BC_daily,"Data/well/Corrected/Randall B and C_well_daily_corrected_02_10_2026.csv")

################################################################################
## Cactus Pasture
Cactus_well<-read.csv("Data/well/2-11-2026/Other Calgro/Randall Cactus Pasture Well - 20240101-20260211.csv")
tail(Cactus_well)
Cactus_well <- remove_last_n(Cactus_well, n=2)
Cactus_well$Date.and.Time<-as.POSIXct(Cactus_well$Date.and.Time)
Cactus_well$Date<-as.Date(Cactus_well$Date.and.Time)

#Other data flagging will be done with daily data
Cactus_daily<-aggregate(ft..below.ground.~Date,Cactus_well,FUN=mean)
Cactus_daily <- Cactus_daily %>%
  filter(!(Date >= as.Date("2026-01-02") & Date <= as.Date(NA)))

#should make this into a function?
df_flagged <- Cactus_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(Cactus_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(Cactus_daily,"Data/well/Corrected/Randall Cactus Pasture_well_daily_corrected_02_10_2026.csv")

################################################################################
## Lower Tweedy
LT_well<-read.csv("Data/well/2-11-2026/Other Calgro/Randall Lower Tweedy well - 20240107-20260211.csv")
tail(LT_well)
LT_well <- remove_last_n(LT_well, n=5)
LT_well$Date.and.Time<-as.POSIXct(LT_well$Date.and.Time)
LT_well$Date<-as.Date(LT_well$Date.and.Time)

#Other data flagging will be done with daily data
LT_daily<-aggregate(ft..below.ground.~Date,LT_well,FUN=mean)
LT_daily <- LT_daily %>%
  filter(!(Date >= as.Date("2025-09-18") & Date <= as.Date("2025-09-19")))

#should make this into a function?
df_flagged <- LT_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 0.5)

ggplot(LT_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(LT_daily,"Data/well/Corrected/Randall Lower Tweedy_well_daily_corrected_02_10_2026.csv")

################################################################################
## Aflalo Canyon
AF_well<-read.csv("Data/well/2-11-2026/Other Calgro/Santa Clara River Aflalo Well - 20240121-20260211.csv")
tail(AF_well)
AF_well <- remove_last_n(AF_well, n=1)
AF_well$Date.and.Time<-as.POSIXct(AF_well$Date.and.Time)
AF_well$Date<-as.Date(AF_well$Date.and.Time)

#Other data flagging will be done with daily data
AF_daily<-aggregate(ft..below.ground.~Date,AF_well,FUN=mean)
AF_daily <- AF_daily %>%
  filter(!(Date >= as.Date("2025-12-04") & Date <= as.Date("2025-12-09")))

#should make this into a function?
df_flagged <- AF_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 2)

ggplot(AF_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

corrections <- tibble::tibble(
  start_date = as.Date(c("2025-12-09")),
  end_date   = as.Date(c(NA)),  # NA = apply to end of dataset
  correction_ft = c(-1.2),
  reason = c("Sensor offset after maintenance"))

df_corrected <- apply_corrections(df_flagged, corrections)

ggplot(df_corrected, aes(Date)) +
  geom_line(aes(y = ft..below.ground.), color = "gray60") +
  geom_line(aes(y = dtw_corrected), color = "blue") +
  geom_point(data = df_corrected %>% filter(flag_big_jump),
             aes(y = ft..below.ground.),color = "red",size = 1) +
  labs(y = "Depth to Water (ft)", title = "Well DTW with QA Flags and Corrections")

write.csv(df_corrected,"Data/well/Corrected/Santa Clara River Aflalo_well_daily_corrected_02_10_2026.csv")

################################################################################
## Hanson
Hanson_well<-read.csv("Data/well/2-11-2026/Other Calgro/Santa Clara River Hanson Retired Well - 20240107-20260211.csv")
tail(Hanson_well)
Hanson_well <- remove_last_n(Hanson_well, n=1)
Hanson_well$Date.and.Time<-as.POSIXct(Hanson_well$Date.and.Time)
Hanson_well$Date<-as.Date(Hanson_well$Date.and.Time)

#Other data flagging will be done with daily data
Hanson_daily<-aggregate(ft..below.ground.~Date,Hanson_well,FUN=mean)
Hanson_daily <- Hanson_daily %>%
  filter(!(Date >= as.Date("2025-12-04") & Date <= as.Date("2025-12-09")))

#should make this into a function?
df_flagged <- Hanson_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 2)

ggplot(Hanson_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(Hanson_daily,"Data/well/Corrected/Santa Clara River Hanson Retired_well_daily_corrected_02_10_2026.csv")

################################################################################
## Prisoners
Prisoners_well<-read.csv("Data/well/2-11-2026/Other Calgro/SCI Prisoners Harbor well 20  - 20241201-20260211.csv")
tail(Prisoners_well)
Prisoners_well <- remove_last_n(Prisoners_well, n=1)
Prisoners_well$Date.and.Time<-as.POSIXct(Prisoners_well$Date.and.Time)
Prisoners_well$Date<-as.Date(Prisoners_well$Date.and.Time)

#Other data flagging will be done with daily data
Prisoners_daily<-aggregate(ft..below.ground.~Date,Prisoners_well,FUN=mean)
Prisoners_daily <- Prisoners_daily %>%
  filter(!(Date >= as.Date("2025-11-05") & Date <= as.Date("2025-11-18")))

#should make this into a function?
df_flagged <- Prisoners_daily %>%
  arrange(Date) %>%
  mutate( dtw_change = ft..below.ground. - lag(ft..below.ground.),
          flag_big_jump = abs(dtw_change) > 2)

ggplot(Prisoners_daily, aes(Date)) +geom_line(aes(y = ft..below.ground.), color = "gray60")

write.csv(Prisoners_daily,"Data/well/Corrected/SCI Prisoners Harbor well 20_well_daily_corrected_02_10_2026.csv")


