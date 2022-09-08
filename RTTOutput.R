
# Run Setup script ----------------------------------------------------------
source("SetupNew.R")
source("RTTData.R")
source("Restoration.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 7
PNGUnits <- "cm"
PNGDPI <- 300


# Incomplete --------------------------------------------------------------------
# Dataframe
RTTIncompleteSomCommData <-
  RTTPerformanceSQLData |>
  filter(`PathwayDescription` == "Incomplete Pathways",
         `CommissionerDescription` == "Somerset",
         `MonthDate` >= StartMonthDate) |> 
  select(`MonthDate`,
         `CommissionerDescription`,
         `Waits`) |>
  group_by(`MonthDate`,
           `CommissionerDescription`) |> 
  summarise(`Waits` = sum(`Waits`)) |> 
  ungroup() |> 
  mutate(`ProviderGroup` = "SOM") |> 
  collect() |> 
  select(`MonthDate`,
         `Waits`,
         `ProviderGroup`)

RTTIncompleteSomProvData <-
  RTTPerformanceSQLData |>
  filter(`PathwayDescription` == "Incomplete Pathways",
         `CommissionerDescription` == "Somerset",
         `MonthDate` >= StartMonthDate,
         `ProviderDescription` == "Somerset") |> 
  select(`MonthDate`,
         `ProviderCode`,
         `Waits`) |>
  group_by(`MonthDate`,
           `ProviderCode`) |> 
  summarise(`Waits` = sum(`Waits`)) |> 
  ungroup() |> 
  mutate(`ProviderGroup` = case_when(`ProviderCode` %in% c("RBA",
                                                           "RH5") ~ "RH5",
                                     `ProviderCode` == "RA4" ~ "RA4",
                                     TRUE ~ "OTH")) |> 
  collect() |> 
  select(`MonthDate`,
         `Waits`,
         `ProviderGroup`)

RTTIncompleteData <- rbind(RTTIncompleteSomCommData,
                           RTTIncompleteSomProvData)


# Graphic
RTTIncompletePlot <-
  ggplot(data = RTTIncompleteData,
         aes(x = `MonthDate`,
             y = `Waits`,
             colour = factor(`ProviderGroup`,
                             level = c('SOM',
                                       'RH5',
                                       'RA4')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SOM" = NHSBlue,
                                 "RH5" = NHSAquaGreen,
                                 "RA4" = NHSPurple)) +
  scale_fill_manual("legend",
                    values = c("SOM" = NHSBlue,
                               "RH5" = NHSAquaGreen,
                               "RA4" = NHSPurple)) +
  scale_x_date(breaks = c(seq(from = min(RTTIncompleteData$`MonthDate`),
                              to = max(RTTIncompleteData$`MonthDate`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                55000),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Incomplete Pathways"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/RTT/RTTIncompletePlot.png"),
       plot = RTTIncompletePlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits)



# 78ww --------------------------------------------------------------------


# Dataframe
RTT78SomCommData <-
  RTTPerformanceSQLData |>
  filter(`PathwayDescription` == "Incomplete Pathways",
         `CommissionerDescription` == "Somerset",
         `MonthDate` >= StartMonthDate,
         `Week` >= 78) |> 
  select(`MonthDate`,
         `CommissionerDescription`,
         `Waits`) |>
  group_by(`MonthDate`,
           `CommissionerDescription`) |> 
  summarise(`Waits` = sum(`Waits`)) |> 
  ungroup() |> 
  mutate(`ProviderGroup` = "SOM") |> 
  collect() |> 
  select(`MonthDate`,
         `Waits`,
         `ProviderGroup`)

RTT78SomProvData <-
  RTTPerformanceSQLData |>
  filter(`PathwayDescription` == "Incomplete Pathways",
         `CommissionerDescription` == "Somerset",
         `MonthDate` >= StartMonthDate,
         `Week` >= 78,
         `ProviderDescription` == "Somerset") |> 
  select(`MonthDate`,
         `ProviderCode`,
         `Waits`) |>
  group_by(`MonthDate`,
           `ProviderCode`) |> 
  summarise(`Waits` = sum(`Waits`)) |> 
  ungroup() |> 
  mutate(`ProviderGroup` = case_when(`ProviderCode` %in% c("RBA",
                                                           "RH5") ~ "RH5",
                                     `ProviderCode` == "RA4" ~ "RA4",
                                     TRUE ~ "OTH")) |> 
  collect() |> 
  select(`MonthDate`,
         `Waits`,
         `ProviderGroup`)

RTT78Data <- rbind(RTT78SomCommData,
                           RTT78SomProvData)


# Graphic
RTT78Plot <-
  ggplot(data = RTT78Data,
         aes(x = `MonthDate`,
             y = `Waits`,
             colour = factor(`ProviderGroup`,
                             level = c('SOM',
                                       'RH5',
                                       'RA4')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SOM" = NHSBlue,
                                 "RH5" = NHSAquaGreen,
                                 "RA4" = NHSPurple)) +
  scale_fill_manual("legend",
                    values = c("SOM" = NHSBlue,
                               "RH5" = NHSAquaGreen,
                               "RA4" = NHSPurple)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_x_date(breaks = c(seq(from = min(RTT78Data$`MonthDate`),
                              to = max(RTT78Data$`MonthDate`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                1500),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of Patients Waiting >78 Weeks"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/RTT/RTT78Plot.png"),
       plot = RTT78Plot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# 104 ---------------------------------------------------------------------

# Dataframe
RTT104SomCommData <-
  RTTPerformanceSQLData |>
  filter(`PathwayDescription` == "Incomplete Pathways",
         `CommissionerDescription` == "Somerset",
         `MonthDate` >= StartMonthDate,
         `Week` >= 103) |> 
  select(`MonthDate`,
         `CommissionerDescription`,
         `Waits`) |>
  group_by(`MonthDate`,
           `CommissionerDescription`) |> 
  summarise(`Waits` = sum(`Waits`)) |> 
  ungroup() |> 
  mutate(`ProviderGroup` = "SOM") |> 
  collect() |> 
  select(`MonthDate`,
         `Waits`,
         `ProviderGroup`)

RTT104SomProvData <-
  RTTPerformanceSQLData |>
  filter(`PathwayDescription` == "Incomplete Pathways",
         `CommissionerDescription` == "Somerset",
         `MonthDate` >= StartMonthDate,
         `Week` >= 103,
         `ProviderDescription` == "Somerset") |> 
  select(`MonthDate`,
         `ProviderCode`,
         `Waits`) |>
  group_by(`MonthDate`,
           `ProviderCode`) |> 
  summarise(`Waits` = sum(`Waits`)) |> 
  ungroup() |> 
  mutate(`ProviderGroup` = case_when(`ProviderCode` %in% c("RBA",
                                                           "RH5") ~ "RH5",
                                     `ProviderCode` == "RA4" ~ "RA4",
                                     TRUE ~ "OTH")) |> 
  collect() |> 
  select(`MonthDate`,
         `Waits`,
         `ProviderGroup`)

RTT104Data <- rbind(RTT104SomCommData,
                   RTT104SomProvData)


# Graphic
RTT104Plot <-
  ggplot(data = RTT104Data,
         aes(x = `MonthDate`,
             y = `Waits`,
             colour = factor(`ProviderGroup`,
                             level = c('SOM',
                                       'RH5',
                                       'RA4')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SOM" = NHSBlue,
                                 "RH5" = NHSAquaGreen,
                                 "RA4" = NHSPurple)) +
  scale_fill_manual("legend",
                    values = c("SOM" = NHSBlue,
                               "RH5" = NHSAquaGreen,
                               "RA4" = NHSPurple)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_x_date(breaks = c(seq(from = min(RTT104Data$`MonthDate`),
                              to = max(RTT104Data$`MonthDate`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                300),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of Patients Waiting >104 Weeks"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/RTT/RTT104Plot.png"),
       plot = RTT104Plot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Restoration -------------------------------------------------------------
# Clock start Restoration -------------------------------------------------------------

# Dataframe
RestorationRTTData <-
  RestorationRTT |> 
  filter(`Metric` %in% c("ClockStarts Restoration"))

# Graphic
RestorationStartsPlot <-
  ggplot(data = RestorationRTTData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('SFT',
                                       'YDH',
                                       'Somerset System')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.8,
                                1.2),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(RestorationRTTData$`Date`),
                              to = max(RestorationRTTData$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Clock Starts Restoration"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Bed Occupancy return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/RTT/RestorationStartsPlot.png"),
       plot = RestorationStartsPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Clock stops admitted restoration ----------------------------------------


# Dataframe
RestorationAdRTTData <-
  RestorationRTT |> 
  filter(`Metric` %in% c("ClockStopsAd Restoration"))

# Graphic
RestorationAdPlot <-
  ggplot(data = RestorationAdRTTData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('SFT',
                                       'YDH',
                                       'Somerset System')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 1.1,
             size = 0.5,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                2),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(RestorationAdRTTData$`Date`),
                              to = max(RestorationAdRTTData$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Elective Restoration"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Bed Occupancy return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/RTT/RestorationAdPlot.png"),
       plot = RestorationAdPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# RTT elective restoration ------------------------------------------------



# Dataframe
RestorationElectiveData <-
  RestorationActivity |> 
  filter(`Metric` %in% c("Elective Restoration"))

# Graphic
RestorationElectivePlot <-
  ggplot(data = RestorationElectiveData %>% filter(Provider != "Plan"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('SFT',
                                       'YDH',
                                       'Somerset System')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_line(data = RestorationElectiveData %>% filter(Provider == "Plan"),
            aes(x = `Date`,
                y = `Count`),
            size = 0.5,
            colour = NHSBlue,
            linetype = "dashed") +
  geom_hline(yintercept = 1.1,
             size = 0.5,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.9,
                                1.21),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(RestorationElectiveData$`Date`),
                              to = max(RestorationElectiveData$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 1.11,
                label = "National Ambition = 110%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_text(aes(x = as.Date("2022-04-10"),
                y = 0.985,
                label = "ICB Plan"),
            hjust = 0,
            vjust = 0,
            colour = NHSBlue,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Elective Restoration (Outpatients and Inpatients Combined)"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: SUS ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/RTT/RestorationElectivePlot.png"),
       plot = RestorationElectivePlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

