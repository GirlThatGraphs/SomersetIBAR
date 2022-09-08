# RunSetup ----------------------------------------------------------------
# Run Setup script
source("SetupNew.R")
source("InpatientData.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 8
PNGHeight <- 7
PNGUnits <- "cm"
PNGDPI <- 300

# NonElective -------------------------------------------------------------
# Non-Elective 0 LoS (Commissioner, by Provider) Plot -------------------------------------------------------------
# Dataframe
NonElective0SomCommData <-
  NonElective0SQLData |>
  CommProvPlotGroup() |> 
  select(`MonthDate`,
         `ProviderDescription`,
         `Activity`)

NonElective0SomData <-
  NonElective0SQLData |> 
  filter(`CommissionerCode` == "11X") |> 
  select(`MonthDate`,
         `Activity`) |> 
  group_by(`MonthDate`) |> 
  summarise(`Activity` = sum(`Activity`)) |> 
  ungroup() |> 
  mutate(`ProviderDescription` = "Somerset") |> 
  select(`MonthDate`,
         `ProviderDescription`,
         `Activity`)

NEL0Data <- rbind(NonElective0SomCommData,
                             NonElective0SomData)

# Graphic
NonElective0SomCommPlot <-
  ggplot(data = NEL0Data,
         aes(x = `MonthDate`,
             y = `Activity`,
             colour = factor(`ProviderDescription`,
                             level = c('Taunton',
                                       'Yeovil',
                                       'Bath',
                                       'BristolWeston',
                                       'Somerset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Taunton" = NHSAquaGreen,
                                 "Yeovil" = NHSPurple,
                                 "Bath" = NHSWarmYellow,
                                 "BristolWeston" = NHSPink,
                                 "Somerset" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_x_date(breaks = c(seq(from = min(NonElective0SomCommData$`MonthDate`),
                              to = max(NonElective0SomCommData$`MonthDate`),
                              by = "3 months")),                labels = date_format("%b%y")) +
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                2501),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Non-Elective 0 LoS Activity"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: SUS, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/NonElective0.png"),
       plot = NonElective0SomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Non-Elective 1 LoS (Commissioner, by Provider) Plot -------------------------------------------------------------
# Dataframe
NonElective1SomCommData <-
  NonElective1SQLData |>
  CommProvPlotGroup() |> 
  select(`MonthDate`,
         `ProviderDescription`,
         `Activity`)

NonElective1SomData <-
  NonElective1SQLData |> 
  filter(`CommissionerCode` == "11X") |> 
  select(`MonthDate`,
         `Activity`) |> 
  group_by(`MonthDate`) |> 
  summarise(`Activity` = sum(`Activity`)) |> 
  ungroup() |> 
  mutate(`ProviderDescription` = "Somerset") |> 
  select(`MonthDate`,
         `ProviderDescription`,
         `Activity`)

NEL1Data <- rbind(NonElective1SomCommData,
                  NonElective1SomData)

# Graphic
NonElective1SomCommPlot <-
  ggplot(data = NEL1Data %>% filter(Activity > 5),
         aes(x = `MonthDate`,
             y = `Activity`,
             colour = factor(`ProviderDescription`,
                             level = c('Taunton',
                                       'Yeovil',
                                       'Bath',
                                       'BristolWeston',
                                       'Somerset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Taunton" = NHSAquaGreen,
                                 "Yeovil" = NHSPurple,
                                 "Bath" = NHSWarmYellow,
                                 "BristolWeston" = NHSPink,
                                 "Somerset" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_x_date(breaks = c(seq(from = min(NonElective1SomCommData$`MonthDate`),
                              to = max(NonElective1SomCommData$`MonthDate`),
                              by = "3 months")),                labels = date_format("%b%y")) +
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                5001),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Non-Elective 1+ LoS Activity"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: SUS, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/NonElective1.png"),
       plot = NonElective1SomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Length of Stay ----------------------------------------------------------

LengthofStaySQLData <- LengthofStaySQLData |> mutate(MonthDate = as.Date(MonthDate))

LOSSomCommPlot <-
  ggplot(data = LengthofStaySQLData,
         aes(x = `MonthDate`,
             y = `AvgLOS`,
             colour = factor(`ProviderGrouping`,
                             level = c('Somerset',
                                       'Yeovil',
                                       'Other')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSAquaGreen,
                                 "Yeovil" = NHSPurple,
                                 "Other" = NHSWarmYellow)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                15),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(LengthofStaySQLData$`MonthDate`),
                              to = max(LengthofStaySQLData$`MonthDate`),
                              by = "3 months")),                labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Average Length of Stay"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#FFB81C;'>Other</span>",
       caption = paste0("Data source: SUS, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/AvgLOS.png"),
       plot = LOSSomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Readmissions -------------------------------------------------------------
# Readmissions <= 2 (Commissioner, by Provider) Plot -------------------------------------------------------------
# Graphic
Readmissions2SomCommData <-
  ReadmissionsSQLData |> 
  filter(`Days2` == "Y") |> 
  select(`MonthDate`,
         `Provider`,
         `Activity`) |> 
  collect() |> 
  group_by(`MonthDate`,
           `Provider`) |> 
  summarise(`Activity` = sum(`Activity`)) |> 
  ungroup() |> 
  mutate(MonthDate = as.Date(MonthDate))

Readmissions2SomCommPlot <-
  ggplot(data = Readmissions2SomCommData,
         aes(x = `MonthDate`,
             y = `Activity`,
             colour = factor(`Provider`,
                             level = c('Somerset',
                                       'Yeovil',
                                       'Bath',
                                       'BristolWeston',
                                       'Somerset System')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSAquaGreen,
                                 "Yeovil" = NHSPurple,
                                 "Bath" = NHSWarmYellow,
                                 "BristolWeston" = NHSPink,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                251),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(Readmissions2SomCommData$`MonthDate`),
                              to = max(Readmissions2SomCommData$`MonthDate`),
                              by = "3 months")),                labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Readmissions <= 2 days Activity"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: SUS, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/Readmissions2.png"),
       plot = Readmissions2SomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Readmissions <= 7 (Commissioner, by Provider) Plot -------------------------------------------------------------
# Graphic
Readmissions7SomCommData <-
  ReadmissionsSQLData |> 
  filter(`Days7` == "Y") |> 
  select(`MonthDate`,
         `Provider`,
         `Activity`) |> 
  collect() |> 
  group_by(`MonthDate`,
           `Provider`) |> 
  summarise(`Activity` = sum(`Activity`)) |> 
  ungroup() |> 
  mutate(MonthDate = as.Date(MonthDate))

Readmissions7SomCommPlot <-
  ggplot(data = Readmissions7SomCommData,
         aes(x = `MonthDate`,
             y = `Activity`,
             colour = factor(`Provider`,
                             level = c('Somerset',
                                       'Yeovil',
                                       'Bath',
                                       'BristolWeston',
                                       'Somerset System')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSAquaGreen,
                                 "Yeovil" = NHSPurple,
                                 "Bath" = NHSWarmYellow,
                                 "BristolWeston" = NHSPink,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                501),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(Readmissions7SomCommData$`MonthDate`),
                              to = max(Readmissions7SomCommData$`MonthDate`),
                              by = "3 months")),                labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Readmissions <= 7 days Activity"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: SUS, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/Readmissions7.png"),
       plot = Readmissions7SomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Readmissions <= 30 (Commissioner, by Provider) Plot -------------------------------------------------------------
# Graphic
Readmissions30SomCommData <-
  ReadmissionsSQLData |> 
  filter(`Days30` == "Y") |> 
  select(`MonthDate`,
         `Provider`,
         `Activity`) |> 
  collect() |> 
  group_by(`MonthDate`,
           `Provider`) |> 
  summarise(`Activity` = sum(`Activity`)) |> 
  ungroup() |> 
  mutate(MonthDate = as.Date(MonthDate))

Readmissions30SomCommPlot <-
  ggplot(data = Readmissions30SomCommData,
         aes(x = `MonthDate`,
             y = `Activity`,
             colour = factor(`Provider`,
                             level = c('Somerset',
                                       'Yeovil',
                                       'Bath',
                                       'BristolWeston',
                                       'Somerset System')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSAquaGreen,
                                 "Yeovil" = NHSPurple,
                                 "Bath" = NHSWarmYellow,
                                 "BristolWeston" = NHSPink,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1001),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(Readmissions30SomCommData$`MonthDate`),
                              to = max(Readmissions7SomCommData$`MonthDate`),
                              by = "3 months")),                labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Readmissions <= 30 days Activity"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: SUS, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/Readmissions30.png"),
       plot = Readmissions30SomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       dpi = PNGDPI,
       units = PNGUnits,
       type = "cairo")


# NRTR --------------------------------------------------------------------

# No Right to Reside (Commissioner, by Provider) Plot

NRTRSomCommData <-
  NRTR_All |> 
  pivot_longer(!Date,
               names_to = "Provider",
               values_to = "NRTR") |> 
  filter(`Provider` %in% c("SFT Acute",
                           "YDH Acute",
                           "SFT Community"))

# Graphic
NRTRSomCommPlot <-
  ggplot(data = NRTRSomCommData,
         aes(x = `Date`,
             y = `NRTR`,
             colour = factor(`Provider`,
                             level = c('SFT Acute',
                                       'YDH Acute',
                                       'SFT Community')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT Acute" = NHSAquaGreen,
                                 "YDH Acute" = NHSPurple,
                                 "SFT Community" = NHSWarmYellow)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                201),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(NRTRSomCommData$`Date`),
                              to = max(NRTRSomCommData$`Date`),
                              by = "3 months")),                labels = date_format("%b%y")) +
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("No Right to Reside"),
       subtitle = "<span style='color:#00A499;'>SFT (Acute), </span><span style='color:#330072;'>YDH, </span><span style='color:#FFB81C;'>SFT (Community)</span>",
       caption = paste0("Data source: NRTR, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/NRTR.png"),
       plot = NRTRSomCommPlot,
       width = 16,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Bed Occupancy -----------------------------------------------------------

# Bed Occupancy (Commissioner, by Provider) Plot
BedOccSomCommData <-
  BedOcc_All |> 
  pivot_longer(!Date,
               names_to = "Provider",
               values_to = "BedOccupancy") |> 
  filter(`Provider` %in% c("SFTOccPerc",
                           "YDHOccPerc",
                           "SomOccPerc"))

# Graphic
BedOccSomCommPlot <-
  ggplot(data = BedOccSomCommData,
         aes(x = `Date`,
             y = `BedOccupancy`,
             colour = factor(`Provider`,
                             level = c('SFTOccPerc',
                                       'YDHOccPerc',
                                       'SomOccPerc')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFTOccPerc" = NHSAquaGreen,
                                 "YDHOccPerc" = NHSPurple,
                                 "SomOccPerc" = NHSWarmYellow)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.75,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Bed Occupancy"),
       subtitle = "<span style='color:#00A499;'>SFT (Acute), </span><span style='color:#330072;'>YDH, </span><span style='color:#FFB81C;'>SFT (Community)</span>",
       caption = paste0("Data source: Bed Occupancy return, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/BedOcc.png"),
       plot = BedOccSomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Discharge Pathways ------------------------------------------------------

DischargePathwaysSomCommData <-
  DischargePathways_All |>
  pivot_longer(!Date,
               names_to = "Pathways",
               values_to = "Discharges") |> 
  filter(`Pathways` %in% c("Pathway0",
                           "Pathway1",
                           "Pathway2",
                           "Pathway3",
                           "Pathway4"),
         `Date` >= "2021-06-01")

# Graphic
DischargePathwaysSomCommPlot <-
  ggplot(data = DischargePathwaysSomCommData,
         aes(x = `Date`,
             y = `Discharges`,
             colour = factor(`Pathways`,
                             level = c('Pathway0',
                                       'Pathway1',
                                       'Pathway2',
                                       'Pathway3',
                                       'Pathway4')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Pathway0" = NHSAquaGreen,
                                 "Pathway1" = NHSPurple,
                                 "Pathway2" = NHSWarmYellow,
                                 "Pathway3" = NHSPink,
                                 "Pathway4" = NHSLightBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_x_date(breaks = c(seq(from = min(DischargePathwaysSomCommData$`Date`),
                              to = max(DischargePathwaysSomCommData$`Date`),
                              by = "3 months")),                labels = date_format("%b%y")) +
               labels = date_format("%b%y")) +
  scale_y_continuous(limits = c(0,
                                3001),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Discharge Pathways"),
       subtitle = "<span style='color:#00A499;'>Pathway 0, </span><span style='color:#330072;'>Pathway 1, </span><span style='color:#AE2573;'>Pathway 2, </span><span style='color:#FFB81C;'>Pathway 3</span>",
       caption = paste0("Data source: Pathways Return, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/DischargePathways.png"),
       plot = DischargePathwaysSomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Long Stays --------------------------------------------------------------

LongStaySomCommData <-
  LongStay_21_All |>
  pivot_longer(!Date,
               names_to = "Provider",
               values_to = "LongStay")

LongStaySomCommPlot <-
  ggplot(data = LongStaySomCommData,
         aes(x = `Date`,
             y = `LongStay`,
             colour = factor(`Provider`,
                             level = c('SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                201),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(LongStaySomCommData$`Date`),
                              to = max(LongStaySomCommData$`Date`),
                              by = "3 months")),                labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0(">21 Day Length of Stays"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("Data source: Long Stay report, updated on ",
                        format(Sys.time(),
                               '%d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/EmergencyAdmissions/LongStay.png"),
       plot = LongStaySomCommPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")
