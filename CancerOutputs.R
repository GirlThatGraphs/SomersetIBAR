# Run Setup script
source("SetupNew.R")

source("CancerData.R")


# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 5
PNGUnits <- "cm"
PNGDPI <- 300

# 28Day ---------------------------------------------------------------------

# Dataframe
Cancer28DayData <-
  Cancer |> 
  filter(`Metric` %in% c("Cancer 28Day"))

# Graphic
Cancer28DayPlot <-
  ggplot(data = Cancer28DayData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Somerset System',
                                       'SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0.75,
             size = 0.5,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.5,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(Cancer$`Date`),
                              to = max(Cancer$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("28-Day Faster Diagnosis Standard"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Cancer return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Cancer/Cancer28DayPlot.png"),
       plot = Cancer28DayPlot,
       width = PNGWidth,
       height = PNGHeight,        
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# 2ww ---------------------------------------------------------------------

# Dataframe
Cancer2wwData <-
  Cancer |> 
  filter(`Metric` %in% c("Cancer 2WW"))

# Graphic
Cancer2wwPlot <-
  ggplot(data = Cancer2wwData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Somerset System',
                                       'SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0.93,
             size = 0.5,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.5,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(Cancer$`Date`),
                              to = max(Cancer$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Suspected Cancer Referrals Seen Within 2 Weeks"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Cancer return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Cancer/Cancer2wwPlot.png"),
       plot = Cancer2wwPlot,
       width = PNGWidth,
       height = PNGHeight,        
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# 31 day ------------------------------------------------------------------

# Dataframe
Cancer31Data <-
  Cancer |> 
  filter(`Metric` %in% c("Cancer 31Day"))

# Graphic
Cancer31Plot <-
  ggplot(data = Cancer31Data,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Somerset System',
                                       'SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0.96,
             size = 0.5,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.8,
                                1),
                     expand = c(0,
                                0),
                     labels = percent_format(accuracy = 1)) +
  scale_x_date(breaks = c(seq(from = min(Cancer$`Date`),
                              to = max(Cancer$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Cancer First Definitive Treatment Within 31 Days"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Cancer return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Cancer/Cancer31Plot.png"),
       plot = Cancer31Plot,
       width = PNGWidth,
       height = PNGHeight,        
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# 62 day ------------------------------------------------------------------

# Dataframe
Cancer62Data <-
  Cancer |> 
  filter(`Metric` %in% c("Cancer 62Day"))

# Graphic
Cancer62Plot <-
  ggplot(data = Cancer62Data,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Somerset System',
                                       'SFT',
                                       'YDH')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "Somerset System" = NHSBlue)) +
  geom_hline(yintercept = 0.85,
             size = 0.5,
             colour = NHSDarkGrey,
             linetype = "dashed") +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.5,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(Cancer$`Date`),
                              to = max(Cancer$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Cancer First Definitive Treatment Within 62 Days, Following GP Referral"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Cancer return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Cancer/Cancer62Plot.png"),
       plot = Cancer62Plot,
       width = PNGWidth,
       height = PNGHeight,        
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Cancer restoration ------------------------------------------------------

# Dataframe
Cancer2wwRestData <-
  Cancer |> 
  filter(`Metric` %in% c("Cancer 2ww restoration"))

# Graphic
Cancer2wwRestPlot <-
  ggplot(data = Cancer2wwRestData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Somerset (Commissioner)',
                                       'SFT',
                                       'YDH',
                                       'RUH',
                                       'UBHW',
                                       'Others')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "RUH" = NHSWarmYellow,
                                 "UBHW" = NHSPink,
                                 "Others" = NHSLightBlue,
                                 "Somerset (Commissioner)" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                3),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(Cancer$`Date`),
                              to = max(Cancer$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Cancer 2ww restoration"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#41B6E6;'>Others, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Cancer return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Cancer/Cancer2wwRestPlot.png"),
       plot = Cancer2wwRestPlot,
       width = PNGWidth,
       height = PNGHeight,        
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Dataframe
CancerTreatRestData <-
  Cancer |> 
  filter(`Metric` %in% c("Cancer Treatment restoration"))

# Graphic
CancerTreatRestPlot <-
  ggplot(data = CancerTreatRestData,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Somerset (Commissioner)',
                                       'SFT',
                                       'YDH',
                                       'RUH',
                                       'UBHW',
                                       'Others')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple,
                                 "RUH" = NHSWarmYellow,
                                 "UBHW" = NHSPink,
                                 "Others" = NHSLightBlue,
                                 "Somerset (Commissioner)" = NHSBlue)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                2),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(Cancer$`Date`),
                              to = max(Cancer$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Cancer Treatment restoration"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#FFB81C;'>RUH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#41B6E6;'>Others, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Cancer return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Cancer/CancerTreatRestPlot.png"),
       plot = CancerTreatRestPlot,
       width = PNGWidth,
       height = PNGHeight,
       dpi = PNGDPI,        
       units = PNGUnits,
       type = "cairo")