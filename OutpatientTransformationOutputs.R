# RunSetup ----------------------------------------------------------------
# Run Setup script
source("SetupNew.R")
source("OutpatientTransformationData.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300


# Advice&Guidance -------------------------------------------------------------

# Dataframe
AGData <-
  OPTransformation |> 
  filter(`Metric` %in% c("OPTransformation AG"))

# Graphic
AGPlot <-
  ggplot(data = AGData,
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
  scale_x_date(labels = date_format("%b%y")) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                41),
                     expand = c(0,
                                0),
                     labels = comma) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Advice & Guidance rate per 100 1st Outpatients"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Bed Occupancy return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/OutpatientTransformation/AGPlot.png"),
       plot = AGPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# PIFU --------------------------------------------------------------------


# Dataframe
PIFUData <-
  OPTransformation |> 
  filter(`Metric` %in% c("OPTransformation PIFU"))

# Graphic
PIFUPlot <-
  ggplot(data = PIFUData,
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
  scale_x_date(breaks= seq(min(PIFUData$Date), max(PIFUData$Date), by = "3 months"),
               date_labels = "%b %Y")+
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                0.2),
                     expand = c(0,
                                0),
                     labels = percent) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("PIFU"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Data source: Bed Occupancy return, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))


# Save Output
ggsave(file = paste0("Outputs/OutpatientTransformation/PIFUPlot.png"),
       plot = PIFUPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")