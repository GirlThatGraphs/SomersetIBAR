# Run Setup script
source("SetupNew.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300

# Data ----------------------------------------------------------

Leder <-  read_excel("S:/Shared Area/Somerset Performance/Nursing and Patient Safety/LeDeR/LeDer for IBAR/LeDeR raw data.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric")) %>% 
  filter(Month >= "2021-04-01") |> 
  mutate(Month = as.Date(Month))


# -	Number of notifications received in Month -----------------------------

Notifications <- Leder %>% 
  select(Month, `Notifications in month`) %>% 
  ggplot(aes(x = Month, y = `Notifications in month`)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,8),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(as.Date(min(Leder$Month)), as.Date(max(Leder$Month)), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("LeDer - Number of notifications received in Month"),
       subtitle = paste0("Somerset"),
       caption = paste0("Data Source: ")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/LeDer/Notifications.png"),
       plot = Notifications,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# -	-	Number of reviews completed in month -----------------------------


CompletionsMonth <- Leder %>% 
  select(Month, `Completions in month`) %>% 
  ggplot(aes(x = Month, y = `Completions in month`)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,25),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(as.Date(min(Leder$Month)), as.Date(max(Leder$Month)), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("LeDer - Number of reviews completed in month"),
       subtitle = paste0("Somerset"),
       caption = paste0("Data Source: ")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/LeDer/CompletionsMonth.png"),
       plot = CompletionsMonth,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# -	-	-	Number of reviews allocated within 3 months (KPI) -----------------------------


Allocated3 <- Leder %>% 
  select(Month, `3mo allocation KPI met, split by month allocated\r\nExcludes exceptions`) %>% 
  ggplot(aes(x = Month, y = `3mo allocation KPI met, split by month allocated\r\nExcludes exceptions`)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,8),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(as.Date(min(Leder$Month)), as.Date(max(Leder$Month)), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("LeDer - Number of reviews allocated within 3 months"),
       subtitle = paste0("Somerset"),
       caption = paste0("Data Source: ")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/LeDer/Allocated3.png"),
       plot = Allocated3,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




# --	Number of reviews completed within 6 months (KPI) -----------------------------


Completed6 <- Leder %>% 
  select(Month, `6mo completion KPI met, split by month completed\r\nExcludes exceptions`) %>% 
  ggplot(aes(x = Month, y = `6mo completion KPI met, split by month completed\r\nExcludes exceptions`)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,12),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(as.Date(min(Leder$Month)), as.Date(max(Leder$Month)), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("LeDer - Number of reviews completed within 6 months"),
       subtitle = paste0("Somerset"),
       caption = paste0("Data Source: ")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/LeDer/Completed6.png"),
       plot = Completed6,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




