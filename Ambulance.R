# Run Setup script
source("SetupNew.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 15
PNGHeight <- 5.5
PNGUnits <- "cm"
PNGDPI <- 300


M032Ambulance <- read_excel("Data/M032Ambulance.xlsx") %>% 
  filter(AreaOrHospital %in% c("Somerset", "MUSGROVE PARK HOSPITAL", "YEOVIL DISTRICT HOSPITAL"),
         AreaOrHospitalType %in% c("CCG", "Hospital")) %>% 
  mutate(firstOfMonth = as.Date(firstOfMonth),
         Metric1 = `Benchmark:Total Public incidents`,
         Metric2 = `Performance:Category 1 Mean Response Duration (Mins)`,
         Metric3 = `Performance:Category 1 90th Percentile Response Duration (Mins)`,
         Metric4 = `Performance:Category 2 Mean Response Duration (Mins)`,
         Metric5 = `Performance:Category 2 90th Percentile Response Duration (Mins)`,
         Metric6 = `ALL Handovers:15+`/`ALL Handovers:Total Acute Handovers`,
         Metric7 = (`ALL Handovers:15+`-`ALL Handovers:15-30`)/`ALL Handovers:Total Acute Handovers`,
         Metric8 = `ALL Handovers:60+`/`ALL Handovers:Total Acute Handovers`,
         Metric9 = `ALL Handovers:SecondsLostOver15minutes`/3600)


NationalAmbulance <-   tbl(DataSQLConnectionDM, in_schema("Performance", "Ambulance_Quality_Indicators")) |> 
  filter(Indicator_Code %in% c("A25", "A31"),
         Provider_Organisation_Name %in% c("England", "SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST"),
         Report_Date >= '2021-04-01') %>% 
  select(firstOfMonth = Report_Date, AreaOrHospital=Provider_Organisation_Name, Indicator_Code, Indicator_Value) %>% 
  collect() %>% 
  mutate(AreaOrHospital = recode(AreaOrHospital, "SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST" = "SWAST"),
         Indicator_Code = recode(Indicator_Code, "A25" = "Metric2", "A31" = "Metric4"),
         Indicator_Value = Indicator_Value/60) %>% 
  pivot_wider(names_from = Indicator_Code, values_from = Indicator_Value)


#Total Calls
AmbulanceCalls <- M032Ambulance %>% 
  filter(AreaOrHospital == "Somerset") %>% 
  select(firstOfMonth, Metric1) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric1)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(breaks = seq(5000,7000, by=500),
                     limits = c(5000,7000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Total number of calls to SWAST"),
       subtitle = paste0("Somerset"),
       caption = paste0("SWAST")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/Ambulance/AmbulanceCalls.png"),
       plot = AmbulanceCalls,
       width = PNGWidth,
       height = 6.3,        
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



#Ambulance mean response times CAT 1 (Mins)
Ambulance1Mean <- M032Ambulance %>% 
  filter(AreaOrHospital == "Somerset") %>% 
  select(firstOfMonth, AreaOrHospital, Metric2) %>% 
  rbind(NationalAmbulance %>% select(-Metric4)) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric2, colour = AreaOrHospital)) +
  geom_line(size = 0.7)+
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSBlue,
                                 "England" = NHSLightBlue,
                                 "SWAST" = NHSDarkPink)) +
  scale_y_continuous(limits = c(6,16),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  geom_hline(yintercept = 7,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 7.2,
                label = "Target = 7 minutes"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Ambulance mean response times CAT 1 (Mins)"),
       subtitle = "<span style='color:#005EB8;'>Somerset, </span><span style='color:#41B6E6;'>England, </span><span style='color:#7C2855;'>SWAST</span>",
       caption = paste0("SWAST M032 and https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/")) 
  

ggsave(file = paste0("Outputs/Ambulance/Ambulance1Mean.png"),
       plot = Ambulance1Mean,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



#Ambulance mean response times CAT 1 90th Centile (Mins)
Ambulance190C <- M032Ambulance %>% 
  filter(AreaOrHospital == "Somerset") %>% 
  select(firstOfMonth, Metric3) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric3)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(14,26),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Ambulance mean response times CAT 1 90th Centile (Mins)"),
       subtitle = paste0("Somerset"),
       caption = paste0("SWAST")) +
  geom_hline(yintercept = 15,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 15.2,
                label = "Target = 15 minutes"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/Ambulance/Ambulance190C.png"),
       plot = Ambulance190C,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





#Ambulance mean response times CAT 2 (Mins)
Ambulance2Mean <- M032Ambulance %>% 
  filter(AreaOrHospital == "Somerset") %>% 
  select(firstOfMonth, AreaOrHospital, Metric4) %>% 
  rbind(NationalAmbulance %>% select(-Metric2)) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric4, colour = AreaOrHospital)) +
  geom_line(size = 0.7)+
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSBlue,
                                 "England" = NHSLightBlue,
                                 "SWAST" = NHSDarkPink)) +
  scale_y_continuous(breaks = seq(15,120, by=15),
                     limits = c(15,120),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+

  geom_hline(yintercept = 18,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 20,
                label = "Target = 18 minutes"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Ambulance mean response times CAT 2 (Mins)"),
       subtitle = "<span style='color:#005EB8;'>Somerset, </span><span style='color:#41B6E6;'>England, </span><span style='color:#7C2855;'>SWAST</span>",
       caption = paste0("SWAST M032 and https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/")) 



ggsave(file = paste0("Outputs/Ambulance/Ambulance2Mean.png"),
       plot = Ambulance2Mean,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




#Ambulance mean response times CAT 2 90th Centile (Mins)
Ambulance290C <- M032Ambulance %>% 
  filter(AreaOrHospital == "Somerset") %>% 
  select(firstOfMonth, Metric5) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric5)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(30,180),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Ambulance mean response times CAT 2 90th Centile (Mins)"),
       subtitle = paste0("Somerset"),
       caption = paste0("SWAST")) +
  geom_hline(yintercept = 40,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 45,
                label = "Target = 40 minutes"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/Ambulance/Ambulance290C.png"),
       plot = Ambulance290C,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



#Ambulance Handovers over 15 minutes
Ambulance15 <- M032Ambulance %>% 
  select(firstOfMonth, AreaOrHospital, Metric6) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric6, colour = AreaOrHospital)) +
  geom_line(size = 0.7)+
  scale_y_continuous(breaks = seq(0,0.75, by=0.25),
                     limits = c(0,0.75),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  scale_color_manual(values = c("Somerset" = NHSBlue,
                     "YEOVIL DISTRICT HOSPITAL" = NHSPurple,
                     "MUSGROVE PARK HOSPITAL" = NHSAquaGreen)) +
  geom_hline(yintercept = 0.35,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.36,
                label = "Target = 35%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Percentage of Ambulance Handovers over 15 minutes"),
       subtitle="<span style='color:#005EB8;'>Somerset, </span><span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("SWAST")) 

ggsave(file = paste0("Outputs/Ambulance/Ambulance15.png"),
       plot = Ambulance15,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


#Ambulance Handovers over 30 minutes
Ambulance30 <- M032Ambulance %>% 
  select(firstOfMonth, AreaOrHospital, Metric7) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric7, colour = AreaOrHospital)) +
  geom_line(size = 0.7)+
  scale_y_continuous(breaks = seq(0,0.3, by=0.1),
                     limits = c(0,0.3),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  scale_color_manual(values = c("Somerset" = NHSBlue,
                                "YEOVIL DISTRICT HOSPITAL" = NHSPurple,
                                "MUSGROVE PARK HOSPITAL" = NHSAquaGreen)) +
  geom_hline(yintercept = 0.05,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.06,
                label = "Target = 5%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Percentage of Ambulance Handovers over 30 minutes"),
       subtitle="<span style='color:#005EB8;'>Somerset, </span><span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("SWAST")) 

ggsave(file = paste0("Outputs/Ambulance/Ambulance30.png"),
       plot = Ambulance30,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



#Ambulance Handovers over 60 minutes
Ambulance60 <- M032Ambulance %>% 
  select(firstOfMonth, AreaOrHospital, Metric8) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric8, colour = AreaOrHospital)) +
  geom_line(size = 0.7)+
  scale_y_continuous(breaks = seq(0,0.15, by=0.05),
                     limits = c(0,0.15),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  scale_color_manual(values = c("Somerset" = NHSBlue,
                                "YEOVIL DISTRICT HOSPITAL" = NHSPurple,
                                "MUSGROVE PARK HOSPITAL" = NHSAquaGreen)) +
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Percentage of Ambulance Handovers over 60 minutes"),
       subtitle="<span style='color:#005EB8;'>Somerset, </span><span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("SWAST")) 

ggsave(file = paste0("Outputs/Ambulance/Ambulance60.png"),
       plot = Ambulance60,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")






#Lost Hours >15 Minutes
AmbulanceLostTime <- M032Ambulance %>% 
  select(firstOfMonth, AreaOrHospital, Metric9) %>% 
  ggplot(aes(x = firstOfMonth, y = Metric9, colour = AreaOrHospital)) +
  geom_line(size = 0.7)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(breaks= seq(min(M032Ambulance$firstOfMonth), max(M032Ambulance$firstOfMonth), by = "3 months"),
               date_labels = "%b %Y")+
  scale_color_manual(values = c("Somerset" = NHSBlue,
                                "YEOVIL DISTRICT HOSPITAL" = NHSPurple,
                                "MUSGROVE PARK HOSPITAL" = NHSAquaGreen)) +
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Lost Hours >15 Minutes"),
       subtitle="<span style='color:#005EB8;'>Somerset, </span><span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("SWAST")) 

ggsave(file = paste0("Outputs/Ambulance/AmbulanceLostTime.png"),
       plot = AmbulanceLostTime,
       width = PNGWidth,
       height = PNGHeight,        units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



















