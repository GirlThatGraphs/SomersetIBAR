# SetUp ----------------------------------------------------------
source("SetupNew.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300

# Data ----------------------------------------------------------

AEPerformance <-  tbl(DataSQLConnectionDM, in_schema("Performance", "A&E_Data")) %>% 
  filter(Financial_Year >= "2021-22",
         Provider_Organisation_Code %in% c("RH5", "RA4", "RBA", "RA3", "RA7", "RD1")) %>% 
  collect() %>% 
  mutate(Report_Date = as.Date(Report_Date),
         Metric1 = `A&E_Attendances_Type_1`,
         Metric2 = (1-(Attendances_Over_4hrs_Type_1/`A&E_Attendances_Type_1`)),
         Metric3 = Attendances_Over_4hrs_Type_1,
         Metric4 = `12+_Hour_Waits_From_DTA_To_Admission`,
         Metric5 = (`Emergency_Admissions_Via_A&E_Type_1` / `A&E_Attendances_Type_1`))

AEPerformanceNational <-  tbl(DataSQLConnectionDM, in_schema("Performance", "A&E_Data")) %>% 
  filter(Financial_Year >= "2021-22") %>% 
  select(Report_Date,`Emergency_Admissions_Via_A&E_Type_1`,`A&E_Attendances_Type_1` ) |> 
  group_by(Report_Date) |> 
  summarise(`Emergency_Admissions_Via_A&E_Type_1` = sum(`Emergency_Admissions_Via_A&E_Type_1`),
            `A&E_Attendances_Type_1` = sum(`A&E_Attendances_Type_1`)) |> 
  ungroup() |> 
  collect() %>% 
  mutate(Report_Date = as.Date(Report_Date),
         Metric5 = (`Emergency_Admissions_Via_A&E_Type_1` / `A&E_Attendances_Type_1`)) |> 
  mutate(Provider_Organisation_Code = "National")


AEHIU <- read_excel("S:/Shared Area/Somerset Performance/BI Data/GB Data/High intensity users.xlsx", 
                                   col_types = c("text", "date", "numeric")) %>% 
  rename(Metric6 = `Total Users`)

AEUnheralded <- tbl(DataSQLConnectionASA, in_schema("SOM", "AE_UNHERALDED")) %>% 
  filter(date >= "20210401") %>% 
  collect() %>% 
  mutate(Month = as.Date(floor_date(as.Date(date), "month"))) %>% 
  filter(as.Date(ceiling_date(as.Date(date), "month") -1)<=max(date)) %>% 
  select(Month, PROVIDER, `WALK INS`, `All Attendances`) %>% 
  group_by(Month, PROVIDER) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(Metric7 = `WALK INS`/`All Attendances`)

# Number of A&E Attendance ----------------------------------------------------------

AECount <- AEPerformance %>% 
  select(Report_Date, Metric1,Provider_Organisation_Code) %>% 
  ggplot(aes(x = Report_Date, y = Metric1, colour = Provider_Organisation_Code)) +
  geom_line(size = 0.7)+
    scale_color_manual(values = c("RA4" = NHSPurple,
                                  "RH5" = NHSAquaGreen,
                                  "RA7" = NHSPink,
                                  "RD1" = NHSWarmYellow)) +
  scale_y_continuous(breaks = seq(4000,16000, by=2000),
                     limits = c(4000,16000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Total number of A&E Attendance"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH</span>",
       caption = paste0("NHSE: A&E Attendances and Emergency Admissions"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AECount.png"),
       plot = AECount,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# 4 hour performance ----------------------------------------------------------

AE4hr <- AEPerformance %>% 
  select(Report_Date, Metric2,Provider_Organisation_Code) %>% 
  ggplot(aes(x = Report_Date, y = Metric2, colour = Provider_Organisation_Code)) +
  geom_line(size = 0.7)+
  scale_color_manual(values = c("RA4" = NHSPurple,
                                "RH5" = NHSAquaGreen,
                                "RA7" = NHSPink,
                                "RD1" = NHSWarmYellow)) +
  scale_y_continuous(breaks = seq(0.25,1, by=0.25),
                     limits = c(0.25,1),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  geom_hline(yintercept = 0.95,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.96,
                label = "Target = 95%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("A&E 4 Hour Performance"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH</span>",
       caption = paste0("NHSE: A&E Attendances and Emergency Admissions"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AE4hr.png"),
       plot = AE4hr,
       width = 16,
       height = 6,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Number of 4 Hour Breaches ----------------------------------------------------------
AE4hrCount <- AEPerformance %>% 
  select(Report_Date, Metric3,Provider_Organisation_Code) %>% 
  ggplot(aes(x = Report_Date, y = Metric3, colour = Provider_Organisation_Code)) +
  geom_line(size = 0.7)+
  scale_color_manual(values = c("RA4" = NHSPurple,
                                "RH5" = NHSAquaGreen,
                                "RA7" = NHSPink,
                                "RD1" = NHSWarmYellow)) +
  scale_y_continuous(breaks = seq(0,6500, by=500),
                     limits = c(0,6500),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Total number of 4 Hour Breaches"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH</span>",
       caption = paste0("NHSE: A&E Attendances and Emergency Admissions"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AE4hrCount.png"),
       plot = AE4hrCount,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




# 12 Hour trolley Breaches ----------------------------------------------------------


AE12hrCount <- AEPerformance %>% 
  select(Report_Date, Metric4,Provider_Organisation_Code) %>% 
  ggplot(aes(x = Report_Date, y = Metric4, colour = Provider_Organisation_Code)) +
  geom_line(size = 0.7)+
  scale_color_manual(values = c("RA4" = NHSPurple,
                                "RH5" = NHSAquaGreen,
                                "RA7" = NHSPink,
                                "RD1" = NHSWarmYellow)) +
  scale_y_continuous(breaks = seq(0,1000, by=200),
                     limits = c(0,1000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Total number of 12 hour trolley breaches"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH</span>",
       caption = paste0("NHSE: A&E Attendances and Emergency Admissions"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AE12hrCount.png"),
       plot = AE12hrCount,
       width = 8,
       height = 6,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo") 



# % patients admitted from A&E ----------------------------------------------------------
AEAdms <- AEPerformance %>% 
  select(Report_Date, Metric5,Provider_Organisation_Code) %>% 
  ggplot(aes(x = Report_Date, y = Metric5, colour = Provider_Organisation_Code)) +
  geom_line(size = 0.7)+
  scale_color_manual(values = c("RA4" = NHSPurple,
                                "RH5" = NHSAquaGreen,
                                "RA7" = NHSPink,
                                "RD1" = NHSWarmYellow,
                                "National" = NHSBlue)) +
  geom_line(data = AEPerformanceNational,
            size = 0.7) +
  scale_y_continuous(breaks = seq(0.20,0.45, by=0.05),
                     limits = c(0.2,0.45),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Patients admitted from A&E"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH,</span><span style='color:#005EB8;'>National</span>",
       caption = paste0("NHSE: A&E Attendances and Emergency Admissions"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AEAdms.png"),
       plot = AEAdms,
       width = 8,
       height = 6,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





# Proportion of ED patients who turn up unheralded ----------------------------------------------------------
AEUnheral <- AEUnheralded %>% 
  select(Month, Metric7,PROVIDER) %>% 
  ggplot(aes(x = Month, y = Metric7, colour = PROVIDER)) +
  geom_line(size = 0.7)+
  scale_color_manual(values = c("YDH" = NHSPurple,
                                "SFT" = NHSAquaGreen,
                                "WAH" = NHSPink,
                                "RUH" = NHSWarmYellow)) +
  scale_y_continuous(breaks = seq(0.55,0.85, by=0.05),
                     limits = c(0.55,0.85),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("Proportion of ED patients who turn up unheralded"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH</span>",
       caption = paste0("ECDS"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AEUnheral.png"),
       plot = AEUnheral,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




 
# A&E high intensity Users (TRUST WIDE) ----------------------------------------------------------



AEHighUse <- AEHIU %>% 
  select(`Rolling Month`, Metric6,Provider) %>% 
  ggplot(aes(x = as.Date(`Rolling Month`), y = Metric6, colour = Provider)) +
  geom_line(size = 0.7)+
  scale_color_manual(values = c("YDH" = NHSPurple,
                                "SFT" = NHSAquaGreen,
                                "WGH" = NHSPink,
                                "RUH" = NHSWarmYellow)) +
  scale_y_continuous(breaks = seq(0,200, by=50),
                     limits = c(0,200),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle=element_markdown())+
  labs(title = paste0("A&E high intensity Users (Trust Wide)"),
       subtitle="<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#AE2573;'>UHBW, </span><span style='color:#FFB81C;'>RUH</span>",
       caption = paste0("ECDS"))

ggsave(file = paste0("Outputs/UrgentCare_A&E/AEHighUse.png"),
       plot = AEHighUse,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo") 




