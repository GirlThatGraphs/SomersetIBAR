# RunSetup ----------------------------------------------------------------
# Run Setup script
source("SetupNew.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300



# Data ----------------------------------------------------------------

PC_Consultations <- read_excel("S:/Shared Area/Somerset Performance/Performance Report/Primary Care Datasets/NHSD PC Consultations/NHSD PC Consultations_Consolidation_2017-2021.xlsx", 
                               sheet = "Data", col_types = c("text", 
                                                             "text", "text", "text", "text", "text", 
                                                             "date", "text", "text", "text", "text", 
                                                             "numeric")) %>% 
  select(Appointment_Date, APPT_MODE, COUNT_OF_APPOINTMENTS) %>% 
  mutate(Appointment_Date = as.Date(floor_date(Appointment_Date, "month")),
         APPT_MODE = recode(APPT_MODE, "Face-to-Face" = "Face to Face",
                            "Home Visit" = "Face to Face",
                            "Telephone" = "Non Face to Face",
                            "Unknown" = "Non Face to Face",
                            "Video Conference/Online" = "Non Face to Face")) %>% 
  filter(Appointment_Date >= "2021-04-01") %>% 
  group_by(Appointment_Date, APPT_MODE) %>% 
  summarise(COUNT_OF_APPOINTMENTS = sum(COUNT_OF_APPOINTMENTS)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = APPT_MODE, values_from = COUNT_OF_APPOINTMENTS) %>% 
  mutate(Metric1 = `Face to Face` + `Non Face to Face`,
         Metric2 = `Face to Face`,
         Metric3 = `Non Face to Face`,
         Metric4 = `Face to Face` / (`Face to Face` + `Non Face to Face`),
         Metric5 = `Non Face to Face` / (`Face to Face` + `Non Face to Face`))

Antimicrobial <- read_excel("Data/Antimicrobial.xlsx")

GPSurveya <- read_excel("O:/BSS/BI/Intelligence Services/BSSW/Somerset/Reports//Primary_Care_Report/Data/GPSurvey.xlsx")

GPSurvey <- GPSurveya %>% 
  filter(Question == "overall_experience",
         Year == max(Year)) %>% 
  spread(Response,Count, fill=0) %>%
  mutate(Score = (`Very good`+`Fairly good`)/(`Very good`+`Fairly good`+`Fairly poor`+`Very poor`+`Neither good nor poor`),
         `Practice name` = str_to_title(`Practice name`)) %>%
  select(`Practice name`, `Score`)



ConsultationsPop <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Benchmarking_1920 to 2223.xlsx", 
                               sheet = "GP Consultations", range = "AA16:AX18", 
                               col_names = FALSE) %>% 
  mutate(Provider = c("England","South West","Somerset")) %>% 
  pivot_longer(!Provider, names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Consultations Per 100,000 Population",
         Date =rep(seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month"),3)) %>% 
  drop_na()


GPFTE <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Benchmarking_1920 to 2223.xlsx", 
                    sheet = "GP workforce data", range = "AC7:AX9", 
                    col_names = FALSE) %>% 
  mutate(Provider = c("England","South West","Somerset")) %>% 
  pivot_longer(!Provider, names_to = "Date", values_to = "Count") %>% 
  mutate(Date =rep(seq(as.Date("2021-06-01"), as.Date("2023-03-01"), by="month"),3)) %>% 
  drop_na()



DCPFTE <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Benchmarking_1920 to 2223.xlsx", 
                     sheet = "GP workforce data", range = "AC13:AX15", 
                     col_names = FALSE) %>% 
  mutate(Provider = c("England","South West","Somerset")) %>% 
  pivot_longer(!Provider, names_to = "Date", values_to = "Count") %>% 
  mutate(Date =rep(seq(as.Date("2021-06-01"), as.Date("2023-03-01"), by="month"),3)) %>% 
  drop_na()




#GP Consultations - All ----------------------------------------------------------------
GPAll <- PC_Consultations %>% 
  select(Appointment_Date, Metric1) %>% 
  ggplot(aes(x = Appointment_Date, y = Metric1)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(200000,350000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(PC_Consultations$Appointment_Date), max(PC_Consultations$Appointment_Date), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("General Practice Consultations - All"),
       subtitle = paste0("Somerset"),
       caption = paste0("https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/GeneralPractice/GPAll.png"),
       plot = GPAll,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

#GPF2F ----------------------------------------------------------------
GPF2F <- PC_Consultations %>% 
  select(Appointment_Date, Metric2) %>% 
  ggplot(aes(x = Appointment_Date, y = Metric2)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(100000,225000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(PC_Consultations$Appointment_Date), max(PC_Consultations$Appointment_Date), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("General Practice Consultations - Face-to-Face"),
       subtitle = paste0("Somerset"),
       caption = paste0("https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/GeneralPractice/GPF2F.png"),
       plot = GPF2F,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

#GPNF2F ----------------------------------------------------------------
GPNF2F <- PC_Consultations %>% 
  select(Appointment_Date, Metric3) %>% 
  ggplot(aes(x = Appointment_Date, y = Metric3)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(90000,140000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(PC_Consultations$Appointment_Date), max(PC_Consultations$Appointment_Date), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("General Practice Consultations - Non Face-to-Face"),
       subtitle = paste0("Somerset"),
       caption = paste0("https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/GeneralPractice/GPNF2F.png"),
       plot = GPNF2F,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

#GPF2FPerc ----------------------------------------------------------------

GPF2FPerc <- PC_Consultations %>% 
  select(Appointment_Date, Metric4) %>% 
  ggplot(aes(x = Appointment_Date, y = Metric4)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(breaks = seq(0.4,0.7, by=0.1),
                     limits = c(0.4,0.7),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(PC_Consultations$Appointment_Date), max(PC_Consultations$Appointment_Date), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("General Practice Consultations - Face-to-Face (%)"),
       subtitle = paste0("Somerset"),
       caption = paste0("https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/GeneralPractice/GPF2FPerc.png"),
       plot = GPF2FPerc,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


#GPNF2FPerc ----------------------------------------------------------------


GPNF2FPerc <- PC_Consultations %>% 
  select(Appointment_Date, Metric5) %>% 
  ggplot(aes(x = Appointment_Date, y = Metric5)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(breaks = seq(0.3,0.6, by=0.1),
                     limits = c(0.3,0.6),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(PC_Consultations$Appointment_Date), max(PC_Consultations$Appointment_Date), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("General Practice Consultations - Non Face-to-Face (%)"),
       subtitle = paste0("Somerset"),
       caption = paste0("https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/GeneralPractice/GPNF2FPerc.png"),
       plot = GPNF2FPerc,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


#Antimicrobial ----------------------------------------------------------------



Antimicrobial1 <- Antimicrobial %>% 
  select(Date, Metric6) %>% 
  ggplot(aes(x = Date)) +
  geom_bar(aes(weight = Metric6),
           fill = NHSBlue)+
  scale_y_continuous(limits = c(0,0.06),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  labs(title = paste0("Co-amoxiclav Cephalosporins & Quinolones % of all \nAntibiotics"),
       subtitle = paste0("Somerset"),
       caption = paste0("Antimicrobial Stewardship"),
       x = "12 months to and including") +
  BarPlotTheme() +
  theme(axis.title.x = element_text(family = "Inter",
                                    size = 0.70,
                                    color = NHSBlack))

ggsave(file = paste0("Outputs/GeneralPractice/Antimicrobial1.png"),
       plot = Antimicrobial1,
       width = 11,
       height = 7.4,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




Antimicrobial2 <- Antimicrobial %>% 
  select(Date, Metric7) %>% 
  ggplot(aes(x = Date)) +
  geom_bar(aes(weight = Metric7),
           fill = NHSBlue)+
  scale_y_continuous(limits = c(0,1),
                     expand = c(0,0))+
  labs(title = paste0("Antibacterial items per STAR PU"),
       subtitle = paste0("Somerset"),
       caption = paste0("Antimicrobial Stewardship"),
       x = "12 months to and including") +
  BarPlotTheme() +
  theme(axis.title.x = element_text(family = "Inter",
                                    size = 0.70,
                                    color = NHSBlack))

ggsave(file = paste0("Outputs/GeneralPractice/Antimicrobial2.png"),
       plot = Antimicrobial2,
       width = 11,
       height = 7.4,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




#GP Consultations - Population ----------------------------------------------------------------


ConsultationsPopPlot <- ConsultationsPop %>%
  ggplot(aes(x = Date, y = Count, colour = Provider)) +
  geom_line(size = 0.7)+
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSBlue,
                                 "South West" = NHSPurple,
                                 "England" = NHSWarmYellow)) +
  scale_y_continuous(limits = c(3000,6000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(ConsultationsPop$Date), max(ConsultationsPop$Date), by = "3 months"),
               date_labels = "%b %Y")+
    LinePlotTheme()+
    theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Consultations Per 100,000 Population"),
       subtitle = "<span style='color:#005EB8;'>Somerset System,</span><span style='color:#330072;'>South West, </span><span style='color:#FFB81C;'>England </span>",
       caption = paste0("Benchmarking_1920 to 2223"))

ggsave(file = paste0("Outputs/GeneralPractice/ConsultationsPopPlot.png"),
       plot = ConsultationsPopPlot,
       width = 16,
       height = 9,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





#GP FTE - Population ----------------------------------------------------------------




GPFTEPlot <- GPFTE %>%
  ggplot(aes(x = Date, y = Count, colour = Provider)) +
  geom_line(size = 0.7)+
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSBlue,
                                 "South West" = NHSPurple,
                                 "England" = NHSWarmYellow)) +
  scale_y_continuous(limits = c(0,6),
                     expand = c(0,0),
                     labels = number_format(accuracy = 1))+
  scale_x_date(breaks= seq(min(GPFTE$Date), max(GPFTE$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("GPs: FTE per 10,000 population"),
       subtitle = "<span style='color:#005EB8;'>Somerset System,</span><span style='color:#330072;'>South West, </span><span style='color:#FFB81C;'>England </span>",
       caption = paste0("Benchmarking_1920 to 2223"))

ggsave(file = paste0("Outputs/GeneralPractice/GPFTEPlot.png"),
       plot = GPFTEPlot,
       width = 16,
       height = 6,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





#DCP FTE - Population ----------------------------------------------------------------


DCPFTEPlot <- DCPFTE %>%
  ggplot(aes(x = Date, y = Count, colour = Provider)) +
  geom_line(size = 0.7)+
  scale_colour_manual("legend",
                      values = c("Somerset" = NHSBlue,
                                 "South West" = NHSPurple,
                                 "England" = NHSWarmYellow)) +
  scale_y_continuous(limits = c(0,5),
                     expand = c(0,0),
                     labels = number_format(accuracy = 1))+
  scale_x_date(breaks= seq(min(DCPFTE$Date), max(DCPFTE$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme()+
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("DCPs: FTE per 10,000 population"),
       subtitle = "<span style='color:#005EB8;'>Somerset System,</span><span style='color:#330072;'>South West, </span><span style='color:#FFB81C;'>England </span>",
       caption = paste0("Benchmarking_1920 to 2223"))

ggsave(file = paste0("Outputs/GeneralPractice/DCPFTEPlot.png"),
       plot = DCPFTEPlot,
       width = 16,
       height = 6,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





#GPSurvey ----------------------------------------------------------------

NationalSurvey <- data.frame(Question = rep(c("getting_through",
                                              "appt_experience",
                                              "overall_experience"), each = 5),
                             Year = rep("2022", 15),
                             `Practice code` = rep("National", 15),
                             `Practice name` = rep("National", 15),
                             Response = rep(c("Very Good", "Good", "Medium", "Poor", "Very Poor"), 3),
                             Count = c(0.143361920763489,	0.383384510580378,	0,	0.250620642996049,	0.222632925660054, 
                                       0.234166230542316,	0.327721690700621,	0.178896506071019,	0.134764467957414,	0.1244511047286, 
                                       0.376747943004333,	0.347053508225918,	0.139975212886057,	0.0805959541015677,	0.0556273817820967),
                             check.names = FALSE)


Survey_Chart_Data <- GPSurveya %>% 
  group_by(Year, Question, Response) %>% 
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "CCG")) %>% 
  ungroup() %>% 
  filter(Year == max(Year),
         `Practice code` %in% c("CCG")) %>% 
  mutate(Response = recode(Response, "Very poor" = "Very Poor",
                           "Not very easy" = "Very Poor",
                           "Fairly poor" = "Poor",
                           "Not at all easy" = "Poor",
                           "Neither good nor poor" = "Medium",
                           "Fairly good" = "Good",
                           "Fairly easy" = "Good",
                           "Very good" = "Very Good",
                           "Very easy" = "Very Good")) %>% 
  rbind(NationalSurvey)

Survey_Chart_Data$Response<-factor(Survey_Chart_Data$Response,
                                   levels=c('Very Poor',
                                            'Poor',
                                            'Medium',
                                            'Good',
                                            'Very Good'))

Survey_Chart_Data$Question<-factor(Survey_Chart_Data$Question,
                                   levels=c('getting_through',
                                            'appt_experience',
                                            'overall_experience')) 

Survey_Chart_Data$`Practice code`<-factor(Survey_Chart_Data$`Practice code`,
                                          levels=c('National',
                                                   'CCG'))

Survey_Chart_Data <- Survey_Chart_Data %>% 
  filter(Question == "overall_experience") |> 
  mutate(`Practice code` = case_when(`Practice code` == "National" ~ "National",
                                     `Practice code` == "CCG" ~ "ICS"),
         Question= recode(Question, "getting_through"="Generally, how easy is it to get through to someone at your GP practice on the phone?",
                          "appt_experience" = "Overall, how would you describe your experience of making an appointment?",
                          "overall_experience" = "Overall, how would you describe your experience of your GP practice?")) 



SurveyPlot <- Survey_Chart_Data %>% 
  ggplot()+
  geom_bar(aes(fill=Response, y=`Practice code`, x = Count),
           position="fill", stat="identity") +
  facet_wrap(~Question, ncol=1) +
  scale_x_continuous(labels = percent_format())+
  scale_fill_manual("Response",
                    values = c("Very Poor" = "#AE2573",
                               "Poor" = alpha("#AE2573", 0.7),
                               "Medium" = alpha("#005EB8", 0.1),
                               "Good" = alpha("#005EB8", 0.7),
                               "Very Good" = "#005EB8")) +
  BarFlipPlotTheme()+
  theme(legend.position = "bottom",
        legend.justification = 'right') +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = ("2022 GP Patient Survey Results"),
       caption = paste0("Data sourced via The GP Patient Survey: CCG report (2022 publication)"),
       x = (""),
       y = ("")) 


ggsave(file = paste0("Outputs/GeneralPractice/SurveyPlot.png"),
       plot = SurveyPlot,
       width = 16,
       height = 9,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

