# Run Setup script --------------------------------------------------
source("SetupNew.R")


# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 15
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300


# Import Data --------------------------------------------------
NHS111 <-  tbl(DataSQLConnectionASA, in_schema("SOM", "IUC_ADC_VIEW")) %>% 
  select(Date,
         A01_Calls_received,
         A03_Answered_calls,
         B01_Calls_answered_in_60_seconds,
         B06_Total_time_to_call_answer,
         `D13_Callers_offered_a_call_back_by_a_clinician_or_Clinical_Advisor_in_20_minutes_(immediately)`,
         `D14_Callers_offered_a_call_back_in_20_minutes_(immediately)_who_received_a_call_back_in_20_minutes`,
         D15_Callers_offered_a_call_back_by_a_clinician_or_Clinical_Advisor_in_a_timeframe_between_20_minutes_and_1_hour_inclusive,
         D16_Callers_offered_a_call_back_in_a_timeframe_between_20_minutes_1_hour_inclusive_who_received_a_call_back_in_1_hour,
         D17_Callers_offered_a_call_back_by_a_clinician_or_Clinical_Advisor_in_a_timeframe_over_1_hour,
         D18_Callers_offered_a_call_back_in_a_timeframe_over_1_hour_who_received_a_call_back_in_the_specified_timeframe,
         D01_Calls_assessed_by_a_clinician_or_Clinical_Advisor,
         C01_Calls_where_person_triaged,
         G09_Calls_where_caller_given_a_booked_time_slot_with_an_ED,
         B02_Calls_abandoned,
         G22_patients_requiring_a_F2F_consult_in_IUC_TC,
         G23_patients_receiving_a_F2F_consult_in_IUC_TC_in_the_timeframe_agreed,
         G20_patients_requiring_a_F2F_consult_in_home_residence,
         G21_patients_receiving_a_F2F_consult_in_home_residence_in_the_timeframe_agreed) %>% 
  collect() %>% 
  mutate(Month = as.Date(floor_date(Date, "month"))) %>% 
  filter(as.Date(ceiling_date(as.Date(Date), "month") -1)<=max(Date)) %>% 
  select(-Date) %>% 
  group_by(Month) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(Metric1 = (B01_Calls_answered_in_60_seconds/A03_Answered_calls),
         Metric2 = (B06_Total_time_to_call_answer/A03_Answered_calls),
         Metric3 = (`D14_Callers_offered_a_call_back_in_20_minutes_(immediately)_who_received_a_call_back_in_20_minutes`
                    /`D13_Callers_offered_a_call_back_by_a_clinician_or_Clinical_Advisor_in_20_minutes_(immediately)`),
         Metric4 = ((D16_Callers_offered_a_call_back_in_a_timeframe_between_20_minutes_1_hour_inclusive_who_received_a_call_back_in_1_hour
                     +D18_Callers_offered_a_call_back_in_a_timeframe_over_1_hour_who_received_a_call_back_in_the_specified_timeframe)
                    /(D15_Callers_offered_a_call_back_by_a_clinician_or_Clinical_Advisor_in_a_timeframe_between_20_minutes_and_1_hour_inclusive
                      +D17_Callers_offered_a_call_back_by_a_clinician_or_Clinical_Advisor_in_a_timeframe_over_1_hour)),
         Metric5 = (D01_Calls_assessed_by_a_clinician_or_Clinical_Advisor/C01_Calls_where_person_triaged),
         Metric6 = B02_Calls_abandoned/A01_Calls_received,
         Metric7 = A01_Calls_received,
         Metric8 = G09_Calls_where_caller_given_a_booked_time_slot_with_an_ED,
         Metric9 = G21_patients_receiving_a_F2F_consult_in_home_residence_in_the_timeframe_agreed/G20_patients_requiring_a_F2F_consult_in_home_residence,
         Metric10 = G23_patients_receiving_a_F2F_consult_in_IUC_TC_in_the_timeframe_agreed/G22_patients_requiring_a_F2F_consult_in_IUC_TC)
         
  



# Calls answered Within 60 seconds --------------------------------------------------
NHS11160Seconds <- NHS111 %>% 
  select(Month, Metric1) %>% 
  ggplot(aes(x = as.Date(Month), y = Metric1)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Percentage of calls answered within 60 seconds"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
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
  LinePlotTheme()
  
  ggsave(file = paste0("Outputs/NHS111/NHS11160Seconds.png"),
         plot = NHS11160Seconds,
         width = PNGWidth,
         height = PNGHeight,
         units = PNGUnits,
         dpi = PNGDPI,
         type = "cairo")


# Average speed to answer calls (seconds) --------------------------------------------------
NHS111CallTime <- NHS111 %>% 
  select(Month, Metric2) %>% 
  ggplot(aes(x = Month, y = Metric2)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(breaks = seq(0,840, by=60),
                     limits = c(0,840),
                     expand = c(0,0))+
    scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
                 date_labels = "%b %Y")+
  labs(title = paste0("Average time to answer calls (seconds)"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
  geom_hline(yintercept = 20,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 30,
                label = "Target = 20 seconds"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111CallTime.png"),
       plot = NHS111CallTime,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Proportion of callers who needed to speak to a clinician or Clinical Advisor within 20 minutes (immediately), who were warm transferred or received a call back within 20 minutes --------------------------------------------------
NHS111Clinical20 <- NHS111 %>% 
  select(Month, Metric3) %>% 
  ggplot(aes(x = Month, y = Metric3)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Percentage of callers who needed to speak to a clinician or Clinical Advisor within \n20 minutes (immediately),who were warm transferred or received a call back within \n20 minutes"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
  geom_hline(yintercept = 0.9,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.91,
                label = "Target = 90%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111Clinical20.png"),
       plot = NHS111Clinical20,
       width = PNGWidth,
       height = 9,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# # Proportion of callers who needed to speak to a clinician or Clinical Advisor Over a timeframe over 20 minutes, who were warm transferred or received a call back within the specified timeframe
# NHS111Clinical20plus <- NHS111 %>%
#   select(Month, Metric4) %>%
#   ggplot(aes(x = Month, y = Metric4)) +
#   geom_line(size = 0.7,
#             colour = NHSBlue)+
#   scale_y_continuous(limits = c(0,1),
#                      labels = percent_format(accuracy=1),
#                      expand = c(0,0))+
#   labs(title = paste0("Percentage of callers who needed to speak to a clinician or Clinical Advisor Over a timeframe over 20 minutes,\nwho were warm transferred or received a call back within the specified timeframe"),
#        subtitle = paste0("Somerset"),
#        caption = paste0("Devon Doctors")) +
#   LinePlotTheme()
# 
# ggsave(file = paste0("Outputs/NHS111/NHS111Clinical20plus.png"),
#        plot = NHS111Clinical20plus,
#        width = PNGWidth,
#        height = PNGHeight,
#        dpi = PNGDPI,
#        type = "cairo")


# Calls assessed by a clinician or clinical advisor --------------------------------------------------
NHS111ClinicalAssessed <- NHS111 %>% 
  select(Month, Metric5) %>% 
  ggplot(aes(x = Month, y = Metric5)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Percentage of callers assessed by a clinician or clinical advisor"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.51,
                label = "Target = 50%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111ClinicalAssessed.png"),
       plot = NHS111ClinicalAssessed,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Call Abandoment --------------------------------------------------
NHS111Abandoned <- NHS111 %>% 
  select(Month, Metric6) %>% 
  ggplot(aes(x = Month, y = Metric6)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,0.35),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Percentage of calls abandoned"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
  geom_hline(yintercept = 0.03,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.04,
                label = "Target = 3%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111Abandoned.png"),
       plot = NHS111Abandoned,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Number of calls received --------------------------------------------------
NHS111CallsRecieved <- NHS111 %>% 
  select(Month, Metric7) %>% 
  ggplot(aes(x = Month, y = Metric7)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(breaks = seq(10000,25000, by=5000),
                     limits = c(10000,25000),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Number of calls received"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111CallsRecieved.png"),
       plot = NHS111CallsRecieved,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Number of calls where caller was given booked timeslot with ED  --------------------------------------------------
NHS111CallsED <- NHS111 %>% 
  select(Month, Metric8) %>% 
  ggplot(aes(x = Month, y = Metric8)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,500),
                     expand = c(0,0),
                     labels = comma)+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Number of calls where caller was given booked timeslot with ED"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111CallsED.png"),
       plot = NHS111CallsED,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Proportion of patients receiving a face-to-face consultation within their home residence within the specified timeframe --------------------------------------------------
NHS111F2FHome <- NHS111 %>% 
  select(Month, Metric9) %>% 
  ggplot(aes(x = Month, y = Metric9)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Percentage of patients receiving a face-to-face consultation within their home residence \nwithin the specified timeframe"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
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
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111F2FHome.png"),
       plot = NHS111F2FHome,
       width = 16,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Proportion of patients receiving a face-to-face consultation in an IUC Treatment Centre within the specified timeframe --------------------------------------------------
NHS111F2FTC <- NHS111 %>% 
  select(Month, Metric10) %>% 
  ggplot(aes(x = Month, y = Metric10)) +
  geom_line(size = 0.7,
            colour = NHSBlue)+
  scale_y_continuous(limits = c(0,1),
                     labels = percent_format(accuracy=1),
                     expand = c(0,0))+
  scale_x_date(breaks= seq(min(NHS111$Month), max(NHS111$Month), by = "3 months"),
               date_labels = "%b %Y")+
  labs(title = paste0("Percentage of patients receiving a face-to-face consultation in an IUC Treatment Centre \nwithin the specified timeframe"),
       subtitle = paste0("Somerset"),
       caption = paste0("Devon Doctors")) +
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
  LinePlotTheme()

ggsave(file = paste0("Outputs/NHS111/NHS111F2FTC.png"),
       plot = NHS111F2FTC,
       width = 16,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


