Restoration_Elective_Som <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                              sheet = "Somerset ICB", range = "BZ47:CW47", 
                              col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Elective Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Restoration_Elective_Som_Plan <- data.frame(Date = seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month"),
                                            Count = c(rep(NA,12),1.0048,1.0019,1.0183,1.0083,1.0075,1.0205,1.0260,1.0162,1.0202,1.0090,1.0072,1.0052),
                                            Metric = rep("Elective Restoration",24),
                                            Provider = rep("Plan", 24)) %>% 
  head(length(Restoration_Elective_Som$Date))

Restoration_Elective_SFT <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "SFT to ICB", range = "BZ47:CW47", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Elective Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_Elective_YDH <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "YDH to ICB", range = "BZ47:CW47", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Elective Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_DayCase_Som <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "Somerset ICB", range = "BZ12:CW12", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "DayCase Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_DayCase_SFT <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "SFT to ICB", range = "BZ12:CW12", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "DayCase Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_DayCase_YDH <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "YDH to ICB", range = "BZ12:CW12", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "DayCase Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_Inpatient_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "Somerset ICB", range = "BZ15:CW15", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Inpatient Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_Inpatient_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "SFT to ICB", range = "BZ15:CW15", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Inpatient Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_Inpatient_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "YDH to ICB", range = "BZ15:CW15", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Inpatient Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()



Restoration_1OP_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "Somerset ICB", range = "BZ34:CW34", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "1OP Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_1OP_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "SFT to ICB", range = "BZ34:CW34", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "1OP Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_1OP_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "YDH to ICB", range = "BZ34:CW34", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "1OP Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_FUPOP_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "Somerset ICB", range = "BZ37:CW37", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "FUPOP Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_FUPOP_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "SFT to ICB", range = "BZ37:CW37", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "FUPOP Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Restoration_FUPOP_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/SUS Activty Timeseries from 1920.xlsx", 
                                       sheet = "YDH to ICB", range = "BZ37:CW37", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "FUPOP Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()



RestorationActivity <- rbind(Restoration_Elective_Som,
      Restoration_Elective_Som_Plan,
      Restoration_Elective_SFT,
      Restoration_Elective_YDH,
      Restoration_DayCase_Som,
      Restoration_DayCase_SFT,
      Restoration_DayCase_YDH,
      Restoration_Inpatient_Som,
      Restoration_Inpatient_SFT,
      Restoration_Inpatient_YDH,
      Restoration_1OP_Som,
      Restoration_1OP_SFT,
      Restoration_1OP_YDH,
      Restoration_FUPOP_Som,
      Restoration_FUPOP_SFT,
      Restoration_FUPOP_YDH)



Restoration_ClockStarts_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                       sheet = "Somerset CCG Dashboard", range = "CG35:DD35", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStarts Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na() %>% 
  filter(Count>0)

Restoration_ClockStarts_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                          sheet = "Taunton", range = "CG35:DD35", 
                                          col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStarts Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()%>% 
  filter(Count>0)

Restoration_ClockStarts_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                          sheet = "Yeovil", range = "CF35:DC35", 
                                          col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStarts Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()%>% 
  filter(Count>0)


Restoration_ClockStopsNAd_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                          sheet = "Somerset CCG Dashboard", range = "CG38:DD38", 
                                          col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStopsNAd Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na() %>% 
  filter(Count>0)

Restoration_ClockStopsNAd_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                          sheet = "Taunton (Trust)", range = "CF38:DC38", 
                                          col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStopsNAd Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()%>% 
  filter(Count>0)

Restoration_ClockStopsNAd_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                          sheet = "Yeovil (Trust)", range = "CF38:DC38", 
                                          col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStopsNAd Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()%>% 
  filter(Count>0)


Restoration_ClockStopsAd_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                            sheet = "Somerset CCG Dashboard", range = "CG37:DD37", 
                                            col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStopsAd Restoration",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na() %>% 
  filter(Count>0)

Restoration_ClockStopsAd_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                            sheet = "Taunton (Trust)", range = "CF37:DC37", 
                                            col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStopsAd Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()%>% 
  filter(Count>0)

Restoration_ClockStopsAd_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Constitutional Standards Dashboard.xlsx", 
                                            sheet = "Yeovil (Trust)", range = "CF37:DC37", 
                                            col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "ClockStopsAd Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()%>% 
  filter(Count>0)


RestorationRTT <- rbind(Restoration_ClockStarts_Som,
                        Restoration_ClockStarts_SFT,
                        Restoration_ClockStarts_YDH,
                        Restoration_ClockStopsNAd_Som,
                        Restoration_ClockStopsNAd_SFT,
                        Restoration_ClockStopsNAd_YDH,
                        Restoration_ClockStopsAd_Som,
                        Restoration_ClockStopsAd_SFT,
                        Restoration_ClockStopsAd_YDH)







