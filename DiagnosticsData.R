

Dianostics_WL_SFT <- 
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "Diagnostics HV", range = "CE11:DB11", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics WL",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Dianostics_WL_YDH <- 
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                sheet = "Diagnostics HV", range = "EF11:FC11", 
                                col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics WL",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Dianostics_More6_SFT <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                sheet = "Diagnostics HV", range = "CE15:DB15", 
                                col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics More6",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Dianostics_More6_YDH <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                sheet = "Diagnostics HV", range = "EF15:FC15", 
                                col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics More6",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Dianostics_Less6_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                   sheet = "Diagnostics HV", range = "CE19:DB19", 
                                   col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics Less6",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Dianostics_Less6_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                   sheet = "Diagnostics HV", range = "EF19:FC19", 
                                   col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics Less6",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Dianostics_Restoration_SOM <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                         sheet = "Diagnostics HV", range = "AD29:BA29", 
                                         col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics Restoration",
         Provider = "SOM",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Dianostics_Restoration_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                sheet = "Diagnostics HV", range = "CE29:DB29", 
                                col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics Restoration",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Dianostics_Restoration_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                sheet = "Diagnostics HV", range = "Ef29:FC29", 
                                col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Diagnostics Restoration",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Diagnostics <- rbind(Dianostics_WL_SFT,
                     Dianostics_WL_YDH,
                     Dianostics_More6_SFT,
                     Dianostics_More6_YDH,
                     Dianostics_Less6_SFT,
                     Dianostics_Less6_YDH,
                     Dianostics_Restoration_SOM,
                     Dianostics_Restoration_SFT,
                     Dianostics_Restoration_YDH)

