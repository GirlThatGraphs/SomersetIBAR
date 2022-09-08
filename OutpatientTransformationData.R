
OPTransformation_PIFU_Som <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                       sheet = "All Activity Metrics", range = "AB451:AY451", 
                                       col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation PIFU",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

OPTransformation_PIFU_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "CC451:CZ451", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation PIFU",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

OPTransformation_PIFU_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "ED451:FA451", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation PIFU",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()




OPTransformation_AG_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "AB400:AY400", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation AG",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

OPTransformation_AG_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "CC400:CZ400", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation AG",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

OPTransformation_AG_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "ED400:FA400", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation AG",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()





OPTransformation_Virtual_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "AB136:AY136", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation Virtual",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

OPTransformation_Virtual_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "CC136:CZ136", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation Virtual",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

OPTransformation_Virtual_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
                                        sheet = "All Activity Metrics", range = "ED136:FA136", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "OPTransformation Virtual",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()



OPTransformation <- rbind(OPTransformation_PIFU_Som,
                          OPTransformation_PIFU_SFT,
                          OPTransformation_PIFU_YDH,
                          OPTransformation_AG_Som,
                          OPTransformation_AG_SFT,
                          OPTransformation_AG_YDH,
                          OPTransformation_Virtual_Som,
                          OPTransformation_Virtual_SFT,
                          OPTransformation_Virtual_YDH)
