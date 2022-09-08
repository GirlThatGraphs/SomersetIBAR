Cancer_2WW_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                                        sheet = "Somerset Commissioner total", range = "CW11:DT11", 
                                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 2WW",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Cancer_2WW_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                             sheet = "SFT to NHS Som", range = "CW11:DT11", 
                             col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 2WW",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_2WW_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                             sheet = "YDH to NHS Som", range = "CW11:DT11", 
                             col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 2WW",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()



Cancer_31Day_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                             sheet = "Somerset Commissioner total", range = "CW31:DT31", 
                             col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 31Day",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Cancer_31Day_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                             sheet = "SFT to NHS Som", range = "CW31:DT31", 
                             col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 31Day",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_31Day_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                             sheet = "YDH to NHS Som", range = "CW31:DT31", 
                             col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 31Day",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_62Day_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "Somerset Commissioner total", range = "CW19:DT19", 
                               col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 62Day",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Cancer_62Day_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "SFT to NHS Som", range = "CW19:DT19", 
                               col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 62Day",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_62Day_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "YDH to NHS Som", range = "CW19:DT19", 
                               col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 62Day",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_28Day_Som <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "Somerset Commissioner total", range = "CW60:DT60", 
                               col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 28Day",
         Provider = "Somerset System",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

Cancer_28Day_SFT <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "SFT to NHS Som", range = "CW60:DT60", 
                               col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 28Day",
         Provider = "SFT",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_28Day_YDH <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "YDH to NHS Som", range = "CW60:DT60", 
                               col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 28Day",
         Provider = "YDH",
         Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


Cancer_2ww_restoration <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                               sheet = "Restoration", range = "AO7:BL12", 
                               col_names = FALSE) %>% 
  mutate(`Provider` = c("Somerset (Commissioner)",
                        "SFT",
                        "YDH",
                        "RUH",
                        "UBHW",
                        "Others")) |> 
  pivot_longer(!Provider, names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer 2ww restoration",
         Date = rep(seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month"),
                    6))%>% 
  drop_na() |> 
  select(`Date`,
         `Count`,
         `Metric`,
         `Provider`)

Cancer_Treatment_restoration <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Cancer/2022-23/2022-23 Cancer Analysis.xlsx", 
                                     sheet = "Restoration", range = "AO16:BL21", 
                                     col_names = FALSE) %>% 
  mutate(`Provider` = c("Somerset (Commissioner)",
                        "SFT",
                        "YDH",
                        "RUH",
                        "UBHW",
                        "Others")) |> 
  pivot_longer(!Provider, names_to = "Date", values_to = "Count") %>% 
  mutate(Metric = "Cancer Treatment restoration",
         Date = rep(seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month"),
                    6))%>% 
  drop_na() |> 
  select(`Date`,
         `Count`,
         `Metric`,
         `Provider`)


Cancer <- rbind (Cancer_2WW_Som,
                 Cancer_2WW_SFT,
                 Cancer_2WW_YDH,
                 Cancer_31Day_Som,
                 Cancer_31Day_SFT,
                 Cancer_31Day_YDH,
                 Cancer_62Day_Som,
                 Cancer_62Day_SFT,
                 Cancer_62Day_YDH,
                 Cancer_28Day_Som,
                 Cancer_28Day_SFT,
                 Cancer_28Day_YDH,
                 Cancer_2ww_restoration,
                 Cancer_Treatment_restoration)
