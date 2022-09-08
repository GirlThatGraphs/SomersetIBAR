# Run Setup script
source("SetupNew.R")


CDPdata <- 
  read_excel("S:/Shared Area/Somerset Performance/Performance Framework Monitoring/Mental health Benchmarking/Mental health core data pack/2022-2023/Aug_CDPdata.xlsx") %>% 
  filter(`STP Code` %in% c("QSL", "E54000038"),
         `Org_Type` =="STP",
         `Reporting Period` >= "2021-04-01",
         `Data Source` %in% c("Dementia diagnosis rate", 
                              "Perinatal Access", 
                              "72hr follow up",
                              "CYP 1+ Actual", 
                              "IAPT 90d")) %>% 
  select(`Reporting Period`, 
         `Dementia diagnosis rate`, 
         `Perinatal Access`,
         `72hr follow up`,
         `CYP 1+ Actual`, 
         `IAPT 90d`) %>% 
  mutate(`Dementia diagnosis rate` = as.numeric(replace(`Dementia diagnosis rate`, `Dementia diagnosis rate` =="NULL" , NA)),
         `Perinatal Access` = as.numeric(replace(`Perinatal Access`, `Perinatal Access` =="NULL" , NA)),
         `72hr follow up` = as.numeric(replace(`72hr follow up`, `72hr follow up` =="NULL" , NA)),
         `CYP 1+ Actual` = as.numeric(replace(`CYP 1+ Actual`, `CYP 1+ Actual` =="NULL" , NA)),
         `IAPT 90d` = as.numeric(replace(`IAPT 90d`, `IAPT 90d` =="NULL" , NA))) %>% 
  group_by(`Reporting Period`) %>% 
  summarise_all(sum, na.rm=TRUE) %>% 
  ungroup() |> 
  mutate(Month = as.Date(floor_date(`Reporting Period`, "month")))


CYP_ED <- 
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Framework Monitoring/2022-23/CYP/CEDs/CYPED Pathways Somerset Partnership v3.xlsx", 
                     range = "BA3:BX14", col_names = FALSE)
  names(CYP_ED) <- seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")


CYP_ED2 <- 
  CYP_ED %>% 
  mutate(ColName = 1:12) %>% 
filter(!ColName %in% 7:9) %>% 
  pivot_longer(!ColName, names_to = "Date", values_to = "Count") %>% 
  pivot_wider(names_from = ColName, values_from = Count) %>% 
  drop_na() %>% 
  select(Date, `6`, `12`) %>% 
  rename(EDRoutine = `6`,
         EDUrgent = `12`)



IAPTAccess <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Board Assurance/Graphs/MH Graphs - 202206v1.xlsx", 
                         sheet = "IAPT Data", range = "AD6:BA7", 
                         col_names = FALSE) %>% 
  mutate(Metric = c("Target", "IAPTAccess")) %>% 
  pivot_longer(!Metric, names_to = "Date", values_to = "Count") %>% 
  pivot_wider(names_from = Metric, values_from = Count) %>% 
  mutate(Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()

IAPT6Week <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Board Assurance/Graphs/MH Graphs - 202206v1.xlsx", 
                         sheet = "IAPT Data", range = "AD23:BA23", 
                         col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month")) %>% 
  drop_na()


SMI <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/Benchmarking_1920 to 2223.xlsx", 
                        sheet = "Mental Health ", range = "O61:V61", 
                        col_names = FALSE) %>% 
  pivot_longer(everything(), names_to = "Date", values_to = "Count") %>% 
  mutate(`Date` = c("Q1 2021/22",
                    "Q2 2021/22",
                    "Q3 2021/22",
                    "Q4 2021/22",
                    "Q1 2022/23",
                    "Q2 2022/23",
                    "Q3 2022/23",
                    "Q4 2022/23")) %>% 
  mutate( Date = factor(Date, Date)) %>% 
  drop_na()
