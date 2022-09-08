# NonElective -------------------------------------------------------------
# Dataframe
NonElective0SQLData <- 
  tbl(DataSQLConnectionDM,
      in_schema("SEM",
                "APC_SEMPBR_Current_Spell_001")) |>
  APCDateCommProvSpecFilter() |> 
  select(`StartDate_HospitalProviderSpell`,
         `DischargeDate_FromHospitalProviderSpell`,
         `StartDate_ConsultantEpisode`,
         `EndDate_ConsultantEpisode`,
         `OrganisationCode_CodeOfCommissioner`,
         `OrganisationCode_CodeOfProvider`,
         `AdmissionMethod_HospitalProviderSpell`,
         `PatientClassification`,
         `IntendedManagement`) |> 
  collect() |> 
  filter(substr(`AdmissionMethod_HospitalProviderSpell`,
                1,
                1) %in% c("2",
                          "3",
                          "8") &
           difftime(as.Date(`DischargeDate_FromHospitalProviderSpell`),
                    as.Date(`StartDate_HospitalProviderSpell`),
                    units = "days") == "0") |>
  select(`Date` = `DischargeDate_FromHospitalProviderSpell`,
         `CommissionerCode` = `OrganisationCode_CodeOfCommissioner`,
         `ProviderCode` = `OrganisationCode_CodeOfProvider`) |>
  MonthCommProvMutate() |>
  mutate(`PlanningFlagCode` = "11a",
         `PlanningFlagDescription` = "Inpatient Non-Elective 0LoS") |> 
  SummariseActivity()

# Dataframe
NonElective1SQLData <- 
  tbl(DataSQLConnectionDM,
      in_schema("SEM",
                "APC_SEMPBR_Current_Spell_001")) |>
  APCDateCommProvSpecFilter() |> 
  select(`StartDate_HospitalProviderSpell`,
         `DischargeDate_FromHospitalProviderSpell`,
         `StartDate_ConsultantEpisode`,
         `EndDate_ConsultantEpisode`,
         `OrganisationCode_CodeOfCommissioner`,
         `OrganisationCode_CodeOfProvider`,
         `AdmissionMethod_HospitalProviderSpell`,
         `PatientClassification`,
         `IntendedManagement`) |> 
  collect() |> 
  filter(substr(`AdmissionMethod_HospitalProviderSpell`,
                1,
                1) %in% c("2",
                          "3",
                          "8") &
           difftime(as.Date(`DischargeDate_FromHospitalProviderSpell`),
                    as.Date(`StartDate_HospitalProviderSpell`),
                    units = "days") > "0") |>
  select(`Date` = `DischargeDate_FromHospitalProviderSpell`,
         `CommissionerCode` = `OrganisationCode_CodeOfCommissioner`,
         `ProviderCode` = `OrganisationCode_CodeOfProvider`) |>
  MonthCommProvMutate() |>
  mutate(`PlanningFlagCode` = "11b",
         `PlanningFlagDescription` = "Inpatient Non-Elective 1LoS") |> 
  SummariseActivity()


# Length of Stay ----------------------------------------------------------
LengthofStaySQLData <-
  tbl(DataSQLConnectionASA,
      in_schema("SOM",
                "SUS_LIVE")) |>
  filter(`Month Date` >= StartMonthDate,
         `Commissioner Code (Provider Derived)` == "11X",
         `NHSE PoD Description` == "Inpatient Emergency",
         `Length of Stay` != "0") |>
  select(`MonthDate` = `Month Date`,
         `Provider` = `Provider Grouping 1`,
         `LOS` = `Length of Stay`,
         `Activity`) |> 
  collect() |> 
  mutate(`ProviderGrouping` = case_when(`Provider` == "Somerset NHS FT" ~ "Somerset",
                                        `Provider` == "Yeovil District Hospital NHS FT" ~ "Yeovil",
                                        TRUE ~ "Other")) |> 
  select(`MonthDate`,
         `ProviderGrouping`,
         `LOS`,
         `Activity`) |> 
  group_by(`MonthDate`,
           `ProviderGrouping`) |> 
  summarise(`AvgLOS` = mean(`LOS`)) |> 
  ungroup()


# Readmissions ------------------------------------------------------------
# Dataframe

ReadmissionsCommSQLData <- 
  tbl(DataSQLConnectionASA,
      in_schema("SOM",
                "SUS_READMISSIONS")) |>
  filter(`Month Date (Re-Admission)` >= "2021-04-01") |> 
  select(`MonthDate` = `Month Date (Re-Admission)`,
         `Days` = `Days Difference (Initial Admission -> Re-Admission)`,
         `Activity`) |> 
  collect() |> 
  mutate(`Provider` = "Somerset System",
         `Days2` = case_when(`Days` <= 2 ~ "Y",
                             TRUE ~ "N"),
         `Days7` = case_when(`Days` <= 7 ~ "Y",
                             TRUE ~ "N"),
         `Days30` = case_when(`Days` <= 30 ~ "Y",
                              TRUE ~ "N")) |> 
  select(`MonthDate`,
         `Provider`,
         `Days`,
         `Days2`,
         `Days7`,
         `Days30`,
         `Activity`)

ReadmissionsProvSQLData <- 
  tbl(DataSQLConnectionASA,
      in_schema("SOM",
                "SUS_READMISSIONS")) |>
  filter(`Month Date (Re-Admission)` >= StartMonthDate) |> 
  select(`MonthDate` = `Month Date (Re-Admission)`,
         `ProviderAcronym` = `Provider Grouping Acronym 1 (Re-Admission)`,
         `Days` = `Days Difference (Initial Admission -> Re-Admission)`,
         `Activity`) |> 
  collect() |> 
  mutate(`Provider` = case_when(`ProviderAcronym` == "S" ~ "Somerset",
                                `ProviderAcronym` == "YDH" ~ "Yeovil",
                                `ProviderAcronym` == "RUH" ~ "Bath",
                                `ProviderAcronym` == "UHBW" ~ "BristolWeston",
                                TRUE ~ "Other"),
         `Days2` = case_when(`Days` <= 2 ~ "Y",
                             TRUE ~ "N"),
         `Days7` = case_when(`Days` <= 7 ~ "Y",
                             TRUE ~ "N"),
         `Days30` = case_when(`Days` <= 30 ~ "Y",
                             TRUE ~ "N")) |> 
  filter(`Provider` != "Other") |> 
  select(`MonthDate`,
         `Provider`,
         `Days`,
         `Days2`,
         `Days7`,
         `Days30`,
         `Activity`)

ReadmissionsSQLData <- rbind(ReadmissionsProvSQLData,
      ReadmissionsCommSQLData)


# NRTR --------------------------------------------------------------------
# Dataframe
NRTR_SFT_Acute <- 
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/UC Data/NRTR (New Summary) v2.xlsx", 
             sheet = "NRTR",
             range = "C10:AEQ10",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "SFT Acute") 

NRTR_SFT_Acute$Date <-
  seq(as.Date("2021-01-01"),
      as.Date("2023-04-01"),
      by = "days")

NRTR_YDH_Acute <- 
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/UC Data/NRTR (New Summary) v2.xlsx", 
             sheet = "NRTR",
             range = "C45:AEQ45",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "YDH Acute") 

NRTR_YDH_Acute$Date <-
  seq(as.Date("2021-01-01"),
      as.Date("2023-04-01"),
      by = "days")

NRTR_SFT_Comm <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/UC Data/NRTR (New Summary) v2.xlsx", 
             sheet = "NRTR",
             range = "C115:AEQ115",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "SFT Community") 

NRTR_SFT_Comm$Date <- 
  seq(as.Date("2021-01-01"),
      as.Date("2023-04-01"),
      by = "days")

NRTR_All <- 
  NRTR_SFT_Acute |> 
  left_join(NRTR_SFT_Comm,
            by = "Date") |> 
  left_join(NRTR_YDH_Acute,
            by = "Date") |> 
  filter(Date >= "2021-04-01") |> 
  drop_na() |> 
  filter(as.Date(ceiling_date(as.Date(Date),
                              "month")-1) <= max(Date)) |> 
  mutate(Date = as.Date(floor_date(as.Date(Date),
                                   "month"))) |> 
  group_by(Date) |> 
  summarise_all(~round(mean(.),
                       0)) |>  
  ungroup()


# Bed Occupancy -----------------------------------------------------------

BedOcc_SFT_Open <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/D&C Bed Return_ActVsPlan v2.xlsx", 
             sheet = "SFT (acute) Actuals & Plan",
             range = "NF11:AKK11",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "SFT Acute Open") 


BedOcc_SFT_Open$Date <-
  seq(as.Date("2021-04-01"),
      as.Date("2022-11-25"),
      by = "days")

BedOcc_SFT_Occupied <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/D&C Bed Return_ActVsPlan v2.xlsx", 
             sheet = "SFT (acute) Actuals & Plan",
             range = "NF17:AKK17",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "SFT Acute Occupied") 

BedOcc_SFT_Occupied$Date <- 
  seq(as.Date("2021-04-01"),
      as.Date("2022-11-25"),
      by = "days")

BedOcc_YDH_Open <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/D&C Bed Return_ActVsPlan v2.xlsx", 
             sheet = "YDH Actuals & Plan",
             range = "NF11:AEW11",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "YDH Acute Open") 

BedOcc_YDH_Open$Date <-
  seq(as.Date("2021-04-01"),
      as.Date("2022-07-04"),
      by = "days")

BedOcc_YDH_Occupied <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/D&C Bed Return_ActVsPlan v2.xlsx", 
             sheet = "YDH Actuals & Plan",
             range = "NF17:AEW17",
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "YDH Acute Occupied") 

BedOcc_YDH_Occupied$Date <-
  seq(as.Date("2021-04-01"),
      as.Date("2022-07-04"),
      by = "days")

BedOcc_All <-
  BedOcc_SFT_Open |> 
  left_join(BedOcc_SFT_Occupied, by = "Date") |> 
  left_join(BedOcc_YDH_Open, by = "Date") |> 
  left_join(BedOcc_YDH_Occupied, by = "Date") |> 
  filter(Date >= "2021-04-01") |> 
  drop_na() |> 
  filter(as.Date(ceiling_date(as.Date(Date),
                              "month") -1) <= max(Date)) |> 
  mutate(Date = as.Date(floor_date(as.Date(Date),
                                   "month"))) |> 
  group_by(Date) |> 
  summarise_all(sum) |> 
  ungroup() |> 
  mutate(SFTOccPerc = `SFT Acute Occupied` / `SFT Acute Open`,
         YDHOccPerc = `YDH Acute Occupied` / `YDH Acute Open`,
         SomOccPerc = (`YDH Acute Occupied` + `SFT Acute Occupied`) / (`YDH Acute Open` + `SFT Acute Open`))


# Discharge Pathways ------------------------------------------------------

DischargePathways_SFT_Acute <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/UC Data/NRTR (New Summary) v2.xlsx", 
             sheet = "Discharge x Pathway",
             range = "C7:AEQ10", 
             col_names = FALSE) |> 
  pivot_longer(!`...1`,
               names_to = "Date",
               values_to = "Count") |> 
  pivot_wider(names_from = `...1`,
              values_from = "Count") |> 
  rename(Pathway0SFT = 2,
         Pathway1SFT = 3,
         Pathway2SFT = 4,
         Pathway3SFT = 5)

DischargePathways_SFT_Acute$Date <- 
  seq(as.Date("2021-01-01"),
      as.Date("2023-03-31"),
      by = "days")


DischargePathways_YDH_Acute <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/d  -  Weekly Tracker & Data/UC Data/NRTR (New Summary) v2.xlsx", 
             sheet = "Discharge x Pathway",
             range = "C51:AEQ54",
             col_names = FALSE) |> 
  pivot_longer(!`...1`,
               names_to = "Date",
               values_to = "Count") |> 
  pivot_wider(names_from = `...1`,
              values_from = "Count") |> 
  rename(Pathway0YDH = 2,
         Pathway1YDH = 3,
         Pathway2YDH = 4,
         Pathway3YDH = 5)


DischargePathways_YDH_Acute$Date <-
  seq(as.Date("2021-01-01"),
      as.Date("2023-03-31"),
      by = "days")

DischargePathways_All <- 
  DischargePathways_SFT_Acute |> 
  left_join(DischargePathways_YDH_Acute,
            by = "Date") |> 
  filter(Date >= "2021-04-01") |> 
  drop_na() |> 
  filter(as.Date(ceiling_date(as.Date(Date),
                              "month")-1) <= max(Date)) |> 
  mutate(Date = as.Date(floor_date(as.Date(Date),
                                        "month"))) |> 
  group_by(Date) |> 
  summarise_all(sum) |>  
  ungroup() |> 
  mutate(Pathway0 = Pathway0SFT + Pathway0YDH,
         Pathway1 = Pathway1SFT + Pathway1YDH,
         Pathway2 = Pathway2SFT + Pathway2YDH,
         Pathway3 = Pathway3SFT + Pathway3YDH,
         PathwayAll = Pathway0 + Pathway1 + Pathway2 + Pathway3)


# Long Stays --------------------------------------------------------------

LongStay_21_SFT <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
             sheet = "Bed Occ (incl >21 Day LOS)",
             range = "AB4:AY4", 
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "SFT") 

LongStay_21_SFT$Date <-
  seq(as.Date("2021-04-01"),
      as.Date("2023-03-01"),
      by = "month")


LongStay_21_YDH <-
  read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Performance Report/2022-23/Plan Monitoring/Activity Vs Operational Plan 2223.xlsx", 
             sheet = "Bed Occ (incl >21 Day LOS)",
             range = "AB22:AY22", 
             col_names = FALSE) |> 
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "YDH") 

LongStay_21_YDH$Date <- 
  seq(as.Date("2021-04-01"),
      as.Date("2023-03-01"),
      by = "month")

LongStay_21_All <-
  LongStay_21_SFT |> 
  left_join(LongStay_21_YDH,
            by = "Date") |> 
  drop_na()
