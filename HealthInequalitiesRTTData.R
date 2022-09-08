LSOADecileSQLData <- 
  tbl(DataSQLConnectionDM,
      in_schema("Demography",
                "Domains_Of_Deprivation_By_LSOA")) |>
  filter(`Effective_Snapshot_Date` == as.Date("2019-12-31")) |> 
  select(`LSOA` = `LSOA_Code`,
         `IMDDecile` = `IMD_Decile`) |> 
  collect()



# Clock Stops -------------------------------------------------------------
# Dataframe
# RTTClockStopsSQLData <- 
#   tbl(DataSQLConnectionDM,
#       in_schema("COVID19",
#                 "National_Wait_List_Clock_Stops")) |> 
#   filter(substr(`Derived_Registered_CCG`,
#                 1,
#                 3) == "11X" |
#            substr(`NECS_Derived_Provider_Code_From_DLP`,
#                   1,
#                   3) %in% c("RH5",
#                             "RA4")) |> 
#   select(`PseudoNHSNumber` = `Pseudo_NHS_Number`,
#          `StartDate` = `Referral_To_Treatment_Period_Start_Date`,
#          `SnapshotDate` = `NECS_Derived_Week_Ending`,
#          `CommissionerCode` = `Derived_Registered_CCG`,
#          `ProviderCode` = `NECS_Derived_Provider_Code_From_DLP`,
#          `WaitingListCode` = `Waiting_List_Type`,
#          `Age` = `Derived_Age_At_Activity`,
#          `SexCode` = `Person_Stated_Gender_Code`,
#          `EthnicityCode` = `Ethnic_Category`,
#          `LSOA` = `Derived_LSOA`) |> 
#   collect() |>
#   mutate(`StartDate` = as.Date(`StartDate`),
#          `SnapshotDate` = as.Date(`SnapshotDate`)) |> 
#   mutate(`WaitDays` = difftime(`StartDate`,
#                                `SnapshotDate`,
#                                units = "days"),
#          `WaitWeeks` = difftime(`StartDate`,
#                                 `SnapshotDate`,
#                                 units = "weeks")) |> 
#   mutate(`MonthDate` = floor_date(as.Date(`StartDate`),
#                                   "month"),
#          `CommissionerCode` = case_when(`CommissionerCode` == "11X" ~ `CommissionerCode`,
#                                         TRUE ~ "OTH"),
#          `CommissionerDescription` = case_when(`CommissionerCode` == "11X" ~ "Somerset",
#                                                TRUE ~ "Other"),
#          `ProviderCode` = case_when(`ProviderCode` %in% c("RH5",
#                                                           "RA4") ~ `ProviderCode`,
#                                     TRUE ~ "OTH"),
#          `ProviderDescription` = case_when(`ProviderCode` %in% c("RH5",
#                                                                  "RBA",
#                                                                  "RA4") ~ "Somerset",
#                                            TRUE ~ "Other"),
#          `WaitingListDescription` = case_when(`WaitingListCode` == "IRTT" ~ "Admitted",
#                                               `WaitingListCode` == "ORTT" ~ "Non-admitted",
#                                               TRUE ~ "Unknown"),
#          `WaitWeeksGrouping` = case_when(`WaitWeeks` <= 51 ~ "0-51",
#                                          `WaitWeeks` <= 64 ~ "52-64",
#                                          `WaitWeeks` <= 77 ~ "65-77",
#                                          `WaitWeeks` <= 103 ~ "78-103",
#                                          `WaitWeeks` >= 104 ~ "104+"),
#          `AgeGrouping` = case_when(`Age` >= 0 &
#                                      `Age` <= 17 ~ "0-17",
#                                    `Age` >= 18 &
#                                      `Age` <= 64 ~ "18-64",
#                                    `Age` >= 65 ~ "65+",
#                                    TRUE ~ "Unknown"),
#          `AgeBanding` = case_when(`Age` >= 0 &
#                                     `Age` <= 4 ~ "0-4",
#                                   `Age` >= 5 &
#                                     `Age` <= 9 ~ "5-9",
#                                   `Age` >= 10 &
#                                     `Age` <= 14 ~ "10-14",
#                                   `Age` >= 15 &
#                                     `Age` <= 19 ~ "15-19",
#                                   `Age` >= 20 &
#                                     `Age` <= 24 ~ "20-24",
#                                   `Age` >= 25 &
#                                     `Age` <= 29 ~ "25-29",
#                                   `Age` >= 30 &
#                                     `Age` <= 34 ~ "30-34",
#                                   `Age` >= 35 &
#                                     `Age` <= 39 ~ "35-39",
#                                   `Age` >= 40 &
#                                     `Age` <= 44 ~ "40-44",
#                                   `Age` >= 45 &
#                                     `Age` <= 49 ~ "45-49",
#                                   `Age` >= 50 &
#                                     `Age` <= 54 ~ "50-54",
#                                   `Age` >= 55 &
#                                     `Age` <= 59 ~ "55-59",
#                                   `Age` >= 60 &
#                                     `Age` <= 64 ~ "60-64",
#                                   `Age` >= 65 &
#                                     `Age` <= 69 ~ "65-69",
#                                   `Age` >= 70 &
#                                     `Age` <= 74 ~ "70-74",
#                                   `Age` >= 75 &
#                                     `Age` <= 79 ~ "75-79",
#                                   `Age` >= 80 &
#                                     `Age` <= 84 ~ "80-84",
#                                   `Age` >= 85 &
#                                     `Age` <= 89 ~ "85-89",
#                                   `Age` >= 90 &
#                                     `Age` <= 94 ~ "90-94",
#                                   `Age` >= 95 &
#                                     `Age` <= 99 ~ "95-99",
#                                   `Age` >= 100 ~ "100+",
#                                   TRUE ~ "Unknown"),
#          `SexDescription` = case_when(`SexCode` == "1" ~ "Male",
#                                       `SexCode` == "2" ~ "Female",
#                                       TRUE ~ "Other"),
#          `EthnicityGroupDescription` = case_when(`EthnicityCode` %in% c("A",
#                                                                         "B",
#                                                                         "C") ~ "White",
#                                                  `EthnicityCode` %in% c("D",
#                                                                         "E",
#                                                                         "F",
#                                                                         "G") ~ "Mixed",
#                                                  `EthnicityCode` %in% c("H",
#                                                                         "J",
#                                                                         "K",
#                                                                         "L") ~ "Asian",
#                                                  `EthnicityCode` %in% c("M",
#                                                                         "N",
#                                                                         "P") ~ "Black",
#                                                  `EthnicityCode` %in% c("R",
#                                                                         "S") ~ "Other",
#                                                  `EthnicityCode` == "Z" ~ "Not stated",
#                                                  TRUE ~ "Unknown"),
#          `EthnicityDescription` = case_when(`EthnicityCode` == "A" ~ "British",
#                                             `EthnicityCode` == "B" ~ "Irish",
#                                             `EthnicityCode` == "C" ~ "Other",
#                                             `EthnicityCode` == "D" ~ "White and Black Caribbean",
#                                             `EthnicityCode` == "E" ~ "White and Black African",
#                                             `EthnicityCode` == "F" ~ "White and Asian",
#                                             `EthnicityCode` == "G" ~ "Other",
#                                             `EthnicityCode` == "H" ~ "Indian",
#                                             `EthnicityCode` == "J" ~ "Pakistani",
#                                             `EthnicityCode` == "K" ~ "Bangladeshi",
#                                             `EthnicityCode` == "L" ~ "Other",
#                                             `EthnicityCode` == "M" ~ "Caribbean",
#                                             `EthnicityCode` == "N" ~ "African",
#                                             `EthnicityCode` == "P" ~ "Other",
#                                             `EthnicityCode` == "R" ~ "Chinese",
#                                             `EthnicityCode` == "S" ~ "Other",
#                                             `EthnicityCode` == "Z" ~ "Not stated",
#                                             TRUE ~ "Unknown")) |>
#   group_by(`StartDate`,
#            `CommissionerCode`,
#            `CommissionerDescription`,
#            `ProviderCode`,
#            `ProviderDescription`,
#            `WaitingListCode`,
#            `WaitingListDescription`,
#            `WaitWeeksGrouping`,
#            `Age`,
#            `AgeGrouping`,
#            `AgeBanding`,
#            `SexCode`,
#            `SexDescription`,
#            `EthnicityCode`,
#            `EthnicityGroupDescription`,
#            `EthnicityDescription`,
#            `LSOA`) |> 
#   summarise(`ClockStops` = n()) |> 
#   ungroup()

# Open Pathways -----------------------------------------------------------
# Dataframe

RTTOpenPathwaysEndDate <- 
  tbl(DataSQLConnectionDM,
      in_schema("COVID19",
                "National_Wait_List_Open_Pathways")) %>% 
  select(NECS_Derived_Week_Ending) %>% 
  distinct() %>% 
  collect %>% 
  filter(NECS_Derived_Week_Ending == max(NECS_Derived_Week_Ending)) %>% 
  pull()



RTTOpenPathwaysSQLData <- 
  tbl(DataSQLConnectionDM,
      in_schema("COVID19",
                "National_Wait_List_Open_Pathways")) |> 
  filter(substr(`Derived_Registered_CCG`,
                1,
                3) == "11X" |
           substr(`NECS_Derived_Provider_Code_From_DLP`,
                  1,
                  3) %in% c("RH5",
                            "RA4"),
         `Referral_Request_Received_Date` <= `NECS_Derived_Week_Ending`,
         `NECS_Derived_Week_Ending` == RTTOpenPathwaysEndDate) |> 
  select(`StartDate` = `Referral_To_Treatment_Period_Start_Date`,
         `SnapshotDate` = `NECS_Derived_Week_Ending`,
         `CommissionerCode` = `Derived_Registered_CCG`,
         `ProviderCode` = `NECS_Derived_Provider_Code_From_DLP`,
         `WaitingListCode` = `Waiting_List_Type`,
         `EthnicityCode` = `Ethnic_Category`,
         `LSOA` = `Derived_LSOA`) |> 
  collect() |>
  mutate(`StartDate` = as.Date(`StartDate`),
         `SnapshotDate` = as.Date(`SnapshotDate`)) |> 
  left_join(LSOADecileSQLData,
            by = c("LSOA" = "LSOA")) |> 
  select(`StartDate`,
         `SnapshotDate`,
         `CommissionerCode`,
         `ProviderCode`,
         `WaitingListCode`,
         `EthnicityCode`,
         `LSOA`,
         `IMDDecile`) |> 
  mutate(`WaitWeeks` = interval(`StartDate`,
                                `SnapshotDate`) %/% days(7)) |> 
  mutate(`CommissionerCode` = case_when(`CommissionerCode` == "11X" ~ `CommissionerCode`,
                                        TRUE ~ "OTH"),
         `CommissionerDescription` = case_when(`CommissionerCode` == "11X" ~ "Somerset",
                                               TRUE ~ "Other"),
         `ProviderCode` = case_when(`ProviderCode` %in% c("RH5",
                                                          "RA4") ~ `ProviderCode`,
                                    TRUE ~ "OTH"),
         `ProviderDescription` = case_when(`ProviderCode` %in% c("RH5",
                                                                 "RBA",
                                                                 "RA4") ~ "Somerset",
                                           TRUE ~ "Other"),
         `WaitingListDescription` = case_when(`WaitingListCode` == "IRTT" ~ "Admitted",
                                              `WaitingListCode` == "ORTT" ~ "Non-admitted",
                                              TRUE ~ "Unknown"),
         `WaitWeeksGrouping` = case_when(`WaitWeeks` <= 51 ~ "0-51",
                                         `WaitWeeks` <= 64 ~ "52-64",
                                         `WaitWeeks` <= 77 ~ "65-77",
                                         `WaitWeeks` <= 103 ~ "78-103",
                                         `WaitWeeks` >= 104 ~ "104+"),


         `EthnicityGroupDescription` = case_when(`EthnicityCode` %in% c("A",
                                                                        "B",
                                                                        "C") ~ "White",
                                                 `EthnicityCode` %in% c("D",
                                                                        "E",
                                                                        "F",
                                                                        "G") ~ "Mixed",
                                                 `EthnicityCode` %in% c("H",
                                                                        "J",
                                                                        "K",
                                                                        "L") ~ "Asian",
                                                 `EthnicityCode` %in% c("M",
                                                                        "N",
                                                                        "P") ~ "Black",
                                                 `EthnicityCode` %in% c("R",
                                                                        "S") ~ "Other",
                                                 `EthnicityCode` == "Z" ~ "Not stated",
                                                 TRUE ~ "Unknown")) |>
  select(`ProviderDescription`,
           `WaitingListDescription`,
           `WaitWeeks`,
           `WaitWeeksGrouping`,
           `EthnicityGroupDescription`,
           `IMDDecile`) |>  
  group_by(`ProviderDescription`,
           `WaitingListDescription`,
           `WaitWeeks`,
           `WaitWeeksGrouping`,
           `EthnicityGroupDescription`,
           `IMDDecile`) |> 
  summarise(`OpenPathways` = n()) |> 
  ungroup()





RTTOpenPathwaysSQLDataMedian <- 
  tbl(DataSQLConnectionDM,
      in_schema("COVID19",
                "National_Wait_List_Open_Pathways")) |> 
  filter(substr(`Derived_Registered_CCG`,
                1,
                3) == "11X" |
           substr(`NECS_Derived_Provider_Code_From_DLP`,
                  1,
                  3) %in% c("RH5",
                            "RA4"),
         `Referral_Request_Received_Date` <= `NECS_Derived_Week_Ending`,
         `NECS_Derived_Week_Ending` == RTTOpenPathwaysEndDate) |> 
  select(`StartDate` = `Referral_To_Treatment_Period_Start_Date`,
         `SnapshotDate` = `NECS_Derived_Week_Ending`,
         `CommissionerCode` = `Derived_Registered_CCG`,
         `ProviderCode` = `NECS_Derived_Provider_Code_From_DLP`,
         `WaitingListCode` = `Waiting_List_Type`,
         `EthnicityCode` = `Ethnic_Category`) |> 
  collect() |>
  mutate(`StartDate` = as.Date(`StartDate`),
         `SnapshotDate` = as.Date(`SnapshotDate`)) |> 
  select(`StartDate`,
         `SnapshotDate`,
         `CommissionerCode`,
         `ProviderCode`,
         `WaitingListCode`,
         `EthnicityCode`) |> 
  mutate(`WaitWeeks` = interval(`StartDate`,
                                `SnapshotDate`) %/% days(7)) |> 
  mutate(`CommissionerCode` = case_when(`CommissionerCode` == "11X" ~ `CommissionerCode`,
                                        TRUE ~ "OTH"),
         `CommissionerDescription` = case_when(`CommissionerCode` == "11X" ~ "Somerset",
                                               TRUE ~ "Other"),
         `ProviderCode` = case_when(`ProviderCode` %in% c("RH5",
                                                          "RA4") ~ `ProviderCode`,
                                    TRUE ~ "OTH"),
         `ProviderDescription` = case_when(`ProviderCode` %in% c("RH5",
                                                                 "RBA",
                                                                 "RA4") ~ "Somerset",
                                           TRUE ~ "Other"),
         `WaitingListDescription` = case_when(`WaitingListCode` == "IRTT" ~ "Admitted",
                                              `WaitingListCode` == "ORTT" ~ "Non-admitted",
                                              TRUE ~ "Unknown"),
         `WaitWeeksGrouping` = case_when(`WaitWeeks` <= 51 ~ "0-51",
                                         `WaitWeeks` <= 64 ~ "52-64",
                                         `WaitWeeks` <= 77 ~ "65-77",
                                         `WaitWeeks` <= 103 ~ "78-103",
                                         `WaitWeeks` >= 104 ~ "104+"),
         
         
         `EthnicityGroupDescription` = case_when(`EthnicityCode` %in% c("A",
                                                                        "B",
                                                                        "C") ~ "White",
                                                 `EthnicityCode` %in% c("D",
                                                                        "E",
                                                                        "F",
                                                                        "G") ~ "Mixed",
                                                 `EthnicityCode` %in% c("H",
                                                                        "J",
                                                                        "K",
                                                                        "L") ~ "Asian",
                                                 `EthnicityCode` %in% c("M",
                                                                        "N",
                                                                        "P") ~ "Black",
                                                 `EthnicityCode` %in% c("R",
                                                                        "S") ~ "Other",
                                                 `EthnicityCode` == "Z" ~ "Not stated",
                                                 TRUE ~ "Unknown")) |>
  select(`ProviderDescription`,
         `WaitingListDescription`,
         `WaitWeeks`,
         `WaitWeeksGrouping`,
         `EthnicityGroupDescription`)





