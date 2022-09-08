
# Run Setup script --------------------------------------------------
source("SetupNew.R")
source("HealthInequalitiesRTTData.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 15
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300


# WL by Waittime + split by provider --------------------------------------------------
# Dataframe
RTTOpenPathwaysSomCommWeekData <-
  RTTOpenPathwaysSQLData |>
  filter(`ProviderDescription` == "Somerset",
         `WaitWeeks` < 1000,
         !is.na(`WaitWeeks`)) |> 
  mutate(`WaitWeeks` = case_when(`WaitWeeks` >= 104 ~ 104,
                                 TRUE ~ `WaitWeeks`)) |> 
  select(`WaitWeeksGrouping`,
         `WaitWeeks`,
         `OpenPathways`) |>
  group_by(`WaitWeeksGrouping`,
           `WaitWeeks`) |> 
  summarise(`OpenPathways` = sum(`OpenPathways`)) |> 
  ungroup() |> 
  collect()

# Graphic
OpenPathwaysSomTotWeekPlot <-
  ggplot(data = RTTOpenPathwaysSomCommWeekData,
         aes(x = `WaitWeeks`,
             y = `OpenPathways`,
             colour = factor(`WaitWeeksGrouping`,
                             level = c('104+',
                                       '78-103',
                                       '65-77',
                                       '52-64',
                                       '0-51')),
             fill = factor(`WaitWeeksGrouping`,
                           level = c('104+',
                                     '78-103',
                                     '65-77',
                                     '52-64',
                                     '0-51')))) +
  geom_bar(stat = "identity",
           size = 0.2,
           width = 1,
           alpha = 0.2,
           key_glyph = "point") +
  scale_colour_manual("legend",
                      values = c("104+" = NHSDarkBlue,
                                 "78-103" = NHSBlue,
                                 "65-77" = NHSBrightBlue,
                                 "52-64" = NHSAquaBlue,
                                 "0-51" = NHSLightBlue)) +
  scale_fill_manual("legend",
                    values = c("104+" = NHSDarkBlue,
                               "78-103" = NHSBlue,
                               "65-77" = NHSBrightBlue,
                               "52-64" = NHSAquaBlue,
                               "0-51" = NHSLightBlue)) +
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 4))) +
  scale_x_continuous(breaks = c(0,
                                18,
                                52,
                                65,
                                78,
                                104)) +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0,
                                3001),
                     expand = c(0,
                                0),
                     labels = comma) +
  labs(title = paste0("Open Pathways - Total Somerset Provider"),
       subtitle = paste0("Snapshot taken on ",
                         format(as.Date(RTTOpenPathwaysEndDate),
                                '%d %B %Y'),
                         " of Open Pathways for Somerset providers, split by weeks waited"),
       caption = paste0("Data source: Referral to Treatment MDS, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'))) +
  BarPlotTheme()

# Output
ggsave(file = paste0("Outputs/HealthInequalities/OpenPathwaysSomTotWeekPlot.png"),
       plot = OpenPathwaysSomTotWeekPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# # OpenPathwaysSomProvWeekPlot --------------------------------------------------
# 
# 
# # Dataframe
# RTTOpenPathwaysSomProvWeekData <-
#   RTTOpenPathwaysSQLData |>
#   filter(`ProviderCode` == "RH5",
#          `WaitWeeks` < 1000,
#          !is.na(`WaitWeeks`)) |> 
#   mutate(`WaitWeeks` = case_when(`WaitWeeks` >= 104 ~ 104,
#                                  TRUE ~ `WaitWeeks`)) |> 
#   select(`WaitWeeksGrouping`,
#          `WaitWeeks`,
#          `OpenPathways`) |>
#   group_by(`WaitWeeksGrouping`,
#            `WaitWeeks`) |> 
#   summarise(`OpenPathways` = sum(`OpenPathways`)) |> 
#   ungroup() |> 
#   collect()
# 
# # Graphic
# OpenPathwaysSomProvWeekPlot <-
#   ggplot(data = RTTOpenPathwaysSomProvWeekData,
#          aes(x = `WaitWeeks`,
#              y = `OpenPathways`,
#              colour = factor(`WaitWeeksGrouping`,
#                              level = c('104+',
#                                        '78-103',
#                                        '65-77',
#                                        '52-64',
#                                        '0-51')),
#              fill = factor(`WaitWeeksGrouping`,
#                            level = c('104+',
#                                      '78-103',
#                                      '65-77',
#                                      '52-64',
#                                      '0-51')))) +
#   geom_bar(stat = "identity",
#            size = 0.2,
#            width = 1,
#            alpha = 0.2,
#            key_glyph = "point") +
#   scale_colour_manual("legend",
#                       values = c("104+" = NHSDarkBlue,
#                                  "78-103" = NHSBlue,
#                                  "65-77" = NHSBrightBlue,
#                                  "52-64" = NHSAquaBlue,
#                                  "0-51" = NHSLightBlue)) +
#   scale_fill_manual("legend",
#                     values = c("104+" = NHSDarkBlue,
#                                "78-103" = NHSBlue,
#                                "65-77" = NHSBrightBlue,
#                                "52-64" = NHSAquaBlue,
#                                "0-51" = NHSLightBlue)) +
#   guides(color = guide_legend(override.aes = list(alpha = 1,
#                                                   size = 4))) +
#   scale_x_continuous(breaks = c(0,
#                                 18,
#                                 52,
#                                 65,
#                                 78,
#                                 104)) +
#   expand_limits(y = 0) +
#   scale_y_continuous(limits = c(0,
#                                 2001),
#                      expand = c(0,
#                                 0),
#                      labels = comma) +
#   labs(title = paste0("Open Pathways - Total Taunton Provider"),
#        subtitle = paste0("Snapshot taken on ",
#                          format(as.Date(RTTOpenPathwaysEndDate),
#                                 '%d %B %Y'),
#                          " of Open Pathways for Taunton, split by weeks waited"),
#        caption = paste0("Data source: Referral to Treatment MDS, updated on ",
#                         format(Sys.time(),
#                                '%A %d %B %Y'))) +
#   BarPlotTheme()
# 
# # Output
# ggsave(file = paste0("Outputs/HealthInequalities/OpenPathwaysSomProvWeekPlot.png"),
#        plot = OpenPathwaysSomProvWeekPlot,
#        width = PNGWidth,
#        height = PNGHeight,
#        units = PNGUnits,
#        dpi = PNGDPI,
#        type = "cairo")

# # OpenPathwaysYeoProvWeekPlot --------------------------------------------------
# 
# # Dataframe
# RTTOpenPathwaysYeoProvWeekData <-
#   RTTOpenPathwaysSQLData |>
#   filter(`ProviderCode` == "RA4",
#          `WaitWeeks` < 1000,
#          !is.na(`WaitWeeks`)) |> 
#   mutate(`WaitWeeks` = case_when(`WaitWeeks` >= 104 ~ 104,
#                                  TRUE ~ `WaitWeeks`)) |> 
#   select(`WaitWeeksGrouping`,
#          `WaitWeeks`,
#          `OpenPathways`) |>
#   group_by(`WaitWeeksGrouping`,
#            `WaitWeeks`) |> 
#   summarise(`OpenPathways` = sum(`OpenPathways`)) |> 
#   ungroup() |> 
#   collect()
# 
# # Graphic
# OpenPathwaysYeoProvWeekPlot <-
#   ggplot(data = RTTOpenPathwaysYeoProvWeekData,
#          aes(x = `WaitWeeks`,
#              y = `OpenPathways`,
#              colour = factor(`WaitWeeksGrouping`,
#                              level = c('104+',
#                                        '78-103',
#                                        '65-77',
#                                        '52-64',
#                                        '0-51')),
#              fill = factor(`WaitWeeksGrouping`,
#                            level = c('104+',
#                                      '78-103',
#                                      '65-77',
#                                      '52-64',
#                                      '0-51')))) +
#   geom_bar(stat = "identity",
#            size = 0.2,
#            width = 1,
#            alpha = 0.2,
#            key_glyph = "point") +
#   scale_colour_manual("legend",
#                       values = c("104+" = NHSDarkBlue,
#                                  "78-103" = NHSBlue,
#                                  "65-77" = NHSBrightBlue,
#                                  "52-64" = NHSAquaBlue,
#                                  "0-51" = NHSLightBlue)) +
#   scale_fill_manual("legend",
#                     values = c("104+" = NHSDarkBlue,
#                                "78-103" = NHSBlue,
#                                "65-77" = NHSBrightBlue,
#                                "52-64" = NHSAquaBlue,
#                                "0-51" = NHSLightBlue)) +
#   guides(color = guide_legend(override.aes = list(alpha = 1,
#                                                   size = 4))) +
#   scale_x_continuous(breaks = c(0,
#                                 18,
#                                 52,
#                                 65,
#                                 78,
#                                 104)) +
#   expand_limits(y = 0) +
#   scale_y_continuous(limits = c(0,
#                                 801),
#                      expand = c(0,
#                                 0),
#                      labels = comma) +
#   labs(title = paste0("Open Pathways - Total Yeovil Provider"),
#        subtitle = paste0("Snapshot taken on ",
#                          format(as.Date(RTTOpenPathwaysEndDate),
#                                 '%d %B %Y'),
#                          " of Open Pathways for Yeovil, split by weeks waited"),
#        caption = paste0("Data source: Referral to Treatment MDS, updated on ",
#                         format(Sys.time(),
#                                '%A %d %B %Y'))) +
#   BarPlotTheme()
# 
# # Output
# ggsave(file = paste0("Outputs/HealthInequalities/OpenPathwaysYeoProvWeekPlot.png"),
#        plot = OpenPathwaysYeoProvWeekPlot,
#        width = PNGWidth,
#        height = PNGHeight,
#        units = PNGUnits,
#        dpi = PNGDPI,
#        type = "cairo")

# Ethnicity coding - median wait --------------------------------------------------

OpenPathwaysEthMedTableData <-
  RTTOpenPathwaysSQLDataMedian |>
  filter(`ProviderDescription` == "Somerset",
         `WaitWeeks` < 1000,
         !is.na(`WaitWeeks`)) |>
  select(`Ethnicity` = `EthnicityGroupDescription`,
         `WaitWeeks`) |>
  collect() |>
  group_by(`Ethnicity`) |> 
  summarise(`MedianWait` = median(`WaitWeeks`)) |> 
  ungroup() |> 
  gt() |>
  fmt_number(columns = c("MedianWait"),
             decimals = 0,
             sep_mark = ",") %>% 
  tab_header(title = paste0("Median Wait Time - Ethnicity"),
             subtitle = paste0("Snapshot taken on ",
                               format(as.Date(RTTOpenPathwaysEndDate),
                                      '%d %B %Y'),
                               " of Open Pathways\nfor Somerset providers, split by Ethnicity")) %>% 
  tab_source_note(source_note = paste0("Data source: Referral to Treatment MDS")) %>% 
  tab_source_note(source_note = paste0("Updated on ",
                                       format(Sys.time(),
                                              '%A %d %B %Y'))) 

gtsave(OpenPathwaysEthMedTableData,
       "Outputs/HealthInequalities/OpenPathwaysEthMedTable.png", vwidth = 350, vheight = 1500)

# Ethnicity coding - Percentage --------------------------------------------------

OpenPathwaysEthWeekTableData <-
  RTTOpenPathwaysSQLData |>
  filter(`ProviderDescription` == "Somerset",
         `WaitWeeks` < 1000,
         !is.na(`WaitWeeks`)) |>
  select(`Ethnicity` = `EthnicityGroupDescription`,
         `OpenPathways`) |>
  collect() |>
  group_by(`Ethnicity`) |> 
  summarise(`Count` = sum(OpenPathways)) |> 
  ungroup() |> 
  mutate(`Percentage` = `Count`/sum(`Count`)) |> 
  gt() |> 
  fmt_number(columns = c("Count"),
             decimals = 0,
             sep_mark = ",") |> 
  fmt_percent(columns = c("Percentage"),
              decimals = 1)
  
  gtsave(OpenPathwaysEthWeekTableData,
         "Outputs/HealthInequalities/OpenPathwaysEthWeekTable.png")
  

  # Ethnicity coding - Time Bands --------------------------------------------------
  
  OpenPathwaysEthWeekTableData2 <-
    RTTOpenPathwaysSQLData |>
    filter(`ProviderDescription` == "Somerset",
           `WaitWeeks` < 1000,
           !is.na(`WaitWeeks`)) |>
    select(`Ethnicity` = `EthnicityGroupDescription`,
           `OpenPathways`,
           WaitWeeksGrouping) |>
    collect() |>
    select(Ethnicity,
           WaitWeeksGrouping,
           OpenPathways) |> 
    group_by(`Ethnicity`,
             WaitWeeksGrouping) |> 
    summarise(`Count` = sum(OpenPathways)) |> 
    ungroup() |> 
    drop_na() |> 
    pivot_wider(names_from = WaitWeeksGrouping, values_from = Count, values_fill = 0) %>% 
    mutate(Total = (`0-51`+`52-64`+`65-77`+`78-103`+`104+`),
           `78+` = (`78-103`+`104+`)) %>% 
    mutate(`78+%` = `78+`/Total,
           `104+%` = `104+`/Total) %>% 
    select(Ethnicity, Total, `78+`,`78+%`,`104+`,`104+%`) %>% 
    gt() |> 
    fmt_number(columns = c("Total", "78+", "104+"),
               decimals = 0,
               sep_mark = ",") |> 
    fmt_percent(columns = c("78+%","104+%" ),
                decimals = 1) %>% 
    tab_header(title = paste0("Open Pathways - Ethnicity"),
               subtitle = paste0("Snapshot taken on ",
                                 format(as.Date(RTTOpenPathwaysEndDate),
                                        '%d %B %Y'),
                                 " of Open Pathways for Somerset providers, split by ethnicity")) %>% 
    tab_source_note(source_note = paste0("Data source: Referral to Treatment MDS, updated on ",
                                         format(Sys.time(),
                                                '%A %d %B %Y')))
  
  gtsave(OpenPathwaysEthWeekTableData2,
         "Outputs/HealthInequalities/OpenPathwaysEthWeekTableData2.png")
  
  
  
  
#   
#   
# ClockStopsEthWeekTableData <-
#   RTTClockStopsSQLData |>
#   filter(`ProviderDescription` == "Somerset") |> 
#   select(`Ethnicity` = `EthnicityGroupDescription`,
#          `ClockStops`) |>
#   collect() |>
#   group_by(`Ethnicity`) |> 
#   summarise(`Count` = n()) |> 
#   ungroup() |> 
#   mutate(`Percentage` = `Count`/sum(`Count`)) |> 
#   gt() |> 
#   fmt_number(columns = c("Count"),
#              decimals = 0,
#              sep_mark = ",") |> 
#   fmt_percent(columns = c("Percentage"),
#               decimals = 1) |> 
#   tab_header(title = paste0("Open Pathways - Ethnicity"),
#              subtitle = paste0("Snapshot taken on ",
#                                format(as.Date(RTTOpenPathwaysEndDate),
#                                       '%B %Y'),
#                                " of Open Pathways for Somerset providers, split by ethnicity")) %>% 
#   tab_source_note(source_note = paste0("Data source: Referral to Treatment MDS, updated on ",
#                                        format(Sys.time(),
#                                               '%A %d %B %Y')))
# 
# gtsave(ClockStopsEthWeekTableData,
#        "Outputs/HealthInequalities/ClockStopsEthWeekTable.png")



## Deprivation Coding - Time Bands --------------------------------------------------

OpenPathwaysDepWeekTableData2 <-
  RTTOpenPathwaysSQLData |>
  filter(`ProviderDescription` == "Somerset",
         `WaitWeeks` < 1000,
         !is.na(`WaitWeeks`)) |>
  select(IMDDecile,
         `OpenPathways`,
         WaitWeeksGrouping) |>
  group_by(`IMDDecile`,
           WaitWeeksGrouping) |> 
  summarise(`Count` = sum(OpenPathways)) |> 
  ungroup() |> 
  drop_na() |> 
  pivot_wider(names_from = WaitWeeksGrouping, values_from = Count, values_fill = 0) %>% 
  mutate(Total = (`0-51`+`52-64`+`65-77`+`78-103`+`104+`),
         `78+` = (`78-103`+`104+`)) %>% 
  mutate(`78+%` = `78+`/Total,
         `104+%` = `104+`/Total) %>% 
  select(IMDDecile, Total, `78+`,`78+%`,`104+`,`104+%`) %>% 
  gt() |> 
  fmt_number(columns = c("Total", "78+", "104+"),
             decimals = 0,
             sep_mark = ",") |> 
  fmt_percent(columns = c("78+%","104+%" ),
              decimals = 1) %>% 
  tab_header(title = paste0("Open Pathways - Deprivation"),
             subtitle = paste0("Snapshot taken on ",
                               format(as.Date(RTTOpenPathwaysEndDate),
                                      '%d %B %Y'),
                               " of Open Pathways for Somerset providers, split by deprivation")) %>% 
  tab_source_note(source_note = paste0("Data source: Referral to Treatment MDS, updated on ",
                                      format(Sys.time(),
                                             '%A %d %B %Y')))

gtsave(OpenPathwaysDepWeekTableData2,
       "Outputs/HealthInequalities/OpenPathwaysDepWeekTable2.png")



## Deprivation Coding - Percentage --------------------------------------------------


OpenPathwaysDepWeekTableData3 <-
  RTTOpenPathwaysSQLData |>
  filter(`ProviderDescription` == "Somerset",
         `WaitWeeks` < 1000,
         !is.na(`WaitWeeks`)) |>
  select(IMDDecile,
         `OpenPathways`) |>
  group_by(`IMDDecile`) |> 
  summarise(`OpenPathways` = sum(OpenPathways)) |> 
  ungroup() |> 
  drop_na() |> 
  mutate(`OpenPathways%` = `OpenPathways`/sum(`OpenPathways`)) |> 
  left_join(PopulationIMD, by = c("IMDDecile" = "IndexofMultipleDeprivationDecile2019")) %>% 
  mutate(`Population%` = `Population`/sum(`Population`)) |> 
  select(IMDDecile,Population,`Population%`,OpenPathways,`OpenPathways%`) %>% 
  gt() |> 
  fmt_number(columns = c("Population", "OpenPathways"),
             decimals = 0,
             sep_mark = ",") |> 
  fmt_percent(columns = c("Population%", "OpenPathways%"),
              decimals = 1) |> 
  tab_header(title = paste0("Open Pathways - Deprivation"),
             subtitle = paste0("Snapshot taken on ",
                               format(as.Date(RTTOpenPathwaysEndDate),
                                      '%d %B %Y'),
                               " of Open Pathways for Somerset providers, split by deprivation")) %>% 
  tab_source_note(source_note = paste0("Data source: Referral to Treatment MDS, updated on ",
                                       format(Sys.time(),
                                              '%A %d %B %Y')))

gtsave(OpenPathwaysDepWeekTableData3,
       "Outputs/HealthInequalities/OpenPathwaysDepWeekTable3.png",
       )


# 
# 
# ClockStopsDepWeekTableData <-
#   RTTClockStopsSQLData |>
#   filter(`ProviderDescription` == "Somerset",
#          `WaitWeeks` < 1000,
#          !is.na(`WaitWeeks`)) |>
#   select(LSOA,
#          `OpenPathways`) |>
#   collect() |>
#   left_join(`REFERENCE_DEPRIVATION` |> select(LSOACode,IMD = IndexofMultipleDeprivationDecile2019) |> collect(),
#             by = c("LSOA" = "LSOACode")) |> 
#   select(IMD) |> 
#   group_by(`IMD`) |> 
#   summarise(`Count` = n()) |> 
#   ungroup() |> 
#   drop_na() |> 
#   mutate(`Percentage` = `Count`/sum(`Count`)) |> 
#   gt() |> 
#   fmt_number(columns = c("Count"),
#              decimals = 0,
#              sep_mark = ",") |> 
#   fmt_percent(columns = c("Percentage"),
#               decimals = 1) |> 
#   tab_header(title = paste0("Clock Stops - Deprivation"),
#              subtitle = paste0("Snapshot taken on ",
#                                format(as.Date(RTTOpenPathwaysEndDate),
#                                       '%B %Y'),
#                                " of Clock Stops for Somerset providers, split by deprivation")) %>% 
#   tab_source_note(source_note = paste0("Data source: Referral to Treatment MDS, updated on ",
#                                        format(Sys.time(),
#                                               '%A %d %B %Y')))
# 
# gtsave(OpenPathwaysDepWeekTableData,
#        "Outputs/HealthInequalities/ClockStopsDepWeekTable.png")


# Median Wait time - boxplot --------------------------------------------------




RTTOpenPathwaysSomCommEthWeekData <-
  RTTOpenPathwaysSQLDataMedian |>
  filter(`ProviderDescription` == "Somerset",
         `WaitWeeks` < 1000,
         !is.na(`WaitWeeks`)) |> 
  mutate(`WaitWeeks` = case_when(`WaitWeeks` >= 104 ~ 104,
                                 TRUE ~ `WaitWeeks`)) |> 
  select(`EthnicityGroupDescription`,
         `WaitWeeksGrouping`,
         `WaitWeeks`) |>
  collect()

RTTOpenPathwaysSomCommEthWeekPlot <-
  ggplot(data = RTTOpenPathwaysSomCommEthWeekData,
         aes(x = factor(EthnicityGroupDescription, 
                        levels = rev(levels(factor(EthnicityGroupDescription)))),
             y = `WaitWeeks`,
             fill = factor(`EthnicityGroupDescription`,
                           level = c('Asian',
                                     'Black',
                                     'Mixed',
                                     'Not stated',
                                     'Other',
                                     'Unknown',
                                     'White')))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual("legend",
                    values = c("Asian" = NHSBlue,
                               "Black" = NHSBlue,
                               "Mixed" = NHSBlue,
                               "Not stated" = NHSBlue,
                               "Other" = NHSBlue,
                               "Unknown" = NHSBlue,
                               "White" = NHSBlue)) +
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 4))) +
  expand_limits(y = 0) +
  scale_y_continuous(breaks = c(0,
                                18,
                                52,
                                65,
                                78,
                                104),
                     limits = c(0,
                                120),
                     expand = c(0,
                                0),
                     labels = comma) +
  labs(title = paste0("Open Pathways - Ethnicity"),
       subtitle = paste0("Snapshot taken on ",
                         format(as.Date(RTTOpenPathwaysEndDate),
                                '%d %B %Y'),
                         " of Open Pathways for Somerset providers, split by ethnicity"),
       caption = paste0("Data source: Referral to Treatment MDS, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'))) +
  BarFlipPlotTheme()

ggsave(file = paste0("Outputs/HealthInequalities/RTTOpenPathwaysSomCommEthWeekPlot.png"),
       plot = RTTOpenPathwaysSomCommEthWeekPlot,
       width = 18,
       height = 9,
       units = "cm",
       dpi = PNGDPI,
       type = "cairo")






# Map with Proportion of population in each decile --------------------------------------------------

# Dataframe
REFERENCE_DEPRIVATION <-
  tbl(DataSQLConnectionASA,
      in_schema("SOM",
                "REFERENCE_DEPRIVATION"))

REFERENCE_RMAPPING_LSOA_LAD <-
  tbl(DataSQLConnectionASA,
      in_schema("SOM",
                "REFERENCE_RMAPPING_LSOA_LAD"))

# Dataframe
PopulationDeprivation <-
  REFERENCE_DEPRIVATION %>% 
  select(`IndexofMultipleDeprivationDecile2019`,
         `LSOACode`) %>% 
  collect()

PopulationIMD <- 
  tbl(DataSQLConnectionDM,
      in_schema("Population",
                "vw_NHAIS_Current_All")) %>% 
  filter(`CCG_Of_Registration` == '11X') %>% 
  select(`Derived_SuperOutputArea`) %>% 
  group_by(`Derived_SuperOutputArea`) %>% 
  summarise(`Population` = n()) %>% 
  ungroup() %>% 
  collect() %>% 
  left_join(`PopulationDeprivation`,
            by = c("Derived_SuperOutputArea" = "LSOACode")) %>% 
  group_by(`IndexofMultipleDeprivationDecile2019`) %>% 
  summarise(`Population` = sum(`Population`)) %>% 
  ungroup() %>% 
  drop_na()



# Graphic
DeprivationMultiPlot <-
  ggplot(data = PopulationIMD,
         aes(x = factor(`IndexofMultipleDeprivationDecile2019`,
                        level = c('1',
                                  '2',
                                  '3',
                                  '4',
                                  '5',
                                  '6',
                                  '7',
                                  '8',
                                  '9',
                                  '10')),
             y = Population)) +
  geom_bar(stat = "identity",
           size = 0.08,
           fill = "#005EB8") +
  geom_hline(yintercept = 0,
             size = 1,
             colour = "#000000") + 
  scale_x_discrete(limits = c('1',
                              '2',
                              '3',
                              '4',
                              '5',
                              '6',
                              '7',
                              '8',
                              '9',
                              '10')) +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0,
                                110001),
                     expand = c(0,
                                0),
                     labels = comma) +
  labs(title = paste0("Index of Multiple Deprivation Decile by LSOA"),
       subtitle = paste0("Somerset GP Population (1 = Most Deprived, 10 = Least Deprived)"),
       caption = paste0("Data source: NHAIS, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'))) +
  BarPlotTheme()

# Output
ggsave(file = paste0("Outputs/HealthInequalities/DeprivationMultiPlot.png"),
       plot = DeprivationMultiPlot,
       width = 16,
       height = 7,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# Dataframe
REFERENCE_DEPRIVATION_MULTI_MAP_DATA <-
  REFERENCE_DEPRIVATION %>%
  filter(`LocalAuthorityDistrictDescription` %in% c("Mendip",
                                                    "Sedgemoor",
                                                    "South Somerset",
                                                    "Taunton Deane",
                                                    "West Somerset")) %>%
  select(`LSOACode`,
         `IndexofMultipleDeprivationDecile2019`) %>% 
  collect()

# Dataframe
REFERENCE_RMAPPING_SOM <-
  REFERENCE_RMAPPING_LSOA_LAD %>%
  filter(LAD17NM %in% c("Mendip",
                        "Sedgemoor",
                        "South Somerset",
                        "Taunton Deane",
                        "West Somerset")) %>%
  collect() %>%
  left_join(REFERENCE_DEPRIVATION_MULTI_MAP_DATA,
            by = c("LSOA11CD" = "LSOACode"),
            COPY = TRUE)

# Graphic
DeprivationMultiMapPlot <-
  ggplot() +
  geom_polygon(data = REFERENCE_RMAPPING_SOM,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = factor(`IndexofMultipleDeprivationDecile2019`))) +
  scale_fill_viridis(discrete = TRUE,
                     direction = -1) +
  labs(title = paste0("Index of Multiple Deprivation Decile by LSOA"),
       subtitle = paste0("Somerset (1 = Most Deprived, 10 = Least Deprived)"),
       caption = paste0("Data source: ONS")) +
  MapPlotTheme()+
  coord_map() +
  theme(legend.position = "bottom",
        legend.key.height= unit(0.15, "cm"),
        legend.key.width= unit(0.35, "cm"))

# Output
ggsave(file = paste0("Outputs/HealthInequalities/DeprivationMultiMapPlot.png"),
       plot = DeprivationMultiMapPlot,
       width = 16,
       height = 8,
       units = "cm",
       dpi = PNGDPI,
       type = "cairo")




