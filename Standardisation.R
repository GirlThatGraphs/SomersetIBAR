
PROV <- data.frame(Provider = c("RA4", "RH5", "RA3", "RD1"),
                   Lat = c(50.944843000000000, 51.011968556759900, 51.322333509667900,51.391664000000000),
                   Long = c(-2.634712000000000, -3.119650000000000, -2.971398632185570, -2.391203448000000))

POP <-  tbl(DataSQLConnectionASA, in_schema("SOM","POPULATION_QUARTER")) |> 
  select(`Quarter Date`,
         `GP Practice Code`,
         `GP-Registered Population`) |> 
  collect() |> 
  filter(`Quarter Date` == max(`Quarter Date`)) |> 
  select(`GP Practice Code`,
         `GP-Registered Population`) |>   
  group_by(`GP Practice Code`) |> 
  summarise(`GP-Registered Population` = sum(`GP-Registered Population`)) |> 
  ungroup()

SUS_LIVE <- tbl(DataSQLConnectionASA, in_schema("SOM","SUS_LIVE")) |> 
  filter(`Financial Year` >= "2021/22",
         `Commissioner Code (Provider Derived)` == "11x",
         `Provider Code` %in% c("RA4", "RH5", "RA3", "RD1"),
         `NHSE PoD Description` == "Accident + Emergency",
         `CSU PoD Description 1` == "Emergency Department") |> 
  select(`Month Date`,
         `Provider Code`,
         `GP Practice Code (Activity)`,
         `GP Practice Latitude`,
         `GP Practice Longitude`,
         `Activity`) |> 
  collect() |> 
  filter(`Month Date` > max(`Month Date`) - years(1)) |> 
select(`Provider Code`,
       `GP Practice Code (Activity)`,
       `GP Practice Latitude`,
       `GP Practice Longitude`,
       `Activity`) |> 
  group_by(`Provider Code`,
           `GP Practice Code (Activity)`,
           `GP Practice Latitude`,
           `GP Practice Longitude`) |> 
  summarise(`Activity` = sum(`Activity`)) |> 
  ungroup() |> 
  drop_na() |> 
  mutate(`GP Practice Latitude` = as.numeric(`GP Practice Latitude`),
         `GP Practice Longitude` = as.numeric(`GP Practice Longitude`)) |> 
  left_join(PROV, by = c("Provider Code" = "Provider")) |> 
  left_join(POP, by =c("GP Practice Code (Activity)" = "GP Practice Code")) |> 
  mutate(ActiivtyPop = Activity / (`GP-Registered Population`/1000)) |> 
  filter(ActiivtyPop > 100) 


GPLocation <- tbl(DataSQLConnectionASA, in_schema("SOM","SUS_LIVE")) |> 
  filter(`Financial Year` >= "2022/23",
         `Commissioner Code (Provider Derived)` == "11x",
         `Provider Code` %in% c("RA4", "RH5", "RA3", "RD1"),
         `NHSE PoD Description` == "Accident + Emergency",
         `CSU PoD Description 1` == "Emergency Department") |> 
  select(`Month Date`,
         `Provider Code`,
         `GP Practice Code (Activity)`,
         `GP Practice Latitude`,
         `GP Practice Longitude`) |> 
  collect() |> 
  filter(`Month Date` == max(`Month Date`)) |> 
  drop_na() |> 
  mutate(`GP Practice Latitude` = as.numeric(`GP Practice Latitude`),
         `GP Practice Longitude` = as.numeric(`GP Practice Longitude`)) |> 
  select(`GP Practice Code (Activity)`,
         `GP Practice Latitude`,
         `GP Practice Longitude`) |> 
  distinct()


StandardisationMap <- 
  Standardisation |> 
  filter(`NHSE PoD Description` == "Accident + Emergency (ED)",
                        `Length of Stay Flag` == 0) |> 
  left_join(GPLocation, by = c("GP Practice Code" = "GP Practice Code (Activity)"))

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



REFERENCE_RMAPPING_LSOA_LAD <-
  tbl(DataSQLConnectionASA,
      in_schema("SOM",
                "REFERENCE_RMAPPING_LSOA_LAD"))

# Dataframe
REFERENCE_RMAPPING_SOM <-
  REFERENCE_RMAPPING_LSOA_LAD %>%
  filter(LAD17NM %in% c("Mendip",
                        "Sedgemoor",
                        "South Somerset",
                        "Taunton Deane",
                        "West Somerset")) %>%
  collect() 






ggplot() +
  geom_polygon(data = REFERENCE_RMAPPING_SOM,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NHSPaleGrey) +
  geom_point(data = StandardisationMap ,
               aes(x=`GP Practice Longitude`, 
                   y=`GP Practice Latitude`,
                   col = `GPStandardRate100,000`,
                   size = `GPStandardRate100,000`))+
  scale_color_gradient(low = NHSWhite, high = NHSDarkBlue) +
  scale_size_continuous(range = c(0.3, 4))+
  labs(title = paste0("Accident and Emergency (ED) Activity"),
       subtitle = paste0("Somerset; Standardised Activity Rates"),
       caption = paste0("Data source: ONS")) +
  MapPlotTheme()+
  coord_map()  +
  theme(legend.position = "none")




















# Graphic
MapPlot <-
  ggplot() +
  geom_polygon(data = REFERENCE_RMAPPING_SOM,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NHSPaleGrey) +
  geom_segment(data = SUS_LIVE[order(SUS_LIVE$Activity, decreasing = FALSE), ] ,
               aes(x=`GP Practice Longitude`, 
                   y=`GP Practice Latitude`,
                   xend=Long, 
                   yend=Lat,
                   col = ActiivtyPop,
                   size = ActiivtyPop),
               lineend = "round")+
  scale_color_gradient(low = NHSWhite, high = NHSDarkBlue) +
  scale_size_continuous(range = c(0.3, 1))+
  labs(title = paste0("Somerset Activity (per 1,000 patients)"),
       subtitle = paste0("Somerset"),
       caption = paste0("Data source: ONS")) +
  MapPlotTheme()+
  coord_map()  +
  theme(legend.position = "bottom")
# Output
ggsave(file = paste0("Outputs/HealthInequalities/DeprivationMultiMapPlot.png"),
       plot = DeprivationMultiMapPlot,
       width = 16,
       height = 8,
       units = "cm",
       dpi = PNGDPI,
       type = "cairo")


scale_color_viridis(direction = -1) +
ggplot() +
  geom_polygon(data = REFERENCE_RMAPPING_SOM,
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_segment(data = SUS_LIVE[order(SUS_LIVE$Activity, decreasing = FALSE), ] ,
               aes(x=`GP Practice Longitude`, 
                   y=`GP Practice Latitude`,
                   xend=Long, 
                   yend=Lat,
                   alpha = ActiivtyPop),
               size = 1.5,
               colour=NHSWhite,
               lineend = "round")+
  scale_alpha_continuous(range = c(0.3, 1))+
  labs(title = paste0("Somerset Activity (per 1,000 patients)"),
       subtitle = paste0("Somerset"),
       caption = paste0("Data source: ONS")) +
  MapPlotTheme()+
  coord_map()  +
  theme(legend.position = "bottom")







ggplot(SUS_LIVE, aes(oX, oY))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below
  geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.03, 0.3))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()