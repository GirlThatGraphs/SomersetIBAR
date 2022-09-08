
#RuleA = single point outside control
#RuleB = 7 points above/below mean
#RuleC = 6 points increasing or decreasing
#RuleD = 2 of 3 points near process limit

SPC_Data <- function(data, value_field, date_field,improvement_direction) {
  data |> 
    mutate(`Average` = mean({{value_field}}),
           MovingRangea={{value_field}} - lag({{value_field}}),
           MovingRange = abs(MovingRangea)) |> 
  mutate(MeanMovingRange = mean(MovingRange, na.rm =T)) |> 
    mutate(UCL = (Average + 2.66*MeanMovingRange) ,
           LCL = (Average - 2.66*MeanMovingRange),
           MeanCompare = case_when({{value_field}} <Average ~ -1,
                                   TRUE ~ 1),
           MovingRangeCompare = case_when(MovingRangea <0 ~ -1,
                                   TRUE ~ 1)) %>% 
  mutate(RuleA = case_when({{value_field}} < LCL ~ "Y",
                           {{value_field}} > UCL ~ "Y",
                           TRUE ~ "N"),
         RuleB = case_when(rollsum(x = MeanCompare, 7, align = "right", fill = NA) == 7 ~ "Y",
                           rollsum(x = MeanCompare, 7, align = "right", fill = NA) == -7 ~ "Y",
                           TRUE ~ "N"),
         RuleC = case_when(rollsum(x = MovingRangeCompare, 6, align = "right", fill = NA) == 6 ~ "Y",
                           rollsum(x = MovingRangeCompare, 6, align = "right", fill = NA) == -6 ~ "Y",
                           TRUE ~ "N"),
         RuleDa = case_when({{value_field}} > (Average + 2.66 *2/3*MeanMovingRange) & {{value_field}} < UCL ~1,
                            {{value_field}} < (Average - 2.66 *2/3*MeanMovingRange) & {{value_field}} > LCL ~1,
                            TRUE~0),
         RuleD = case_when(rollsum(x = RuleDa, 3, align = "right", fill = NA) >= 2 ~ "Y",
                           TRUE ~ "N"),
         Rules = case_when(RuleA == "Y" ~ "Y",
                           RuleB == "Y" ~ "Y",
                           RuleC == "Y" ~ "Y",
                           RuleD == "Y" ~ "Y",
                           TRUE ~ "N")
         )
}



SPC_Plot <- function(data, value_field, date_field,improvement_direction) {
  data |> 
    ggplot(aes(x = {{date_field}},
               y = {{value_field}})) +
    geom_line(size = 0.7, colour = NHSBlue) +
    geom_line(aes(y= UCL), size = 0.3)+
    geom_line(aes(y= LCL), size = 0.3)+
    geom_line(aes(y= Average), size = 0.3)+
    geom_point(aes(colour = factor(Rules,
                                   level = c('Y', 'N'))), size = 2)+
    scale_colour_manual("legend",
                        values = c("Y" = NHSOrange,
                                   "N" = NHSBlue)) +
    geom_hline(yintercept = 0,
               size = 0.5,
               colour = "#000000") +
    LinePlotTheme()
}


EColiPlot <- IPC %>% filter(Organism == "E Coli", 
                 Provider == "Primary Care") |> 
  SPC_Data(value_field = Count, date_field = Date, improvement_direction = "decrease") |> 
  SPC_Plot(value_field = Count, date_field = Date, improvement_direction = "decrease")+
  scale_y_continuous(limits = c(0,
                                46),
                     expand = c(0,
                                0),
                     labels = comma),
  labs(title = paste0("Count of E Coli"),
       subtitle = "Primary Care",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 


# Save Output
ggsave(file = paste0("Outputs/IPC/EColiPlotNew.png"),
       plot = EColiPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")
