
# Run Setup script ----------------------------------------------------------
source("SetupNew.R")
source("QualityData.R")
source("nhs-sickness-absence-rates.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 7
PNGUnits <- "cm"
PNGDPI <- 300



# PressureUlcers --------------------------------------------------------

PressureUlcers <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric1_PressureUlcers) %>% 
  drop_na() 

# Graphic
PressureUlcersPlot <-
  ggplot(data = PressureUlcers,
         aes(x = `Month`,
             y = `Metric1_PressureUlcers`,
             color = Organisation)) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,2),
                     expand = c(0,0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(PressureUlcers$Month), max(PressureUlcers$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of hospital acquired pressure ulcers (category 2 and above) per 1000 bed days"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/PressureUlcersPlot.png"),
       plot = PressureUlcersPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Nutrition --------------------------------------------------------

Nutrition <-
  Quality_Scorecard %>% 
  select(Month, Metric2_NutritionScreening_YDH,Metric2_NutritionScreening_SFTAcute,Metric2_NutritionScreening_SFTCommunity) %>% 
  group_by(Month) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  drop_na() 



# Graphic
NutritionPlot <-
  ggplot(data = Nutrition) +
  geom_line(aes(x = as.Date(`Month`),
                y = `Metric2_NutritionScreening_YDH`),
            colour = NHSPurple,
            size = 0.7) +
  geom_line(aes(x = as.Date(`Month`),
                y = `Metric2_NutritionScreening_SFTAcute`),
            colour = NHSAquaGreen,
            size = 0.7) +
  geom_line(aes(x = as.Date(`Month`),
                y = `Metric2_NutritionScreening_SFTCommunity`),
            colour = NHSWarmYellow,
            size = 0.7) +
  geom_hline(yintercept = 0.9,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.91,
                label = "Standard = 90%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.4,
                                1),
                     expand = c(0,
                                0),
                     labels = percent_format(accuracy=1),) +
  scale_x_date(breaks= seq(min(Nutrition$Month), max(Nutrition$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Percentage of adult inpatients reported as having had nutrition screening using a validated tool"),
       subtitle = "<span style='color:#00A499;'>SFT (Acute), </span><span style='color:#FFB81C;'>SFT (Community), </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/NutritionPlot.png"),
       plot = NutritionPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# StaffAbsence --------------------------------------------------------

StaffAbsence <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric3_StaffAbscence) %>% 
  drop_na() 

# Graphic
StaffAbsencePlot <-
  ggplot(data = StaffAbsence,
         aes(x = as.Date(`Month`),
             y = `Metric3_StaffAbscence`,
             color = Organisation)) +
  geom_line(size = 0.7) +
  geom_line(data = NationalAbsenceRates,
            aes(x = as.Date(`Date`),
                y = (`Absence`/100)),
            color = NHSBlue,
            size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0.04,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.0405,
                label = "SFT Threshold = 4%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0.035,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.0355,
                label = "YDH Threshold = 3.5%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                0.07),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks= seq(min(StaffAbsence$Month), max(StaffAbsence$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Percentage absence rate (including isolation) 12 month rolling period"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH, </span><span style='color:#005EB8;'>National (Acutes),</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/StaffAbsencePlot.png"),
       plot = StaffAbsencePlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# MandatoryTraining --------------------------------------------------------



MandatoryTraining <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric4_MandatoryTraining) %>% 
  drop_na() 


# Graphic
MandatoryTrainingPlot <-
  ggplot(data = MandatoryTraining,
         aes(x = as.Date(`Month`),
             y = `Metric4_MandatoryTraining`,
             color = Organisation)) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0.9,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.91,
                label = "Standard = 90%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.85,
                                1),
                     expand = c(0,
                                0),
                     labels = percent_format(accuracy=1)) +
  scale_x_date(breaks= seq(min(MandatoryTraining$Month), max(MandatoryTraining$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Percentage of staff who have undertaken mandatory training (total applicable courses)"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/MandatoryTrainingPlot.png"),
       plot = MandatoryTrainingPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





# CPAReview --------------------------------------------------------



CPAReview <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric5_CPAReview) %>% 
  drop_na() 


# Graphic
CPAReviewPlot <-
  ggplot(data = CPAReview,
         aes(x = as.Date(`Month`),
             y = `Metric5_CPAReview`),
             color = NHSBlue) +
  geom_line(size = 0.7,
            color = NHSBlue) +
  geom_hline(yintercept = 0.95,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.955,
                label = "Standard = 95%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.9,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks= seq(min(CPAReview$Month), max(CPAReview$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of patients who have received an annual review (CPA Level 2)"),
       subtitle = "<span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/CPAReviewPlot.png"),
       plot = CPAReviewPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# CPAReview --------------------------------------------------------



CHC28Day <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric6_CHC28Day) %>% 
  drop_na() 


# Graphic
CHC28DayPlot <-
  ggplot(data = CHC28Day,
         aes(x = as.Date(`Month`),
             y = `Metric6_CHC28Day`),
         color = NHSBlue) +
  geom_line(size = 0.7,
            color = NHSBlue) +
  geom_hline(yintercept = 0.80,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.81,
                label = "Standard = 80%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.7,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks= seq(min(CHC28Day$Month), max(CHC28Day$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of referrals concluded within 28 days"),
       subtitle = "<span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/CHC28DayPlot.png"),
       plot = CHC28DayPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




# CHCBacklog --------------------------------------------------------



CHCBacklog <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric7_CHCBacklog) %>% 
  drop_na() 


# Graphic
CHCBacklogPlot <-
  ggplot(data = CHCBacklog,
         aes(x = as.Date(`Month`),
             y = `Metric7_CHCBacklog`),
         color = NHSBlue) +
  geom_line(size = 0.7,
            color = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                2),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(CHCBacklog$Month), max(CHCBacklog$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of deferred assessments"),
       subtitle = "<span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/CHCBacklogPlot.png"),
       plot = CHCBacklogPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# DentalCheck --------------------------------------------------------
  
  DentalCheck <-
    Quality_Scorecard %>% 
    select(Month, Organisation, Metric8_DentalCheck) %>% 
    drop_na() 

# Graphic
DentalCheckPlot <-
  ggplot(data = DentalCheck,
         aes(x = as.Date(`Month`),
             y = `Metric8_DentalCheck`)) +
  geom_line(size = 0.7,
            color = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                0.81),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks= seq(min(DentalCheck$Month), max(DentalCheck$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Percentage of children looked after for more than one year that have had their dental checks"),
       subtitle = "<span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/DentalCheckPlot.png"),
       plot = DentalCheckPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# Falls --------------------------------------------------------

Falls <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Metric9_Falls) %>% 
  drop_na() 

# Graphic
FallsPlot <-
  ggplot(data = Falls,
         aes(x = `Month`,
             y = `Metric9_Falls`,
             color = Organisation)) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,10),
                     expand = c(0,0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(Falls$Month), max(Falls$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Rate of sllips, trips and falls (irrespective of grade) per 1000 bed days"),
       subtitle = "<span style='color:#00A499;'>SFT, </span><span style='color:#330072;'>YDH</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/FallsPlot.png"),
       plot = FallsPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




# InitialAssesment --------------------------------------------------------

InitialAssesment <-
  Quality_Scorecard %>% 
  select(Month, Organisation, Mertric10_InitialAssesment) %>% 
  drop_na() 

# Graphic
InitialAssesmentPlot <-
  ggplot(data = InitialAssesment,
         aes(x = `Month`,
             y = `Mertric10_InitialAssesment`,
             color = Organisation)) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("SFT" = NHSAquaGreen,
                                 "YDH" = NHSPurple)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks= seq(min(InitialAssesment$Month), max(InitialAssesment$Month), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Percentage of children who received an Initial Health Assessment within 20 working days"),
       subtitle = "<span style='color:#00A499;'>SFT</span>",
       caption = paste0("Source: Quality Scorecard, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/Quality/InitialAssesmentPlot.png"),
       plot = InitialAssesmentPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")
