
# Run Setup script
source("SetupNew.R")
source("MentalHealthData.R")


# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 6
PNGUnits <- "cm"
PNGDPI <- 300


# Dementia Diagnosis rate -------------------------------------------------

# Dataframe
DementiaDiagData <-
  CDPdata |> 
  filter(`Dementia diagnosis rate` > 0)

# Graphic
DementiaDiagPlot <-
  ggplot(data = DementiaDiagData,
         aes(x = `Month`,
             y = `Dementia diagnosis rate`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  geom_hline(yintercept = 0.667,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.68,
                label = "National Ambition = 66.7%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  scale_y_continuous(limits = c(0,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(DementiaDiagData$`Month`),
                              to = max(DementiaDiagData$`Month`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Dementia Diagnosis Rate"),
       subtitle = "<span style='color:#005EB8;'>Somerset System: % estimated dementia diagnosis rate for people aged 65+</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/DementiaDiagPlot.png"),
       plot = DementiaDiagPlot,
       width = PNGWidth,
       height = 9.7,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Perinatal Access --------------------------------------------------------

PerinatalAccData <-
  CDPdata |> 
  select(`Month`,`Perinatal Access`) %>% 
  mutate(Target = c(rep(471,12 ),rep(547, length(CDPdata$Month)-12 ))) %>% 
  filter(`Perinatal Access` > 0)




# Graphic
PerinatalAccPlot <-
  ggplot(data = PerinatalAccData,
         aes(x = `Month`,
             y = `Perinatal Access`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_line(aes(x = `Month`,
                y = `Target`),
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 476,
                label = "Target = 471"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_text(aes(x = as.Date("2022-03-01"),
                y = 552,
                label = "Target = 547"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                600),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(PerinatalAccData$`Month`),
                              to = max(PerinatalAccData$`Month`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_text(colour = NHSBlue)) +
  labs(title = paste0("Perinatal Mental Health Access "),
       subtitle = "Somerset System: Number of women accessing specialist community Perinatal MH services in the reporting period on a rolling\n12 month basis",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/PerinatalAccPlot.png"),
       plot = PerinatalAccPlot,
       width = PNGWidth,
       height = 9.7,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# 72 hour follow-up -------------------------------------------------------

FUP72hourData <-
  CDPdata |> 
  filter(`72hr follow up` > 0)

# Graphic
FUP72hourPlot <-
  ggplot(data = FUP72hourData,
         aes(x = `Month`,
             y = `72hr follow up`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1.2),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(FUP72hourData$`Month`),
                              to = max(FUP72hourData$`Month`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("72 Hour Follow-up"),
       subtitle = "<span style='color:#005EB8;'>Somerset System</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/FUP72hourPlot.png"),
       plot = FUP72hourPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# CYP 1+ actual -----------------------------------------------------------

CYPActualData <-
  CDPdata |> 
  select(`Month`,`CYP 1+ Actual`) %>% 
  mutate(Target = c(rep(6167,12 ),rep(6785, length(CDPdata$Month)-12 ))) %>% 
  filter(`CYP 1+ Actual` > 0)

# Graphic
CYPActualPlot <-
  ggplot(data = CYPActualData,
         aes(x = `Month`,
             y = `CYP 1+ Actual`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_line(aes(x = `Month`,
                 y = `Target`),
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 6300,
                label = "Target = 6,176"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_text(aes(x = as.Date("2022-03-01"),
                y = 6900,
                label = "Target = 6,785"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                7100),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(CYPActualData$`Month`),
                              to = max(CYPActualData$`Month`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_text(colour = NHSBlue)) +
  labs(title = paste0("Children & Young People Mental Health Access Number of CYP Who Have At Least \nOne Contact"),
       subtitle = "Somerset System: Number of CYP aged under 18 supported through NHS funded mental health with at least one contact on a \n12 month rolling basis",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/CYPActualPlot.png"),
       plot = CYPActualPlot,
       width = PNGWidth,
       height = 9.7,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# IAPT Access - monthly ---------------------------------------------------

# Graphic
IAPTAccessPlot <-
  ggplot(data = IAPTAccess) +
  geom_line(aes(x = `Date`,
                y = `IAPTAccess`),
            size = 0.7,
            colour = NHSBlue) +
  geom_line(aes(x = `Date`,
                y = `Target`),
            linetype = "dashed",
            color = NHSMidGrey,
            size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1100),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(IAPTAccess$`Date`),
                              to = max(IAPTAccess$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 940,
                label = "Local Target"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("IAPT Access"),
       subtitle = "<span style='color:#005EB8;'>Somerset System: the number of people receiving psychological therapies </span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/IAPTAccessPlot.png"),
       plot = IAPTAccessPlot,
       width = PNGWidth,
       height = 9.8,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# IAPT 6wk wait - patient level -------------------------------------------

# Graphic
IAPT6wwPlot <-
  ggplot(data = IAPT6Week,
         aes(x = `Date`,
             y = `Count`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0.4,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(IAPT6Week$`Date`),
                              to = max(IAPT6Week$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  geom_hline(yintercept = 0.75,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.76,
                label = "National Ambition = 75%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("IAPT 6 week wait standard"),
       subtitle = "<span style='color:#005EB8;'>Somerset System: % referrals that finish a course of treatment who received their first treatment within 6 weeks</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/IAPT6wwPlot.png"),
       plot = IAPT6wwPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




# IAPT 90d -------------------------------------------

IAPT90dData <-
  CDPdata |> 
  filter(`IAPT 90d` > 0)

# Graphic
IAPT90dPlot <-
  ggplot(data = IAPT90dData,
         aes(x = `Month`,
             y = `IAPT 90d`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                0.5),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(IAPT90dData$`Month`),
                              to = max(IAPT90dData$`Month`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  geom_hline(yintercept = 0.1,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.11,
                label = "National Ambition = 10%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("IAPT second stage waits"),
       subtitle = "<span style='color:#005EB8;'>Somerset System: % of patients waiting >90 days between their first and second treatment</span>",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/IAPT90dPlot.png"),
       plot = IAPT90dPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# CYP ED Routine ----------------------------------------------------------


EDRoutineData <-
  CYP_ED2 |> 
  filter(`EDRoutine` > 0)

# Graphic
EDRoutinePlot <-
  ggplot(data = EDRoutineData,
         aes(x = as.Date(`Date`),
             y = `EDRoutine`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0.95,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.96,
                label = "National Ambition = 95%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(as.Date(EDRoutineData$`Date`)),
                              to = max(as.Date(EDRoutineData$`Date`)),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_text(colour = NHSBlue)) +
  labs(title = paste0("Children and Young People accessing Routine Eating Disorder Services"),
       subtitle = "Somerset System: % of CYP with ED (routine cases) referred with a suspected ED that start treatment within 4 weeks of \nreferral on a rolling 12 month basis",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/EDRoutinePlot.png"),
       plot = EDRoutinePlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")

# CYP ED Urgent ----------------------------------------------------------

EDUrgentData <-
  CYP_ED2 |> 
  filter(`EDUrgent` > 0)

# Graphic
EDUrgentPlot <-
  ggplot(data = EDUrgentData,
         aes(x = as.Date(`Date`),
             y = `EDUrgent`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0.95,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_text(aes(x = as.Date("2021-04-01"),
                y = 0.96,
                label = "National Ambition = 95%"),
            hjust = 0,
            vjust = 0,
            colour = NHSMidGrey,
            size = 2,
            check_overlap = TRUE) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(as.Date(EDUrgentData$`Date`)),
                              to = max(as.Date(EDUrgentData$`Date`)),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_text(colour = NHSBlue)) +
  labs(title = paste0("Children and Young People accessing Urgent Eating Disorder Services"),
       subtitle = "Somerset System: % of CYP with ED (urgent cases) referred with a suspected ED that start treatment within 1 week of \nreferral on a rolling 12 month basis",
       caption = paste0("Source: Referral to Treatment MDS, updated on ",
                        SystemTime)) 
# Output
ggsave(file = paste0("Outputs/MentalHealth/EDUrgentPlot.png"),
       plot = EDUrgentPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# SMI ---------------------------------------------------------------------

# Graphic
SMIPlot <-
  ggplot(data = SMI,
         aes(x = `Date`,
             y = `Count`)) +
  geom_bar(stat='identity',
           fill = NHSBlue)+
  geom_hline(yintercept = 0.6,
             linetype = "dashed",
             color = NHSMidGrey,
             size = 0.7) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                1.01),
                     expand = c(0,
                                0),
                     labels = percent) +
  LinePlotTheme() +
  labs(title = paste0("Physical Health Checks for People with serious mental illness"),
       subtitle = paste0("% physical health checks undertaken for people with serious mental illness"),
       caption = paste0("Source: SMI dataset, updated on ",
                        SystemTime))

# Output
ggsave(file = paste0("Outputs/MentalHealth/SMIPlot.png"),
       plot = SMIPlot,
       width = PNGWidth,
       height = 9.7,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")
