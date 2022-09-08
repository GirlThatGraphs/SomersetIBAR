# Run Setup script
source("SetupNew.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 16
PNGHeight <- 7
PNGUnits <- "cm"
PNGDPI <- 300


# Data ----------------------------------------------------------

Maternity_1 <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Maternity/a- Joint Maternity Dashboard/2022-23/M4 - July/22-23Joint maternity dashboard with MDT July22v0.xlsx", 
                          sheet = "LMS Dashboard", range = "AR88:BO88", col_names = FALSE) %>% 
  mutate(Metric = "Metric1_SATOD")


Maternity_2 <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Maternity/a- Joint Maternity Dashboard/2022-23/M4 - July/22-23Joint maternity dashboard with MDT July22v0.xlsx", 
                          sheet = "LMS Dashboard", range = "AR36:BO36", col_names = FALSE) %>% 
  mutate(Metric = "Denominator")


Maternity_3 <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Maternity/a- Joint Maternity Dashboard/2022-23/M4 - July/22-23Joint maternity dashboard with MDT July22v0.xlsx", 
                          sheet = "LMS Dashboard", range = "AR39:BO39", col_names = FALSE) %>% 
  mutate(Metric = "Metric2_Numerator")


Maternity_4 <- read_excel("//somerset.xswhealth.nhs.uk/CCG/Directorate/Shared Area/Somerset Performance/Secondary Care/Maternity/a- Joint Maternity Dashboard/2022-23/M4 - July/22-23Joint maternity dashboard with MDT July22v0.xlsx", 
                          sheet = "LMS Dashboard", range = "AR116:BO116", col_names = FALSE) %>% 
  mutate(Metric = "Metric3_Numerator")


Maternity <- rbind(Maternity_1,
                   Maternity_2,
                   Maternity_3,
                   Maternity_4) %>% 
  pivot_longer(!Metric, names_to = "Date", values_to = "Count") %>% 
  pivot_wider(names_from = Metric, values_from = Count) %>% 
  mutate(Date =seq(as.Date("2021-04-01"), as.Date("2023-03-01"), by="month"),
         Metric2_PreTerm = 1000*Metric2_Numerator/Denominator,
         Metric3_StillBirths = 1000*Metric3_Numerator/Denominator) %>% 
  drop_na() %>% 
  select(Date, Metric1_SATOD, Metric2_PreTerm, Metric3_StillBirths)


# Maternity SATOD ---------------------------------------------------------

MatSATODData <-
  Maternity |>
  pivot_longer(!Date,
               names_to = "Metric",
               values_to = "Value") |> 
  filter(`Metric` == "Metric1_SATOD")

MatSATODPlot <-
  ggplot(data = MatSATODData,
         aes(x = `Date`,
             y = `Value`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                0.2),
                     expand = c(0,
                                0),
                     labels = percent) +
  scale_x_date(breaks = c(seq(from = min(MatSATODData$`Date`),
                              to = max(MatSATODData$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("% of women smoking at time of delivery"),
       caption = paste0("Data source: Maternity dataset, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Maternity/MatSATODPlot.png"),
       plot = MatSATODPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Maternity PreTerm -------------------------------------------------------

MatPreTermData <-
  Maternity |>
  pivot_longer(!Date,
               names_to = "Metric",
               values_to = "Value") |> 
  filter(`Metric` == "Metric2_PreTerm")

MatPreTermPlot <-
  ggplot(data = MatPreTermData,
         aes(x = `Date`,
             y = `Value`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                101),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(MatPreTermData$`Date`),
                              to = max(MatPreTermData$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of Pre Term births per 1000 live births"),
       caption = paste0("Data source: Maternity dataset, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Maternity/MatPreTermPlot.png"),
       plot = MatPreTermPlot,
       width = PNGWidth,
       height = 3.5,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")


# Maternity Still Births --------------------------------------------------


MatStillBirthsData <-
  Maternity |>
  pivot_longer(!Date,
               names_to = "Metric",
               values_to = "Value") |> 
  filter(`Metric` == "Metric3_StillBirths")

MatStillBirthsPlot <-
  ggplot(data = MatStillBirthsData,
         aes(x = `Date`,
             y = `Value`)) +
  geom_line(size = 0.7,
            colour = NHSBlue) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(limits = c(0,
                                21),
                     expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks = c(seq(from = min(MatStillBirthsData$`Date`),
                              to = max(MatStillBirthsData$`Date`),
                              by = "3 months")),
               labels = date_format("%b%y")) +
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Number of neonatal deaths per 1000 live births (S023a)"),
       caption = paste0("Data source: Maternity dataset, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " "))

# Save Output
ggsave(file = paste0("Outputs/Maternity/MatStillBirthsPlot.png"),
       plot = MatStillBirthsPlot,
       width = PNGWidth,
       height = 3.5,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")
