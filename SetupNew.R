# ColourPalette -----------------------------------------------------------
# NHS
NHSBlue <- "#005EB8"
NHSWhite <- "#FFFFFF"
NHSDarkBlue <- "#003087"
NHSBrightBlue <- "#0072CE"
NHSLightBlue <- "#41B6E6"
NHSAquaBlue <- "#00A9CE"
NHSBlack <- "#231F20"
NHSDarkGrey <- "#425563"
NHSMidGrey <- "#768692"
NHSPaleGrey <- "#E8EDEE"
NHSDarkGreen <- "#006747"
NHSGreen <- "#009639"
NHSLightGreen <- "#78BE20"
NHSAquaGreen <- "#00A499"
NHSPurple <- "#330072"
NHSDarkPink <- "#7C2855"
NHSPink <- "#AE2573"
NHSDarkRed <- "#8A1538"
NHSOrange <- "#ED8B00"
NHSWarmYellow <- "#FFB81C"
NHSYellow <- "#FAE100"
NHSText <- "#212B32"
NHSTextSecondary <- "#4C6272"
NHSLink <- "#005EB8"
NHSLinkHover <- "#7C2855"
NHSLinkVisited <- "#330072"
NHSLinkActive <- "#002F5C"
NHSFocus <- "#FFEB3B"
NHSFocusText <- "#212B32"
NHSBorder <- "#D8DDE0"
NHSBorderForm <- "#4C6272"
NHSError <- "#D5281B"
NHSButton <- "#007F3B"
NHSButtonSecondary <- "#4C6272"

# Viridis
Viridis1 <- "#440154"
Viridis2 <- "#481568"
Viridis3 <- "#482677"
Viridis4 <- "#453781"
Viridis5 <- "#3F4788"
Viridis6 <- "#39558C"
Viridis7 <- "#32648E"
Viridis8 <- "#2D718E"
Viridis9 <- "#287D8E"
Viridis10 <- "#238A8D"
Viridis11 <- "#1F968B"
Viridis12 <- "#20A386"
Viridis13 <- "#29AF7F"
Viridis14 <- "#3CBC75"
Viridis15 <- "#55C667"
Viridis16 <- "#74D055"
Viridis17 <- "#94D840"
Viridis18 <- "#B8DE29"
Viridis19 <- "#DCE318"
Viridis20 <- "#FDE725"
Magma1 <- "#000004"
Magma2 <- "#07071D"
Magma3 <- "#160F3B"
Magma4 <- "#29115A"
Magma5 <- "#400F73"
Magma6 <- "#56147D"
Magma7 <- "#6B1D81"
Magma8 <- "#802582"
Magma9 <- "#952C80"
Magma10 <- "#AB337C"
Magma11 <- "#C03A76"
Magma12 <- "#D6456C"
Magma13 <- "#E85362"
Magma14 <- "#F4685C"
Magma15 <- "#FA815F"
Magma16 <- "#FD9A6A"
Magma17 <- "#FEB37B"
Magma18 <- "#FECC8F"
Magma19 <- "#FDE4A6"
Magma20 <- "#FCFDBF"

# Packages ----------------------------------------------------------------
# R
library(DBI)
library(dplyr)
library(dbplyr)
library(ggtext)
library(gt)
library(htmltools)
library(htmlwidgets)
library(httr)
library(jsonlite)
library(knitr)
library(lubridate)
library(odbc)
library(patchwork)
library(plotly)
library(readxl)
library(scales)
library(stringr)
library(svglite)
library(tidyverse)
library(viridis)
library(writexl)
library(zoo)

# Fonts -------------------------------------------------------------------
windowsFonts(`Bierstadt` = windowsFont("Bierstadt"))

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 10
PNGHeight <- 6
PNGDPI <- 300

# DateTime ----------------------------------------------------------------
# Time
SystemTime <- format(Sys.time(),
                     '%A %d %B %Y')
# Start Month Date
StartMonthDate <- "2021-04-01"


# LinePlotTheme -----------------------------------------------------------
# Function
LinePlotTheme <- function() {
  theme(plot.title = element_text(family = "Bierstadt",
                                  size = 10,
                                  margin = margin(6,
                                                  0,
                                                  2,
                                                  0),
                                  color = NHSBlack,
                                  face = "bold",
                                  hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Bierstadt",
                                     size = 8,
                                     margin = margin(2,
                                                     0,
                                                     6,
                                                     0),
                                     colour = NHSBlack),
        plot.caption = element_text(family = "Bierstadt",
                                    size = 6,
                                    color = NHSMidGrey,
                                    margin = margin(2,
                                                    0,
                                                    4,
                                                    0),
                                    hjust = 0),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = NHSWhite),
        legend.position = "none",
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Bierstadt",
                                   size = 10,
                                   color = NHSBlack),
        legend.justification = 'left',
        legend.margin = margin(-0.2,
                               0,
                               0.2,
                               -1.7,
                               "cm"),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(family = "Bierstadt",
                                 size = 8,
                                 color = NHSBlack),
        axis.text.x = element_text(margin = margin(5,
                                                   0,
                                                   4,
                                                   0)),
        axis.text.y = element_text(margin = margin(0,
                                                   5,
                                                   0,
                                                   0)),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = NHSPaleGrey,
                                   size = 0.25),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = NHSPaleGrey,
                                          size = 0.25),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = NHSWhite,
                                        colour = NHSWhite,
                                        size = 0.5,
                                        linetype = "solid"),
        strip.background = element_blank(),
        strip.text = element_text(size = 8,
                                  hjust = 0),
        plot.margin = unit(c(0,
                             0.2,
                             0,
                             0.2),
                           "cm"))}

# BarPlotTheme ------------------------------------------------------------
# Function
BarPlotTheme <- function() {
  theme(plot.title = element_text(family = "Bierstadt",
                                  size = 10,
                                  margin = margin(6,
                                                  0,
                                                  2,
                                                  0),
                                  color = NHSBlack,
                                  face = "bold",
                                  hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Bierstadt",
                                     size = 8,
                                     margin = margin(2,
                                                     0,
                                                     6,
                                                     0),
                                     colour = NHSBlack),
        plot.caption = element_text(family = "Bierstadt",
                                    size = 6,
                                    color = NHSMidGrey,
                                    margin = margin(2,
                                                    0,
                                                    4,
                                                    0),
                                    hjust = 0),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = NHSWhite),
        legend.position = "none",
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Bierstadt",
                                   size = 10,
                                   color = NHSBlack),
        legend.justification = 'left',
        legend.margin = margin(-0.2,
                               0,
                               0.2,
                               -4.68,
                               "cm"),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(family = "Bierstadt",
                                 size = 8,
                                 color = NHSBlack),
        axis.text.x = element_text(margin = margin(5,
                                                   0,
                                                   4,
                                                   0)),
        axis.text.y = element_text(margin = margin(0,
                                                   5,
                                                   0,
                                                   0)),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_line(color = NHSBlack,
                                   size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = NHSPaleGrey,
                                          size = 0.25),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = NHSWhite,
                                        colour = NHSWhite,
                                        size = 0.5,
                                        linetype = "solid"),
        strip.background = element_rect(fill = NHSWhite),
        strip.text = element_text(size = 8,
                                  hjust = 0),
        plot.margin = unit(c(0,
                             0.2,
                             0,
                             0.2),
                           "cm"))}

# BarFlipPlotTheme --------------------------------------------------------
# Function
BarFlipPlotTheme <- function() {
  theme(plot.title = element_text(family = "Bierstadt",
                                  size = 10,
                                  margin = margin(12,
                                                  0,
                                                  4,
                                                  0),
                                  color = NHSBlack,
                                  face = "bold",
                                  hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Bierstadt",
                                     size = 8,
                                     margin = margin(4,
                                                     0,
                                                     12,
                                                     0),
                                     colour = NHSBlack),
        plot.caption = element_text(family = "Bierstadt",
                                    size = 6,
                                    color = NHSMidGrey,
                                    margin = margin(4,
                                                    0,
                                                    8,
                                                    0),
                                    hjust = 0),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = NHSWhite),
        legend.position = "none",
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Bierstadt",
                                   size = 10,
                                   color = NHSBlack),
        legend.justification = 'left',
        legend.margin = margin(-0.2,
                               0,
                               0.2,
                               -4.68,
                               "cm"),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(family = "Bierstadt",
                                 size = 8,
                                 color = NHSBlack),
        axis.text.x = element_text(margin = margin(5,
                                                   0,
                                                   10,
                                                   0)),
        axis.text.y = element_text(margin = margin(0,
                                                   5,
                                                   0,
                                                   0)),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.line.y = element_line(color = NHSBlack,
                                   size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = NHSPaleGrey,
                                          size = 0.25),
        panel.background = element_rect(fill = NHSWhite,
                                        colour = NHSWhite,
                                        size = 0.5,
                                        linetype = "solid"),
        strip.background = element_rect(color = NHSWhite,
                                        fill = NHSWhite),
        strip.text = element_text(size = 10,
                                  hjust = 0),
        plot.margin = unit(c(0,
                             1,
                             0,
                             0.4),
                           "cm"))}

# MapPlotTheme ------------------------------------------------------------
# Function
MapPlotTheme <- function() {
  theme(plot.title = element_text(family = "Bierstadt",
                                  size = 10,
                                  margin = margin(6,
                                                  0,
                                                  2,
                                                  0),
                                  color = NHSBlack,
                                  face = "bold",
                                  hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Bierstadt",
                                     size = 8,
                                     margin = margin(2,
                                                     0,
                                                     6,
                                                     0),
                                     colour = NHSBlack),
        plot.caption = element_text(family = "Bierstadt",
                                    size = 6,
                                    color = NHSMidGrey,
                                    margin = margin(2,
                                                    0,
                                                    4,
                                                    0),
                                    hjust = 0),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = NHSWhite),
        legend.position = "none",
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Bierstadt",
                                   size = 10,
                                   color = NHSBlack),
        legend.justification = 'left',
        legend.margin = margin(-2,
                               0,
                               0.2,
                               0,
                               "cm"),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = NHSWhite,
                                        colour = NHSWhite,
                                        size = 0.5,
                                        linetype = "solid"),
        strip.background = element_rect(fill = NHSWhite),
        strip.text = element_text(size = 8,
                                  hjust = 0),
        plot.margin = unit(c(0,
                             1,
                             0,
                             0.4),
                           "cm"))}

# MonthCommProvMutate -----------------------------------------------------
# Function
MonthCommProvMutate <- function(x) {
  x |>
    mutate(`MonthDate` = floor_date(as.Date(`Date`),
                                    "month"),
           `CommissionerCode` = case_when(substr(`CommissionerCode`,
                                                 1,
                                                 3) %in% c("5QL",
                                                           "11X") ~ "11X",
                                          TRUE ~ "OTH"),
           `CommissionerDescription` = case_when(substr(`CommissionerCode`,
                                                        1,
                                                        3) %in% c("5QL",
                                                                  "11X") ~ "Somerset",
                                                 TRUE ~ "Other"),
           `ProviderCode` = case_when(substr(`ProviderCode`,
                                             1,
                                             3) %in% c("RBA",
                                                       "RH5") ~ "RH5",
                                      substr(`ProviderCode`,
                                             1,
                                             3) %in% c("RA4") ~ "RA4",
                                      substr(`ProviderCode`,
                                             1,
                                             3) %in% c("RD1") ~ "RD1",
                                      substr(`ProviderCode`,
                                             1,
                                             3) %in% c("RA3",
                                                       "RA7") ~ "RA7",
                                      TRUE ~ "OTH"),
           `ProviderDescription` = case_when(substr(`ProviderCode`,
                                                    1,
                                                    3) %in% c("RBA",
                                                              "RH5") ~ "Taunton",
                                             substr(`ProviderCode`,
                                                    1,
                                                    3) %in% c("RA4") ~ "Yeovil",
                                             substr(`ProviderCode`,
                                                    1,
                                                    3) %in% c("RD1") ~ "Bath",
                                             substr(`ProviderCode`,
                                                    1,
                                                    3) %in% c("RA3",
                                                              "RA7") ~ "BristolWeston",
                                             TRUE ~ "Other"))}

# ECDSDateCommProvFilter --------------------------------------------------
# Function
ECDSDateCommProvFilter <- function(x) {
  x |>
    filter(as.Date(`CDS_Activity_Date`) >= "2018-04-01",
           as.Date(`CDS_Activity_Date`) <= "2023-03-31",
           (substr(`Organisation_Code_Commissioner`,
                   1,
                   3) %in% c("5QL",
                             "11X") |
              substr(`Organisation_Code_Provider`,
                     1,
                     3) %in% c("RBA",
                               "RH5",
                               "RA4")))}


# CommPlotGroup -----------------------------------------------------------
# Function
CommProvPlotGroup <- function(x) {
  x |>
    filter(`CommissionerDescription` == "Somerset",
           `ProviderDescription` != "Other") |> 
    select(`MonthDate`,
           `CommissionerDescription`,
           `ProviderDescription`,
           `Activity`) |>
    group_by(`MonthDate`,
             `CommissionerDescription`,
             `ProviderDescription`) |>
    summarise(`Activity` = sum(`Activity`)) |>
    ungroup() |>
    collect()}

# CommPlotCalDayGroup -----------------------------------------------------
# Function
CommPlotCalDayGroup <- function(x) {
  x |>
    left_join(CalWorDaysSQLData,
              by = c("MonthDate" = "MonthDate")) |>
    filter(`DayType` == "Calendar") |>
    mutate(`ActivityAdjusted` = `Activity` / `Days`) |>
    select(`MonthDate`,
           `CommissionerDescription`,
           `ActivityAdjusted`)}

# APCDateCommProvSpecFilter -----------------------------------------------
# Function
APCDateCommProvSpecFilter <- function(x) {
  x |>
    filter(as.Date(`DischargeDate_FromHospitalProviderSpell`) >= StartMonthDate,
           as.Date(`DischargeDate_FromHospitalProviderSpell`) <= "2023-03-31",
           (substr(`OrganisationCode_CodeOfCommissioner`,
                   1,
                   3) %in% c("5QL",
                             "11X") |
              substr(`OrganisationCode_CodeOfProvider`,
                     1,
                     3) %in% c("RBA",
                               "RH5",
                               "RA4")),
           substr(`TreatmentFunctionCode`,
                  1,
                  3) %in% c("100",
                            "101",
                            "102",
                            "103",
                            "104",
                            "105",
                            "106",
                            "107",
                            "108",
                            "109",
                            "110",
                            "111",
                            "113",
                            "115",
                            "120",
                            "130",
                            "140",
                            "141",
                            "142",
                            "143",
                            "144",
                            "145",
                            "150",
                            "160",
                            "161",
                            "170",
                            "171",
                            "172",
                            "173",
                            "174",
                            "180",
                            "190",
                            "191",
                            "192",
                            "200",
                            "211",
                            "212",
                            "213",
                            "214",
                            "215",
                            "216",
                            "217",
                            "218",
                            "219",
                            "220",
                            "221",
                            "222",
                            "230",
                            "240",
                            "241",
                            "242",
                            "250",
                            "251",
                            "252",
                            "253",
                            "254",
                            "255",
                            "256",
                            "257",
                            "258",
                            "259",
                            "260",
                            "261",
                            "262",
                            "263",
                            "264",
                            "270",
                            "280",
                            "300",
                            "301",
                            "302",
                            "303",
                            "304",
                            "305",
                            "306",
                            "307",
                            "308",
                            "309",
                            "310",
                            "311",
                            "313",
                            "314",
                            "315",
                            "316",
                            "317",
                            "318",
                            "319",
                            "320",
                            "321",
                            "322",
                            "323",
                            "324",
                            "325",
                            "326",
                            "327",
                            "328",
                            "329",
                            "330",
                            "333",
                            "335",
                            "340",
                            "341",
                            "342",
                            "343",
                            "347",
                            "348",
                            "350",
                            "352",
                            "361",
                            "370",
                            "371",
                            "400",
                            "401",
                            "410",
                            "420",
                            "421",
                            "422",
                            "430",
                            "431",
                            "450",
                            "451",
                            "460",
                            "461",
                            "502",
                            "203",
                            "505",
                            "663",
                            "670",
                            "673",
                            "675",
                            "677",
                            "800",
                            "811",
                            "812",
                            "822",
                            "834"))}

# DataSQLConnection -------------------------------------------------------
# Data Mart
DataSQLConnectionDM <-
  dbConnect(odbc(),
            driver = "SQL Server",
            server = "BIS-000-SP08.bis.xswhealth.nhs.uk, 14431",
            database = "Data_Mart",
            trustedconnection = TRUE)

# Analyst SQL Area
DataSQLConnectionASA <-
  dbConnect(odbc(),
            driver = "SQL Server",
            server = "BIS-000-SP08.bis.xswhealth.nhs.uk, 14431",
            database = "Analyst_SQL_Area",
            trustedconnection = TRUE)


