library(htmltools)
library(rvest)
library(xml2)



# Data ----------------------------------------------------------

Quality_Scorecard <- read_excel("S:/Shared Area/Somerset Performance/Nursing and Patient Safety/QUALITY DEVELOPMENT/Quality Scorecard.xlsx", 
                                sheet = "Sheet1", col_types = c("numeric", 
                                                                "text", "text", "text", "text", "text", 
                                                                "text", "text", "text", "text", "text", 
                                                                "text", "text", "date", "numeric", 
                                                                "numeric", "text")) %>% 
  filter(Month >= "2021-04-01",
         Measure_Description %in% c("Number of acute hospital acquired pressure ulcers (category 2 and above)",
                                    "Number of (acute ward) bed days",
                                    "Number of adult inpatients (acute setting) having had nutrition screening using a validated tool within 24 hours of admission",
                                    "Number of adults admitted as inpatients (acute setting, exclude day units and areas formally excluded in agreement with the PCO)",
                                    "Number of adult inpatients (community setting) having had nutrition screening using a validated tool within 24 hours of admission",
                                    "Number of adults admitted as inpatients (community setting)",
                                    "Percentage of adult inpatients (acute setting) having had nutrition screening using a validated tool within 24 hours of admission",
                                    "Percentage of staff sick / absent (12 month rolling average)",
                                    "Percentage of staff who have undertaken mandatory training (total applicable courses)",
                                    "Number of patients who have received an annual review (CPA Level 2)",
                                    "Number of patients requiring annual review (CPA Level 2)",
                                    "CLA - Number of children who received an Initial Health Assessment within 20 working days",
                                    "CLA - Total number of children who became looked after in month",
                                    "Number of referrals concluded within 28 days",
                                    "Number of deferred assessments",
                                    "CLA - Number of children looked after for more than one year that have had their dental checks",
                                    "CLA - Total number of children looked after for more than one year",
                                    "Number of patient slips, trips and falls (irrespective of grade)",
                                    "Number of occupied bed days")) %>% 
  select(Month, Organisation, Measure_Description, Value) %>% 
  pivot_wider(names_from = Measure_Description, values_from = Value) %>% 
  mutate(Metric1_PressureUlcers = 1000 * `Number of acute hospital acquired pressure ulcers (category 2 and above)` / `Number of (acute ward) bed days`,
         Metric2_NutritionScreening_YDH = `Percentage of adult inpatients (acute setting) having had nutrition screening using a validated tool within 24 hours of admission`,
         Metric2_NutritionScreening_SFTAcute = `Number of adult inpatients (acute setting) having had nutrition screening using a validated tool within 24 hours of admission` / `Number of adults admitted as inpatients (acute setting, exclude day units and areas formally excluded in agreement with the PCO)`,
         Metric2_NutritionScreening_SFTCommunity = `Number of adult inpatients (community setting) having had nutrition screening using a validated tool within 24 hours of admission` /  `Number of adults admitted as inpatients (community setting)`,
         Metric3_StaffAbscence = `Percentage of staff sick / absent (12 month rolling average)`,
         Metric4_MandatoryTraining = `Percentage of staff who have undertaken mandatory training (total applicable courses)`,
         Metric5_CPAReview = `Number of patients who have received an annual review (CPA Level 2)` / `Number of patients requiring annual review (CPA Level 2)`,
         Metric6_CHC28Day = `Number of referrals concluded within 28 days`,
         Metric7_CHCBacklog = `Number of deferred assessments`,
         Metric8_DentalCheck = `CLA - Number of children looked after for more than one year that have had their dental checks`/ `CLA - Total number of children looked after for more than one year`,
         Metric9_Falls = 1000*`Number of patient slips, trips and falls (irrespective of grade)`/`Number of occupied bed days`,
         Mertric10_InitialAssesment = `CLA - Number of children who received an Initial Health Assessment within 20 working days`/ `CLA - Total number of children who became looked after in month`) |> 
  mutate(Month = as.Date(Month))



# National Staff Abscence Rates ----------------------------------------------------------




#Specifying the url for desired website to be scraped
url <- paste("https://digital.nhs.uk/data-and-information/publications/statistical/nhs-sickness-absence-rates")

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
web_data_html <- html_nodes(webpage,'.cta__button')

#Converting the ranking data to text
web_data <- xml_attrs(web_data_html[[1]]) %>% 
  data.frame() %>% 
  head(1) %>% 
  pull()

url2 <- paste0("https://digital.nhs.uk",web_data)



#Reading the HTML code from the website
webpage2 <- read_html(url2)

#Using CSS selectors to scrape the rankings section
web_data_html2 <- html_nodes(webpage2,'.nhsd-a-box-link')[9] 

web_data2 <- xml_attrs(web_data_html2[[1]]) %>% 
  data.frame() %>% 
  head(1) %>% 
  pull()

destfile <- "NationalAbsenceRates.xlsx"
curl::curl_download(web_data2, destfile)
NationalAbsenceRates <- read_excel(destfile, sheet = "Table 3", skip = 2) |> 
  select(`...1`, Acute) |> 
  drop_na() |> 
  mutate(Date = paste("01",`...1`)) |> 
  mutate(Date = as.Date(Date, "%d %B %Y")) |> 
  mutate(Absence = rollmean(Acute, k=12, fill=NA, align = "right")) |> 
  filter(Date >= '2021-04-01') |> 
  select(Date, Absence)
  

