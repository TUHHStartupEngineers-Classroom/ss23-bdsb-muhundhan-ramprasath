library(tidyverse)
library("readxl")
bikes_tbl <- read_excel("00_data/01_raw_data/bikes.xlsx") %>% 
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bikes_tbl %>%
  select(bike_id, model, model_year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))

bikes_tbl %>%
  select(category_1:category_3, everything())

bikes_tbl %>%
  relocate(category_1:category_3)

bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>% 
  select(model,category_1,category_2,category_3,price) %>%
  set_names(c("Model","Bike Family","Ride Style","Bike Category","Price in Euro"))


bikes_tbl %>%
  select(model,price) %>%
  arrange(desc(price)) %>% 
  view

bikes_tbl %>% 
  select(model,price) %>%
  filter(price > mean(price)) %>% arrange(desc(price)) %>% view


bikes_tbl %>%
  select(model,price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>% 
  view()

bikes_tbl %>% 
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

bikes_tbl %>%
  filter(price > 5000, model %>% str_detect("Endurace"))

bikes_tbl %>%
  filter(category_2 %in% c("E-Mountain"))

bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>% 
  arrange(desc(price)) %>% 
  slice(1:5)

bikes_tbl %>% 
  arrange(desc(price)) %>%
  slice(nrow(.)-4:nrow(.))

bikes_tbl %>% 
  distinct(category_1, category_2, category_3)

bike_orderlines_tbl <- read_rds("ds_data/01_bike_sales/02_wrangled_data/orderlines.rds")

library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
class(covid_data_dt)


test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
write.csv(test_df, 'test_df.csv', row.names = F)

system.time({test_df_base <- read.csv("test_df.csv")})
system.time({test_dt <- fread("test_df.csv")})


test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)

covid_data_dt[year == 2019, sum(cases), by = continentExp]

covid_data_dt[countriesAndTerritories == "Germany" & lubridate::month(dateRep, label = T, abbr = F) == "June"]
covid_data_dt[1:2]
covid_data_dt[,geoId]
covid_data_dt[,c("geoId", "countriesAndTerritories")]
covid_data_dt[,list(geoId)]
covid_data_dt[,.(geoId)]
covid_data_dt[,.(geoId, countriesAndTerritories)]
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

data()
data("airquality")
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone),.(Solar.R,Temp,Wind)]

covid_data_dt[,sum(deaths > 1000)]
covid_data_dt[deaths > 1000]

covid_data_dt[,death_per_capita := deaths / popData2019]

#Exercise
data("mtcars")
mtcars_dt <- data.table(mtcars)
mtcars_dt[,mileage_type := ifelse(mpg>20,"high","low")]

covid_data_dt[countriesAndTerritories == "Germany" & month == 4,.(m_cases = mean(cases),m_death = mean(deaths))]
covid_data_dt[deaths > 1000, .N, by = countriesAndTerritories]
library(magrittr)
mtcars_dt[,.(.N, mileage = mean(mpg) %>% round(2)), by=gear]

covid_cases_means <- covid_data_dt[,.(m_cases = mean(cases)%>% round(1), m_death = mean(deaths) %>% round(1)), by = .(countriesAndTerritories)][order(-m_cases)]
covid_data_dt[, .N, 
              .(
                death_gt_1k = deaths > 1000, 
                cases_lt_1k = cases < 1000
              )
]

covid_data_dt[, print(.SD), by = year]
covid_data_dt[, lapply(.SD, mean), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]

#Business Case
library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))

acquisition_data <- vroom(
  file       = "loan_data/Acquisition_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_acq),
  col_types  = col_types_acq,
  na         = c("", "NA", "NULL"))

acquisition_data %>% glimpse()

# 2.2 Performance Data ----
col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))

performance_data %>% glimpse()

# 3.1 Acquisition Data ----
class(acquisition_data)

setDT(acquisition_data)

class(acquisition_data)

acquisition_data %>% glimpse()

# 3.2 Performance Data ----
setDT(performance_data)

performance_data %>% glimpse()

# 4.0 DATA WRANGLING ----

tic()
combined_data <- merge(x = acquisition_data, y = performance_data, 
                       by    = "loan_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()
combined_data %>% glimpse()

setkey(combined_data, "loan_id")
key(combined_data)
setorderv(combined_data, c("loan_id", "monthly_reporting_period"))

combined_data %>% dim()
keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")

combined <- combined_data[, ..keep_cols]
combined_data %>% dim()
combined_data %>% glimpse()
combined_data[,current_loan_delinquency_status] %>% unique()

tic()
temp <- combined_data %>%
  group_by(loan_id) %>%
  mutate(gt_1mo_behind_in_3mo_dplyr = lead(current_loan_delinquency_status, n = 3) >= 1) %>%
  ungroup()  
toc()

combined_data %>% dim()
temp %>% dim()

tic()
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]
toc()

combined_data %>% dim()

rm(temp)

tic()
combined_data[!is.na(monthly_reporting_period),.N, by = monthly_reporting_period]
toc()

tic()
combined_data[current_loan_delinquency_status >= 1, 
              list(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)][
                , max(current_loan_delinquency_status), by = loan_id][
                  order(V1, decreasing = TRUE)]
toc()

tic()
combined_data[current_loan_delinquency_status >= 1, .SD[.N], by = loan_id][
  !is.na(current_upb)][
    order(-current_upb), .(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)  
  ]
toc()