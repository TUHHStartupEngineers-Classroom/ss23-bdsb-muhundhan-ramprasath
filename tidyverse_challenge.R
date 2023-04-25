# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "00_data/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
bikeshops_tbl
glimpse(bikeshops_tbl)
bikeshops_tbl %>% head(n=5)

# 4.0 Joining Data ----
left_join(bikeshops_tbl,orderlines_tbl, by = c("bikeshop.id" = "customer.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col = location, 
           into = c("City","State"),
           sep = ",") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1,-gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by State ----
library(lubridate)
# Step 1 - Manipulate
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(State,total_price) %>%
  group_by(State) %>%
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_state_tbl
# Step 2 - Visualize
sales_by_state_tbl %>% ggplot(aes(x = State, y =sales)) +
  geom_col(fill = "#2DC6D6") + 
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by State",
    subtitle = "State wise trend of sales",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Location ----

# Step 1 - Manipulate
sales_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date,total_price,State) %>%
  mutate(year = year(order_date)) %>%
  group_by(year,State) %>%
  summarise(sales = sum(total_price)) %>% 
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))


sales_by_year_state_tbl
# Step 2 - Visualize
sales_by_year_state_tbl %>% ggplot(aes(x = year, y = sales, fill = State)) + 
  geom_col() +
  facet_wrap(~ State) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €"))+
  labs(
    title = "Revenue by year and state",
    subtitle = "Each state different sales trend",
    fill = "State" 
  )



# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
