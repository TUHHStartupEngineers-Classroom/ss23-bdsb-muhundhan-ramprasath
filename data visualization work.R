library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/02_wrangled_data/bike_orderlines.rds")

sales_by_year_tbl <- bike_orderlines_tbl %>%
  select(order_date,total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales,big.mark = ".",
                                          ,decimal.mark = ","
                                          ,prefix = ""
                                          ,suffix = "€"))

#Aesthetic mapping
sales_by_year_tbl %>% ggplot(aes(x = year, y = sales, color = sales)) +
  geom_line(size = 1) +
  geom_point(size = 3, color = "red") +
  geom_smooth(method = "lm", se = FALSE)

order_value_tbl <- bike_orderlines_tbl %>% 
  group_by(order_id) %>%
  summarize(total_quantity = sum(quantity),
            total_price = sum(total_price)) %>% ungroup()

order_value_tbl %>% ggplot(aes(x = total_quantity, y = total_price)) + 
  geom_point(alpha = 0.5,size = 2) +
  geom_smooth(method = "lm",se= FALSE)


revenue_by_month_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_month_tbl %>% ggplot(aes(year_month, revenue)) + 
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess",span = 0.2)


revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()

bike_orderlines_tbl %>% distinct(model,price) %>%
  ggplot(aes(price)) +
  geom_histogram(bins = 25, fill = "blue", color = "white")

sales_by_year_tbl %>% 
  ggplot(aes(x = year, y = sales_text,color = sales)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +
  expand_limits(y = 0) + 
  scale_color_continuous(low    = "#95E1EA", high = "#2097A3", 
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) +
  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )

#Business Case
#Case 1

#Load Libraries
library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/02_wrangled_data/bike_orderlines.rds")

#Data Manipulation
n <- 10
top_customer_tbl <- bike_orderlines_tbl %>% 
  select(bikeshop,total_price) %>%
  mutate(bikeshop = as_factor(bikeshop) %>% fct_lump(n = n, w = total_price)) %>% 
  group_by(bikeshop) %>% 
  summarize(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  mutate(bikeshop = bikeshop %>% fct_reorder(revenue)) %>%
  mutate(bikeshop = bikeshop %>% fct_relevel("Other", after = 0)) %>%
  arrange(desc(bikeshop)) %>%
  mutate(revenue_text = scales::dollar(revenue, 
                                       scale  = 1e-6, 
                                       prefix = "", 
                                       suffix = "M €")) %>%
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>%
  mutate(cum_pct_text = scales::percent(cum_pct)) %>%
  mutate(rank = row_number()) %>%
  mutate(rank = case_when(
    rank == max(rank) ~ NA_integer_,
    TRUE ~ rank
  )) %>%
  mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))

#Data Visualization
top_customer_tbl %>% ggplot(aes(revenue, bikeshop)) +
  geom_segment(aes(xend = 0, yend = bikeshop), 
               color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
               size  = 1) +
  
  geom_point(aes(size = revenue),
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  geom_label(aes(label = label_text), 
             hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M €")) +
  labs(
    title = str_glue("Top {n} Customers"),
    subtitle = str_glue(
      "Start: {year(min(bike_orderlines_tbl$order_date))}
               End:  {year(max(bike_orderlines_tbl$order_date))}"),
    x = "Revenue (M €)",
    y = "Customer",
    caption = str_glue("Top 6 customers contribute
                           52% of purchasing power.")
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )


#Case 2
#Data Manipulation
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain","Road")) %>% 
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%
  complete(bikeshop, nesting(category_1, category_2)) %>% 
  mutate(across(total_qty, ~replace_na(., 0))) %>% 
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  mutate(bikeshop_num = as.numeric(bikeshop))

#Data Visualization
pct_sales_by_customer_tbl %>% ggplot(aes(category_2, bikeshop)) +
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)), 
            size = 3) +
  facet_wrap(~ category_1, scales = "free_x") +
  scale_fill_gradient(low = "white", high = "#2C3E50") +
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road: 
        To be discussed ...
        
        Customers that prefer Mountain: 
        To be discussed ...")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )