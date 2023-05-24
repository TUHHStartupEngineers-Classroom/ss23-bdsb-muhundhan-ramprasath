url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
library(rvest)
sp_500 <- url %>%
  read_html() %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>% 
  .[[1]] %>% 
  tibble:: as_tibble()
sp_500

url2 <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

html <- url2 %>% read_html()

rank <- html %>% html_nodes(css = ".titlecolumn") %>%
  html_text() %>%
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)") %>%
  as.numeric()

title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

bike_data_lst <- fromJSON("bike_data.json")
View(bike_data_lst)

bike_data_lst[["productDetail"]][["variationAttributes"]][["values"]][[1]][["displayValue"]]

bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")