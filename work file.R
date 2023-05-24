# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

url_home          <- "https://www.canyon.com/en-de"
xopen(url_home)

html_home <- read_html(url_home)
bike_family_tbl <- html_home %>%
                   html_nodes(css = ".header__navBarPreloadItem--level1") %>%
                   html_text() %>%
                   discard(.p = ~stringr::str_detect(.x,"Service|Outlet")) %>%
                   enframe(name = "position", value = "family_class") %>%
                   mutate(
                   family_id = str_glue("#{family_class}")
                   )

bike_family_tbl

bike_category_tbl <- html_home %>%
                     html_nodes(css = ".header__navBarPreloadItem--level1") %>%
                     html_attr('href') %>%
                     enframe(name= "position", value = "subdirectory") %>%
                     mutate(
                       url = glue("https://www.canyon.com{subdirectory}")
                     )

bike_category_tbl

#2.0 Collect bike data
# 2.1 Get URL for each bike of the Product categories

bike_category_url <- bike_category_tbl$url[1]
xopen(bike_category_url)

html_bike_category <- read_html(bike_category_url)
bike_url_tbl <- html_bike_category %>% 
  html_nodes(css = ".productTileDefault__imageWrapper > a") %>%
  html_attr('href') %>% 
  str_remove(pattern = "\\?.*") %>%
  enframe(name = "position", value = "url")


# get more data from json files


#2.2 Wrap it into function
get_bike_data <- function(url) {
  html_bike_category <- read_html("https://www.canyon.com/en-de/road-bikes/")
  bike_url_tbl <- html_bike_category %>% 
    html_nodes(css = ".productTileDefault__imageWrapper > a") %>%
    html_attr('href') %>% 
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTileDefault__productSummary > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

bike_category_url <- bike_category_tbl$url[1]

bike_data_tbl     <- get_bike_data(url = bike_category_url)
  