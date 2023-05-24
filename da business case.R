#webscrapping for data acquistion

#Load relevant Libraries 
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing


url_home          <- "https://www.canyon.com/en-de"
xopen(url_home)
html_home <- read_html(url_home) %>% html_nodes(css = ".js-navigationDrawer__list--secondary") %>% html_attr('id') %>% 
             discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
              enframe(name = "position", value = "family_class") %>% 
              mutate(
                family_id = str_glue("#{family_class}")
              )

