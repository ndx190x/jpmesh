# File Download -----------------------------------------------------------
# https://github.com/uribo/jpmesh/blob/c91d9325319de6d48e959315581384293606ff62/data-raw/download_mesh_csv.R
library(purrr)
library(readr)

if (dir.exists("data-raw/city_mesh/") == FALSE) {
  library(rvest)
  base.url <- "http://www.stat.go.jp/data/mesh/"
  x <- read_html(paste0(base.url, "m_itiran.htm"))
  links <- x %>% html_nodes(xpath = '//*[@id="contents"]/ul/li/a') %>%
    html_attr("href")
  1:length(links) %>% 
    map(~ download.file(paste0(base.url, links[.x]),
                        destfile = paste0("data-raw/city_mesh/city_mesh_", gsub(".+/", "",  links[.x]))))
}
x <- list.files("data-raw/city_mesh/", full.names = TRUE)
df_city_mesh <- 1:length(x) %>% 
  map_df(~ read_csv(x[.x],
                           locale = locale(encoding = "cp932"),
                           col_types = list(col_character(), col_character(), col_character())) %>% 
           purrr::set_names(c("city_code", "city_name", "meshcode")))

usethis::use_data(df_city_mesh,
                   overwrite = TRUE,
                   internal = TRUE)
