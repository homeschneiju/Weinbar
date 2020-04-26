rm(list=ls())
setwd("C:/Users/Juliana Schneider/dev/Weinbar/")
install.load::install_load(c("assertr", "data.table", "tidyverse"))
wein <- fread("./Weinbar.csv")
source("./utils/prepare_data.R")


# PREPARE DATA



wein <- translate_grapes(wein)
wein <- adjust_classes(wein)


wein <- translate_regions(wein)


grapeplot_df <- wein %>%
  count(grape) %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  left_join(wein[,c("grape", "colour")])


priceplot_df <-  wein %>%
  count(price) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:5)

countryplot_df <- wein %>%
  count(country) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:5)

regionplot_df <- wein %>%
  filter(country == names(sort(table(wein$country), decreasing = TRUE)[1])) %>%
  count(region) %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  left_join(wein[,c("country", "region")])

country_col <- union(countryplot_df$country,regionplot_df$country)
country_col.col <- rainbow(length(country_col))
names(country_col.col) <- unique(countryplot_df$country)


saveRDS(wein, "./Weinbar.rds")