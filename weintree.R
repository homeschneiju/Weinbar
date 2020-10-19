
 

source("./utils/prepare_data.R")

# PREPARE DATA


wein_grapes2 <- cSplit(wein, "grape", ",")
wein["burgunder" %in% wein$grape,]
df_test <- wein[!is.na(wein$synonyme),][1:2,]
!is.na(df_test$synonyme)
change_synonymes(df_test[2,])

 eh <- data.frame(t(apply(wein,1,change_synonymes)))
 class(eh)
df_result <- df_test %>% as.data.table() %>% rowwise() %>% do(data.frame(change_synonymes(.)))

View(df_result)




change_synonymes(df_test)



sapply(wein$grape, function(x) agrep(x,varietals$synonyme, value=TRUE))

wein_test <- lapply(wein_grapes[, c("grape", "grape_variety")], function(x) gsub(x[,"grape"],x[,"grape_variety"],x))
gsub(,wein_grapes$grape_variety,wein_grapes$grape)
wein_grapes[, c("grape", "grape_variety")]
wein_grapes <- wein %>% left_join(varietals, by=c("grape" = "synonyme"))
wein_grapes[!is.na(wein_grapes$grape_variety),"grape"]# <-

####
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