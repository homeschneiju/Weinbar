

library(data.table); library(tidyverse)
wein <- fread("./Weinbar.csv")



# PREPARE DATA
wein$price <- gsub(",",".",wein$price)
wein$Alcohol <- gsub(",",".",wein$Alcohol)
wein$year <- as.integer(wein$year)
wein$Alcohol <- as.numeric(wein$Alcohol)
wein$price <- as.numeric(wein$price)
wein <- wein[which(wein$liked != ""),]
wein <- wein[which(wein$country != ""),]
wein$tested <- as.factor(wein$tested)
wein$liked <- as.factor(wein$liked)
wein$colour <- as.factor(wein$colour)
wein$country <- as.factor(wein$country)
wein$region <- as.factor(wein$region)

# catalogue:
## pinot noir = spÃ¤tburgunder = pinot nero
## pinot grigio = grauburgunder = grauer burgunder = pinot gris 
## grenache = garnacha = cannonau
wein$grape[wein$grape %in% "Pinot Noir" | wein$grape %in% "Pinot Nero" | wein$grape %in% "SpÃ¤tburgunder"] <- "Pinot Noir"
wein$grape[wein$grape %in% "Pinot Grigio" | wein$grape %in% "Pinot Gris" | wein$grape %in% "Grauburgunder" | wein$grape %in% "Grauer Burgunder"] <- "Pinot Gris"
wein$grape[wein$grape %in% "Grenache" | wein$grape %in% "Cannonau" | wein$grape %in% "Garnacha"] <- "Grenache"


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


dd <- union(countryplot_df$country,regionplot_df$country)
dd.col <- rainbow(length(dd))
names(dd.col)  <- dd

saveRDS(wein, "./Weinbar.rds")

# # FEATURE: FILTER BY LAND, COLOUR
# land <- "Germany"
# col <- "red"
# 
# if(nrow(wein[wein$country == land & wein$colour == col,]) == 0){sprintf("Sorry, you haven't had a %s from %s yet", col, land)
#   }else {print(wein[wein$country == land & wein$colour == col & wein$liked == "yes",])}
# 
# # FEATURE: SEARCH BY DESCRIPTION
# i <- "sueffig"
# wein[i %in% wein$description,]
# 
# 
# # FEATURE: RECOMMENDER
# val <- wein[,c("liked","grape", "colour", "country", "region")]
# library(kernlab);library(caret)
# weinlike <- train(y=val$liked[complete.cases(val)],
#                   x=valdum,
#                   method="svmRadial",
#                   trControl = trainControl(method="cv"))
# valdum <- model.matrix(liked~.,val[complete.cases(val),])
# valdum <- valdum[,2:ncol(valdum)]
# 
# head(valdum)
# any(is.na(wein$liked[complete.cases(wein)]))
# wein$liked
# nrow(val)
# 
# 
# 
# 
# # grap <- strsplit(wein$grape, ", ")
# # desc <- strsplit(wein$description, ", ")
# # 
# # max(sapply(grap,length))
# # max(sapply(desc,length))
# # 
# # 
# # 
# # library(tidyverse)
# # wein$grape <- as.factor(wein$grape)
# # wein <- wein %>% separate(grape, paste("grape", c(1:max(sapply(grap,length))), sep=""),", ")
# # wein <- wein %>% separate(description, paste("descr", c(1:max(sapply(desc,length))), sep=""), ", ")
