rm(list=ls())


adjust_classes <- function(weini){
  weini$price <- gsub(",",".",weini$price)
  weini$Alcohol <- gsub(",",".",weini$Alcohol)
  weini$year <- as.integer(weini$year)
  weini$Alcohol <- as.numeric(weini$Alcohol)
  weini$price <- as.numeric(weini$price)
  weini <- weini[which(weini$liked != ""),]
  weini <- weini[which(weini$country != ""),]
  weini$tested <- as.factor(weini$tested)
  weini$liked <- as.factor(weini$liked)
  weini$colour <- as.factor(weini$colour)
  weini$country <- as.factor(weini$country)
  weini$region <- as.factor(weini$region)
  
  return(weini)
}
change_synonymes <- function(df){
  
  grapes <- df["grape"]
  if(!any(is.na(df["synonyme"]))){
    syn <- sprintf("(%s)",df["synonyme"])
    grape <- aregexec(syn, grapes, max.distance = 10)
    replgrape <- regmatches(x=grapes, m=grape)
    df["grape"] <- gsub(replgrape[[1]][1], df["grape_variety"], grapes)
  }
  
  return(df)
}


# Sys.setenv(RETICULATE_PYTHON="C:\\Users\\Juliana\ Schneider\\Anaconda3\\envs\\py374\\python.exe")
# use_condaenv("py374")
# py_config()
# py_install("translate", pip=TRUE,envname="py374")
# py_install("lxml", pip=TRUE,envname="py374", ignore_installed=TRUE)

install.load::install_load(c("reticulate", "purrr", "assertr", "data.table", "glue","tidyverse", "stringi", "fuzzyjoin", "splitstackshape"))
use_python("C:\\Users\\Juliana\ Schneider\\Anaconda3\\envs\\py374\\python.exe", required=TRUE)
trans <- import("translate_wine")
lator <- trans$Translator(from_lang="de", to_lang="en")

translate_regions <- function(weini){
  weini <- setDF(weini)
  transregions <- purrr::map(weini[,c("region")], ~lator$translate(.))
transregions <- as.vector(unlist(transregions))
weini$transregions <- transregions
return(weini)
}

translate_grapes <- function(weini){
  # catalogue:
  ## pinot noir = spÃ¤tburgunder = pinot nero
  ## pinot grigio = grauburgunder = grauer burgunder = pinot gris 
  ## grenache = garnacha = cannonau
  weini$grape[weini$grape %in% "Pinot Noir" | weini$grape %in% "Pinot Nero" | weini$grape %in% "SpÃ¤tburgunder"] <- "Pinot Noir"
  weini$grape[weini$grape %in% "Pinot Grigio" | weini$grape %in% "Pinot Gris" | weini$grape %in% "Grauburgunder" | weini$grape %in% "Grauer Burgunder"] <- "Pinot Gris"
  weini$grape[weini$grape %in% "Grenache" | weini$grape %in% "Cannonau" | weini$grape %in% "Garnacha"] <- "Grenache"
  return(weini)
}

setwd("C:/Users/Juliana Schneider/dev/Weinbar/")

wein <- fread("./Weinbar2.csv", encoding="UTF-8")
varietals <- fread("./grapevarietals.csv", encoding="UTF-8")

unify_letters <- function(x){stringi::stri_replace_all_fixed(x, c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"), 
                                c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
                                vectorize_all = FALSE)}


varietals <- as.data.frame(sapply(varietals, function(x) unify_letters(x)))

varietals <- varietals %>% 
  mutate(synonyme = strsplit(as.character(synonyme), ", ")) %>% 
  mutate(synonyme = str_replace(synonyme, "\n", "")) #%>%
  unnest(synonyme) %>%
  as.data.frame(.)



wein <- as.data.frame(sapply(wein, function(x) stringi::stri_replace_all_fixed(x, c("Ã¤", "Ã¶", "Ã¼", "Ã„", "Ã–", "Ãœ", "ÃŸ"), 
                                                                               c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
                                                                               vectorize_all = FALSE)))




wein <- wein %>% fuzzy_left_join(varietals, by = c("grape" = "synonyme"), match_fun = str_detect)

