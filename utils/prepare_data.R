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

# Sys.setenv(RETICULATE_PYTHON="C:\\Users\\Juliana\ Schneider\\Anaconda3\\envs\\py374\\python.exe")
# use_python("C:\\Users\\Juliana\ Schneider\\Anaconda3\\envs\\py374\\python.exe", required=TRUE)
# use_condaenv("py374")
# py_config()
# py_install("translate", pip=TRUE,envname="py374")
# py_install("lxml", pip=TRUE,envname="py374", ignore_installed=TRUE)

library(reticulate)
trans <- import("translate")
lator <- trans$Translator(from_lang="de", to_lang="en")

install.load::install_load("purrr")
lator$translate(wein[1,c("region")])
lapply(wein[,c("region")], function(x) lator$translate(x))
translate_data <- function(weini){
  # catalogue:
  ## pinot noir = spätburgunder = pinot nero
  ## pinot grigio = grauburgunder = grauer burgunder = pinot gris 
  ## grenache = garnacha = cannonau
  weini$grape[weini$grape %in% "Pinot Noir" | weini$grape %in% "Pinot Nero" | weini$grape %in% "Spätburgunder"] <- "Pinot Noir"
  weini$grape[weini$grape %in% "Pinot Grigio" | weini$grape %in% "Pinot Gris" | weini$grape %in% "Grauburgunder" | weini$grape %in% "Grauer Burgunder"] <- "Pinot Gris"
  weini$grape[weini$grape %in% "Grenache" | weini$grape %in% "Cannonau" | weini$grape %in% "Garnacha"] <- "Grenache"
  
}

install.load::install_load("deeplr")

test <- data.frame(land = c("Deutschland", "Frankreich", "Italien"),
                   region = c("Bayern", "Burgund", "Toskana"),
                   city = c("N�rnberg", "Dijon", "Florenz"))
translate_data(test$land, domain = )
?gettext
