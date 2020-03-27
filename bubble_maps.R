install.load::install_load(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "sf", "rnaturalearth", "rnaturalearthdata"))

install.load::install_load("rgeos")
install.load::install_load("tidyverse")

world <- ne_countries(scale = "medium", returnclass = "sf")
head(world)                 

wine_coords <- ne_countries(country=unique(wein$country), returnclass="sp")
eim/coordinates(wine_coords)
head(wine_countries)
ggplot(data=wine_countries) +
 # geom_sf() +
  geom_sf(data=world)

wein <- wein %>% group_by(country) %>% mutate(bubble_size = n())
wine_countries <- merge(wine_countries, wein[,c("country", "bubble_size")], by.x="name", by.y="country")
st_crs(wine_countries$geometry[1])

install.load::install_load("maps")
colnames(cities_co)
cities_co <- merge(wein, world.cities, by.x="country", by.y="country.etc")
cities_co
ggplot(data=wine_countries) +
   geom_sf() +
  geom_point(data=cities_co, aes(x=long, y=lat, size=bubble_size, col=bubble_size))

unique(wein$region)
map_region_capitals <- data.frame(region = sort(unique(wein$region)), capital=c("",
                                                                          "Strasbourg",
                                                                          "Freiburg",
                                                                          "Timisoara",
                                                                          "Dijon",
                                                                          "San Francisco",
                                                                          "Zaragoza",
                                                                          "Barcelona",
                                                                          "Nimes",
                                                                          "Bologna", 
                                                                          "Wurzburg",
                                                                          "Wurzburg",
                                                                          "Freiburg",
                                                                          "Telavi",
                                                                          "Trier",
                                                                          "Plovdiv", 
                                                                          "Montpellier",
                                                                          "Barcelona", 
                                                                          "Wiesbaden", 
                                                                          "Turin",
                                                                          "Bari",
                                                                          "Kaiserslautern",
                                                                          "Valladolid",
                                                                          "Cagliari",
                                                                          "Palermo",
                                                                          "Merano",  
                                                                          "Florence",
                                                                          "Trento", 
                                                                          "Perugia",  
                                                                          "Erfurt",
                                                                          "Santiago",
                                                                          "Venice",  
                                                                      "Cape Town"
                                                                    ))

map_region_capitals <- merge(map_region_capitals[map_region_capitals$region != "",], wein[,c("region", "country")], all.x=TRUE)
map_region_capitals <- unique(map_region_capitals)
wein <- merge(wein, map_region_capitals, all.x=TRUE)
for (i in unique(wein$country)){
  for(j in unique(wein[wein$country == i, "region.x"])){
    wein[wein$country == i & wein$region.x == j & is.na(wein$capital), "capital"] <- world.cities[world.cities$country.etc == i & world.cities$name == j & world.cities$capital == 1, "name"]
  }
  }


wein <- merge(wein, world.cities[, c(1,2,4,5)], by.x=c("country", "capital"), by.y=c("country.etc", "name"), all.x=TRUE)
world.cities[grep("Florence",world.cities$name),]
