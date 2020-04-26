install.load::install_load(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "sf", "rnaturalearth", "rnaturalearthdata","RColorBrewer"))

install.load::install_load("rgeos")
install.load::install_load("tidyverse")

world <- ne_countries(scale = "medium", returnclass = "sf")
head(world)                 

#wine_coords <- ne_countries(country=unique(wein$country), returnclass = "sf")
wine_countries <- ne_countries(country=unique(wein$country), returnclass = "sf")



win <- wein %>% group_by(country) %>% mutate(Anzahl = n())
wine_countries <- merge(wine_countries, win[,c("country", "Anzahl")], by.x="name", by.y="country")
dd <- data.frame("Anzahl"=unique(wine_countries$Anzahl)) #union(countryplot_df$country,regionplot_df$country)
dd$col <- rainbow(nrow(dd))

# wine_countries <-
  
wc <-  merge(wine_countries[,c("Anzahl", "name")], dd, by.x="Anzahl",by.y="Anzahl", all.x=TRUE)


# install.load::install_load("maps")
# cities_co <- merge(win, world.cities, by.x="country", by.y="country.etc")

# ggplot(data=world) +
#    geom_sf()+
#   geom_sf(data =wc, aes(fill=Anzahl)) +
# scale_fill_gradient(low="blue", high="red")
# geom_point(data=cities_co[!duplicated(cities_co$country),], aes(x=long, y=lat, col=Anzahl, size=0.1))