library(dplyr)
library(GGally)
library(skimr)
library(ggplots2)
library(gridExtra)
library(corrplot)
library(psych)

jeffsData = modelingData%>%
  select(49:73)

skim(jeffsData)

#ID_railroad_station_walk
jeffsData = jeffsData %>% mutate(railroad_station_walk_min = if_else(is.na(railroad_station_walk_min),0,railroad_station_walk_min))
walk = ggplot(jeffsData, aes(x = railroad_station_walk_min)) +geom_histogram()

#public_transport_station_km
rr = ggplot(jeffsData, aes(x = railroad_station_walk_min)) +geom_histogram()

#ID_railroad_station_walk
jeffsData = jeffsData %>% mutate(ID_railroad_station_walk = if_else(is.na(ID_railroad_station_walk),0,ID_railroad_station_walk))
id = ggplot(jeffsData, aes(x = ID_railroad_station_walk)) +geom_histogram()

#railroad_station_avto_km"
rr_avto = ggplot(jeffsData, aes(x = railroad_station_avto_km)) +geom_histogram()

#railroad_station_avto_min
rr_avto_min = ggplot(jeffsData, aes(x = railroad_station_avto_min)) +geom_histogram()

#public_transport_station_km
rr_sta_min = ggplot(jeffsData, aes(x = public_transport_station_km)) +geom_histogram()

#kremlin_km
kremlin = ggplot(jeffsData, aes(x = kremlin_km)) +geom_histogram()

#big_road1_km
br1 = ggplot(jeffsData, aes(x = big_road1_km)) +geom_histogram()

#big_road2_km
br2 = ggplot(jeffsData, aes(x = big_road2_km)) +geom_histogram()

#railroad_km
rr_km = ggplot(jeffsData, aes(x = railroad_km)) +geom_histogram()

#bus_terminal_avto_km
bt = ggplot(jeffsData, aes(x = bus_terminal_avto_km)) +geom_histogram()

#big_market_km
bm = ggplot(jeffsData, aes(x = big_market_km)) +geom_histogram()

#market_shop_km
market_shop = ggplot(jeffsData, aes(x = market_shop_km)) +geom_histogram()

#fitness_km
fit = ggplot(jeffsData, aes(x = fitness_km)) +geom_histogram()

#swim_pool_km
swim = ggplot(jeffsData, aes(x = swim_pool_km)) +geom_histogram()

#ice_rink_km
ice = ggplot(jeffsData, aes(x = ice_rink_km)) +geom_histogram()

#stadium_km
stadium = ggplot(jeffsData, aes(x = stadium_km)) +geom_histogram()

#basketball_km
bbkm = ggplot(jeffsData, aes(x = basketball_km)) +geom_histogram()

#public_healthcare_km
phc = ggplot(jeffsData, aes(x = public_healthcare_km)) +geom_histogram()

#university_km
uni = ggplot(jeffsData, aes(x = university_km)) +geom_histogram()

#workplaces_km
work = ggplot(jeffsData, aes(x = workplaces_km)) +geom_histogram()

#shopping_centers_km
shop = ggplot(jeffsData, aes(x = shopping_centers_km)) +geom_histogram()

#office_km
office = ggplot(jeffsData, aes(x = office_km)) +geom_histogram()

#big_church_km
church = ggplot(jeffsData, aes(x = big_church_km)) +geom_histogram()

#price_doc
price = ggplot(jeffsData, aes(x = price_doc)) +geom_histogram()


grid.arrange(walk,rr,id,rr_avto,rr_avto_min,rr_sta_min,
             kremlin,br1,br2,rr_km,
             bt,bm,market_shop,swim,
             ice,stadium,bbkm,phc,
             uni,work,shop,office,
             church,price,fit, nrow = 4)
pairs(jeffsData)

modelingData = modelingData %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))

cor(jeffsData)
