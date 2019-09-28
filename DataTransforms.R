library(pacman)
p_load(lmtest
       ,dplyr
       ,Hmisc
       ,skimr
       ,tidyr
       ,na.tools
       ,tidyverse
       ,olsrr
       ,caret
       ,multcomp
       ,ggthemes
       ,MASS# for OLS
       ,regclass# for VIF
       ,stats
       ,glmnet
       ,sjPlot
       ,sjmisc
       ,ggplot2
       ,xlsx)

#format dates:
df <- read.csv("./modelingData.csv",  header=T, sep=",", strip.white=T, stringsAsFactors = F)

names(df)[36] <- "build_count_1921_1945"
names(df)[37] <- "build_count_1946_1970"
names(df)[38] <- "build_count_1971_1995"
names(df)[54] <- "public_trans_station_time_walk"


df <- df %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

timestamp2 <- df$timestamp
df <- data.frame(timestamp2, df)

df <- df %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

df <- df %>% separate(timestamp2, sep="-", into = c("year", "month", "day"))

#names(df$build_count_1921.1945)[36] <- "build_count_1921_1945"
#names(df$build_count_1946.1970)[37] <- "build_count_1946_1970"
#names(df$build_count_1971.1995)[38] <- "build_count_1971_1995"
#names(df$public_transport_station_min_walk)[54] <- "public_trans_station_time_walk"


#$colnames(df) <- c('year', 'month', 'day', 'id', 'timestamp', 'full_sq', 'life_sq', 'floor', 'max_floor', 'num_room', 'kitch_sq', 'product_type', 'raion_popul', 'green_zone_part', 'indust_part', 'children_preschool', 'preschool_quota', 'children_school', 'healthcare_centers_raion', 'university_top_20_raion', 'shopping_centers_raion', 'office_raion', 'railroad_terminal_raion', 'big_market_raion', 'full_all', 'X0_6_all', 'X7_14_all', 'X0_17_all', 'X16_29_all', 'X0_13_all', 'build_count_block', 'build_count_wood', 'build_count_frame', 'build_count_brick', 'build_count_before_1920', 'build_count_1921_1945', 'build_count_1946_1970', 'build_count_1971_1995', 'build_count_after_1995', 'metro_min_avto', 'metro_km_avto', 'metro_min_walk', 'metro_km_walk', 'school_km', 'park_km', 'green_zone_km', 'industrial_km', 'railroad_station_walk_km', 'railroad_station_walk_min', 'ID_railroad_station_walk', 'railroad_station_avto_km', 'railroad_station_avto_min', 'public_transport_station_km', 'public_trans_station_time_walk', 'kremlin_km', 'big_road1_km', 'big_road2_km', 'railroad_km', 'bus_terminal_avto_km', 'big_market_km', 'market_shop_km', 'fitness_km', 'swim_pool_km', 'ice_rink_km', 'stadium_km', 'basketball_km', 'public_healthcare_km', 'university_km', 'workplaces_km', 'shopping_centers_km', 'office_km', 'big_church_km', 'price_doc')

########## Floor
#df$floor <- df$floor %>% replace_na(0)
df$floor[is.na(df$floor)] <- 0
df$floor <- log(df$floor+1)
##########

df$product_type <- as.factor(df$product_type)

########## Material, Hospital bed raion
df <- subset(df, select = -c(material, hospital_beds_raion))
##########
df[which(df$year == 2011),]
########## max floor
df2.maxfl <- df[which(!is.na(df$max_floor)),]
maxfl.Mean <- data.frame(df2.maxfl$max_floor/df2.maxfl$full_sq)
colnames(maxfl.Mean) <- "percentofFloor"
maxflMultiplier <- mean(head(maxfl.Mean$percentofFloor,7000))
df[which(is.na(df$max_floor)),6] <- df[which(is.na(df$max_floor)),3]*maxflMultiplier
##########
df[which(df$year == 2011),]
########## life sq
df2.lifesq <- df[which(!is.na(df$life_sq)),]
life.Mean <- data.frame(df2.lifesq$life_sq/df2.lifesq$full_sq)
colnames(life.Mean) <- "percentofFull"
lifesqMultiplier <- mean(head(life.Mean$percentofFull,11000))
df[which(is.na(df$life_sq)),4] <- df[which(is.na(df$life_sq)),3]*lifesqMultiplier
dishonestLivingSpace <- (df$life_sq - df$full_sq)
df <- data.frame(dishonestLivingSpace, df)
df <- df[which(df$dishonestLivingSpace < 0),] #living space shouldn't be greater than the full sq, considering lofts that have shared external bathrooms
df <- subset(df, select = -c(dishonestLivingSpace)) # remove the counter variable dishonestLivingSpace
##########
df[which(df$year == 2011),]
########## kithcen sq
df2.kitchsq <- df[which(!is.na(df$kitch_sq)),]
kitch.Mean <- data.frame(df2.kitchsq$kitch_sq/df2.kitchsq$full_sq)
colnames(kitch.Mean) <- "percentofFullkitch"
kitchsqMultiplier <- mean(head(kitch.Mean$percentofFullkitch,7000))
df[which(is.na(df$kitch_sq)),10] <- df[which(is.na(df$kitch_sq)),3]*kitchsqMultiplier
df$kitch_sq[is.na(df$kitch_sq)] <- 0
dishonestKitchens <- (df$kitch_sq - df$full_sq)
df <- data.frame(dishonestKitchens, df)
df <- df[which(df$dishonestKitchens < -2),] #kitchen space shouldn't be greater than more than 2 meters less than the full sq
df <- subset(df, select = -c(dishonestKitchens)) # remove the counter variable dishonestKitchens
df$kitch_sq <- sqrt(df$kitch_sq^1/16+1)
##########
df[which(df$year == 2011),]
########## num room
df2.numRm <- df[which(!is.na(df$num_room)),]
numRm.Mean <- data.frame(df2.numRm$num_room/df2.numRm$full_sq)
colnames(numRm.Mean) <- "percentofFullrm"
numRmMultiplier <- mean(head(numRm.Mean$percentofFullrm,7000))
df[which(is.na(df$num_room)),9] <- df[which(is.na(df$num_room)),3]*numRmMultiplier
df <- df[which(df$num_room < 30),]
##########
df[which(df$year == 2011),]
########## preschool quota
df.preK.no.NA <- df[which(!is.na(df$preschool_quota)),]
preK.Mean <- data.frame(df.preK.no.NA$preschool_quota/df.preK.no.NA$X0_6_all)
colnames(preK.Mean) <- "percentofFloor"
preKquotaMultiplier <- mean(head(preK.Mean$percentofFloor,5000))
df[which(is.na(df$preschool_quota)),16] <- df[which(is.na(df$preschool_quota)),26]*preKquotaMultiplier
df$preschool_quota <- sqrt(df$preschool_quota)
df$preschool_quota[is.na(df$preschool_quota)] <- 1
##########
df[which(df$year == 2011),]
########## build year
df <- subset(df, select = -c(build_year))
##########
df[which(df$year == 2011),]
########## office raion
df$office_raion <- sqrt(df$office_raion^1/10)
##########
df[which(df$year == 2011),]
########## big market raion
df$big_market_raion <- dplyr::recode(df$big_market_raion,  "no" = 0, "yes"= 1)
##########
df[which(df$year == 2011),]
########## railroad terminal raion
df$railroad_terminal_raion <- dplyr::recode(df$railroad_terminal_raion,  "no" = 0, "yes"= 1)
##########
df[which(df$year == 2011),]
##########
#df <- df %>% mutate(railroad_station_walk_min = if_else(is.na(railroad_station_walk_min),0,railroad_station_walk_min))
df$railroad_station_walk_min[is.na(df$railroad_station_walk_min)] <- 0
##########
df[which(df$year == 2011),]
##########
#df <- df %>% mutate(ID_railroad_station_walk = if_else(is.na(ID_railroad_station_walk),0,ID_railroad_station_walk))
df$ID_railroad_station_walk[is.na(df$ID_railroad_station_walk)] <- 0
##########
df[which(df$year == 2011),]
##########
#df$railroad_station_walk_km <- df$railroad_station_walk_km %>% replace_na(0)
df$railroad_station_walk_km[is.na(df$railroad_station_walk_km)] <- 0
##########
df[which(df$year == 2011),]
##########
df <- df[which(!is.na(df$build_count_before_1920)),] #one fell swoop to take out all NA 'build_count_{year range}' rows
##########
df[which(df$year == 2011),]
##########
#df$metro_min_walk <- df$metro_min_walk %>% replace_na(0)
df$metro_min_walk[is.na(df$metro_min_walk)] <- 0
##########
df[which(df$year == 2011),]
##########
#df$metro_km_walk <- df$metro_km_walk %>% replace_na(0)
df$metro_km_walk[is.na(df$metro_km_walk)] <- 0
##########
df[which(df$year == 2011),]

year_month <- as.factor(paste0(df$year,df$month))
df <- data.frame(year_month,df)


############################## conversion to numeric and factor only for modeling consistency #############################

df <- df %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.character, as.factor) %>% data.frame()

df <- df[-c(2958, 1192, 2998,134, 3156, 1452, 2994, 3151, 98),]

write.csv(df,"cleanData.csv", row.names = F)