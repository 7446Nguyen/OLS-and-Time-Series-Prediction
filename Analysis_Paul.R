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


na_count <- sapply(df, function(cnt) sum(length(which(is.na(cnt)))))
na_count

#format dates:
df <- read.csv("./modelingData.csv",  header=T, sep=",", strip.white=T, stringsAsFactors = F)

df <- df %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

timestamp2 <- df$timestamp
df <- data.frame(timestamp2, df)

df <- df %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

df <- df %>% separate(timestamp2, sep="-", into = c("year", "month", "day"))

colnames(df$build_count_1921.1945) <- "build_count_1921_1945"
colnames(df$build_count_1946.1970) <- "build_count_1946_1970"
colnames(df$build_count_1971.1995) <- "build_count_1971_1995"
colnames(df$public_transport_station_min_walk) <- "public_trans_station_time_walk"

########## Floor
#df$floor <- df$floor %>% replace_na(0)
df$floor[is.na(df$floor)] <- 0
df$floor <- log(df$floor+1)
##########

df$product_type <- as.factor(df$product_type)

########## Material, Hospital bed raion
df <- subset(df, select = -c(material, hospital_beds_raion))
##########

########## max floor
df2.maxfl <- df[which(!is.na(df$max_floor)),]
maxfl.Mean <- data.frame(df2.maxfl$max_floor/df2.maxfl$full_sq)
colnames(maxfl.Mean) <- "percentofFloor"
maxflMultiplier <- mean(head(maxfl.Mean$percentofFloor,7000))
df[which(is.na(df$max_floor)),6] <- df[which(is.na(df$max_floor)),3]*maxflMultiplier
##########

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

########## num room
df2.numRm <- df[which(!is.na(df$num_room)),]
numRm.Mean <- data.frame(df2.numRm$num_room/df2.numRm$full_sq)
colnames(numRm.Mean) <- "percentofFullrm"
numRmMultiplier <- mean(head(numRm.Mean$percentofFullrm,7000))
df[which(is.na(df$num_room)),9] <- df[which(is.na(df$num_room)),3]*numRmMultiplier
df <- df[which(df$num_room < 30),]
##########

########## preschool quota
df.preK.no.NA <- df[which(!is.na(df$preschool_quota)),]
preK.Mean <- data.frame(df.preK.no.NA$preschool_quota/df.preK.no.NA$X0_6_all)
colnames(preK.Mean) <- "percentofFloor"
preKquotaMultiplier <- mean(head(preK.Mean$percentofFloor,5000))
df[which(is.na(df$preschool_quota)),16] <- df[which(is.na(df$preschool_quota)),26]*preKquotaMultiplier
df$preschool_quota <- sqrt(df$preschool_quota)
df$preschool_quota[is.na(df$preschool_quota)] <- 1
##########

########## build year
df <- subset(df, select = -c(build_year))
##########

########## office raion
df$office_raion <- sqrt(df$office_raion^1/10)
##########

########## big market raion
df$big_market_raion <- dplyr::recode(df$big_market_raion,  "no" = 0, "yes"= 1)
##########

########## railroad terminal raion
df$railroad_terminal_raion <- dplyr::recode(df$railroad_terminal_raion,  "no" = 0, "yes"= 1)
##########

##########
#df <- df %>% mutate(railroad_station_walk_min = if_else(is.na(railroad_station_walk_min),0,railroad_station_walk_min))
df$railroad_station_walk_min[is.na(df$railroad_station_walk_min)] <- 0
##########

##########
#df <- df %>% mutate(ID_railroad_station_walk = if_else(is.na(ID_railroad_station_walk),0,ID_railroad_station_walk))
df$ID_railroad_station_walk[is.na(df$ID_railroad_station_walk)] <- 0
##########

##########
#df$railroad_station_walk_km <- df$railroad_station_walk_km %>% replace_na(0)
df$railroad_station_walk_km[is.na(df$railroad_station_walk_km)] <- 0
##########

##########
df <- df[which(!is.na(df$build_count_before_1920)),] #one fell swoop to take out all NA 'build_count_{year range}' rows
##########

##########
#df$metro_min_walk <- df$metro_min_walk %>% replace_na(0)
df$metro_min_walk[is.na(df$metro_min_walk)] <- 0
##########

##########
#df$metro_km_walk <- df$metro_km_walk %>% replace_na(0)
df$metro_km_walk[is.na(df$metro_km_walk)] <- 0
##########

############################## conversion to numeric and factor only for modeling consistency #############################

df <- df %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.character, as.factor) %>% data.frame()

df <- df[-c(2958, 1192, 2998,134, 3156, 1452, 2994, 3151, 98),]

write.csv(df,"cleanData.csv")


#######################################################################################################################
############################################### Correlation Matrix Start ##############################################
#######################################################################################################################

df.numeric <- dplyr::select_if(df, is.numeric) %>% data.frame()

df.numeric.no.NA <- df.numeric %>% na.omit()

flattenCorrMatrix <- function(cormatrix, pmatrix) {
  ut <- upper.tri(cormatrix)
  data.frame(
    row = rownames(cormatrix)[row(cormatrix)[ut]],
    column = rownames(cormatrix)[col(cormatrix)[ut]],
    cor  =(cormatrix)[ut],
    p = pmatrix[ut]
  )
}

options(scipen=999)
options(max.print=100000)

#See what variables are correlated with eachother, p-values
correlation.matrix <- rcorr(as.matrix(df.numeric.no.NA))
corDF <- data.frame(flattenCorrMatrix(correlation.matrix$r, correlation.matrix$P))

corDF.ordered <- data.frame(corDF[order(-corDF$cor),])
collinear.correlation <- corDF[which(corDF$cor >= 0.75),]

collinear.correlation <- data.frame(collinear.correlation[order(-collinear.correlation$cor),])
#write.csv(colinear.correlation, "Collinear_Correlation_Matrix.csv")
#write.csv(corDF.ordered, "All_Vars_Correlation_Matrix.csv")

#######################################################################################################################
############################################### Correlation Matrix End ################################################
#######################################################################################################################

##########
########## Start of OLS (shells for now)
##########

########## Forward Selection
model.forward.Start <- lm(log(price_doc) ~ ., data = df) # all variables

model.end <- lm(log(price_doc) ~ 1, data = df) # add one at a time

# All Variables Model - Forward Selection
model.Allvar <- lm(log(price_doc) ~ id + timestamp + full_sq +	life_sq + floor + max_floor + material + num_room + kitch_sq + product_type
                   + raion_popul + green_zone_part + indust_part + children_preschool + preschool_quota + children_school + hospital_beds_raion
                   + healthcare_centers_raion + university_top_20_raion + shopping_centers_raion + office_raion + railroad_terminal_raion
                   + big_market_raion + full_all + X0_6_all + X7_14_all + X0_17_all + X16_29_all + X0_13_all + build_count_block + build_count_wood 
                   + build_count_frame + build_count_brick + build_count_before_1920 + build_count_1921.1945 + build_count_1946.1970
                   + build_count_1971.1995 + build_count_after_1995 + metro_min_avto + metro_km_avto + metro_min_walk + metro_km_walk + school_km
                   + park_km + green_zone_km + industrial_km + railroad_station_walk_km + railroad_station_walk_min + ID_railroad_station_walk
                   + railroad_station_avto_km + railroad_station_avto_min + public_transport_station_km + public_transport_station_min_walk
                   + kremlin_km + big_road1_km + big_road2_km + railroad_km + bus_terminal_avto_km + big_market_km + market_shop_km + fitness_km
                   + swim_pool_km + ice_rink_km + stadium_km + basketball_km + public_healthcare_km + university_km + workplaces_km
                   + shopping_centers_km + office_km + big_church_km, data = df)

#### Forward Selection, first pass
model.Forward <- stepAIC(model.forward.Start, direction = "forward", trace = F, scope = formula(model.Allvar))
summary(model.Forward)
model.Forward$anova


########## Backward Elimination, first pass
model.Backward <- stepAIC(model.Allvar, direction = "backward", trace = F, scope = formula(model.forward.Start))
summary(model.Backward)
model.Backward$anova

########## Stepwise Selection, first pass
model.Stepwise <- stepAIC(model.forward.Start, direction = "both", trace = F)
summary(model.Stepwise)
model.Stepwise$anova

##########
##########
########## End of OLS
##########
##########

#######################################################################################################################
#######################################################################################################################
#********************************Suggested models from AIC selection**************************************************#
#######################################################################################################################

# Forward Suggestion
fwd.suggested.lmModel <- lm(price_doc ~ full_sq + life_sq + floor + max_floor + kitch_sq + children_school + university_top_20_raion + metro_min_walk + industrial_km + railroad_station_walk_km + 
                              kremlin_km + big_road1_km + big_road2_km + public_healthcare_km + full_sq:life_sq, data = df)

#### Forward Selection, second pass
model.Forward2 <- stepAIC(fwd.suggested.lmModel, direction = "forward", trace = F, scope = formula(model.forward.Start))
summary.lm(model.Forward2)
model.Forward2$anova


theme_set(theme_sjplot())
plot(model.Forward2)
plot_model(model.Forward2)
# Backward Elimination
df2.Backward.maxVars <- data.frame(df$price_doc, df$timestamp, df$full_sq, df$life_sq, df$floor, df$max_floor, df$kitch_sq, df$product_type, df$children_preschool,
                                   df$preschool_quota, df$children_school, df$university_top_20_raion, df$office_raion, df$build_count_before_1920,
                                   df$build_count_1921.1945, df$build_count_1946.1970, df$build_count_1971.1995, df$build_count_after_1995, df$metro_km_avto,
                                   df$metro_min_walk, df$industrial_km, df$kremlin_km, df$big_road1_km, df$big_road2_km, df$fitness_km, df$stadium_km,
                                   df$public_healthcare_km)

back.suggested.lm.Model<- lm(price_doc ~ timestamp + full_sq + life_sq + floor + max_floor + kitch_sq + 
                                    product_type + children_preschool + preschool_quota + children_school + 
                                    university_top_20_raion + office_raion + build_count_before_1920 + build_count_1921.1945 + 
                                    build_count_1946.1970 + build_count_1971.1995 + build_count_after_1995 + metro_km_avto + 
                                    metro_min_walk + industrial_km + kremlin_km + big_road1_km + big_road2_km + fitness_km + 
                                    stadium_km + public_healthcare_km, data = df)

########## Backward Elimination, second pass. Adjusted R-squared:  0.3671 
model.Backward2 <- stepAIC(back.suggested.lm.Model, direction = "backward", trace = F, scope = formula(model.forward.Start))
summary(model.Backward2)
model.Backward2$anova
plot(model.Backward2)


#Stepwise Regression
df2.Stepwise <- data.frame(df$price_doc, df$timestamp, df$full_sq, df$indust_part, df$university_top_20_raion, df$build_count_1946.1970,
                           df$metro_km_avto, df$kremlin_km, df$life_sq, df$children_preschool, df$office_raion,
                           df$build_count_1971.1995, df$metro_min_walk, df$big_road1_km, df$floor, df$preschool_quota,
                           df$build_count_before_1920, df$industrial_km, df$stadium_km, df$max_floor, df$public_healthcare_km, df$kitch_sq)

step.suggested.lm.Model<- lm(price_doc ~ full_sq + floor + max_floor + kitch_sq + 
                               product_type + preschool_quota + 
                               office_raion + metro_min_walk + industrial_km + 
                              kremlin_km + big_road1_km + public_healthcare_km + full_sq*kitch_sq + kremlin_km*big_road1_km, data = df)
VIF(step.suggested.lm.Model)
########## Stepwise Selection, second pass
model.Stepwise2 <- stepAIC(step.suggested.lm.Model, direction = "both", trace = F)
#model.Stepwise2 <- stepAIC(step.suggested.lm.Model, direction = "both", trace = F)
summary(model.Stepwise2)
model.Stepwise2$anova
plot(model.Stepwise2)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#*********************************DANGER ZONE: UNDER CONSTRUCTION. DO NOT USE YET*************************************#
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model.cv <- train(log(price_doc) ~ id + timestamp + full_sq +	life_sq + floor + max_floor + material + num_room + kitch_sq + product_type
                   + raion_popul + green_zone_part + indust_part + children_preschool + preschool_quota + children_school + hospital_beds_raion
                   + healthcare_centers_raion + university_top_20_raion + shopping_centers_raion + office_raion + railroad_terminal_raion
                   + big_market_raion + full_all + X0_6_all + X7_14_all + X0_17_all + X16_29_all + X0_13_all + build_count_block + build_count_wood 
                   + build_count_frame + build_count_brick + build_count_before_1920 + build_count_1921.1945 + build_count_1946.1970
                   + build_count_1971.1995 + build_count_after_1995 + metro_min_avto + metro_km_avto + metro_min_walk + metro_km_walk + school_km
                   + park_km + green_zone_km + industrial_km + railroad_station_walk_km + railroad_station_walk_min + ID_railroad_station_walk
                   + railroad_station_avto_km + railroad_station_avto_min + public_transport_station_km + public_transport_station_min_walk
                   + kremlin_km + big_road1_km + big_road2_km + railroad_km + bus_terminal_avto_km + big_market_km + market_shop_km + fitness_km
                   + swim_pool_km + ice_rink_km + stadium_km + basketball_km + public_healthcare_km + university_km + workplaces_km
                   + shopping_centers_km + office_km + big_church_km,
                   data = df,
                   method = 'lm',
                   trControl = train.control)

# print model summary
model.cv

# get the CV results
res <- model.cv$results

# get cross-validated PRESS statistic
PCV.forward <- PRESS.cv(model.cv)

#Our custom model achieved a Kaggle score of 0.13290. This was our best score.

## To test in Kaggle, submit the produced "submit" file "fitFull.all4.Final" is the model we want to use based on adj R-sqr and other stats.
test$predicted.log.price <- predict.lm(fit.model.Final, test)
test$predicted.log.price[is.na(test$predicted.log.price)] <- mean(test$predicted.log.price, na.rm = T)

submit <- test %>% mutate(SalePrice = exp(predicted.log.price)) %>% subset(select=c(Id, price_doc))

write.csv(submit, file = "./kaggle_submission.csv", row.names = F)