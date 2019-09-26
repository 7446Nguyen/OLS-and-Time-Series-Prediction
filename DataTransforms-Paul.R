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
       ,MissMech) #MissMech to test if data missing completely at random with ?TestMCARNormality

#format dates:
modelingData = modelingData %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

df <- read.csv("./modelingData.csv",  header=T, sep=",", strip.white=T)


#TestMCARNormality(df, imputation.number = 5333, imputed.data = df$life_sq, alpha = 0.05) # can only use non NAs to test NAs


colnames(df)

na_count <- sapply(df, function(cnt) sum(length(which(is.na(cnt)))))
na_count

########################################################################## 
######################## Imputing NAs##################################### 
##########################################################################

#1
df$floor <- df$floor %>% replace_na(0) # Replace NA in Floor (this is for apts.; not all bldgs are apts.)
hist(df$floor,breaks=500) # this looks better normal

#2
df$hospital_beds_raion <- df$hospital_beds_raion %>% replace_na(0) # Replace NA in hospital beds with zero
hist(df$hospital_beds_raion,breaks = 100)
hist(logb(df$hospital_beds_raion, exp(3)),breaks = 50)

df$hospital_beds_raion <- logb(df$hospital_beds_raion, exp(3))
hist(df$hospital_beds_raion, breaks = 50)

#if NA, then no. Otherwise, Yes############################################################################################################
#if NA, then no. Otherwise, Yes############################################################################################################

df$material <- dplyr::recode(df$material, `1` = "Material_1",`2` = "Material_2",`3` = "Material_3"
                             ,`4` = "Material_4",`5` = "Material_5",`6` = "Material_6") %>% as.factor()


#3
# round materials to get the average material used, grouped by product type:
df <- df %>% group_by(product_type) %>% mutate(material = na.mean(material))
df$material <- round(df$material) # round because this will be converted to factor
# Recoding to qualitative because these are not best represented as numeric (material 2 is not expressly twice
# as good as material 1, for example, so this is a naive assumption)
df$material <- dplyr::recode(df$material, `1` = "Material_1",`2` = "Material_2",`3` = "Material_3"
                             ,`4` = "Material_4",`5` = "Material_5",`6` = "Material_6") %>% as.factor()

#4
########## replace NA max_flor with the proportion of full_sq to max_floor where max_floor not null
df2.maxfl <- df[which(!is.na(df$max_floor)),]
# using domain knowledge that floor count depends on full square footage
maxfl.Mean <- data.frame(df2.maxfl$max_floor/df2.maxfl$full_sq)
colnames(maxfl.Mean) <- "percentofFloor"
maxflMultiplier <- mean(head(maxfl.Mean$percentofFloor,7000)) # roughly 7k NaNs
#max_fl is position 6, full_sq is position 3:
df[which(is.na(df$max_floor)),6] <- df[which(is.na(df$max_floor)),3]*maxflMultiplier

hist(df$max_floor, breaks=100)
##########

#5
########## replace NA life_sq with the proportion of full_sq to life_sq where life_sq not null
df2.lifesq <- df[which(!is.na(df$life_sq)),]
life.Mean <- data.frame(df2.lifesq$life_sq/df2.lifesq$full_sq)
colnames(life.Mean) <- "percentofFull"
lifesqMultiplier <- mean(head(life.Mean$percentofFull,11000)) # roughly 11k NAs
#life_sq is position 4, full_sq is position 3. Living area depends on full area
df[which(is.na(df$life_sq)),4] <- df[which(is.na(df$life_sq)),3]*lifesqMultiplier

hist(df$life_sq, breaks=100)
##########

#######################################################################################################################
#######################################################################################################################
#################################   Below are Naive Transformations   #################################################
#######################  Turn these transforms off if they don't help the model########################################
#######################################################################################################################
#######################################################################################################################

#6 - if using this log transform produces large standard error in the model (if selected) model, consider from model altogether
########## replace NA kitch_sq with the proportion of full_sq to kitch_sq where kitch_sq not null
df2.kitchsq <- df[which(!is.na(df$kitch_sq)),]
kitch.Mean <- data.frame(df2.kitchsq$kitch_sq/df2.kitchsq$full_sq)
colnames(kitch.Mean) <- "percentofFull"
kitchsqMultiplier <- mean(head(kitch.Mean$percentofFull,7000)) #roughly 7k NAs

# below is a logical transform to impute the NA values, but will still need transformation as an entire predictor
#kitch_sq is position 10, full_sq is position 3:
df[which(is.na(df$kitch_sq)),10] <- df[which(is.na(df$kitch_sq)),3]*kitchsqMultiplier

par(mfrow=c(2,2))
hist(log(df$kitch_sq + 1), breaks=100, xlim=c(0,10))
hist(log(df$kitch_sq), breaks=100, xlim=c(0,10))
# The log transformation yields the best result across all levels of breaks, indicating the closest adherence to normality
# While unorthodox, this may yiled better fits in the modeling phase
hist(sqrt(df$kitch_sq^1/16+1), breaks=100, xlim=c(1,2))

df$kitch_sq <- sqrt(df$kitch_sq^1/16+1)
par(mfrow=c(1,1)) # resetting frame for downstream graphing

##########

#8 
# round materials to get the average number of rooms, grouped by floor count, full_sq:
df2.numRm <- df[which(!is.na(df$num_room)),]
numRm.Mean <- data.frame(df2.numRm$num_room/df2.numRm$full_sq)
colnames(numRm.Mean) <- "percentofFullrm"
numRmMultiplier <- mean(head(numRm.Mean$percentofFullrm,7000)) #roughly 7k NAs
df[which(is.na(df$num_room)),9] <- df[which(is.na(df$num_room)),3]*numRmMultiplier # leave continuous for more descriptive metrics

#those houses with full_sq=0 will have 0 for num_room, life_sq, etc.
hist(df$num_room, breaks = 500)  (right skewed)########################*****************************################$$$$$$$$$$$$
max(df$num_room)
df[!df$num_room > 30,]
df <- df[which(df$num_room < 30),] # remove the outlier 192.6242

#########################################################################################
# at this point, there are some NA values in columns that aren't obvious using domain knowledge
# to impute for these - where the model will apply them, and if they don't work, will remove them
# we're removing all NAs from the dataframe to build a correlation matrix
#########################################################################################
#convert all char to char, numeric and int to numeric for consistency in analysis
df <- df %>% mutate_if(is.integer, as.numeric) %>% 
  mutate_if(is.character, as.factor) %>% data.frame()

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
view(correlation.matrix)
correlation.matrix[which(correlation.matrix =="price_doc"),]
corDF.ordered[which(corDF.ordered$row=="price_doc"),]

#Order the correlation matrix to show the highest correlated
corDF.ordered <- data.frame(corDF[order(-corDF$cor),])

somewhat.correlated <- corDF[which(corDF$cor >= 0.5),]
#view(somewhat.correlated)

#########################################################################################
# this is                                                                               #
#     where the                                                                         #  
#           corrplot section                                                            #
#                          ends                                                         #
#########################################################################################

##########
#9 preschool_quota
df.preKquota <- somewhat.correlated[which(somewhat.correlated$row=="preschool_quota"),]
view(df.preKquota)

mostCorrelated.preKquota <- df.preKquota[1,]
mostCorr.PreK.var <- mostCorrelated.preKquota$column
mostCorr.PreK.var # this is X0_6_all - this attribute has no NA values


# replace NA preschoo_quota with the proportion of most related variable "X0_6_all" to preschool_quota where preschool_quota not null
df.preK.no.NA <- df[which(!is.na(df$preschool_quota)),]
preK.Mean <- data.frame(df.preK.no.NA$preschool_quota/df.preK.no.NA$X0_6_all)
colnames(preK.Mean) <- "percentofFloor"
preKquotaMultiplier <- mean(head(preK.Mean$percentofFloor,5000)) # roughly 5k NaNs
#preschool_quota is position 16, x0_6_all is position 26:
df[which(is.na(df$preschool_quota)),16] <- df[which(is.na(df$preschool_quota)),26]*preKquotaMultiplier

par(mfrow=c(2,1))
hist(df$preschool_quota, breaks=100)
hist(log(df$preschool_quota), breaks=100) #log does not appear useful
hist(sqrt(df$preschool_quota), breaks = 100) #the square root transformation looks better than log or non-transformed
df$preschool_quota <- sqrt(df$preschool_quota)
#reset output to 1x1
par(mfrow=c(1,1))
##########


##########
#10 
#hospital_beds_raion
#there is weak correlation between this and all other variables in the correlation matrix. 0.29 was strongest correlation
# therefore, this variable will have NA imputed with 0
df.hospital_beds_raion <- corDF.ordered[which(corDF.ordered$row=="hospital_beds_raion"),]
view(df.hospital_beds_raion)

df$hospital_beds_raion <- df$hospital_beds_raion %>% replace_na(0)

par(mfrow=c(2,1))
hist(df$hospital_beds_raion, breaks=100)
hist(log(df$hospital_beds_raion), breaks=100) #log looks a little better
df$hospital_beds_raion <- log(df$hospital_beds_raion)
#reset output to 1x1
par(mfrow=c(1,1))
##########

class(df$build_year)
plot(nubuildyr$build_year,nubuildyr$price_doc) 

nubuildyr <- df %>% filter(build_year<2019 & build_year>1700)
df$build_year <- nubuildyr$build_year


#######################################################################################################################
#####################################End of Naive ("high-risk") Imputations############################################
#####################################End of Naive ("high-risk") Imputations############################################
#####################################End of Naive ("high-risk") Imputations############################################
#####################################End of Naive ("high-risk") Imputations############################################
#######################################################################################################################

#11
# Build_Year

# Count of build_year values equal to NA
nrow(df[which(is.na(df$build_year)),])/nrow(df)

# Build_year has no strong correlation with other non-NA values using the correlation plot
# Therefore, imputation would be arbitrary
df.buildYr <- corDF.ordered[which(corDF.ordered$row=="build_year"),]

# NA build_year values also total roughly 45% so imputation is too risky for inclusion. Thus, dropping build_year from model
df <- subset(df, select = -c(build_year))

#12
hist(df$raion_popul, breaks = 1000)
hist(logb(df$raion_popul,exp(7)), breaks=1000) 
hist(sqrt(df$raion_popul), breaks=1000)

hist(sqrt(df$office_raion^1/10), breaks = 10)
df$office_raion <- sqrt(df$office_raion^1/10)

df$big_market_raion <- dplyr::recode(df$big_market_raion,  "No" = 0, "Yes"= 1)

df$railroad_terminal_raion <- dplyr::recode(df$railroad_terminal_raion,  "No" = 0, "Yes"= 1)

# This is better non-transformed. Also, the range doesn't warrant transformation compared to the data overall
hist(log(df$shopping_centers_raion), breaks = 10)

hist(log(df$children_school), breaks = 100)
hist(sqrt(df$children_school), breaks = 100)
hist(df$children_school, breaks = 100) # this looks the best, but is not great. No touchy.
hist(df$children_preschool,breaks = 100) #similar to children school...no touchy.

hist(log(df$indust_part), breaks = 100)
hist(sqrt(df$indust_part^1/6), breaks = 100)
hist(df$indust_part, breaks = 100) # also, no touchy

hist(sqrt(df$green_zone_part), breaks = 100)
hist(df$green_zone_part, breaks = 100) # nothing looks great- no transformations applied

hist(df$full_sq, breaks = 1000)
hist(log(df$full_sq+1), breaks=1000)
range(log(df$full_sq+1.1))

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
skim(df)

hist(df$full_sq, breaks=100)
hist(log(df$full_sq), breaks=100) #normal approximation so will log-transform this one
df$full_sq <- log(df$full_sq) # log transform


##################################################################### colinearity/covariance


df %>%keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() + 
  geom_histogram(binwidth = NULL) +
  theme_fivethirtyeight() +
  ggtitle("Numeric Predictor Variables, Post Log-Transforming")


#####################################################################


lm.buildyr <- lm(price_doc~build_year,data=df)
summary(lm.buildyr)
dwtest(lm.buildyr, max.lag=1, simulate=T, reps=1000, method = "normal", alternative = "two.sided")

######################################################################
###We need the graphs for interactive terms to display interaction####
######################################################################