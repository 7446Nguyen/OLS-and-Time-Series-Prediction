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
       ,ggthemes)

#format dates:
modelingData = modelingData %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

df <- read.csv("./modelingData.csv",  header=T, sep=",", strip.white=T)

colnames(df)

na_count <- sapply(df, function(cnt) sum(length(which(is.na(cnt)))))
na_count

########################################################################## 
######################## Imputing NAs##################################### 
########################################################################## 
#1
df$floor <- df$floor %>% replace_na(0) # Replace NA in Floor (this is for apts.; not all bldgs are apts.)

#2
df$hospital_beds_raion <- df$hospital_beds_raion %>% replace_na(0) # Replace NA in hospital beds with zero

#3
# round materials to get the average material used, grouped by product type:
df <- df %>% group_by(product_type) %>% mutate(material = na.mean(material))
df$material <- round(df$material) # round because this will be converted to factor

#4
########## replace NA max_flor with the proportion of full_sq to max_floor where max_floor not null
df2.maxfl <- df[which(!is.na(df$max_floor)),]
maxfl.Mean <- data.frame(df2.maxfl$max_floor/df2.maxfl$full_sq)
colnames(maxfl.Mean) <- "percentofFloor"
maxflMultiplier <- mean(head(maxfl.Mean$percentofFloor,7000)) # roughly 7k NaNs
df[which(is.na(df$max_floor)),6] <- df[which(is.na(df$max_floor)),3]*maxflMultiplier

hist(df$max_floor, breaks=100)
##########


#5
########## replace NA life_sq with the proportion of full_sq to life_sq where life_sq not null
df2.lifesq <- df[which(!is.na(df$life_sq)),]
life.Mean <- data.frame(df2.lifesq$life_sq/df2.lifesq$full_sq)
colnames(life.Mean) <- "percentofFull"
lifesqMultiplier <- mean(head(life.Mean$percentofFull,11000)) # roughly 11k NAs
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
df[which(is.na(df$kitch_sq)),10] <- df[which(is.na(df$kitch_sq)),3]*kitchsqMultiplier

hist(log(df$kitch_sq), breaks=50)
hist(log(df$kitch_sq+1), breaks=50)
##########

#7 
########## drop build_year from model
# Count of build_year values equal to NA
nrow(df[which(is.na(df$build_year)),])/nrow(df)
# NA build_year values total roughly 45% so imputation is too risky for inclusion. Thus, dropping build_year from model
df = subset(df, select = -c(build_year))


#8 
# round materials to get the average number of rooms, grouped by floor count, full_sq:
df <- df %>% group_by(full_sq) %>% mutate(num_room = na.mean(num_room))
#those houses with full_sq=0 will have 0 for num_room, life_sq, etc.

nrow(df[which(is.na(df$build_year)),])/nrow(df)

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

#See what variables are correlated with eachother, p-values
correlation.matrix <- rcorr(as.matrix(df.numeric.no.NA))
corDF <- data.frame(flattenCorrMatrix(correlation.matrix$r, correlation.matrix$P))

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
df[which(is.na(df$preschool_quota)),6] <- df[which(is.na(df$preschool_quota)),3]*preKquotaMultiplier

par(mfrow=c(2,1))
hist(df$preschool_quota, breaks=100)
hist(log(df$preschool_quota), breaks=100) #log does not appear needed
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



#######################################################################################################################
#####################################End of Naive ("high-risk") Imputations############################################
#####################################End of Naive ("high-risk") Imputations############################################
#####################################End of Naive ("high-risk") Imputations############################################
#####################################End of Naive ("high-risk") Imputations############################################
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