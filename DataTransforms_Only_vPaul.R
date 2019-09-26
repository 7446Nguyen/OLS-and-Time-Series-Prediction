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

########## Floor
df$floor <- df$floor %>% replace_na(0)
##########

########## Hospital bed raion
df[which(!is.na(df$hospital_beds_raion)),] <- 1
df$hospital_beds_raion <- df$hospital_beds_raion %>% replace_na(0)
##########

########## material
df <- df %>% group_by(product_type) %>% mutate(material = na.mean(material))
df$material <- round(df$material)
df$material <- dplyr::recode(df$material, `1` = "Material_1",`2` = "Material_2",`3` = "Material_3"
                             ,`4` = "Material_4",`5` = "Material_5",`6` = "Material_6") %>% as.factor()
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
##########

########## kithcen sq
df2.kitchsq <- df[which(!is.na(df$kitch_sq)),]
kitch.Mean <- data.frame(df2.kitchsq$kitch_sq/df2.kitchsq$full_sq)
colnames(kitch.Mean) <- "percentofFull"
kitchsqMultiplier <- mean(head(kitch.Mean$percentofFull,7000))
df[which(is.na(df$kitch_sq)),10] <- df[which(is.na(df$kitch_sq)),3]*kitchsqMultiplier
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
##########

########## build year
df <- subset(df, select = -c(build_year))
##########

########## office raion
df$office_raion <- sqrt(df$office_raion^1/10)
##########

########## big market raion
df$big_market_raion <- dplyr::recode(df$big_market_raion,  "No" = 0, "Yes"= 1)
##########

########## railroad terminal raion
df$railroad_terminal_raion <- dplyr::recode(df$railroad_terminal_raion,  "No" = 0, "Yes"= 1)
##########

######################################
##############            ###########
####             end              ##    oh
###              of              ##     like
##            transforms        ##      a
###########            ##########       rock
################################

############################## conversion to numeric and factor only for modeling consistency #############################
df <- df %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.character, as.factor) %>% data.frame()