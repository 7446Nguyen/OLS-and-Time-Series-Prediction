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

df <- data.frame(df$id, df$timestamp, df$price_doc)
colnames(df) <- c("id", "timestamp", "price_doc")

df <- df %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

timestamp2 <- df$timestamp
df <- data.frame(timestamp2, df)

df <- df %>% mutate(timestamp = as.Date(timestamp, origin="1899-12-30"))
tryFormats = c("%Y-%m-%d", "%Y/%m/%d")

df <- df %>% separate(timestamp2, sep="-", into = c("year", "month", "day"))

monthYear <- as.factor(paste0(df$year,df$month))
df <- data.frame(df$id, df$timestamp, df$month, df$day, df$year, monthYear, df$price_doc)
colnames(df) <- c("id", "timestamp", "month", "day", "year", "monthYear", "price_doc")

write.csv(df, "timeSeriesA.csv", row.names = F)

df2 <- data.frame(monthYear, df)

df2 <- data.frame(df2$month, df2$monthYear, df2$price_doc)

colnames(df2) <- c("MonthNumber", "monthYear", "price_doc")


# creating ordered dataframe for overall medians for each state 
dfAvgPriceMoYr <- df2 %>% group_by(monthYear, MonthNumber) %>% summarise(AvgPrice = mean(price_doc)) %>% data.frame()
df2 <- data.frame(dfAvgPriceMoYr$MonthNumber, dfAvgPriceMoYr$monthYear, dfAvgPriceMoYr$AvgPrice)
colnames(df2) <- c("MonthNumber", "monthYear", "AvgPrice")

write.csv(df2,"timeSeriesB.csv", row.names = F)