df <- readRDS("df.rds")



df[, c("xcoord", "ycoord", "perobs", "formated_date", "timestop", "offunif" , "crimsusp", "CPW")] <-NULL


df$year <- year(df$formated_date)
training <- subset(df, year== 2013 | year== 2014)
test <- subset(df, year== 2015 | year== 2016)
df$year <- NULL



sgd(weaponfound ~ . + .*.)


