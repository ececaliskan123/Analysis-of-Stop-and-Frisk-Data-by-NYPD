# **********************
# This snippet calculates the hit-rate as described on p. 372 in the paper
# ***********************


# load dataset (for testing purpose only one year so far)
df <- readRDS("df.rds")
df13 <- df[df$year==2013,]

# some spatial stuff
if(!require("spatstat")) install.packages("spatstat"); library("spatstat")

  # spatstat requires xy-coordintes in .ppp format

range(df$long) # -73.5 - -74.3
range(df$lat) # 40.5 - 41.0
range(df$utmE)

df13$long

data(bei)

ppp = spatstat::ppp(df13$long,df13$lat,c(-73.5,-74.3),c(40.5,41.0))
summary(ppp)

# DEFINE COMPONENTS OF THE FORMULA

    # a) n = total number of CPW stops during year t
    StrToMatch <- c("CPW","C.P.W","WEAPON")
    n <- length(which(grepl(paste(StrToMatch,collapse="|"),df2009$crimsusp)==TRUE))

    # b) y(i) indicates whether the ith-stop was succesful (i.e. weapon found or not)
    # columns addressing this issue: pistol, riflshot, asltweap, knifcuti, machgun, othrweap
    
    df2009$weaponfound <- ifelse(df2009$pistol=="Y" | df2009$riflshot=="Y" | df2009$asltweap=="Y" | df2009$knifcuti=="Y" | df2009$machgun=="Y" | df2009$othrweap=="Y", 1,0)

    # c) geodesic distance
    # s(i) is the location of the i-th stop (probably coordinates)
    # but what is s (without index) ???
    
    head(df2009$xcoord)
    head(df2009$ycoord)
    head(df2009$addrpct) # address precinct
    head(df2009$post) # location of stop post
    head(df2009$sector) # location of stop sector
