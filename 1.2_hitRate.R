# **********************
# This snippet calculates the hit-rate as described on p. 372 in the paper
# ***********************


# load dataset (for testing purpose only one year so far)
df <- readRDS("df.rds")
df13 <- df[df$year==2013,]

# some spatial stuff
if(!require("spatstat")) install.packages("spatstat"); library("spatstat")
if(!require("leaflet")) install.packages("leaflet"); library("leaflet")

# change UTMW to UTMN
names(df13)[29] = "utmN"

  # spatstat requires xy-coordintes in .ppp format

range(df$long) # -73.5 - -74.3
range(df$lat) # 40.5 - 41.0
range(df$utmE)
range(df$utmN)

ppp = spatstat::ppp(df13$utmE,df13$utmN,xrange=c(range(df$utmE)),yrange=c(range(df$utmW)))
summary(ppp)
plot(ppp)

# rescale can be used to obtain a standard unit of length
test = rescale(ppp)
intensity = intensity(test)

summary(ppp)$intensity
plot(density(ppp,sigma=50))
den <- density(ppp,sigma=50)

# kernel density estimation with stats-package
density(ppp,bw = "nrd0", kernel=gaussian, na.rm=TRUE) # just errors :(



# pdf: pointpatterntutorial probably helpful! special chapter on "intensities"



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
    
## NEW SKETCH
    
    # we need a matrix first that creates all pairwise geodesic distances
    
    # geodesic distance in kilometres
    if(!require("Imap")) install.packages("Imap"); library("Imap") # function gdist
    if(!require("geodist")) install.packages("geodist"); library("geodist") # check
    
    # for the geodist-package, we need rectangualer objects with lon/lat 
    x     = df[df$year>=2016,c("long","lat")]
    test  = as.data.frame(geodist(x)/1000)  # creates a pairwise matrix! -- does the job 
    
    
    df = df[df$year>=2016,]
    df$new = (df$weaponfound*colSums(exp(-test^2/2))) / colSums(exp(-test^2/2))
    
    
    
    
    # 
    
  # to be done!
    
  which(df$year==2016 & df$weaponfound==1)
  
  sapply(df[df$year==2016,])
  
  sum(df$weaponfound*exp(-test[,]^2/2)/exp(-test[,]^2/2))
    
    
      
    
    yrs = 2015:2016
    ObsYr = sapply(yrs, function(x) which(df$year==x)) # this fct. yields row numbers for each year
    ObsAll = which(df$year==2015 | df$year==2016)
    
    sapply(ObsYr, function(x) sapply())
    
    sapply(test)
    
    for (i in unique(df$year==2015 | df$year==2016)){
      
      yr = 2012:2016
      rows = 1:100
      
      test = sapply(obs, function(x) {
        Imap::gdist(df$long[1],df$lat[1],df$long[x],df$lat[x],units="km")
      })
      test = exp (-(test*test)/2) 
      HR = sum(df$weaponfound[obs]*test)/sum(test)
      
      
    }
    
