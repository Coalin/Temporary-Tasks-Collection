library(spdep)
library(readxl)
library(sqldf)

# Data Preparation
# STEP I: Guangzhou
guangzhou <- read_excel("~/Desktop/data/guangzhoushenzhen.xlsx",sheet = "gz")
gz <-  guangzhou[c("Longitude","Latitude","Price","Year")]
summary(gz)
gz_new <- sqldf('SELECT Longitude, Latitude, Year, AVG(Price) 
                   AS AveragePrice 
                FROM gz 
                GROUP BY Longitude, Latitude, Year')
gz_new <- na.omit(gz_new)

moran <- function(data, year, k){
  gzyear <- data[data$Year == year,]
  gzyear <- na.omit(gzyear)
  longlat <- cbind(gzyear$Longitude, gzyear$Latitude)
  splonglat <- SpatialPoints(longlat, proj4string = 
                             CRS("+proj=longlat +datum=WGS84"))
  nb <- knn2nb(knearneigh(splonglat, k, longlat = TRUE))
  snb <- make.sym.nb(nb)
  plot(nb2listw(snb), cbind(longlat[,1], longlat[,2]))
  moran.test(gzyear$AveragePrice, nb2listw(snb))
}

moran(gz_new, 2015, 3)
moran(gz_new, 2014, 3)
moran(gz_new, 2013, 3)
moran(gz_new, 2012, 3)
moran(gz_new, 2011, 3)
moran(gz_new, 2010, 3)
moran(gz_new, 2009, 3)
moran(gz_new, 2008, 3)
moran(gz_new, 2007, 3)
# moran(gz_new, 2006, 3)
# 广州06年仅有一条记录，05年甚至没有记录！
# moran(gz, 2005)
# moran(gz, 2004)

# STEP II: Shenzhen
shenzhen <- read_excel("~/Desktop/data/guangzhoushenzhen.xlsx",sheet = "sz")
sz <-  shenzhen[c("Longitude","Latitude","Price","Year")]
summary(sz)
sz_new <- sqldf('SELECT Longitude, Latitude, Year, AVG(Price) 
                AS AveragePrice 
                FROM sz 
                GROUP BY Longitude, Latitude, Year')
sz_new <- na.omit(sz_new)
summary(sz_new)

# moran(sz_new, 2015, 3)
# 2015年数据量不够...
moran(sz_new, 2014, 3)
moran(sz_new, 2013, 3)
moran(sz_new, 2012, 3)
moran(sz_new, 2011, 3)
moran(sz_new, 2010, 3)
moran(sz_new, 2009, 3)
moran(sz_new, 2008, 3)
moran(sz_new, 2007, 3)



