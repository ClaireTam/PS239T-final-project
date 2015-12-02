setwd("~/Desktop/Berkeley/Classes15_16/POLS239T/PS239T/10_webscraping")

rm(list = ls())

library(ggplot2)
library(maps)
library(sandwich)
library(classInt)
library(RColorBrewer)
library(sp)
library(maptools)
library(rgeos)
library(rgdal) 
library(ggmap)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

world <- map_data("world") 
ggplot(world[world$region=="Iraq",], aes(x=long, y=lat)) + geom_path(aes(group=group)) 
  
geom_point(data = AIF, aes(y = AIF$Longitude, x = AIF$Latitude))


AIF <- read.csv('AIFoutput.csv', header = F, stringsAsFactors = F)
colnames(AIF) = c("ID", "Link", "Date", "Latitude", "Longitude")
names(AIF)

date <- as.character(AIF_2$Date)

class(date)

AIF$Year <- substr(as.character(AIF$Date), 1, 4)

dim(AIF)
names(AIF)

unique(dat_coords)

dat_coords <- cbind(AIF$Longitude, AIF$Latitude)
row.names(dat_coords) <- AIF$ID
str(dat_coords)
dat_coords

dat_sp <- SpatialPoints(dat_coords, 
                        proj4string=CRS("+proj=longlat + ellps=mercator"))

class(dat_sp) 
str(dat_sp) 
summary(dat_sp)

slot(dat_sp, "coords")
bbox(dat_sp)
proj4string(dat_sp)

plot(dat_sp)
points(coordinates(dat_sp),
       coordinates(dat_sp),col="red", pch=20)

multiplot(p2) + plot(dat_sp) 



library(plyr)
year_mon_freq <- as.data.frame(table(AIF$Date))


year.month <- count(AIF, vars = c("Date"))

head(year.month)

library(ggplot2)

def.off()
hist(year_mon_freq$Freq, xlab = 'month_year')

plot(x = year_mon_freq$Var1, y = year_mon_freq$Freq)
ggplot(data = year_mon_freq, aes(x = Var1, y = Freq)) + geom_point()

dev.off()

library(RgoogleMaps)
library(maps)
install.packages("mapdata")
library(mapdata)
map('worldHires','Iraq') + multiplot(p2) + plot(dat_sp)

Iraq_map <- get_map('Iraq', zoom = 6)

ggmap(Iraq_map) +
  geom_point(data=AIF, aes(x=Longitude, y=Latitude), size = 3.5, colour = I(AIF$Year))

ggmap(Iraq_map) +
  geom_point(data=AIF, aes(x=Longitude, y=Latitude), size = 3.5)

unique(AIF$Year)

AIF[AIF$Year == 2004 ]


legend(2000, 9.5, c(2004, 2005, 2006, 2007, 2008, 2009))
       