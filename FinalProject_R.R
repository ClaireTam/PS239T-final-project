setwd("~/Desktop/Berkeley/Classes15_16/PS23T-final-project")

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

# load AIFoutput data
AIF <- read.csv('AIFoutput.csv', header = F, stringsAsFactors = F)

# merge latitude and longitude 
dat_coords <- cbind(AIF$Longitude, AIF$Latitude)
unique(dat_coords)

colnames(AIF) = c("ID", "Link", "Date", "Latitude", "Longitude")
names(AIF)

date <- as.character(AIF$Date)


AIF$Year <- substr(as.character(AIF$Date), 1, 4)

dim(AIF)
names(AIF)

unique(dat_coords)


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

#correct mapping starts here
library(RgoogleMaps)
library(maps)
install.packages("mapdata")

library(mapdata)
map('worldHires','Iraq') + multiplot(p2) + plot(dat_sp)

Iraq_map <- get_map('Iraq', zoom = 6)
 
# With dots set to match different years
ggmap(Iraq_map) +
  geom_point(data=AIF, aes(x=Longitude, y=Latitude, color = factor(Year)), size = 2) +
  scale_color_manual(values=c("deeppink", "darkorchid4", "red3", "chartreuse4", "chocolate4", "darkslategray"))


#graphing distribution of attacks
year_month <- AIF$Date

#creates data frame with year/month and frequency
year_month_freq <- as.data.frame(table(year_month))
ggplot(data = year_month_freq, aes(x = year_month, y = Freq )) + geom_bar(stat = "identity")

# looking at distribution by year
year_df <- as.data.frame(table(AIF$Year))

names(year_df)

ggplot(data = year_df, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")

#multiplot
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
