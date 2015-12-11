# set working directory and clear environment
setwd("~/Desktop/Berkeley/Classes15_16/PS23T-final-project")
rm(list = ls())

# load packages 
library(ggplot2)
library(ggmap)

# load AIFoutput data
AIF <- read.csv('AIFoutput.csv', header = F, stringsAsFactors = F)

# clean data: naming columns
colnames(AIF) = c("ID", "Link", "Date", "Latitude", "Longitude")

# add column with only the attack's year
date <- as.character(AIF$Date)
AIF$Year <- substr(as.character(AIF$Date), 1, 4)
 
# With dots set to match different years
Iraq_map <- get_map('Iraq', zoom = 6)
ggmap(Iraq_map) +
  geom_point(data=AIF, aes(x=Longitude, y=Latitude, color = factor(Year)), size = 2) +
  scale_color_manual(values=c("deeppink", "darkorchid4", "red3", "chartreuse4", "chocolate4", "darkslategray"))

# Zoomed In Map
Iraq_map_Zoom <- get_googlemap(center = c(lat = 44.3, lon = 33.3), zoom = 10)
ggmap(Iraq_map_Zoom) + 
  geom_point(data=AIF, aes(x=Longitude, y=Latitude, color = factor(Year)), size = 3) +
  scale_color_manual(values=c("deeppink", "darkorchid4", "red3", "chartreuse4", "chocolate4", "darkslategray"))

# Create barplot examining count attack per year and month
year_month <- AIF$Date
year_month_freq <- as.data.frame(table(year_month))
ggplot(data = year_month_freq, aes(x = year_month, y = Freq )) + geom_bar(stat = "identity") +
  ggtitle("AIF Attacks by Month and Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Frequency") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(face = "bold", size = 16))

# Create barplot examining count attack per year
year_df <- as.data.frame(table(AIF$Year))
ggplot(data = year_df, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") +
  ggtitle("AIF Attacks by Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Frequency") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(face = "bold", size = 16))
