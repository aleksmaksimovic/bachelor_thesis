library(dplyr)
library(tidyverse)
library(tibble)
library(tibbletime)

# import dataset Massnahmen Oberkategorien
dataset_massnahmen <- read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Corona Datenplattform/Massnahmen/bl_massnahmen_oberkategorien.csv", header=TRUE)
# remove unnecessary columns
dataset_massnahmen$X_id<- NULL 
dataset_massnahmen$ags2<- NULL 

#reduce dataset to location "Berlin" and category M17 "Arbeitsplatzbeschränkung"
dataset_massnahmen=filter(dataset_massnahmen, bundesland == "Berlin" & m_code== "M17")

# remove unnecessary columns
dataset_massnahmen$bundesland<- NULL 
dataset_massnahmen$m_code<- NULL 

# Transpose dataset and rename columns
dataset_massnahmen=as.data.frame(t(dataset_massnahmen))
dataset_massnahmen <- tibble::rownames_to_column(dataset_massnahmen, "date")
names(dataset_massnahmen)[2] <- "indicator"

# Parse date and convert column to date format
dataset_massnahmen=dataset_massnahmen %>%
  mutate(date = substr(date, 2, 9))
dataset_massnahmen=dataset_massnahmen %>%
  mutate(dataset_massnahmen, date = as.POSIXct(date, format = "%Y%m%d"))
dataset_massnahmen$date <- as.Date(dataset_massnahmen$date)

data(dataset_massnahmen)






#Plot time series

p <- ggplot(dataset_massnahmen, aes(x=date, y=indicator)) +
  geom_line(color="red") +
  xlab("Date") +ylab("Restriction enforced")+ggtitle("Workplace restriction introduced by the government") +
  theme(axis.line = element_line(size = 0.5, colour = "black", linetype=1), panel.background=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
p

p+scale_x_date(date_labels = "%m-%y")+ylim(0, 2)






