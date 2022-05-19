library(dplyr)
library(tidyverse)
library(tibble)
library(tibbletime)
library(reshape2)                       

# import dataset Massnahmen Oberkategorien
dataset_measure_category_p_year <- read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Corona Datenplattform/Massnahmen/bl_massnahmen_oberkategorien.csv", header=TRUE)
# remove unnecessary columns
dataset_measure_category_p_year$X_id<- NULL 
dataset_measure_category_p_year$ags2<- NULL 

#reduce dataset to location "Berlin" and category M17 "Arbeitsplatzbeschränkung"
dataset_measure_category_p_year=filter(dataset_measure_category_p_year, bundesland == "Berlin" & m_code== "M17")
  
# remove unnecessary columns
dataset_measure_category_p_year$bundesland<- NULL 
dataset_measure_category_p_year$m_code<- NULL 

# Transpose dataset and rename columns
dataset_measure_category_p_year=as.data.frame(t(dataset_measure_category_p_year))
dataset_measure_category_p_year <- tibble::rownames_to_column(dataset_measure_category_p_year, "date")
names(dataset_measure_category_p_year)[2] <- "indicator"

# Parse date and convert column to date format
dataset_measure_category_p_year=dataset_measure_category_p_year %>%
  mutate(date = substr(date, 2, 9))
dataset_measure_category_p_year=dataset_measure_category_p_year %>%
  mutate(dataset_measure_category_p_year, date = as.POSIXct(date, format = "%Y%m%d"))
dataset_measure_category_p_year$date <- as.Date(dataset_measure_category_p_year$date)

# Convert dataset_measure_subcategory_p_year to tbl_time in order to filter by year
dataset_measure_category_p_year <- as_tbl_time(dataset_measure_category_p_year, index = date)

dataset_measure_category_p_year=filter_time(dataset_measure_category_p_year, ~'2021')















# import dataset Massnahmen Oberkategorien
dataset_measure_subcategory_p_year <- read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Corona Datenplattform/Massnahmen/bl_massnahmen_unterkategorien.csv", header=TRUE)
# remove unnecessary columns
dataset_measure_subcategory_p_year$X_id<- NULL 
dataset_measure_subcategory_p_year$ags2<- NULL 

#reduce dataset to location "Berlin" and category which contains "M17"
dataset_measure_subcategory_p_year<-subset(dataset_measure_subcategory_p_year, grepl("M17", dataset_measure_subcategory_p_year$code))
dataset_measure_subcategory_p_year=filter(dataset_measure_subcategory_p_year, bundesland == "Berlin")

# remove unnecessary columns
dataset_measure_subcategory_p_year$bundesland<- NULL 
dataset_measure_subcategory_p_year$m_code<- NULL 

# Transpose dataset and rename columns, Parse date and convert column to date format
dataset_measure_subcategory_p_year=as.data.frame(t(dataset_measure_subcategory_p_year))
dataset_measure_subcategory_p_year <- tibble::rownames_to_column(dataset_measure_subcategory_p_year, "date")
dataset_measure_subcategory_p_year=dataset_measure_subcategory_p_year %>%
  mutate(date = substr(date, 2, 9))
dataset_measure_subcategory_p_year=dataset_measure_subcategory_p_year %>%
  mutate(dataset_measure_subcategory_p_year, date = as.POSIXct(date, format = "%Y%m%d"))
dataset_measure_subcategory_p_year$date <- as.Date(dataset_measure_subcategory_p_year$date)

colnames(dataset_measure_subcategory_p_year) <- dataset_measure_subcategory_p_year[1, ]
names(dataset_measure_subcategory_p_year)[1] <- "date"
dataset_measure_subcategory_p_year = dataset_measure_subcategory_p_year[-1,]

# Convert dataset_measure_subcategory_p_year to tbl_time in order to filter by year
dataset_measure_subcategory_p_year <- as_tbl_time(dataset_measure_subcategory_p_year, index = date)

dataset_measure_subcategory_p_year=filter_time(dataset_measure_subcategory_p_year, ~'2021')


# Convert all columns ofdataset_measure_subcategory_p_year to numeric


dataset_measure_subcategory_p_year$M17_020 <- as.numeric(dataset_measure_subcategory_p_year$M17_020)
dataset_measure_subcategory_p_year$M17_020_1 <- as.numeric(dataset_measure_subcategory_p_year$M17_020_1)
dataset_measure_subcategory_p_year$M17_020_2 <- as.numeric(dataset_measure_subcategory_p_year$M17_020_2)
dataset_measure_subcategory_p_year$M17_020_3 <- as.numeric(dataset_measure_subcategory_p_year$M17_020_3)
dataset_measure_subcategory_p_year$M17_020_4 <- as.numeric(dataset_measure_subcategory_p_year$M17_020_4)
dataset_measure_subcategory_p_year$M17_020_5 <- as.numeric(dataset_measure_subcategory_p_year$M17_020_5)
dataset_measure_subcategory_p_year$M17_030 <- as.numeric(dataset_measure_subcategory_p_year$M17_030)
dataset_measure_subcategory_p_year$M17_030_1 <- as.numeric(dataset_measure_subcategory_p_year$M17_030_1)
dataset_measure_subcategory_p_year$M17_030_2<- as.numeric(dataset_measure_subcategory_p_year$M17_030_2)
dataset_measure_subcategory_p_year$M17_030_3 <- as.numeric(dataset_measure_subcategory_p_year$M17_030_3)
dataset_measure_subcategory_p_year$M17_030_4 <- as.numeric(dataset_measure_subcategory_p_year$M17_030_4)
dataset_measure_subcategory_p_year$M17_030_5 <- as.numeric(dataset_measure_subcategory_p_year$M17_030_5)







#Merge dataframes dataset_measure_category_p_year and dataset_measure_subcategory_p_year
dataset_merged_categories_p_year=merge(x=dataset_measure_subcategory_p_year, y=dataset_measure_category_p_year,by="date")

measure_series=data.frame(date=dataset_merged_categories_p_year$date, M17_20=dataset_merged_categories_p_year$M17_020, M17_30=dataset_merged_categories_p_year$M17_030)
data_long <- melt(measure_series, id.vars = "date")    # Reshaping data to long format
names(data_long)[1] <- "date"
names(data_long)[2] <- "measure"
names(data_long)[3] <-  "value"


#Plot time series
p<-ggplot(data_long,                       
       aes(x = date,
           y = value,
           col = measure)) +
  geom_line()+
  ggtitle("Workplace restrictions introduced by the government in Berlin (2021)")+
  theme(axis.title.x = element_blank(), axis.line = element_line(size = 0.5, colour = "black", linetype=1), panel.background=element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))

p

p+scale_x_date(date_labels = "%b", date_breaks="1 month")+ylim(0, 2)+scale_y_continuous(breaks=c(0,1,2))



#Unterkategorie M17_20: Arbeitsplatzbeschränkung // Einschränkungsempfehlung (z.B. Homeoffice) & Hygienevorschriften
#Unterkategorie M17_30: Arbeitsplatzbeschränkung // teilweise Schließung; ist die stärkere der beiden Maßnahmen
#Diese Plots sollten beim Vergleich zwischen den Bundesländern dazu dienen, Zeiträume zu wählen, in denen zwischen den Bundesländern
#die exakt selben Maßnahmen gelten. Andernfalls ist ein Vergleich nicht sinnvoll. 





