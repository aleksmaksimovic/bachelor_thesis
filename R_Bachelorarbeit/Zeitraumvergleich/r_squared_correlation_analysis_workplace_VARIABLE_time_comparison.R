library(dplyr)


bundeslaender <- c("Berlin", "Bavaria", "Hamburg", "Saarland", "Schleswig-Holstein", "Baden-Württemberg", "Brandenburg", "Hessen", "Mecklenburg-Vorpommern", "Lower Saxony", "North Rhine-Westphalia", "Rhineland-Palatinate","Saxony-Anhalt","Saxony","Thuringia", "Bremen" )
years <- c("2020", "2021", "2022")

library("readxl")
population_density <- read_excel("~/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/population_density_bundeslaender.xlsx")
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/_inhalt.html#sprg475598

column.names <- c("time_from", "time_to", "x", "y", "bundesland", "p-value", "r-value")

x="workplaces_percent_change_from_baseline"
y<-c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline") 



dataframe_final <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(dataframe_final) <-c("year", "from", "to", "bundesland", "variable_x", "variable_y", "R-value", "p-value", "population_density" )

i_counter=1



for (year in years)
  
{
  
  print(paste("year: ", year))


for ( bundesland in bundeslaender) 
{
  print(paste(bundesland, "(timespan 1): "))
  

  

  gsub(" ", "",paste("~/Desktop/Studium/Bachelorarbeit/Data Analytics/Data/Region_Mobility_Report_CSVs/",year, "_DE_Region_Mobility_Report.csv"))
  
  
  dataset1 <- read.csv(gsub(" ", "",paste("~/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Region_Mobility_Report_CSVs/",year, "_DE_Region_Mobility_Report.csv")), header=TRUE)
  dataset1$date <- as.Date(dataset1$date)
  
  from_date1=  gsub(" ", "", paste(year, "-02-15"))
  to_date1=   gsub(" ", "",paste(year, "-05-25"))
  from_date2= gsub(" ", "",paste(year, "-09-11"))
  to_date2= gsub(" ", "",paste(year, "-12-20"))
  
  dataset1 <- dataset1[dataset1$sub_region_1==bundesland,]
  #only consider timespans where people are usually on holidays to prevent distortion
  dataset1=filter(dataset1, (date >= as.Date(from_date1) & date <= as.Date(to_date1)))
  #dataset1<-dataset1%>%
  # select(date, workplaces_percent_change_from_baseline, transit_stations_percent_change_from_baseline)
  dataset1$day <- weekdays(as.Date(dataset1$date))
  dataset1=filter(dataset1, day!="Saturday" & day!="Sunday")
  
  
  for (i in 10:15) {
    
   
    linearModel1 <- lm(dataset1[,i] ~ dataset1$workplaces_percent_change_from_baseline, data = dataset1)
    
    a <- cor.test(dataset1$workplaces_percent_change_from_baseline, dataset1[,i])
    print(paste(colnames(dataset1)[i], " R_squared:", summary(linearModel1)$r.squared, " p-value:", a$p.value))
    #add rows
    dataframe_final [i_counter,] <- c(year, from_date1, to_date1, bundesland, "workplaces_percent_change_from_baseline", colnames(dataset1)[i], summary(linearModel1)$r.squared, a$p.value, population_density[, bundesland])
    i_counter=i_counter+1
  }
  
  
  
  
  
  print(paste(bundesland, "(timespan 2): "))
  
  
  
  dataset2 <- read.csv(gsub(" ", "",paste("~/Desktop/Studium/Bachelorarbeit/DataAnalytics/Data/Region_Mobility_Report_CSVs/",year, "_DE_Region_Mobility_Report.csv")), header=TRUE)
  dataset2$date <- as.Date(dataset2$date)
  
  
  dataset2 <- dataset2[dataset2$sub_region_1==bundesland,]
  #only consider timespans where people are usually on holidays to prevent distortion
  dataset2=filter(dataset2, (date >= as.Date(from_date2) & date <= as.Date(to_date2)))
  #dataset2<-dataset2%>%
  # select(date, workplaces_percent_change_from_baseline, transit_stations_percent_change_from_baseline)
  dataset2$day <- weekdays(as.Date(dataset2$date))
  dataset2=filter(dataset2, day!="Saturday" & day!="Sunday")
  
  
  if (year!="2022")
  {
  for (i in 10:15) {
    
    linearModel2 <- lm(dataset2[,i] ~ dataset2$workplaces_percent_change_from_baseline, data = dataset2)
    
    b <- cor.test(dataset2$workplaces_percent_change_from_baseline, dataset2[,i])
    print(paste(colnames(dataset2)[i], " R:", b$estimate, " p-value:", b$p.value))
    dataframe_final [i_counter,] <- c(year, from_date2, to_date2, bundesland, "workplaces_percent_change_from_baseline", colnames(dataset2)[i], summary(linearModel2)$r.squared, b$p.value, population_density[, bundesland])
    i_counter=i_counter+1
  }
  
  }
  
  
  
}

}


write.csv(dataframe_final,"/Users/Aleks/Desktop/Studium/Bachelorarbeit/DataAnalytics/R projects/R_Bachelorarbeit/Zeitraumvergleich/r_squared_dataframe_final_export.csv", row.names = FALSE)





# linModel <- lm(transit_stations_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = dataset1)
# 
# attributes(linModel)
# 
# 
# 
# x=dataset1$workplaces_percent_change_from_baseline
# y=dataset1$transit_stations_percent_change_from_baseline
# 
# plot(transit_stations_percent_change_from_baseline~workplaces_percent_change_from_baseline, data = dataset1,
#      xlab = "workplace change from baseline",
#      ylab = "transit_stations_percent_change_from_baseline",
#      main = paste("Linear correlation (Saarland,  ", from_date, "-", to_date,"); R:", round(cor(x,y), digits=2))
# )
# 
# abline(linModel, col = "red", lwd = 3)
# 
# summary(linModel)









