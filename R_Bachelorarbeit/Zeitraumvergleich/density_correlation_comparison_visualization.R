library(dplyr)
library(ggplot2)

dataframe=read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/DataAnalytics/R projects/R_Bachelorarbeit/Zeitraumvergleich/dataframe_final_export.csv", header = TRUE, sep = ",")
bundeslaender <- c("Berlin", "Bavaria", "Hamburg", "Saarland", "Schleswig-Holstein", "Baden-Württemberg", "Brandenburg", "Hessen", "Mecklenburg-Vorpommern", "Lower Saxony", "North Rhine-Westphalia", "Rhineland-Palatinate","Saxony-Anhalt","Saxony","Thuringia", "Bremen" )
y<-c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline")
color<-c("blue", "red", "green", "orange", "purple")


dataset_variable1=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="retail_and_recreation_percent_change_from_baseline", from=="2020-02-15")
dataset_variable2=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="retail_and_recreation_percent_change_from_baseline", from=="2020-09-11")
dataset_variable3=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="retail_and_recreation_percent_change_from_baseline", from=="2021-02-15")
dataset_variable4=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="retail_and_recreation_percent_change_from_baseline", from=="2021-09-11")
dataset_variable5=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="retail_and_recreation_percent_change_from_baseline", from=="2022-02-15")

dataset_variable6=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="grocery_and_pharmacy_percent_change_from_baseline", from=="2020-02-15")
dataset_variable7=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="grocery_and_pharmacy_percent_change_from_baseline", from=="2020-09-11")
dataset_variable8=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="grocery_and_pharmacy_percent_change_from_baseline", from=="2021-02-15")
dataset_variable9=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="grocery_and_pharmacy_percent_change_from_baseline", from=="2021-09-11")
dataset_variable10=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="grocery_and_pharmacy_percent_change_from_baseline", from=="2022-02-15")

dataset_variable11=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="parks_percent_change_from_baseline", from=="2020-02-15")
dataset_variable12=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="parks_percent_change_from_baseline", from=="2020-09-11")
dataset_variable13=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="parks_percent_change_from_baseline", from=="2021-02-15")
dataset_variable14=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="parks_percent_change_from_baseline", from=="2021-09-11")
dataset_variable15=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="parks_percent_change_from_baseline", from=="2022-02-15")

dataset_variable16=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="transit_stations_percent_change_from_baseline", from=="2020-02-15")
dataset_variable17=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="transit_stations_percent_change_from_baseline", from=="2020-09-11")
dataset_variable18=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="transit_stations_percent_change_from_baseline", from=="2021-02-15")
dataset_variable19=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="transit_stations_percent_change_from_baseline", from=="2021-09-11")
dataset_variable20=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="transit_stations_percent_change_from_baseline", from=="2022-02-15")

dataset_variable21=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="residential_percent_change_from_baseline", from=="2020-02-15")
dataset_variable22=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="residential_percent_change_from_baseline", from=="2020-09-11")
dataset_variable23=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="residential_percent_change_from_baseline", from=="2021-02-15")
dataset_variable24=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="residential_percent_change_from_baseline", from=="2021-09-11")
dataset_variable25=filter(dataframe, variable_y!="workplaces_percent_change_from_baseline", variable_y=="residential_percent_change_from_baseline", from=="2022-02-15")








a<-ggplot()+ggtitle("Correlation of population density with R-value (retail_and_recreation_percent_change_from_baseline)")+
  geom_line(data=dataset_variable1, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_line(data=dataset_variable2, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_line(data=dataset_variable3, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_line(data=dataset_variable4, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_line(data=dataset_variable5, mapping=aes(x=population_density, y=R.value), color="green")+
  geom_point(data=dataset_variable1, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_point(data=dataset_variable2, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_point(data=dataset_variable3, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_point(data=dataset_variable4, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_point(data=dataset_variable5, mapping=aes(x=population_density, y=R.value), color="green")
a+theme_light()


b<-ggplot()+ggtitle("Correlation of population density with R-value (grocery_and_pharmacy_change_from_baseline)")+
  geom_line(data=dataset_variable6, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_line(data=dataset_variable7, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_line(data=dataset_variable8, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_line(data=dataset_variable9, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_line(data=dataset_variable10, mapping=aes(x=population_density, y=R.value), color="green")+
  geom_point(data=dataset_variable6, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_point(data=dataset_variable7, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_point(data=dataset_variable8, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_point(data=dataset_variable9, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_point(data=dataset_variable10, mapping=aes(x=population_density, y=R.value), color="green")
b+theme_light()

c<-ggplot()+ggtitle("Correlation of population density with R-value (parks_percent_change_from_baseline)")+
  geom_line(data=dataset_variable11, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_line(data=dataset_variable12, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_line(data=dataset_variable13, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_line(data=dataset_variable14, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_line(data=dataset_variable15, mapping=aes(x=population_density, y=R.value), color="green")+
  geom_point(data=dataset_variable11, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_point(data=dataset_variable12, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_point(data=dataset_variable13, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_point(data=dataset_variable14, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_point(data=dataset_variable15, mapping=aes(x=population_density, y=R.value), color="green")
c+theme_light()

d<-ggplot()+ggtitle("Correlation of population density with R-value (transit_percent_change_from_baseline)")+
  geom_line(data=dataset_variable16, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_line(data=dataset_variable17, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_line(data=dataset_variable18, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_line(data=dataset_variable19, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_line(data=dataset_variable20, mapping=aes(x=population_density, y=R.value), color="green")+
  geom_point(data=dataset_variable16, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_point(data=dataset_variable17, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_point(data=dataset_variable18, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_point(data=dataset_variable19, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_point(data=dataset_variable20, mapping=aes(x=population_density, y=R.value), color="green")
d+theme_light()

e<-ggplot()+ggtitle("Correlation of population density with R-value (residential_percent_change_from_baseline)")+
  geom_line(data=dataset_variable21, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_line(data=dataset_variable22, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_line(data=dataset_variable23, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_line(data=dataset_variable24, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_line(data=dataset_variable25, mapping=aes(x=population_density, y=R.value), color="green")+
  geom_point(data=dataset_variable21, mapping=aes(x=population_density, y=R.value), color="#6C3483")+
  geom_point(data=dataset_variable22, mapping=aes(x=population_density, y=R.value), color="#B03A2E")+
  geom_point(data=dataset_variable23, mapping=aes(x=population_density, y=R.value), color="#BB8FCE")+
  geom_point(data=dataset_variable24, mapping=aes(x=population_density, y=R.value), color="#EC7063")+
  geom_point(data=dataset_variable25, mapping=aes(x=population_density, y=R.value), color="green")
e+theme_light()


















  
pdf("plots_density.pdf", width=811/72, height=478/72)
plot(a)
plot(b)
plot(c)
plot(d)
plot(e)
dev.off()



