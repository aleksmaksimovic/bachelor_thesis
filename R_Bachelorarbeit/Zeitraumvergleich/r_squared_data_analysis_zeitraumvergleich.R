library(dplyr)
library(ggplot2)

dataframe=read.csv("/Users/Aleks/Desktop/Studium/Bachelorarbeit/DataAnalytics/R projects/R_Bachelorarbeit/Zeitraumvergleich/r_squared_dataframe_final_export.csv", header = TRUE, sep = ",")
bundeslaender <- c("Berlin", "Bavaria", "Hamburg", "Saarland", "Schleswig-Holstein", "Baden-Württemberg", "Brandenburg", "Hessen", "Mecklenburg-Vorpommern", "Lower Saxony", "North Rhine-Westphalia", "Rhineland-Palatinate","Saxony-Anhalt","Saxony","Thuringia", "Bremen" )
i=1

plot_list<-list()



for (bundesland_iterator in bundeslaender)
{

  dataset_bundesland=filter(dataframe, bundesland==bundesland_iterator, variable_y!="workplaces_percent_change_from_baseline")
  
 

# Grouped
plot_list[[i]]<-ggplot(dataset_bundesland, aes(fill=paste(from,"-", to), y=R.value, x=variable_y)) + ggtitle( paste("R-Values for dependend variables (", bundesland_iterator, ")"))+
  geom_bar(position="dodge", stat="identity")+labs(fill="Timespan")+ theme(panel.background=element_blank(), panel.grid.minor = element_line(colour="black"), legend.text=element_text(size=5),
    axis.title.x = element_blank(), axis.text.x = element_text(face="bold", size=4.5))+
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1))






i=i+1

}


#plot_list=list(bar_chart, bar_chart)

pdf("/Users/Aleks/Desktop/Studium/Bachelorarbeit/DataAnalytics/R projects/R_Bachelorarbeit/Zeitraumvergleich/r_squared_plots.pdf", width=811/72, height=478/72)
invisible(lapply(plot_list, print))
dev.off()


print(plot_list)


