#Importing the covid data set
library(readr)
owid_covid_data <- read_csv("owid-covid-data.csv")
owid_covid_data<-as.data.frame(owid_covid_data)

#Extracting relevant columns
dataset <- owid_covid_data[,c(1,3,4,6,26,39,49,51,54)]

#Filtering out non-countries
dataset_filtered <- dataset[!dataset$location %in% c("Africa","Asia","European Union","Europe","High income","International",
                                                     "Lower middle income","Low income","North America","Soouth Africa","South America",
                                                     "Upper middle income","World"),]

#Aggregating the daily data to monthly data
#Writing a function to sum properly
proper_sum <- function(x){
  if (all(is.na(x))){
    value <- NA
  } else {
    value <- sum(x,na.rm=T)
  }
  return(value)
}
#Extracting the month and year for aggregation
dataset_filtered$month <- months(dataset_filtered$date)
dataset_filtered$year <- as.numeric(format(dataset_filtered$date,"%Y"))

#Aggregation using the dplyr package
library(dplyr)
dataset_monthly <- dataset_filtered %>%
  group_by(location,month,year) %>%
  summarise(new_cases=proper_sum(new_cases),new_tests=proper_sum(new_tests),new_vaccinations=proper_sum(new_vaccinations),
            population=unique(population), median_age=unique(median_age),gdp_per_capita=unique(gdp_per_capita))

#Adding back the date (time dimension)
dataset_monthly$date<-as.Date(paste("01",dataset_monthly$month,dataset_monthly$year),"%d %B %Y")

#Adjusting the data set
dataset_semifinal<-dataset_monthly[,c(1,10,4:9)]
dataset_semifinal<-dataset_semifinal[order(dataset_semifinal$location,dataset_semifinal$date),]

#Loading average temperature
avg_temp <- read_csv('avg_temp.csv')

#Converting months to dates
avg_temp$date <- as.Date(paste("01",avg_temp$Month,"2020"), "%d %B %Y")

#Adding the year 2021
avg_temp_2021 <- avg_temp
avg_temp_2021$date <- as.Date(paste("01",avg_temp_2021$Month,"2021"), "%d %B %Y")

#Concatenating the two data sets
avg_temp_semifinal <- rbind(avg_temp,avg_temp_2021)

#Reordering the new data set
avg_temp_semifinal <- avg_temp_semifinal[order(avg_temp_semifinal$Country,avg_temp_semifinal$date),]

#Finalizing the data set
avg_temp_final <- avg_temp_semifinal[,c(1,4,3)]
colnames(avg_temp_final)[c(1,3)] <- c("location","avg_temp")

#Merging avg_temp with the main data set
#Looking for dissimilar country names
setdiff(unique(dataset_semifinal$location),unique(avg_temp_final$location))
setdiff(unique(avg_temp_final$location),unique(dataset_semifinal$location))
#Unifying the county names
avg_temp_final$location<-gsub("Democratic Republic of the Congo","Democratic Republic of Congo",avg_temp_final$location)
avg_temp_final$location<-gsub("Czech Republic","Czechia",avg_temp_final$location)
avg_temp_final$location<-gsub("United States of America","United States",avg_temp_final$location)
avg_temp_final$location<-gsub("Federated States of Micronesia","Micronesia (country)",avg_temp_final$location)
avg_temp_final$location<-gsub("East Timor","Timor",avg_temp_final$location)
avg_temp_final$location<-gsub("Faroe Islands","Faeroe Islands",avg_temp_final$location)
avg_temp_final$location<-gsub("Macedonia","North Macedonia",avg_temp_final$location)
avg_temp_final$location<-gsub("The Gambia","Gambia",avg_temp_final$location)
avg_temp_final$location[avg_temp_final$location=="Republic of Congo"]="Congo"
avg_temp_final$location<-gsub("Pitcairn Islands","Pitcairn",avg_temp_final$location)
avg_temp_final$location<-gsub("Curaçao","Curacao",avg_temp_final$location)
avg_temp_final$location<-gsub("The Bahamas","Bahamas",avg_temp_final$location)
avg_temp_final$location<-gsub("Sint Maarten","Sint Maarten (Dutch part)",avg_temp_final$location)
#Adding data for "Jersey" and "Guernsey"
jersey=data.frame(location=rep("Jersey",24),date=seq.Date(as.Date("2020-01-01"),as.Date("2021-12-01"),by="month"),avg_temp=rep(c(6.3,6.1,7.9,9.5,12.6,15.1,17.2,17.5,15.8,13,9.6,7.1),2))
guernsey=data.frame(location=rep("Guernsey",24),date=seq.Date(as.Date("2020-01-01"),as.Date("2021-12-01"),by="month"),avg_temp=rep(c(6.9,6.5,7.8,9.3,12.1,14.6,16.6,17,15.6,13,10,7.8),2))
avg_temp_final2<-rbind(avg_temp_final,jersey,guernsey)
avg_temp_final2 <- avg_temp_final2[order(avg_temp_final2$location,avg_temp_final2$date),]
#Merging
dataset_semifinal2<-merge(dataset_semifinal,avg_temp_final2,by=c("location","date"),all.x = T)

#Exporting the data set
write_csv(dataset_semifinal2,"dataset.csv")
