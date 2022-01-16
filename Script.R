#Importing the data set
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

#Inspecting the data set
summary(dataset_semifinal)
dataset_semifinal$new_cases[dataset_semifinal$new_cases<0]<-NA #Negative cases do not make sense

#Histograms
hist(dataset_semifinal$new_cases) #Extremely skewed
hist(log(dataset_semifinal$new_cases+1)) #A bit better, need to add a constant due to zeros

hist(dataset_semifinal$new_tests) #Heavily skewed
hist(log(dataset_semifinal$new_tests))

hist(dataset_semifinal$new_vaccinations) #Skeeeewed
hist(log(dataset_semifinal$new_vaccinations+1)) #A lot better

#Preliminary analysis
library(plm)
model <- plm(log(new_cases+1)~lag(log(new_cases+1),1)+lag(log(new_tests),0:1)+lag(log(new_vaccinations+1),0:4),data=dataset_semifinal,
             model="within",effect = "twoways",index=c("location","date"))
summary(model) 

#Heteroskedasticity
library(lmtest)
bptest(model)

#Serial correlation
pbgtest(model)

#Clustered robust standard errors
library(sandwich)
coeftest(model,vcov=vcovHC(model,type = "HC0",cluster = "group"))

#Difference GMM (lag is very significant => dynamic panel data analysis)
diff_model<-pgmm(log(new_cases+1)~lag(log(new_cases+1),1)+log(new_tests)+log(new_vaccinations+1)|lag(log(new_cases+1),2:10),
                 data=dataset_semifinal,effect = "twoways",model=c("twosteps"),transformation = "d",index=c("location","date"))
summary(diff_model) #System is computationally singlar when I add vaccinations
