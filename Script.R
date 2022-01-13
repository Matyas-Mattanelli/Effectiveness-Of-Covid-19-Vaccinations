#Importing the data set
library(readxl)
owid_covid_data <- read_excel("owid-covid-data.xlsx")

#Extracting relevant columns
dataset <- owid_covid_data[,c(1,3,4,11,12,28,41)]

#Preliminary analysis
library(plm)
dataset <- pdata.frame(dataset, index = c("location", "date"))
model <- plm(total_cases_per_million~total_vaccinations_per_hundred+total_tests_per_thousand,data=dataset,
             model="within",effect = "twoways")
summary(model)
