#Importing the data set
library(readr)
dataset <- read_csv("dataset.csv")
dataset <- as.data.frame(dataset)

#######################
### Data inspection ###
#######################

#Inspecting the data set
summary(dataset) #Negative cases + Vaccination many missing values
dataset$new_cases[dataset$new_cases<0]<-NA #Negative cases do not make sense
min(na.omit(dataset[,c(1:5,9)])$date) #Earliest vaccination data in December 2020
dataset<-dataset[dataset$date>=as.Date("2020-10-01"),] #We retain some past values given the inclusion of lags in the analysis

#Scaling by population (for increased cross-country comparability)
dataset$new_cases_per_thousand<-dataset$new_cases/(dataset$population/1000)
dataset$new_vaccinations_per_thousand<-dataset$new_vaccinations/(dataset$population/1000)
dataset$new_tests_per_thousand<-dataset$new_tests/(dataset$population/1000)

#Histograms
trim <- function(x){#Defining a trimming function to make the histograms nicer
  x[(x > quantile(x, 0.25,na.rm=T)-1.5*IQR(x,na.rm=T)) & (x < quantile(x, 0.75,na.rm=T)+1.5*IQR(x,na.rm=T))]
}

par(mfrow=c(4,2))
hist(trim(dataset$new_cases_per_thousand),border = F,col="grey",main = "New Covid-19 cases per thousand (level)",xlab = "",ylab="") #Extremely skewed, many outliers
hist(trim(log(dataset$new_cases_per_thousand+min(dataset$new_cases_per_thousand[dataset$new_cases_per_thousand>0],na.rm = T))),border=F,col="grey",xlab="",ylab="",main="New Covid-19 cases per thousand (log)") #A bit better, need to add a constant due to zeros

hist(trim(dataset$new_tests_per_thousand),border = F,col="grey",main = "New Covid-19 tests per thousand (level)",xlab = "",ylab="") #Heavily skewed
hist(trim(log(dataset$new_tests_per_thousand)),border=F,col="grey",xlab="",ylab="",main="New Covid-19 tests per thousand (log)")

hist(trim(dataset$new_vaccinations_per_thousand),border = F,col="grey",main = "New Covid-19 vaccinations per thousand (level)",xlab = "",ylab="") #Skeeeewed
hist(trim(log(dataset$new_vaccinations_per_thousand+min(dataset$new_vaccinations_per_thousand[dataset$new_vaccinations_per_thousand>0],na.rm = T))),border=F,col="grey",xlab="",ylab="",main="New Covid-19 vaccinations per thousand (log)") #A lot better

hist(trim(dataset$avg_temp),border=F,col="grey",main = "Average temperature",xlab = "",ylab="") #A bit left-skewed (but log won't help)

#Transforming the variables
dataset$log_new_cases_per_thousand <- log(dataset$new_cases_per_thousand+min(dataset$new_cases_per_thousand[dataset$new_cases_per_thousand>0],na.rm = T))
dataset$log_new_tests_per_thousand <- log(dataset$new_tests_per_thousand)
dataset$log_new_vaccinations_per_thousand <- log(dataset$new_vaccinations+min(dataset$new_vaccinations_per_thousand[dataset$new_vaccinations_per_thousand>0],na.rm = T))

#Descriptive statistics
library(stargazer)
stargazer(na.omit(dataset[,c(10:12,9)]),title = "Descriptive statistics",label="tab:des",header = F,
          summary.stat = c("mean","sd","min","median","max"),
          notes = "Note: All variables apart from Average Temperature are in per thousand people terms")
length(unique(na.omit(dataset)$location)) #103 countries
table(table(na.omit(dataset)$location)) #1 to 13 periods

#Correlation matrix
library(rstatix)
cor_mat<-cor_mat(dataset[,c(13:15,9)])
cor_mat<-cor_mark_significant(cor_mat,cutpoints = c(0,0.05,1),symbols = c("*",""))
for (i in 1:4){
  cor_mat[i,i+1]<-1
}
stargazer(cor_mat,summary = F,header = F,title = "Correlation matrix",
          notes = c("Note: All variables apart from Average temperature are in logs per thousand people","* significant at 95%"),
          label="tab:cor_mat",rownames = F)

################
### Analysis ###
################

#Testing for individual and time effects
library(plm)
plmtest(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
        data=dataset,index=c("location","date"),effect = "individual") #Individual effects significant
plmtest(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
        data=dataset,index=c("location","date"),effect = "time") #Time effects significant
pFtest(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
       data=dataset,index=c("location","date"),effect = "individual") #Individual effects significant
pFtest(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
       data=dataset,index=c("location","date"),effect = "time") #Time effects significant

#Hausman test (RE vs FE model)
phtest(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
       data=dataset,index=c("location","date"),effect = "twoways") #RE is inconsistent

#Pooling model
pooled_model <- plm(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
                    data=dataset,model="pooling",effect = "twoways",index=c("location","date"))
summary(pooled_model)

#Random-effects
model_random <- plm(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
                    data=dataset,model="random",effect = "twoways",index=c("location","date"))
summary(model_random)

#####################
### Fixed-effects ###
#####################

model_fe <- plm(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
                data=dataset,model="within",effect = "twoways",index=c("location","date"))
summary(model_fe) 

#Heteroskedasticity
library(lmtest)
bptest(model_fe) #No heteroskedasticity

#Serial correlation
pbgtest(model_fe) #serial correlation evidence
pwartest(model_fe) #serial correlation evidence (test for short FE panels)
pwfdtest(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
         data=dataset,index=c("location","date")) #serial correlation in differenced errors

#Cross-sectional dependence
pcdtest(model_fe) #No hard evidence (many countries skipped)

#Clustered robust standard errors
summary(model_fe,vcov=vcovHC(model_fe,method="arellano",cluster="group")) 

#First-differencing
model_fd <- plm(log_new_cases_per_thousand~log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
                data=dataset,model="fd",effect = "individual",index=c("location","date"))
summary(model_fd) 

#Heteroskedasticity
bptest(model_fd) #no heteroskedasticity

#Serial correlation
pbgtest(model_fd) #still serial correlation present

#Clustered robust standard errors
coeftest(model_fd,vcov=vcovHC(model_fd,method="arellano",cluster = "group"))

##################################################################################
### Bonus (cannot add vaccinations => system becomes computationally singular) ###
##################################################################################

#Difference GMM (lag is very significant => dynamic panel data analysis)
diff_model<-pgmm(log_new_cases_per_thousand~lag(log_new_cases_per_thousand,1:2)+lag(log_new_tests_per_thousand,0:1)|lag(log_new_cases_per_thousand,3:7),
                 data=dataset,effect = "twoways",model=c("twosteps"),transformation = "d",index=c("location","date"))
summary(diff_model) #Sargan high but not too high (very good)
mtest(diff_model,3) #No third order serial correlation => instruments valid

#Sensitivity of the fixed effects model to the inclusion of a lag
model_fe_sens <- plm(log_new_cases_per_thousand~lag(log_new_cases_per_thousand,1:2)+log_new_tests_per_thousand+lag(log_new_vaccinations_per_thousand,0:3)+avg_temp,
                data=dataset,model="within",effect = "twoways",index=c("location","date"))
summary(model_fe_sens,vcov=vcovHC(model_fe,method="arellano",cluster="group")) 

indicator<-table(dataset_final_no_nas$location)>=12
complet_countries<-names(indicator)[indicator]
complet_countries

dataset_test<-dataset[dataset$location %in% complet_countries,]

diff_test_model <- pgmm(log_new_cases_per_thousand~lag(log_new_cases_per_thousand,1:2)+lag(log_new_tests_per_thousand,0:1)+lag(log_new_vaccinations_per_thousand,0:4)+avg_temp|lag(log_new_cases_per_thousand,3:7),
                        data=na.omit(dataset),effect = "twoways",model=c("twosteps"),transformation = "ld",index=c("location","date"))
summary(diff_test_model)
mtest(diff_test_model,3)
