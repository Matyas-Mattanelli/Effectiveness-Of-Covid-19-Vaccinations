#Importing the data set
library(readr)
dataset <- read_csv("dataset.csv")
dataset <- as.data.frame(dataset)

#######################
### Data inspection ###
#######################

#Descriptive statistics
summary(dataset)
dataset$new_cases[dataset$new_cases<0]<-NA #Negative cases do not make sense

#Scaling by population (for increased cross-country comparability)
dataset$new_cases_per_thousand<-dataset$new_cases/(dataset$population/1000)
dataset$new_vaccinations_per_thousand<-dataset$new_vaccinations/(dataset$population/1000)
dataset$new_tests_per_thousand<-dataset$new_tests/(dataset$population/1000)

#Histograms
hist(dataset$new_cases_per_thousand) #Extremely skewed
hist(log(dataset$new_cases_per_thousand+min(dataset$new_cases_per_thousand[dataset$new_cases_per_thousand>0],na.rm = T))) #A bit better, need to add a constant due to zeros

hist(dataset$new_tests_per_thousand) #Heavily skewed
hist(log(dataset$new_tests_per_thousand))

hist(dataset$new_vaccinations) #Skeeeewed
hist(log(dataset$new_vaccinations+min(dataset$new_vaccinations_per_thousand[dataset$new_vaccinations_per_thousand>0],na.rm = T))) #A lot better

hist(dataset$avg_temp) #A bit left-skewed (but log won't help)

#Transforming the variables accordingly
dataset$log_new_cases_per_thousand <- log(dataset$new_cases_per_thousand+min(dataset$new_cases_per_thousand[dataset$new_cases_per_thousand>0],na.rm = T))
dataset$log_new_tests_per_thousand <- log(dataset$new_tests_per_thousand)
dataset$log_new_vaccinations_per_thousand <- log(dataset$new_vaccinations+min(dataset$new_vaccinations_per_thousand[dataset$new_vaccinations_per_thousand>0],na.rm = T))

#Correlation matrix
cor_mat<-cor(dataset[,c(13:15,9)],use="pairwise.complete.obs")

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
