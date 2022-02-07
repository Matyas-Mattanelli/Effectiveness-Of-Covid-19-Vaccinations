#Importing the data set
library(readr)
dataset <- read_csv("data/dataset.csv")
dataset <- as.data.frame(dataset)

#######################
### Data inspection ###
#######################

#Inspecting the data set
summary(dataset) 
#Negative cases + Vaccination many missing values

dataset$new_cases[dataset$new_cases<0]<-NA 
#Negative cases do not make sense

min(na.omit(dataset[,c(1:5,9)])$date) 
#Earliest vaccination data in December 2020

dataset<-dataset[dataset$date>=as.Date("2020-10-01"),] 
#We retain some past values given the inclusion of lags in the analysis

#Scaling by population (for increased cross-country comparability)
dataset$new_cases_per_thousand<-dataset$new_cases/(dataset$population/1000)
dataset$new_vaccinations_per_thousand<-
  dataset$new_vaccinations/(dataset$population/1000)
dataset$new_tests_per_thousand<-dataset$new_tests/(dataset$population/1000)

#Histograms
trim <- function(x){#Defining a trimming function to make the histograms nicer
  x[(x > quantile(x, 0.25,na.rm=T)-1.5*IQR(x,na.rm=T)) &
      (x < quantile(x, 0.75,na.rm=T)+1.5*IQR(x,na.rm=T))]
}

par(mfrow=c(4,2))
hist(trim(dataset$new_cases_per_thousand),border = F,col="grey",
     main = "New Covid-19 cases per thousand (level)",xlab = "",
     ylab="") #Extremely skewed, many outliers
hist(trim(log(dataset$new_cases_per_thousand+
  min(dataset$new_cases_per_thousand[dataset$new_cases_per_thousand>0],
                    na.rm = T))),border=F,col="grey",xlab="",ylab="",
     main="New Covid-19 cases per thousand (log)") 
#A bit better, need to add a constant due to zeros

hist(trim(dataset$new_tests_per_thousand),border = F,col="grey",
     main = "New Covid-19 tests per thousand (level)",xlab = "",
     ylab="") #Heavily skewed
hist(trim(log(dataset$new_tests_per_thousand)),border=F,col="grey",
     xlab="",ylab="",main="New Covid-19 tests per thousand (log)")

hist(trim(dataset$new_vaccinations_per_thousand),border = F,col="grey",
     main = "New Covid-19 vaccinations per thousand (level)",xlab = "",
     ylab="") #Skeeeewed
hist(trim(log(dataset$new_vaccinations_per_thousand+
min(dataset$new_vaccinations_per_thousand[dataset$new_vaccinations_per_thousand>
                                            0],na.rm = T))),border=F,
col="grey",xlab="",ylab="",
main="New Covid-19 vaccinations per thousand (log)") #A lot better

hist(trim(dataset$avg_temp),border=F,col="grey",
     main = "Average temperature",xlab = "",
     ylab="") #A bit left-skewed (but log won't help)

#Transforming the variables
dataset$log_new_cases_per_thousand <- 
  log(dataset$new_cases_per_thousand+
        min(dataset$new_cases_per_thousand[dataset$new_cases_per_thousand>0],
            na.rm = T))

dataset$log_new_tests_per_thousand <- log(dataset$new_tests_per_thousand)
dataset$log_new_vaccinations_per_thousand <- 
log(dataset$new_vaccinations+
min(dataset$new_vaccinations_per_thousand[dataset$new_vaccinations_per_thousand
                                          >0],na.rm = T))

#Descriptive statistics
library(stargazer)
stargazer(na.omit(dataset[,c(10:12,9)]),title = "Descriptive statistics",
          label="tab:des",header = F,
          summary.stat = c("mean","sd","min","median","max"),
notes="Note: All variables apart from Average Temperature are in per 
thousand people terms")
length(unique(na.omit(dataset)$location)) #103 countries
table(table(na.omit(dataset)$location)) #1 to 13 periods

#Correlation matrix
library(rstatix)
cor_mat<-cor_mat(dataset[,c(13:15,9)])
cor_mat<-cor_mark_significant(cor_mat,cutpoints = c(0,0.05,1),
                              symbols = c("*",""))
for (i in 1:4){
  cor_mat[i,i+1]<-1
}
stargazer(cor_mat,summary = F,header = F,title = "Correlation matrix",
notes = c("Note: All variables apart from Average temperature are in
          logs per thousand people","* significant at 95%"),
          label="tab:cor_mat",rownames = F)

################
### Analysis ###
################

### Static FE ###

library(plm)
model_fe_static <- plm(log_new_cases_per_thousand~
                         lag(log_new_vaccinations_per_thousand,
                         1:2)+log_new_tests_per_thousand+avg_temp,
                data=dataset,model="within",effect = "twoways",
                index=c("location","date"))
pwartest(model_fe_static) #strong evidence of serial correlation
pwfdtest(log_new_cases_per_thousand~log_new_tests_per_thousand+
           lag(log_new_vaccinations_per_thousand,1:2)+avg_temp,
         data=dataset,index=c("location","date")) 
#serial correlation in differenced errors as well

library(lmtest)
bptest(model_fe_static) #evidence of heteroskedasticity
plmtest(log_new_cases_per_thousand~lag(log_new_vaccinations_per_thousand,1:2)
        +log_new_tests_per_thousand+avg_temp,
        data=dataset,effect = "twoways",index=c("location","date")) 
#Significant effects

pFtest(log_new_cases_per_thousand~lag(log_new_vaccinations_per_thousand,1:2)
       +log_new_tests_per_thousand+avg_temp,
       data=dataset,effect = "twoways",index=c("location","date")) 
#Significant effects

phtest(log_new_cases_per_thousand~lag(log_new_vaccinations_per_thousand,1:2)
       +log_new_tests_per_thousand+avg_temp,
       data=dataset,effect = "twoways",index=c("location","date"),
       model = c("within", "random"))

### Difference GMM ###

#One lag Difference GMM
diff_GMM_model_one_lag<-pgmm(log_new_cases_per_thousand~
                        lag(log_new_cases_per_thousand,1)+
                        lag(log_new_vaccinations_per_thousand,1:2)+
                        log_new_tests_per_thousand+avg_temp|
                        lag(log_new_cases_per_thousand,2:7),
                       data=na.omit(dataset),effect = "twoways",
                       model=c("twosteps"),transformation = "d",
                       index=c("location","date"))
summary(diff_GMM_model_one_lag) 
#Arellano-Bond test rejects the null hypothesis of no 2nd order serial 
#correlation at the 10% significance level => We need two lags

#Two lags Difference GMM
diff_GMM_model_two_lags <- pgmm(log_new_cases_per_thousand~
                                  lag(log_new_cases_per_thousand,1:2)+
                                lag(log_new_vaccinations_per_thousand,1:2)+
                                log_new_tests_per_thousand+avg_temp|
                                  lag(log_new_cases_per_thousand,3:99),
                        data=na.omit(dataset),effect = "twoways",
                        model=c("twosteps"),transformation = "d",
                        index=c("location","date"))
summary(diff_GMM_model_two_lags) 
#Sargan p-value 0.13 => instruments valid, no proliferation

mtest(diff_GMM_model_two_lags,3) 
#p-value 0.74 => No serial correlation of 3rd order => instruments valid

sargan_test_diff_model_two_lags<-sargan(diff_GMM_model_two_lags)
as.numeric(sargan_test_diff_model_two_lags$parameter)+
  length(coefficients(diff_GMM_model_two_lags)) #69 instruments

#Instrumenting new tests and vaccination to account for the measurement error
diff_GMM_model_two_lags_endo<-pgmm(log_new_cases_per_thousand~
                             lag(log_new_cases_per_thousand,1:2)+
                             lag(log_new_vaccinations_per_thousand,1:2)+
                             log_new_tests_per_thousand+avg_temp|
                             lag(log_new_cases_per_thousand,3)+
                             lag(log_new_tests_per_thousand,2)+
                            lag(log_new_vaccinations_per_thousand,3),
                             data=na.omit(dataset),effect = "twoways",
                              model=c("twosteps"),transformation = "d",
                                    index=c("location","date"))
summary(diff_GMM_model_two_lags_endo) 
#Sargan p-value 0.29 => instruments valid, slight proliferation

mtest(diff_GMM_model_two_lags_endo,3) 
#p-value 0.29 => No serial correlation of 3rd order => instruments valid

sargan_test_diff_model_two_lags_endo<-sargan(diff_GMM_model_two_lags_endo)
as.numeric(sargan_test_diff_model_two_lags_endo$parameter)+
  length(coefficients(diff_GMM_model_two_lags_endo)) #41 instruments
phtest(diff_GMM_model_two_lags,diff_GMM_model_two_lags_endo) 
#Null rejected => Endogeneity was likely present

#Including only one lag and instrumenting the endogenous variables
diff_GMM_model_one_lag_endo <- pgmm(log_new_cases_per_thousand~
                               lag(log_new_cases_per_thousand,1)+
                               lag(log_new_vaccinations_per_thousand,1:2)+
                               log_new_tests_per_thousand+avg_temp|
                               lag(log_new_cases_per_thousand,2:4)+
                               lag(log_new_tests_per_thousand,2)+
                               lag(log_new_vaccinations_per_thousand,3),
                               data=na.omit(dataset),effect = "twoways",
                               model=c("twosteps"),transformation = "d",
                                    index=c("location","date"))
summary(diff_GMM_model_one_lag_endo)
#Sargan p-value 0.22 => instruments valid, 
#no proliferation, no serial correlation of 2nd order (p-value 0.98)
sargan_test_diff_model_one_lag_endo<-sargan(diff_GMM_model_one_lag_endo)
as.numeric(sargan_test_diff_model_one_lag_endo$parameter)+
  length(coefficients(diff_GMM_model_one_lag_endo)) #60 instruments

### System GMM ###

#One lag System GMM
system_GMM_model_one_lag <- pgmm(log_new_cases_per_thousand~
                            lag(log_new_cases_per_thousand,1)+
                            lag(log_new_vaccinations_per_thousand,1:2)+
                            log_new_tests_per_thousand+avg_temp|
                            lag(log_new_cases_per_thousand,2:7),
                            data=na.omit(dataset),effect = "twoways",
                            model=c("twosteps"),transformation = "ld",
                               index=c("location","date"))
summary(system_GMM_model_one_lag) 
#Arellano-Bond test rejects the null hypothesis of no 
#2nd order serial correlation => We need two lags

#Two lags System GMM
system_GMM_model_two_lags <- pgmm(log_new_cases_per_thousand~
                            lag(log_new_cases_per_thousand,1:2)+
                            lag(log_new_vaccinations_per_thousand,1:2)+
                            log_new_tests_per_thousand+avg_temp|
                            lag(log_new_cases_per_thousand,3:8),
                            data=na.omit(dataset),effect = "twoways",
                            model=c("twosteps"),transformation = "ld",
                              index=c("location","date"))
summary(system_GMM_model_two_lags) 
#Sargan p-value 0.13 => instruments valid, no proliferation

mtest(system_GMM_model_two_lags,3) 
#p-value 0.42 => No serial correlation of 3rd order => instruments valid

sargan_test_system_model_two_lags<-sargan(system_GMM_model_two_lags)
as.numeric(sargan_test_system_model_two_lags$parameter)+
  length(coefficients(system_GMM_model_two_lags)) #75 instruments

#Instrumenting new tests and vaccination to account for the measurement error
system_GMM_model_two_lags_endo<-pgmm(log_new_cases_per_thousand~
                                lag(log_new_cases_per_thousand,1:2)+
                                lag(log_new_vaccinations_per_thousand,1:2)+
                                log_new_tests_per_thousand+avg_temp|
                                lag(log_new_cases_per_thousand,3)+
                                lag(log_new_tests_per_thousand,2)+
                                lag(log_new_vaccinations_per_thousand,4),
                                data=na.omit(dataset),effect = "twoways",
                                model=c("twosteps"),transformation = "ld",
                                     index=c("location","date"))
summary(system_GMM_model_two_lags_endo) 
#Sargan p-value 0.15 => instruments valid, slight proliferation

mtest(system_GMM_model_two_lags_endo,3) 
#p-value 0.44 => No serial correlation of 3rd order => instruments valid

sargan_test_system_model_two_lags_endo<-sargan(system_GMM_model_two_lags_endo)
as.numeric(sargan_test_system_model_two_lags_endo$parameter)+
  length(coefficients(system_GMM_model_two_lags_endo)) #76 instruments
phtest(system_GMM_model_two_lags,system_GMM_model_two_lags_endo) 
#Null rejected => Endogeneity is likely present

### Creating a table of results ###
stargazer(diff_GMM_model_two_lags,system_GMM_model_two_lags,
          diff_GMM_model_two_lags_endo,
          system_GMM_model_two_lags_endo,
          diff_GMM_model_one_lag_endo,header = F,title="Estimation results",
          label="tab:res")