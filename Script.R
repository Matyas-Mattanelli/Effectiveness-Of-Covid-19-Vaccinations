#Importing the data set
library(readr)
dataset <- read_csv("dataset.csv")
dataset <- as.data.frame(dataset)

#Inspecting the data set
summary(dataset)
dataset$new_cases[dataset$new_cases<0]<-NA #Negative cases do not make sense

#Histograms
hist(dataset$new_cases) #Extremely skewed
hist(log(dataset$new_cases+1)) #A bit better, need to add a constant due to zeros

hist(dataset$new_tests) #Heavily skewed
hist(log(dataset$new_tests))

hist(dataset$new_vaccinations) #Skeeeewed
hist(log(dataset$new_vaccinations+1)) #A lot better

hist(dataset$avg_temp) #A bit left-skewed

#Transforming the variables accordingly
dataset$log_new_cases <- log(dataset$new_cases+1)
dataset$log_new_tests <- log(dataset$new_tests)
dataset$log_new_vaccinations <- log(dataset$new_vaccinations+1)

#Correlation matrix
cor_mat<-cor(dataset[,c(3,4,5,9)],use="pairwise.complete.obs")

#Preliminary analysis
library(plm)
model_fe <- plm(log_new_cases~log_new_tests+lag(log_new_vaccinations,0:3)+avg_temp,
             data=dataset,model="within",effect = "twoways",index=c("location","date"))
summary(model_fe) 

#Heteroskedasticity
library(lmtest)
bptest(model_fe)

#Serial correlation
pbgtest(model_fe) #serial correlation evidence

#Clustered robust standard errors
library(sandwich)
coeftest(model_fe,vcov=vcovHC(model_fe,type = "HC0",cluster = "group"))

#First-differencing
model_fd <- plm(log_new_cases~log_new_tests+lag(log_new_vaccinations,0:3)+avg_temp,
                data=dataset,model="fd",effect = "individual",index=c("location","date"))
summary(model_fd) 

#Heteroskedasticity
bptest(model_fd) #no heteroskedasticity

#Serial correlation
pbgtest(model_fd) #still serial correlation present

####################################
### Bonus (does not work yet :/) ###
####################################

#Difference GMM (lag is very significant => dynamic panel data analysis)
diff_model<-pgmm(log_new_cases~lag(log_new_cases,1)+new_vaccinations|lag(log_new_cases,2:99),
                 data=dataset,effect = "twoways",model=c("twosteps"),transformation = "d",index=c("location","date"))
summary(diff_model) #System is computationally singular when I add vaccinations
mtest(diff_model,2)

library(pdynmc)
model_pdynmc<-pdynmc(dat=dataset,varname.i = "location",varname.t = "date",use.mc.diff = T,use.mc.lev = F,include.y = T,
                     varname.y = "log_new_cases",lagTerms.y = 2,maxLags.y = 10,fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                     varname.reg.fur = c("log_new_tests","log_new_vaccinations","avg_temp"),lagTerms.reg.fur = c(1,4,0),
                     include.dum = T,col_tol = 0.99,opt.meth = "none",use.mc.nonlin = F,dum.diff = T,varname.dum = "date",
                     dum.lev = F)
