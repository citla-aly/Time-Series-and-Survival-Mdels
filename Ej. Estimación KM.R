library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

data(veteran)
head(veteran)

"
* trt: 1=standard 2=test 
* celltype: 1=squamous, 2=small cell, 3=adeno, 4=large 
* time: survival time in days 
* status: censoring status 
* karno: Karnofsky performance score (100=good) 
* diagtime: months from diagnosis to randomization 
* age: in years 
* prior: prior therapy 0=no, 10=yes
"
%>%
km <- with(veteran, Surv(time, status))
head(km,400)

km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))


plot(km_fit, xlab="Days", main = 'Estimación Kaplan Meyer')
autoplot(km_fit)

km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)
autoplot(km_trt_fit)

#Grupos de edad
vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

km_AG_fit <- survfit(Surv(time, status) ~ AG, data=vet)
autoplot(km_AG_fit)


km_trt_fit <- survfit(Surv(time, status) ~ trt, data=vet)
autoplot(km_trt_fit)


km_prior_fit <- survfit(Surv(time, status) ~ prior, data=vet)
autoplot(km_prior_fit)







