library(survival)

#loading data
tumour <- read.csv('btumour.csv')
head(tumour)

#plot overall Kaplan-Meier curves
attach(tumour)
surv <- Surv(weeks,event)
plot(survfit(surv~1), xlab='weeks after procedure',
     ylab='survivals')
summary(survfit(surv~1))
#Mortality is very high in the first year following procedure
#Approximately 80% die within the first year
#Mortality is slower after this

#looking at the variable of our interst
#local radiation vs whole brain radiation
surv_bylocal <- survfit(surv~local)
summary(surv_bylocal)
#The result shows that patients with local radiation had better
#median survival time
plot(surv_bylocal,col=c('red','blue'), 
     xlab='weeks after procedure',ylab='survival')
#The red line denotes whole-brain radiation 
#and the blue line represents local radiation
#Seems that local radiation leads to better survival

#Use Log rank test to confirm, state the hypothesis:
#H_0: same survival in each group (local v whole-brain radiation)
#H_A: different survival in the two groups
survdiff(surv~local)
#P-value is large than 0.05, we cannot reject the null hypothsis
#Means that  there is no evidence of a difference in survival between the two groups

#previous exposure to nitrosoureas
surv_bynitro <- survfit(surv~nitro)
plot(surv_bynitro,col=c('red','blue'), 
     xlab='weeks after procedure',ylab='survival')
#The red line denotes no exposure to nitrosoureas
#and the blue line represents having previous exposure to nitrosoureas 
#Seems that no exposure group leads to better survival

#Use Log rank test to confirm, state the hypothesis:
#H_0: same survival in each group (new treatment vs standard of care)
#H_A: different survival in the two groups
survdiff(surv~nitro)
#P-value is less than 0.05, the null hypothsis is rejected at 95% confidence
#Conclude there is a difference in survival distributions
#Group with no exposure to nitrosoureas appear to have better survival 

#Fit a Cox proportional hazards model
summary(tumour)
#There is a NA in local,we need to move it
tumourC <- tumour[complete.cases(tumour[,-14]),]
m_full <- with(tumourC,
               coxph(Surv(weeks,event)~local+treat+resect75+age+interval+karn+race+male+nitro+as.factor(path)+grade))
m1 <- with(tumourC,
           coxph(Surv(weeks,event)~local))
summary(m1)
#'local' is observed not significant (0.55,1.05)
summary(m_full)
#'local' is significant in the full model (0.44,0.89)
# Model selection(backwards)
step(m_full)
# Shows that no variables is deleted

#checking the proportional hazards assumption
cox.zph(m_full)
#There does not appear to be any major cause for concern
#regarding the proportional hazards assumption
#Only 'resect75 and 'male' may need to be stratified
m_strat <- coxph(formula=Surv(weeks,event)~local+treat+
                   strata(resect75)+age+interval+karn+race+strata(male)+nitro+
                   as.factor(path)+grade,data=tumourC)
summary(m_strat)
