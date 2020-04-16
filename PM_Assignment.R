getwd()
cellphone<-read_excel("Cellphone (1).xlsx", sheet = 2)
cellphone_Dummy<-cellphone

str(cellphone)
summary(cellphone)
table(cellphone$Churn)

sum(is.na(cellphone))
which(cellphone<0)
attach(cellphone)

##transforming into categorical variables
cellphone$Churn<-as.factor(cellphone$Churn)
cellphone$ContractRenewal<-as.factor(cellphone$ContractRenewal)
cellphone$DataPlan<-as.factor(cellphone$DataPlan)

boxplot(cellphone[,c(2,7,8,9)],col = "pink")
boxplot(cellphone[,c(5,6,10,11)], col= "blue")

# ContVarList<-c(2,5,6,7,8,9,10,11)
# for(i in ContVarList){
#   outlier_check<-boxplot(cellphone[i],plot = FALSE)$out
#   print(outlier_check)
# }

##Capping and flooring outlier values between 5th and 95th percentile
cellphone$AccountWeeks <- squish(cellphone$AccountWeeks, round(quantile(cellphone$AccountWeeks, c(.05, .95))))
cellphone$DataUsage <- squish(cellphone$DataUsage, round(quantile(cellphone$DataUsage, c(.05, .95))))
cellphone$CustServCalls <- squish(cellphone$CustServCalls, round(quantile(cellphone$CustServCalls, c(.05, .95))))
cellphone$DayMins <- squish(cellphone$DayMins, round(quantile(cellphone$DayMins, c(.05, .95))))
cellphone$DayCalls <- squish(cellphone$DayCalls, round(quantile(cellphone$DayCalls, c(.05, .95))))
cellphone$MonthlyCharge <- squish(cellphone$MonthlyCharge, round(quantile(cellphone$MonthlyCharge, c(.05, .95))))
cellphone$OverageFee <- squish(cellphone$OverageFee, round(quantile(cellphone$OverageFee, c(.05, .95))))
cellphone$RoamMins <- squish(cellphone$RoamMins, round(quantile(cellphone$RoamMins, c(.05, .95))))


boxplot(cellphone$CustServCalls,plot = FALSE)

##Univariate analysis
par(mfrow=c(3,3))
hist(cellphone$AccountWeeks)
hist(cellphone$DataUsage)
hist(cellphone$CustServCalls)
hist(cellphone$DayMins)
hist(cellphone$DayCalls)
hist(cellphone$MonthlyCharge)
hist(cellphone$OverageFee)
hist(cellphone$RoamMins)
summary(cellphone)
dev.off()

mytable <- table(cellphone$Churn)
mytable <- table(cellphone$ContractRenewal)
mytable <- table(cellphone$DataPlan)
pie(mytable)

##Bivariate analysis
# Examining the relationship between active account weeks and Churn rate
AccountWeeks_cut = cut(AccountWeeks, breaks = seq(0, 250, by=25))
barplot(table(AccountWeeks_cut,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow", "sky blue"),ylim = c(0,700),
        main = "Number of active weeks", adj= 0.5,
        beside = TRUE, legend=rownames(table(AccountWeeks_cut,Churn)))

# Examining the relationship between contract renewals and Churn rate
barplot(table(ContractRenewal,Churn),
        col=c("pink","lightblue"),
        main = "Number of contract renewals", adj= 0.5,ylim = c(0,3000),
        beside = TRUE, legend=rownames(table(ContractRenewal,Churn)))

# Examining the relationship between Data Plan subscriptions and Churn rate
barplot(table(DataPlan,Churn),
        col=c("pink","lightblue"),
        main = "Number of data plan subscribers", adj= 0.5,ylim = c(0,2300),
        beside = TRUE, legend=rownames(table(DataPlan,Churn)))

# Examining the relationship between Data Usage and Churn rate
DataUsage_cut = cut(DataUsage, breaks = seq(0, 5.5, by=0.5))
barplot(table(DataUsage_cut,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow", "sky blue", "orange"),
        main = "Number of data users", adj= 0.5,ylim = c(0,500),
        beside = TRUE, legend=rownames(table(DataUsage_cut,Churn)))

# Examining the relationship between Customer service callers and Churn rate
barplot(table(CustServCalls,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow", "sky blue"),
        main = "Customer service callers", adj= 0.5,ylim = c(0,1100),
        beside = TRUE, legend=rownames(table(CustServCalls,Churn)))

# Examining the relationship between Day time minutes and Churn rate
DayMins_cut = cut(DayMins, breaks = seq(0, 360, by=40))
barplot(table(DayMins_cut,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow"),
        main = "Average day time minutes by Churn", adj= 0.5,ylim = c(0,900),
        beside = TRUE, legend=rownames(table(DayMins_cut,Churn)))



# Examining the relationship between Day time calls and Churn rate
DayCalls_cut = cut(DayCalls, breaks = seq(0, 180, by=30))
barplot(table(DayCalls_cut,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow"),
        main = "Average day time calls by Churn", adj= 0.5,ylim = c(0,1600),
        beside = TRUE, legend=rownames(table(DayCalls_cut,Churn)))

# Examining the relationship between Monthly charges and Churn rate
MonthlyCharge_cut = cut(MonthlyCharge, breaks = seq(0, 120, by=20))
barplot(table(MonthlyCharge_cut,Churn),
        col=c("mistyrose","lightblue", "pink","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow"),
        main = "Monthly charges by Churn", adj= 0.5,ylim = c(0,1600),
        beside = TRUE, legend=rownames(table(MonthlyCharge_cut,Churn)))
table(MonthlyCharge_cut,Churn)
# Examining the relationship between Overage Fees and Churn rate
OverageFee_cut = cut(OverageFee, breaks = seq(0, 20, by=2))
barplot(table(OverageFee_cut,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow", "orange"),
        main = "Overage fees by Churn", adj= 0.5,ylim = c(0,850),
        beside = TRUE, legend=rownames(table(OverageFee_cut,Churn)))

# Examining the relationship between Roaming minutes and Churn rate
RoamMins_cut = cut(RoamMins, breaks = seq(0, 20, by=2))
barplot(table(RoamMins_cut,Churn),
        col=c("pink","lightblue", "mistyrose","lightcyan","lavender", "cornsilk","light green",
              "violet", "light yellow", "orange"),
        main = "Average roaming minutes by Churn", adj= 0.5,ylim = c(0,820),
        beside = TRUE, legend=rownames(table(RoamMins_cut,Churn)))

#Customers using Data even though they are not on a Data Plan
ggplot(cellphone, aes(DataUsage)) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  geom_histogram(bins = 10)+ facet_wrap(~DataPlan, scales = 'free_x')

#Customers calling Customer Service compared to how many of them renewed contracts
ggplot(cellphone, aes(CustServCalls))+ geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  geom_histogram(bins = 10)+facet_wrap(~ContractRenewal, scales = 'free_x') 


cellphone.scatter<-subset(cellphone[,c(2,5:11)])
cor.plot(cellphone.scatter,numbers = TRUE,xlas=2)


##Logistic regression starts
set.seed(1000)
spl<-sample.split(cellphone, SplitRatio=0.7)
p_train<-subset(cellphone,spl==TRUE)
p_test<-subset(cellphone,spl==FALSE)
table(p_train$Churn)
table(p_test$Churn)

#Creating Logistic Regression Model based upon all the given variables
logreg = glm(Churn ~ ., data= cellphone, family=binomial) 
summary(logreg) 
vif(logreg)


#Model built after Removing MonthlyCharge and DataUsage insignificant variables
logreg2 = glm(Churn ~ . -MonthlyCharge - DataUsage , data= cellphone, family=binomial) 
summary(logreg2) 
vif(logreg2)


##Model built after Removing all insignificant variables
logreg3 = glm(Churn ~ . -MonthlyCharge - DataUsage -AccountWeeks -DayCalls, data= cellphone, family=binomial) 
summary(logreg3) 
vif(logreg3)


##Logistic Regression Model Performance Measures
#Confusion Matrix on Train Data
predLRT = predict(logreg3, newdata=p_train[,-1],type="response") 
tab1=table(p_train$Churn, predLRT>0.5) 
sum(diag(tab1))/sum(tab1)

predLRTe = predict(logreg3, newdata=p_test[,-1], type="response") 
tab2=table(p_test$Churn, predLRTe>0.5) 
sum(diag(tab2))/sum(tab2)



##Area Under the ROC curve (AUC - ROC)
#Validation on train data
predictROC1 = predict(logreg3, newdata = p_train) 
pred1 = prediction(predictROC1, p_train$Churn)
perf1 = performance(pred1, "tpr", "fpr") 
plot(perf1,colorize =T) 
as.numeric(performance(pred1, "auc")@y.values)

#Validation on test data
predictROC2 = predict(logreg3, newdata = p_test) 
pred2 = prediction(predictROC2, p_test$Churn) 
perf2 = performance(pred2, "tpr", "fpr") 
plot(perf2,colorize =T) 
as.numeric(performance(pred2, "auc")@y.values)


##KS Test Validation
#KS on train
KS_test <- max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
KS_test

#KS on test
KS_test <- max(attr(perf2, 'y.values')[[1]]-attr(perf2, 'x.values')[[1]])
KS_test


##GINI Test
#Gini on train
gini_test = ineq(predLRT, type="Gini")
gini_test

#Gini on test
gini_test = ineq(predLRTe, type="Gini")
gini_test














