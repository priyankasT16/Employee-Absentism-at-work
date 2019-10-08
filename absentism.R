#rm()
setwd("C:/Users/USER/Desktop/project/employee absentism/r")
getwd()
#f=file.choose()
list.files()
library(tidyverse)
library(readxl)
library(DataExplorer)
data=read_excel('Absenteeism_at_work_Project.xls')
data=as.data.frame(data)
str(data)
create_report(data)
colnames(data)=gsub(" ","_",colnames(data))
colnames(data)=gsub('Work_load_Average/day','Work_load_Average_day',colnames(data))
colnames(data)
str(data)
#str(data$Month_of_absence)
#unique(data$Month_of_absence)
######################################EDA##################3
#data=data[data$Month_of_absence>0,]
#data=data[data$Absenteeism_time_in_hours>0,]
str(data)
data1=data
miss=data.frame(apply(data,2,function(x){sum(is.na(x))}))
miss
mis_col=colnames(data)
#mis_col
for (i in unique(data$ID)){
  for (j in mis_col) {
    #data[(data$ID==i)& is.na(data$j),j]=mean(data$j[data$ID==i],na.rm=T)
    data[,j][(is.na(data[,j]) & (data$ID==i))]=mean(data[,j][data$ID==i],na.rm = T)
    
  }}
miss=data.frame(apply(data,2,function(x){sum(is.na(x))}))
miss
data$Age[which(data$ID==29)]=41
unique(data$Age[which(data$ID==29)])
colnames(data)
data=data[,c(1,2,3,6,7,8,9,10,11,4,5,12,13,14,15,16,17,18,19,20,21)]
data[,10:17]=lapply(data[10:17],as.factor)
#data[,10:17]=sapply(data[10:17],as.factor)
str(data)
#barplt=function(data)
#{
 # col=sapply(data,is.factor)
#  data_cat=data[,col]
 # colnames(data_cat)
 # for (i in colnames(data_cat)) {
  #  hist(data$Absenteeism_time_in_hours ~ data[i])
  #}
#}
#barplt(data)
col=sapply(data,is.factor)
#class(col)
data_cat=data[,col]
colnames(data_cat)
colnames(data)
ggplot(data,aes_string(y=data$Absenteeism_time_in_hours,x=as.factor(data$Reason_for_absence)))+geom_boxplot()+xlab('Reason for absence')+ylab('Absenteeism time in hours')
ggplot(data,aes_string(y=data$Absenteeism_time_in_hours,x=as.factor(data$Month_of_absence)))+geom_boxplot()+xlab('Month of absence')+ylab('Absenteeism time in hours')
ggplot(data,aes_string(y=data$Absenteeism_time_in_hours,x=as.factor(data$Social_drinker)))+geom_boxplot()+xlab('social drinker')+ylab('Absenteeism time in hours')
str(data)
data=data[data$Reason_for_absence!=0,]
unique(data$Reason_for_absence==0)
boxplot(data[,c('Hit_target','Weight', 'Height', 'Body_mass_index','Absenteeism_time_in_hours')])
boxplot(data[,c('Transportation_expense','Distance_from_Residence_to_Work', 'Service_time', 'Age')])
boxplot(data[,c('Work_load_Average_day')])

#for (i in c){
 # value=data_num[,i][data_num[,i] %in% boxplot.stats(data_num[,i])$out]
  #data_num[,i][data_num[,i] %in% value]=NA
#}

outlier_removal=function(data)
{
  col=sapply(data,is.numeric)
  data_num=data[,col]
  colnames(data_num)
  for (i in colnames(data_num)) {
    value=data_num[,i][data_num[,i] %in% boxplot.stats(data_num[,i])$out]
    data_num[,i][data_num[,i] %in% value]=NA
    data_num[,i][is.na(data_num[,i])]=mean(data_num[,i],na.rm = T)
  }
return(data_num)
  }
out_free=outlier_removal(data)
str(out_free)
str(data)
data$Hit_target=out_free$Hit_target
data$Height=out_free$Height
data$Absenteeism_time_in_hours=out_free$Absenteeism_time_in_hours
data$Transportation_expense=out_free$Transportation_expense
data$Service_time=out_free$Service_time
data$Age=out_free$Age
data$Work_load_Average_day=out_free$Work_load_Average_day
boxplot(data[,c('Hit_target','Weight', 'Height', 'Body_mass_index','Absenteeism_time_in_hours')])
boxplot(data[,c('Transportation_expense','Distance_from_Residence_to_Work', 'Service_time', 'Age')])
boxplot(data[,c('Work_load_Average_day')])
plot_density(data$Absenteeism_time_in_hours)
plot_correlation(data,type = 'continuous')
emp=data
colnames(emp)
emp=subset.data.frame(emp,select = -c(Body_mass_index))
#Social_smoker,,Day_of_the_week,Disciplinary_failure,Social_smoker))
col=c('Hit_target','Transportation_expense','Distance_from_Residence_to_Work','Service_time','Age','Work_load_Average_day','Height','Weight')
emp_num=emp[,col]
for(i in colnames(emp_num)){
  print(i)
  emp[,i] = (emp[,i] - min(emp[,i]))/
    (max(emp[,i]) - min(emp[,i]))
}
head(emp)
set.seed(1)
train_index = sample(1:nrow(emp), 0.8*nrow(emp))        
train = emp[train_index,]
test = emp[-train_index,]
str(train)
str(test)
colnames(test)
test
library(rpart)
library(randomForest)
library(DMwR)
library(compare)
library(caret)
train$Absenteeism_time_in_hours=train$Absenteeism_time_in_hours/10
test$Absenteeism_time_in_hours=test$Absenteeism_time_in_hours/10
#head(train)
dt_mdl=rpart(Absenteeism_time_in_hours ~.,data=train,method='anova')
pred_dt=predict(dt_mdl,test[,-20])
#pred_dt
#regr.eval(test$Absenteeism_time_in_hours,pred_dt)
#############################################################
rf_mdl=randomForest(Absenteeism_time_in_hours ~.,train,importance=TRUE,ntree=1500)
pred_rf=predict(rf_mdl,test[,-20])
#summary(rf_mdl)
#summary(pred_rf)
#regr.eval(test$Absenteeism_time_in_hours,pred_rf)
library(gbm)
xgb_mdl=gbm(Absenteeism_time_in_hours ~.,data=train,n.trees=1500)
pred_xgb=predict(xgb_mdl,test[,-20],n.trees = 1500)
#regr.eval(test$Absenteeism_time_in_hours,pred_xgb)
###################################################################
print(postResample(pred =pred_dt, obs =test$Absenteeism_time_in_hours))
print(postResample(pred =pred_rf, obs =test$Absenteeism_time_in_hours))
print(postResample(pred=pred_xgb,obs =test$Absenteeism_time_in_hours ))
############################################
pred_rf=data.frame(pred_xgb)
colnames(pred_rf)='predicted_hr'
str(pred_rf)
loss=c(test,pred_rf)
loss=data.frame(loss)
loss$Absenteeism_time_in_hours=loss$Absenteeism_time_in_hours*10
loss$predicted_hr=loss$predicted_hr*10
loss=subset.data.frame(loss,select = c(Month_of_absence,Absenteeism_time_in_hours,predicted_hr))
#op=aggregate.data.frame(x=loss,by=loss$Month_of_absence,FUN = sum(loss$predicted_hr,loss$Absenteeism_time_in_hours))
colnames(loss)

#loss % > % 
  #Step 2
 # group_by(Month_of_absence) % > % 
  #Step 3
  #summarise(mean_home_run = sum(predicted_hr)) % > % 
  #summarise(mean_ = sum(Absenteeism_time_in_hours)) % > %
library(sqldf) 
op=sqldf('select Month_of_absence,sum(Absenteeism_time_in_hours),sum(predicted_hr) from loss group by Month_of_absence')
op=data.frame(op)
colnames(op)=c('Month_of_absence','Absenteeism_time_in_hours','predicted_hr')
op$loss=(op$predicted_hr/(8*36*22))*100
write.csv(op,file = 'monthlyloss_r.csv',row.names = FALSE)
