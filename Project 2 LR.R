library (tidymodels)
library(tidyr)
library(dplyr)
library(visdat)
library(car)
library(ggplot2)

setwd("C:/Data/Project Data")

s_train=read.csv('store_train.csv')
s_test=read.csv('store_test.csv')

glimpse(s_train)
#ID drop
#convert country, state to categorical factors
#remove countytownname name- way to sparse
#check for above feature
sort(table(s_train$countyname), decreasing=T)[1:5] #threshold=0.01
table(s_train$storecode)[1:5] #take initial 5 letters
sort(table(s_train$Areaname), decreasing=T)[1:5] #take only last after comma value
sort(table(s_train$countytownname), decreasing=T)[1:5]  #drop
table(s_train$state_alpha)
table(s_train$store_Type)

area_extraction=function(x){
  x=sub("^.*,\\s*","",x)
  return(x)
}

extract_five_letters= function(x){
  x=substr(x, 1, 5)
  return(x)
}

s_train$store=as.factor(s_train$store)
glimpse(s_train)

vis_dat(s_train)
dp_pipe=recipe(store~., data=s_train) %>% 
  update_role(Id, countytownname, new_role = "drop_vars") %>% 
  step_mutate_at(country, State, fn=as.character) %>% 
  step_mutate_at(storecode, fn=extract_five_letters) %>% 
  step_mutate_at(Areaname, fn=area_extraction) %>%
  update_role(country, State, countyname, Areaname, storecode, 
              state_alpha, store_Type, new_role = "to_dummies") %>% 
  step_rm(has_role("drop_vars")) %>% 
  step_unknown(has_role("to_dummies"), new_level = "__missing__") %>% 
  step_other(has_role("to_dummies"), threshold = 0.02, other = "__other__") %>% 
  step_dummy(has_role("to_dummies")) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe, new_data=NULL)
test=bake(dp_pipe, new_data = s_test)

set.seed(2)
s=sample(1:nrow(train), 0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

vis_dat(train)  


fit=lm(store~.- storecode_X__other__-state_alpha_GA-state_alpha_IA-
         state_alpha_VT-state_alpha_VA-state_alpha_TX-state_alpha_NH-
         state_alpha_IL-state_alpha_KS-state_alpha_KY-state_alpha_MA-
         state_alpha_ME-state_alpha_MO-state_alpha_NC-state_alpha_X__other__-
         store_Type_X__other__-sales0-sales2-sales3-State_X__other__-
         Areaname_VT, data = t1)
sort(vif(fit), decreasing=T)[1:3]

summary(fit)


log_fit=glm(store~.- storecode_X__other__-state_alpha_GA-state_alpha_IA-
              state_alpha_VT-state_alpha_VA-state_alpha_TX-state_alpha_NH-
              state_alpha_IL-state_alpha_KS-state_alpha_KY-state_alpha_MA-
              state_alpha_ME-state_alpha_MO-state_alpha_NC-state_alpha_X__other__-
              store_Type_X__other__-sales0-sales2-sales3-State_X__other__-
              Areaname_VT, data=t1, family="binomial")

log_fit=stats::step(log_fit)

summary(log_fit)
formula(log_fit)

log_fit=glm(store ~ country_X13 + State_X9 + storecode_METRO + 
              Areaname_CT.HUD.Metro.FMR.Area, 
            data=t1)

val.score=predict(log_fit, newdata = t2, type = "response")
pROC::auc(pROC::roc(t2$store, val.score))

##Final Model

for_vif=lm(store~.-storecode_X__other__-state_alpha_GA-state_alpha_IA-
             state_alpha_VT-state_alpha_VA-state_alpha_TX-state_alpha_NH-
             state_alpha_IL-state_alpha_KS-state_alpha_KY-state_alpha_MA-
             state_alpha_ME-state_alpha_MO-state_alpha_NC-state_alpha_X__other__-
             store_Type_X__other__-sales0-sales2-sales3-State_X__other__-
             Areaname_VT, data=train)
sort(vif(for_vif), decreasing=T)[1:3]

summary(for_vif)

log_fit=glm(store~.-storecode_X__other__-state_alpha_GA-state_alpha_IA-
              state_alpha_VT-state_alpha_VA-state_alpha_TX-state_alpha_NH-
              state_alpha_IL-state_alpha_KS-state_alpha_KY-state_alpha_MA-
              state_alpha_ME-state_alpha_MO-state_alpha_NC-state_alpha_X__other__-
              store_Type_X__other__-sales0-sales2-sales3-State_X__other__-
              Areaname_VT, data=train, family="binomial")

log_fit=stats::step(log_fit)

formula(log_fit)

log_fit=glm(store ~   State_X17 + State_X9 + 
              storecode_METRO + Areaname_CT.HUD.Metro.FMR.Area,
            data=train, family="binomial")
summary(log_fit)


test.prob.score=predict(log_fit, newdata=test, type='response')

write.csv(test.prob.score, 'Nimmi_Tulsyan_P2_part2.csv', row.names=F)

