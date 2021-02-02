




wean_data<-anal_data%>%
  filter(Outcome_Weaning_success!='전원')%>%
  mutate(wean_fail=1-as.numeric(Outcome_Weaning_success))%>%
  mutate(fu=(Outcome_ECMO_제거일-Insertion_ECMO_시술일)/ddays(1))

wean_data$TS<-Surv(wean_data$fu,wean_data$wean_fail)
wean_data$wTS<-Surv(wean_data$fu,wean_data$wean_fail)



fit<-survfit(TS~Respi_Cx,data=wean_data)
fit<-wean_data%>%
  filter(Group!=1)%>%
  survfit(TS~Group,data=.)
print(fit)
summary(fit)
#ggsurvplot(fit,data=surv_data,risk.table=T,pval=T,fun='event')
ggsurvplot(fit,data=wean_data,risk.table=T,pval=T)



wean_data%>%
  mytable(Group~Outcome_Weaning_success+wean_fail+fu+Outcome_Death,data=.)
wean_data%>%
  mytable(LM_Blood~Outcome_Weaning_success+wean_fail+fu+Outcome_Death,data=.)

LM_wean<-wean_data%>%
  filter(Group!=1)%>%
  filter(fu>=LM_cut)

LM_wean%>%
  mytable(LM_Blood~Outcome_Weaning_success+wean_fail+fu+Outcome_Death,data=.)

fit<-survfit(TS~LM_Respi,data=LM_wean)
print(fit)
summary(fit)
ggsurvplot(fit,data=LM_wean,risk.table=T,pval=T)

anal_data%>%
  filter(LM_Respi)%>%count()

LM_surv%>%
  mytable(Group~wean_fail+fu+Outcome_Death,data=.)
?ggsurvplot


anal_data%>%
  #filter(Outcome_Weaning_success!='전원')%>%
  mytable(Group~.,data=.)
View(anal_data)





#Survival

surv_data<-anal_data%>%
  #filter(Group!=1)%>%
  mutate(surv_duration=(Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1))
surv_data$TS=Surv(surv_data$surv_duration,surv_data$Outcome_Death)

fit<-survfit(TS~Group==1,data=surv_data)
print(fit)
summary(fit)
#ggsurvplot(fit,data=surv_data,risk.table=T,pval=T,fun='event')
ggsurvplot(fit,data=surv_data,risk.table=T,pval=T)

LM_surv<-surv_data%>%
  #filter(Group!=1)%>%
  filter(surv_duration>=LM_cut)

fit<-survfit(TS~LM_Blood,data=LM_surv)
print(fit)
summary(fit)
#ggsurvplot(fit,data=surv_data,risk.table=T,pval=T,fun='event')
ggsurvplot(fit,data=LM_surv,risk.table=T,pval=T,legend.labs = c("Control", "Early RTI"))


#After weaning
survivors<-anal_data%>%
  filter(Outcome_Weaning_success=='1')%>%
  mutate(Last_day=if_else(is.na(Outcome_Death_date),Outcome_Last_FU_date,Outcome_Death_date))%>%
  mutate(fu=(Last_day-Outcome_ECMO_제거일)/ddays(1))


survivors%>%
  filter(fu<0)%>%
  filter(Outcome_Death==1)%>%
  select(Basic_Hospital_ID,Basic_이름,Insertion_ECMO_시술일,Outcome_Weaning_success,Outcome_ECMO_제거일,Outcome_Death,Outcome_Death_date,Outcome_Last_FU_date,Last_day,fu)%>%
  View()
survivors$fu[survivors$fu<0]<-0
survivors$TS=Surv(survivors$fu,survivors$Outcome_Death)

#df<-survivors%>%filter(Group!=1)
df<-survivors
fit<-survfit(TS~Blood_Cx,data=df)
print(fit)
summary(fit)
ggsurvplot(fit,data=df,risk.table=T,pval=T,legend.labs = c("Control", "BSI during ECMO"))
