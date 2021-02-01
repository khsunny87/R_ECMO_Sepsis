library(survival)
library(survminer)

# 1. MV logistic - weaned vs. death
library(stats)

#multivariable Logistic regression
tbl_data%>%
  filter(Outcome_Weaning_success!='전원')->df
res<-glm(as.factor(Outcome_Weaning_success)~Blood_Cx+Respi_Cx+Urine_Cx+Insertion_삽입이유+Insertion_ECMO_type+PMH_HTN+PMH_Malignancy+PMH_PAOD+PMH_CKD+ECPR_ECPR+ECMO_CRRT,family=binomial,data=df)
out<-step(res,direction="backward",trace=T)
summary(out)
anova(out,test='Chisq')
res


# 2. Survival - Survival vs. Death
#    time-dependent Cox

init_BSI<-r_Cx2%>%
  filter(Cx_result,Cate_sp=='BSI')%>%
  left_join(.,anal_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일),by=c('ID'='Basic_Hospital_ID'))%>%
  filter(lab_performed_time<=Outcome_ECMO_제거일)%>%
  mutate(interval=(lab_performed_time-Insertion_ECMO_시술일)/ddays(1))%>%
  filter(interval>=0)%>%
  group_by(ID)%>%
  summarise(interval=min(interval))

anal_data%>%
  mutate(surv_duration=pmax((Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1),ECMO_duration))%>%
  select(Basic_Hospital_ID,Outcome_Death,Outcome_Weaning_success,ECMO_duration,surv_duration)%>%
  left_join(.,init_BSI,by=c('Basic_Hospital_ID'='ID'))%>%
  mutate(BSI=!is.na(interval))->cut_data #
cut_data$id=(1:nrow(cut_data))
tddf<-tmerge(cut_data,cut_data,id=id,
             death=event(surv_duration,Outcome_Death),
             tdBSI=tdc(interval)
)

summary(coxph(Surv(tstart,tstop,death)~tdBSI,data=tddf))

tddf$TS<-Surv(tddf$tstart,tddf$tstop,tddf$death)
fit<-survfit(TS~tdBSI,data=tddf)
ggsurvplot(fit)

# END






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
