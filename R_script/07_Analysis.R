library(survival)
library(survminer)

# 1. MV logistic - weaned vs. death
library(stats)

#multivariable Logistic regression
tbl_data%>%
  filter(Outcome_Weaning_success!='전원')->df
res<-glm(as.factor(Outcome_Weaning_success)~Blood_Cx+Respi_Cx+Urine_Cx+Insertion_삽입이유+Insertion_ECMO_type+PMH_HTN+PMH_Malignancy+PMH_PAOD+PMH_CKD+ECPR_ECPR+ECMO_CRRT,family=binomial,data=df)
out<-step(res,direction="backward",trace=T)
summary(out)%>%print()
anova(out,test='Chisq')%>%print()
res%>%print()


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

summary(coxph(Surv(tstart,tstop,death)~tdBSI,data=tddf))%>%print()

tddf$TS<-Surv(tddf$tstart,tddf$tstop,tddf$death)
fit<-survfit(TS~tdBSI,data=tddf)
ggsurvplot(fit,legend.title="",legend.labs=c('No BSI','BSI'))%>%print()


# END

