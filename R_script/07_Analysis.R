library(survival)
library(survminer)
library(forcats)

# 1. MV logistic - weaned vs. death
library(stats)

#multivariable Logistic regression


anal_data%>%
  filter(Outcome_Weaning_success!='전원')%>%
  mutate(dummy_VV=(Insertion_ECMO_type=='VV-ECMO'),dummy_Others=!(Insertion_ECMO_type %in% c('VA-ECMO','VV-ECMO')))->df

#res<-glm(as.factor(Outcome_Weaning_success)~Blood_Cx+Respi_Cx+Urine_Cx+Insertion_삽입이유+Insertion_ECMO_type+PMH_HTN+PMH_Malignancy+PMH_PAOD+PMH_CKD+ECPR_ECPR+ECMO_CRRT,family=binomial,data=df)
res<-glm(as.factor(Outcome_Weaning_success)~Blood_Cx+Respi_Cx+Urine_Cx+Insertion_삽입이유+dummy_VV+dummy_Others+PMH_HTN+PMH_Malignancy+PMH_PAOD+PMH_CKD+ECPR_ECPR+ECMO_CRRT,family=binomial,data=df)

MV_LR_res<-step(res,direction="backward",trace=T)




#anova(out,test='Chisq')%>%print()
#res%>%print()



# 2. Survival - Survival vs. Death
#    time-dependent Cox

init_BSI<-r_Cx2%>%
  filter(Cx_result,Cate_sp=='BSI')%>%
  left_join(.,anal_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일),by=c('ID'='Basic_Hospital_ID'))%>%
  filter(lab_performed_time<=Outcome_ECMO_제거일+ddays(1))%>%
  mutate(interval=(lab_performed_time-Insertion_ECMO_시술일)/ddays(1))%>%
  filter(interval>=0)%>%
  group_by(ID)%>%
  summarise(interval=min(interval))

anal_data%>%
  mutate(surv_duration=pmax((Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1),ECMO_duration))%>%
  select(Basic_Hospital_ID,Outcome_Death,Outcome_Weaning_success,ECMO_duration,surv_duration,PMH_HTN)%>%
  left_join(.,init_BSI,by=c('Basic_Hospital_ID'='ID'))%>%
  mutate(BSI=!is.na(interval))->cut_data #
cut_data$id=(1:nrow(cut_data))
tddf<-tmerge(cut_data,cut_data,id=id,
             death=event(surv_duration,Outcome_Death),
             tdBSI=tdc(interval)
)

#summary(coxph(Surv(tstart,tstop,death)~tdBSI,data=tddf))%>%print()
#summary(coxph(Surv(tstart,tstop,death)~tdBSI+PMH_HTN,data=tddf))%>%print()


tddf$TS<-Surv(tddf$tstart,tddf$tstop,tddf$death)
fit<-survfit(TS~tdBSI,data=tddf)
Surv_plot<-ggsurvplot(fit,legend.title="",legend.labs=c('No BSI','BSI'))


#MV Cox analysis
cox_trim<-anal_data%>%
  mutate(surv_duration=pmax((Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1),ECMO_duration))%>%
  mutate(TS=Surv(surv_duration/30,Outcome_Death))%>%
  select(TS,ECPR_ECPR,starts_with('Basic_'),starts_with('Insertion_'),starts_with('PMH_'),starts_with('Pre_Lab_'),starts_with('Lactate_'),starts_with('ECMO_'),starts_with('Complication_'))%>%
  mutate(dummy_VV=(Insertion_ECMO_type=='VV-ECMO'),dummy_Others=!(Insertion_ECMO_type %in% c('VA-ECMO','VV-ECMO')))%>%
  select(-Basic_Hospital_ID,-Basic_Primary_Dx,-Basic_이름,-Insertion_ECMO_시술일,-Insertion_Success_ECMO_implantation,-Insertion_ECMO_type)


#cox_trim$


UV_COX_res=mycph(TS~.,data=cox_trim)




#candi2<-c("Info_Male","Info_Age","Cate_F","PMH_preHTN","PMH_smk","PMH_Marfan","Op_Root","Op_Total","Comb_CABG","CPB_CPB","CPB_TCA","DO_interval10")
#candi_label<-c('Male','Age','MP','HTN','Smoking','Marfan','Root procedure','Total Arch','Combined CABG','CPB time','TCA time','Diagnosis to surgery')

#mv_trim<-cox_trim%>%
#  na.omit()

#mv_out<-coxph(TS~.,data = mv_trim)
#res<-step(mv_out,direction='backward') # MV 


#HRplot(res,type=2,show.CI=TRUE)%>%print()

# END


