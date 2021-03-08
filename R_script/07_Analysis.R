library(survival)
library(survminer)
library(forcats)

# 1. MV logistic - weaned vs. death
library(stats)

#multivariable Logistic regression

#0.05

sig_var<-table1$res%>%
  filter(as.numeric(p)<0.05)%>%.[[1]]%>%str_trim()
#sig_var
#LR_df%>%names()

LR_df<-anal_data%>%
  filter(Outcome_Weaning_success!='전원')%>%
  mutate(dummy_VV=(Insertion_ECMO_type=='VV-ECMO'),dummy_Others=!(Insertion_ECMO_type %in% c('VA-ECMO','VV-ECMO')))%>%
  ##여기서 끊으면 기존 plot
  select(Outcome_Weaning_success,dummy_VV,dummy_Others,contains(anal_data[get_label(anal_data)%in%sig_var]%>%names()))%>%
  select(-Outcome_Death,-Insertion_ECMO_type,-Outcome_Death_date,-Outcome_Survival_discharge,-ECMO_duration,-Lactate_Lactic_acid_48)%>%
#  select(-Group)%>% #0.2
  na.omit()


#res<-glm(as.factor(Outcome_Weaning_success)~Blood_Cx+Respi_Cx+Urine_Cx+Insertion_삽입이유+dummy_VV+dummy_Others+PMH_HTN+PMH_Malignancy+PMH_PAOD+PMH_CKD+ECPR_ECPR+ECMO_CRRT,family=binomial,data=LR_df)

res<-glm(as.factor(Outcome_Weaning_success)~.,family=binomial,data=LR_df)
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



cut_data<-anal_data%>%
  mutate(surv_duration=pmax((Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1),ECMO_duration))%>%
#  select(Basic_Hospital_ID,Outcome_Death,Outcome_Weaning_success,ECMO_duration,surv_duration,PMH_HTN)%>%
  left_join(.,init_BSI,by=c('Basic_Hospital_ID'='ID'))%>%
  mutate(BSI=!is.na(interval))
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
cox_trim<-tddf%>%
  select(TS,tdBSI,ECPR_ECPR,starts_with('Basic_'),starts_with('Insertion_'),starts_with('PMH_'),starts_with('Pre_Lab_'),starts_with('Lactate_'),starts_with('ECMO_'),starts_with('Complication_'))%>%
  mutate(dummy_VV=(Insertion_ECMO_type=='VV-ECMO'),dummy_Others=!(Insertion_ECMO_type %in% c('VA-ECMO','VV-ECMO')))%>%
  select(-Basic_Hospital_ID,-Basic_Primary_Dx,-Basic_이름,-Insertion_ECMO_시술일,-Insertion_Success_ECMO_implantation,-Insertion_ECMO_type)


#cox_trim$


UV_COX_res=mycph(TS~.,data=cox_trim)

#UV_COX_res%>%
#  filter(p<0.05)%>%row.names()

MV_COX_candi<-tribble(
          ~col_name,~label,
          'tdBSI','BSI',
          "ECPR_ECPR", 'ECPR',
          "Basic_나이", 'Age',
        #  "Basic_키", 'Height', #0.2
        #  "Insertion_삽입이유", 'ECMO Indication',
        #   "Insertion_Mechanical_ventilation",'Mechanical ventilation', #0.2
          "PMH_Malignancy",'Malignancy',
          "PMH_HTN",'HTN',
        # "PMH_Dyslipidemia",'Dyslipidemia', #0.2
          "PMH_Current_smoker",'Current smoker',
          "PMH_CKD",'CKD',
          "PMH_PAOD",'PAOD',
          "Pre_Lab_Hb",'Hb',
          "Pre_Lab_Plt",'Platelet',
          "Pre_Lab_LDH",'LDH',
          "Pre_Lab_BUN",'BUN',
          "Pre_Lab_Cr",'Creatinine',
          "Pre_Lab_CRP",'CRP',
          "Pre_Lab_WBC",'WBC',
          "Pre_Lab_Seg",'Segmented neutrophil',
          "Lactate_Lactic_acid_00",'Lactate 0h',
          "Lactate_Lactic_acid_24",'Lactate 24h',
          #"Lactate_Lactic_acid_48",'Lactate 48h', #얘 있으면 BSI가 안나와
          "ECMO_CRRT",'CRRT',
        #  "ECMO_Vasopressor",'Vasopressor', #0.2
          "dummy_VV",'VV-ECMO')

MV_trim<-cox_trim%>%
  select(TS,contains(MV_COX_candi$col_name))%>%
  na.omit()
res<-coxph(TS~.,data=MV_trim)
MV_COX_res<-  step(res,direction='backward',trace=T) # MV 




#HRplot(res,type=2,show.CI=TRUE)%>%print()

# END


