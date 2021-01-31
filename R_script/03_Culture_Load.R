library(dplyr)
library(readr)
library(moonBook)
library(tidyr)

Get_Orgname<-function(sp_str){
  culture_reg<-"^[A-z]+ [A-z| |\\(|\\)]+"

  ret<-str_match(sp_str,culture_reg)
  ret[str_detect(sp_str,'^MRSA')]<-'MRSA'
  ret[str_detect(sp_str,'^Non-typhoi')]<-'Non-typhoidal Salmonella species, Group C'
  return(ret)
}

raw_Cx<-read_csv("Input/culture_merge.txt",col_types=cols(ID=col_character(),
                                                    sp1=col_character(),
                                                    sp2=col_character(),
                                                    sp3=col_character(),
                                                    sp4=col_character()))%>%
        #mutate(SP1=str_match(sp1,culture_reg),SP2=str_match(sp2,culture_reg),SP3=str_match(sp3,culture_reg),SP4=str_match(sp4,culture_reg))
        mutate(SP1=Get_Orgname(sp1),SP2=Get_Orgname(sp2),SP3=Get_Orgname(sp3),SP4=Get_Orgname(sp4))

Blood_sp<-c('Blood, Central line','Blood,Peripheral','Catheter, vascular','혈관내 카테터')
Respi_sp<-c('(Endo)Tracheal Aspir','BAL fluid','Bronchial aspi(quan)','Bronchial washing fl','Lung aspirate','Transtracheal aspi','Sputum')
Urine_sp<-c('Urine, CIC/suprapubi','Urine, Foley cath','Urine, voided')
Wound_sp<-c('Wound,surgical/Traum')

raw_Cx$Cate_sp<-factor(
                if_else(raw_Cx$specimen%in%Blood_sp,'BSI',
                if_else(raw_Cx$specimen%in%Respi_sp,'RTI',
                if_else(raw_Cx$specimen%in%Urine_sp,'UTI',
                if_else(raw_Cx$specimen%in%Wound_sp,'SSI','Others')))),
                levels = c('BSI','RTI','UTI','SSI','Others'))

#LM_cut<-7

r_Cx<-inner_join(raw_Cx,inc_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일),by=c('ID'='Basic_Hospital_ID'))%>%
  mutate(interval=lab_performed_time-Insertion_ECMO_시술일)%>%
  mutate(Cx_before_ECMO=Cx_result & (interval<0),Cx_during_ECMO=Cx_result & (interval>=0))%>%
  #mutate(LM_Cx=Cx_during_ECMO & (interval/ddays(1)<LM_cut))%>%
  #mutate(Blood_Cx=Cx_during_ECMO & str_detect(specimen,"^Blood,"))%>%
  mutate(Blood_Cx=Cx_during_ECMO & Cate_sp=='BSI')%>%
  mutate(Respi_Cx=Cx_during_ECMO & Cate_sp=='RTI')%>%
  mutate(Urine_Cx=Cx_during_ECMO & Cate_sp=='UTI')%>%
  mutate(Wound_Cx=Cx_during_ECMO & Cate_sp=='SSI')
  #mutate(BSI_SA=Blood_Cx&(SP1=='Staphylococcus aureus'|SP2=='Staphylococcus aureus'|SP3=='Staphylococcus aureus'|SP4=='Staphylococcus aureus'))%>%
  #mutate(LM_Blood=LM_Cx&Blood_Cx,LM_Respi=LM_Cx&Respi_Cx,LM_Urine=LM_Cx&Urine_Cx,LM_Wound=LM_Cx&Wound_Cx)


sum_Cx<-r_Cx%>%
  group_by(ID)%>%
  summarise_if(is.logical,~(sum(.)>0))
Cx_list<-names(sum_Cx)[-1]



#left_join(inc_data,sum_Cx,by=c('Basic_Hospital_ID'='ID'))%>%
#  filter(LM_Blood)%>%count()
  

"
inc_data3<-inc_data%>%
  mutate(Cx_during_ECMO=F,Cx_before_ECMO=F,LM_Cx=F,Blood_Cx=F,LM_Blood=F)

LM_cut<-7
for(i in 1:nrow(inc_data3)){
  iID<-inc_data3$`Basic_Hospital_ID`[i]
  inc_data3$Cx_during_ECMO[i]<-(raw_Cx%>%
                                 filter(ID==iID,Cx_result,lab_performed_time>=inc_data3$`Insertion_ECMO_시술일`[i])%>%
                                 count())>0
  inc_data3$Cx_before_ECMO[i]<-(raw_Cx%>%
                                 filter(ID==iID,Cx_result,lab_performed_time<inc_data3$`Insertion_ECMO_시술일`[i])%>%
                                 count())>0
    
  inc_data3$LM_Cx[i]<-(raw_Cx%>%
                     filter(ID==iID,Cx_result,lab_performed_time>=inc_data3$`Insertion_ECMO_시술일`[i])%>%
                     filter((lab_performed_time-inc_data3$`Insertion_ECMO_시술일`[i])/ddays(1)<LM_cut)%>%
                     count())>0
    
  inc_data3$Blood_Cx[i]<-(raw_Cx%>%
                            filter(ID==iID,Cx_result,lab_performed_time>=inc_data3$`Insertion_ECMO_시술일`[i],str_detect(specimen,"^Blood,"))%>%
                            count())>0
  inc_data3$LM_Blood[i]<-(raw_Cx%>%
                          filter(ID==iID,Cx_result,lab_performed_time>=inc_data3$`Insertion_ECMO_시술일`[i],str_detect(specimen,"^Blood,"))%>%
                            filter((lab_performed_time-inc_data3$`Insertion_ECMO_시술일`[i])/ddays(1)<LM_cut)%>%
                          count())>0
  
}


inc_data3$LM_Blood%>%sum()

tmp1<-inc_data3%>%select(Basic_Hospital_ID,Blood_Cx)
tmp2<-sum_Cx%>%select(ID,Blood_Cx)

tmp3<-left_join(tmp1,tmp2,by=c('Basic_Hospital_ID'='ID'))%>%
  replace_na(list(Blood_Cx.y=F))%>%
  mutate(diff=(Blood_Cx.x!=Blood_Cx.y))

tmp3[tmp3$diff,]%>%View()
sum_Cx%>%
  filter(ID==tID)

tID<-'32227955'
inc_data%>%
  filter(Basic_Hospital_ID==tID)%>%.$Insertion_ECMO_시술일

raw_Cx%>%
  filter(ID==tID,Cx_result)%>%View()
"

