library(moonBook)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(forcats)
library(sjlabelled)

biomarker_code<-c('WBC Count, Blood'='BL2011', 
                  'Segmented neutrophil'='BL201806',               
                  'CRP, Quantitative (High Sensitivity)'='BL3140',
                  'Procalcitonin, quantitative' ='BL5044')

#lab_data<-read_csv('Input/lab_merge.txt')


#BSI2<-r_Cx2%>%
#  filter(Cx_result,Cate_sp=='BSI')%>%
#  left_join(.,anal_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일),by=c('ID'='Basic_Hospital_ID'))%>%
#  filter(lab_performed_time<=Outcome_ECMO_제거일+ddays(1))%>%
#  mutate(interval=(lab_performed_time-Insertion_ECMO_시술일)/ddays(1))%>%
#  filter(interval>=0)%>%
#  group_by(ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일)%>%
#  summarise(interval=min(interval),lab_performed_time=min(lab_performed_time))


#BSI_lab<-
#BSI_BM<-lab_data%>%
#  filter(검사코드 %in% biomarker_code,ID %in% BSI2$ID)%>%
#  mutate(values=as.numeric(sub('^[^0-9]','',values)))%>%
#  left_join(.,BSI2%>%ungroup()%>%select(ID,lab_performed_time,interval))%>%
#  filter(검사시간<=lab_performed_time+ddays(1))%>%
#  group_by(ID,검사코드)%>%
#  filter(검사시간==max(검사시간))%>%select(-검사명,-단위,-검사시간)%>%
#  pivot_wider(names_from = 검사코드,values_from=values)

#write_csv(BSI_BM,'Input/BSI_BM.txt')


#raw_BT<-read_csv('Input/BT_no_pass.txt',col_types=cols(`환자번호`=col_character()))

#raw_BT%>%
#  select(환자번호,체온측정일시,체온)%>%
#  left_join(.,BSI2%>%ungroup()%>%select(ID,lab_performed_time),by=c('환자번호'='ID'))%>%
#  filter(!is.na(lab_performed_time))%>%
#  filter(체온측정일시>=lab_performed_time-ddays(1),체온측정일시<=lab_performed_time+ddays(1))%>%
#  group_by(환자번호,lab_performed_time)%>%
#  summarise(hBT=max(체온),hBT_time=체온측정일시[which.max(체온)])%>%
#  write_csv(.,'Input/BSI_BT.txt')

BSI_IR<-nrow(init_BSI)/ # n of BSI
  (
    anal_data%>%filter(!(Basic_Hospital_ID%in%init_BSI$ID))%>%.$ECMO_duration%>%sum()+ # n*days of non_infected
    sum(init_BSI$interval) # n*days of infected until infection
    )*1000


BSI_BM<-read_csv('Input/BSI_BM.txt',col_types=cols(ID=col_character()))
BSI_BT<-read_csv('Input/BSI_BT.txt',col_types=cols(`환자번호`=col_character()))

  

BSI_lab<-BSI_BM%>%
  right_join(.,anal_data%>%
               filter(Blood_Cx)%>%select(Basic_Hospital_ID,Outcome_Weaning_success,Outcome_Death),by=c('ID'='Basic_Hospital_ID'))%>%
  ungroup()%>%
  left_join(.,  BSI_BT%>%select(환자번호,hBT),by=c('ID'='환자번호'))%>%
  select(-ID,-lab_performed_time)%>%
  mutate(Outcome_Weaning_success=fct_relevel(if_else(Outcome_Weaning_success==1,'Weaned','Death'),'Weaned'))%>%
  mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))
  

#BSI_lab%>%View()



LabName<-function(code){
  if(sum(biomarker_code==code)==0)
    return(code)
  return(names(biomarker_code[biomarker_code==code]))
}

BSI_lab<-set_label(BSI_lab,sapply(names(BSI_lab),LabName))
attr(BSI_lab$hBT,'label')<-'BT'



#BSI culture시 biomarker
table6<-BSI_lab%>%
  mytable(Outcome_Weaning_success~.,data=.,show.total=T)
table7<-BSI_lab%>%
  mytable(Outcome_Death~.,data=.,show.total=T)


  