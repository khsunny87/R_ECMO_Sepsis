anal_data%>%
  filter(Blood_Cx)


library(stringr)
lab_data<-read_csv('Input/lab_merge.txt')
biomarker_code<-c('WBC Count, Blood'='BL2011', 
                  'Segmented neutrophil'='BL201806',               
                  'CRP, Quantitative (High Sensitivity)'='BL3140',
                  'Procalcitonin, quantitative' ='BL5044')

biomarker_code


BSI2<-r_Cx2%>%
  filter(Cx_result,Cate_sp=='BSI')%>%
  left_join(.,anal_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일),by=c('ID'='Basic_Hospital_ID'))%>%
  filter(lab_performed_time<=Outcome_ECMO_제거일+ddays(1))%>%
  mutate(interval=(lab_performed_time-Insertion_ECMO_시술일)/ddays(1))%>%
  filter(interval>=0)%>%
  group_by(ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일)%>%
  summarise(interval=min(interval),lab_performed_time=min(lab_performed_time))



BSI_lab<-lab_data%>%
  filter(검사코드 %in% biomarker_code,ID %in% BSI2$ID)%>%
  mutate(values=as.numeric(sub('^[^0-9]','',values)))%>%
  left_join(.,BSI2%>%ungroup()%>%select(ID,lab_performed_time,interval))%>%
  filter(검사시간<=lab_performed_time+ddays(1))%>%
  group_by(ID,검사코드)%>%
  filter(검사시간==max(검사시간))%>%select(-검사명,-단위,-검사시간)%>%
  pivot_wider(names_from = 검사코드,values_from=values)%>%
  right_join(.,anal_data%>%
               filter(Blood_Cx)%>%select(Basic_Hospital_ID,Outcome_Weaning_success,Outcome_Death),by=c('ID'='Basic_Hospital_ID'))
  

View(BSI_lab)
BSI_lab%>%
  mytable(Outcome_Weaning_success~.,data=.,method=3)
?mytable
BSI_lab%>%
  mytable(Outcome_Death~.,data=.,method=3)

biomarker_code
anal_data%>%
  filter(Blood_Cx)%>%select(Basic_Hospital_ID,Outcome_Weaning_success,Outcome_Death)


View(anal_data%>%
  filter(Basic_Hospital_ID=='03084381')%>%
  select(Basic_Hospital_ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일))

View(r_Cx2%>%filter(ID=='03084381',Cx_result))

init_BSI$ID
anal_data%>%
  filter(Blood_Cx)%>%
  filter(!(Basic_Hospital_ID %in% init_BSI$ID))%>%
  View()
  str()
