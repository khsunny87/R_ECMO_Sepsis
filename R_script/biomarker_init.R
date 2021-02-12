library(stringr)
lab_data<-read_csv('Input/lab_merge.txt')
biomarker_code<-c('WBC Count, Blood'='BL2011', 
                  'Segmented neutrophil'='BL201806',               
#                  'CRP, Quantitative (High Sensitivity)'='BL3140',
                  'Procalcitonin, quantitative' ='BL5044')
BM_rename<-c('ID'='ID',
             'BL2011'='Pre_Lab_WBC',
              'BL201806'='Pre_Lab_Seg',               
 #             'BL3140'='Pre_Lab_CRP',
              'BL5044'='Pre_Lab_Procalcitonin')

BM_lab<-lab_data%>%
  filter(검사코드 %in% biomarker_code)%>%
  mutate(values=as.numeric(sub('^[^0-9]','',values)))%>%
  left_join(.,anal_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일),by=c('ID'='Basic_Hospital_ID'))%>%
  filter(검사시간<=Insertion_ECMO_시술일+ddays(1))%>%
  group_by(ID,검사코드)%>%
  filter(검사시간==max(검사시간))%>%select(-검사명,-단위,-검사시간)%>%
  pivot_wider(names_from = 검사코드,values_from=values)%>%
  select(-Insertion_ECMO_시술일)
names(BM_lab)<-BM_rename[names(BM_lab)]
BM_lab

write_csv(BM_lab,'Input/BM_lab.txt')
