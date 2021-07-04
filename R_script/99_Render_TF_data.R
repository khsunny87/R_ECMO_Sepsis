library(readr)
library(dplyr)
library(tidyr)

time_format<-"%Y/%m/%d %H:%M:%S"

raw_TF<-read_csv('Input/ECMO_Sepsis_TF.txt',col_types=cols(`ECMO 시술일`=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `ECMO 제거일`=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `처방일자`	=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `불출일시`	=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `수혈시작_일시`=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `수혈종료_일시`=col_datetime("%Y/%m/%d %H:%M:%S")))


raw_TF%>%
  select(-환자번호)%>%
  filter(`ECMO 시술일`<`수혈시작_일시`,`수혈시작_일시`<`ECMO 제거일`)%>%
  group_by(Case_ID,혈액제제구분코드)%>%
  summarise(count=n())%>%
  mutate(units=ifelse(`혈액제제구분코드`=='S',count*6,count),
         type=case_when(
           `혈액제제구분코드`=='C' ~ 'Cryoprecipitate',
           `혈액제제구분코드`=='F' ~ 'FFP',
           `혈액제제구분코드`=='P' ~ 'Platelet',
           `혈액제제구분코드`=='R' ~ 'RBC',
           `혈액제제구분코드`=='S' ~ 'Platelet',
            TRUE ~ ""))%>%ungroup()%>%
  group_by(Case_ID,type)%>%summarise(units=sum(units))%>%
  filter(type!="")%>%
  pivot_wider(names_from = type, names_prefix = "TF_",values_from = units)%>%
  select(Case_ID,TF_RBC,TF_Platelet,TF_FFP,TF_Cryoprecipitate)%>%
  write_excel_csv('Input/TF_count.csv')

?pivot_wider
?case_when
?if_else
raw_TF%>%
  group_by(혈액제제구분코드,`처방명(혈액명)`)%>%
  
  summarise()
  


anal_data$Insertion_ECMO_시술일%>%str()
