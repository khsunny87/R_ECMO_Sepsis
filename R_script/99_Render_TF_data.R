library(readr)

time_format<-"%Y/%m/%d %H:%M:%S"

raw_TF<-read_csv('Input/ECMO_Sepsis_TF.txt',col_types=cols(`ECMO 시술일`=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `ECMO 제거일`=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `처방일자`	=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `불출일시`	=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `수혈시작_일시`=col_datetime("%Y/%m/%d %H:%M:%S"),
                                                   `수혈종료_일시`=col_datetime("%Y/%m/%d %H:%M:%S")))


raw_TF%>%
  filter(`ECMO 시술일`<`수혈시작_일시`,`수혈시작_일시`<`ECMO 제거일`)%>%
  group_by(Case_ID,혈액제제구분코드)%>%View()
  summarise(count=n())

  
raw_TF%>%
  group_by(혈액제제구분코드,`처방명(혈액명)`)%>%
  summarise()
  


anal_data$Insertion_ECMO_시술일%>%str()
