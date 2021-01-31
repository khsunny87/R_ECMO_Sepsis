library(stringr)
library(purrr)
library(lubridate)



Check_Name<-function(pt_name){
  if(!str_detect(pt_name,"#\\d+$")) return(T)
  if(str_detect(pt_name,"#1")) return(T)
  return(F)
}

raw_data%>%
  filter(Insertion_Success_ECMO_implantation==1)%>%
  filter(Basic_이름!='윤재홍' & Basic_이름!='박융석')%>%
  filter(`Insertion_ECMO_시술일`<ymd("2020-7-1"))%>%#count() #852
  filter(Basic_나이>=18)%>%#count() #765
  filter(map_lgl(.$Basic_이름,Check_Name))->inc_data#count() #709

 
  #filter(Insertion_원내삽입)%>%
  

inc_data%>%count()
"
inc_data%>%
  select(Basic_Hospital_ID,Basic_이름,Basic_나이,Insertion_ECMO_시술일,Outcome_Death,Outcome_Death_date,Outcome_Last_FU_date)%>%
  write_csv('survival.txt')
"
