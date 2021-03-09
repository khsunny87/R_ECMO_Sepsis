library(stringr)
library(purrr)
library(lubridate)


#2회이상 삽입 제거
Check_Name<-function(pt_name){
  if(!str_detect(pt_name,"#\\d+$")) return(T)
  if(str_detect(pt_name,"#1")) return(T)
  return(F)
}

ex_table=list()

inc_data<-raw_data%>%
  filter(Insertion_Success_ECMO_implantation==1)%>%
  filter(Basic_이름!='윤재홍' & Basic_이름!='박융석')%>%
  filter(`Insertion_ECMO_시술일`<ymd("2020-7-1"))#count() #852
ex_table[[1]]=c(inc_data%>%count()%>%.$n,0,'')


inc_data<-inc_data%>%
  filter(Basic_나이>=18) #count() #765
ex_table[[2]]<-c(inc_data%>%count()%>%.$n,0,'18세 미만 제외')
ex_table[[2]][2]<-ex_table[[1]][1]-ex_table[[2]][1]

inc_data<-inc_data%>%
  filter(map_lgl(.$Basic_이름,Check_Name)) #709
ex_table[[3]]<-c(inc_data%>%count()%>%.$n,0,'반복 삽입 제외')
ex_table[[3]][2]<-ex_table[[2]][1]-ex_table[[3]][1]

inc_data<-inc_data%>%
  filter(Outcome_Weaning_success!='전원')#count() #705
ex_table[[4]]<-c(inc_data%>%count()%>%.$n,0,'전원 제외')
ex_table[[4]][2]<-ex_table[[3]][1]-ex_table[[4]][1]


ex_table<-do.call(rbind,inc_table)%>%data.frame()
names(ex_table)<-c('Result','Exclusion','Comment')

  #filter(Insertion_원내삽입)%>%
  

inc_data%>%count()
"
inc_data%>%
  select(Basic_Hospital_ID,Basic_이름,Basic_나이,Insertion_ECMO_시술일,Outcome_Death,Outcome_Death_date,Outcome_Last_FU_date)%>%
  write_csv('survival.txt')
"
