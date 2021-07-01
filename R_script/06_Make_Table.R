library(ztable)
library(sjlabelled)
library(forcats)
library(kableExtra)
library(readr)

Get_DF<-function(org_rank){
  return(data.frame(Organism=names(org_rank),Freqeuncy=as.vector(org_rank)))
}


Organism_list<-function(culture_df,length=-1){
  tmp<-culture_df%>%
    filter(interval>=0)%>%
    filter(Cx_result)
  sp_list<-c(tmp$SP1,tmp$SP2,tmp$SP3,tmp$SP4)
  if(length==-1)
    return(sp_list%>%table()%>%sort(decreasing = T))
  return(sp_list%>%table()%>%sort(decreasing = T)%>%.[1:length])
}

group_name<-c('No sepsis','Sepsis before ECMO','Sepsis during ECMO')


#Labelling
label_data<-read_csv('Input/label.csv')
labels<-names(anal_data)
names(labels)<-labels
labels[label_data$var_name]<-label_data$label
anal_data<-set_label(anal_data,labels)

tbl_data<-anal_data%>%
  select(-Basic_Case_ID,-Basic_Hospital_ID,-Basic_이름,-Insertion_ECMO_시술일,-Basic_Primary_Dx,-Insertion_원내삽입,-Insertion_Success_ECMO_implantation,-Outcome_ECMO_제거일,-Outcome_Death_date,-Outcome_Discharge_date,-Outcome_Last_FU_date,-Complication_Sepsis)%>%
  mutate(Group=factor(group_name[anal_data$Group+1],levels=group_name))
  
tbl_data$Outcome_Weaning_success<-set_labels(tbl_data$Outcome_Weaning_success,labels=c('0'="No",'1'="Yes",'전원'="Transfer"))
tbl_data$Outcome_Survival_discharge<-set_labels(tbl_data$Outcome_Survival_discharge,labels=c("No","Yes","Hopeless discharge"))

table1<-tbl_data%>% # Weaned vs. Death
  filter(Outcome_Weaning_success!='전원')%>%
  mutate(Outcome_Weaning_success=fct_relevel(if_else(Outcome_Weaning_success==1,'Weaned','Death'),'Weaned'))%>%
  mytable(Outcome_Weaning_success~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)

table2<-tbl_data%>% # Survivors vs. Nonsurvivors
  mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))%>%
  mytable(Outcome_Death~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)


table3<-tbl_data%>% # Culture during ECMO
  mutate(Cx_during_ECMO=fct_relevel(if_else(Cx_during_ECMO==T,'Culture(+)','Culture(-)'),'Culture(+)'))%>%
  mytable(Cx_during_ECMO~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)

table4<-tbl_data%>% # Blood culture during ECMO
  mutate(Blood_Cx=fct_relevel(if_else(Blood_Cx==T,'BSI(+)','BSI(-)'),'BSI(+)'))%>%
  mytable(Blood_Cx~.,data=.,show.total=T,catMethod=0)%>%
  compress(add.label=F)



#r_Cx%>%
#  filter(interval>=0)%>% #12522
#  filter(Cx_result)%>%#2414
#  .$Cate_sp%>%table()


Cate_label<-c('Total','BSI','RTI','UTI','SSI','Others')


r_Cx<-r_Cx%>%
  filter(ID%in%anal_data$Basic_Hospital_ID)  #preSepsis 제외

Org_tbl<-r_Cx%>%
  Organism_list(length=10)%>%
  Get_DF()
A<-r_Cx%>%filter(interval>=0)%>%count()
B<-r_Cx%>%filter(interval>=0)%>%filter(Cx_result)%>%count()
suffix<-paste0('(',B,'/',A,')')
for(i in c(2:length(Cate_label))){
  Org_tbl<-cbind(Org_tbl,r_Cx%>%filter(Cate_sp==Cate_label[i])%>%
    Organism_list(length=10)%>%
    Get_DF())
  A<-r_Cx%>%filter(Cate_sp==Cate_label[i])%>%filter(interval>=0)%>%count()
  B<-r_Cx%>%filter(Cate_sp==Cate_label[i])%>%filter(interval>=0)%>%filter(Cx_result)%>%count()
  suffix<-c(suffix,paste0('(',B,'/',A,')'))
}

header<-rep(2,length(Cate_label))
names(header)<-paste(Cate_label,suffix)


table5<-Org_tbl%>%kable()%>% # Organism (+) by specimens
  add_header_above(header)%>%
  kable_styling()







  