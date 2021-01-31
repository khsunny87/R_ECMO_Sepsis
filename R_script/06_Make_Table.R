library(ztable)
library(sjlabelled)
library(forcats)
library(kableExtra)

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
options(ztable.type='viewer')

label_data<-read_csv('Input/label.csv')
labels<-names(anal_data)
names(labels)<-labels
labels[label_data$var_name]<-label_data$label
anal_data<-set_label(anal_data,labels)

tbl_data<-anal_data%>%
  select(-Basic_Hospital_ID,-Basic_이름,-Insertion_ECMO_시술일,-Basic_Primary_Dx,-Insertion_원내삽입,-Insertion_Success_ECMO_implantation,-Outcome_ECMO_제거일,-Outcome_Death_date,-Outcome_Discharge_date,-Outcome_Last_FU_date,-Complication_Sepsis)
tbl_data$Outcome_Weaning_success<-set_labels(tbl_data$Outcome_Weaning_success,labels=c('0'="No",'1'="Yes",'전원'="Transfer"))
tbl_data$Outcome_Survival_discharge<-set_labels(tbl_data$Outcome_Survival_discharge,labels=c("No","Yes","Hopeless discharge"))

tbl_data%>%
  mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))%>%
  mutate(Group=factor(group_name[anal_data$Group+1],levels=group_name))%>%
  mytable(Outcome_Death~.,data=.,show.total=T)%>%
  compress(add.label=F)%>%
  ztable()

tbl_data%>%
  filter(Outcome_Weaning_success!='전원')%>%
  #mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))%>%
  #mutate(Group=factor(group_name[anal_data$Group+1],levels=group_name))%>%
  mutate(Outcome_Weaning_success=fct_relevel(if_else(Outcome_Weaning_success==1,'Weaned','Death'),'Weaned'))%>%
  mytable(Outcome_Weaning_success~.,data=.,show.total=T)%>%
  compress(add.label=F)%>%
  ztable()
library(stats)

tbl_data%>%
  filter(Outcome_Weaning_success!='전원')->df
res<-glm(as.factor(Outcome_Weaning_success)~Blood_Cx+Respi_Cx+Urine_Cx+Insertion_삽입이유+Insertion_ECMO_type+PMH_HTN+PMH_Malignancy+PMH_PAOD+PMH_CKD+ECPR_ECPR+ECMO_CRRT,family=binomial,data=df)
out<-step(res,direction="backward",trace=T)
summary(out)
anova(out,test='Chisq')
res

tbl_data%>%
  #mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))%>%
  #mutate(Group=factor(group_name[anal_data$Group+1],levels=group_name))%>%
  mytable(Cx_during_ECMO~.,data=.,show.total=T)%>%
  compress(add.label=F)%>%
  ztable()

tbl_data%>%
  #mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))%>%
  #mutate(Group=factor(group_name[anal_data$Group+1],levels=group_name))%>%
  mytable(Blood_Cx~.,data=.,show.total=T)%>%
  compress(add.label=F)%>%
  ztable()


r_Cx%>%
  filter(interval>=0)%>% #12522
  filter(Cx_result)%>%#2414
  .$Cate_sp%>%table()




Cate_label<-c('Total','BSI','RTI','UTI','SSI','Others')

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
Org_tbl%>%kable()%>%
  add_header_above(header)%>%
  kable_styling()







  