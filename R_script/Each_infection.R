

Print_infection<-function(item='Total',n_length=10){
  
  
  
  tbl_col<-which(Cate_label==item)*2-1
  item_sp<-Org_tbl[1:n_length,tbl_col]
  
  #View(r_Cx%>%filter(ID=='33786301',Cx_during_ECMO))

  item_df<-r_Cx%>%filter(Cx_during_ECMO)%>%
    filter(Cate_sp==item|item=='Total')%>%
    select(-sp1,-sp2,-sp3,-sp4)%>%
    gather('SP1','SP2','SP3','SP4',key='SP',value='Organism')%>%
    select(-SP)%>%
    filter(!is.na(Organism))%>%
    mutate(Organism=if_else(Organism%in%item_sp,Organism,'Others'))%>%
    #filter(Organism%in%item_sp)%>%
    group_by(ID,Organism)%>%
    summarise()%>%
    mutate(val=T)%>%
    spread(key='Organism',value='val',fill=F)
  item_df<-item_df[,c('ID',item_sp,'Others')]
  names(item_df)<-gsub(" ","_",names(item_df))
  
  
  anal_data%>%
    #select(Basic_Hospital_ID,Outcome_Death)%>%
    select(Basic_Hospital_ID,Outcome_Weaning_success)%>%
    inner_join(.,item_df,by=c('Basic_Hospital_ID'='ID'))%>%
    filter(Outcome_Weaning_success!='전원')%>%
    mutate(Outcome_Weaning_success=fct_relevel(if_else(Outcome_Weaning_success==1,'Weaned','Death'),'Weaned'))%>%
    mytable(Outcome_Weaning_success~.-Basic_Hospital_ID,data=.,show.total=T)%>%
    #mutate(Outcome_Death=fct_relevel(if_else(Outcome_Death==0,'Survivors','Nonsurvivors'),'Survivors'))%>%
    #mytable(Outcome_Death~.-Basic_Hospital_ID,data=.,show.total=T)%>%
    compress(add.label=F)%>%
    ztable()
  
  #A<-anal_data%>%filter(Cx_during_ECMO)
  
  
  #A%>%filter(!(Basic_Hospital_ID%in%tmp$Basic_Hospital_ID))%>%View()
  
  
}



Cate_label
Org_tbl

Print_infection('BSI')

anal_data%>%
  filter(Urine_Cx)%>%count()



r_Cx%>%
  filter(Cx_during_ECMO)%>%
  select(-sp1,-sp2,-sp3,-sp4)%>%
  gather('SP1','SP2','SP3','SP4',key='SP',value='Organism')%>%
  select(-SP)%>%
  filter(!is.na(Organism))%>%
  filter(Organism=='Klebsiella pneumoniae')%>%.$Cate_sp%>%table()
