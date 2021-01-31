
BSI_sp<-Org_tbl[1:10,3]

BSI<-r_Cx%>%
  filter(Blood_Cx)%>%
  select(-sp1,-sp2,-sp3,-sp4)%>%
  gather('SP1','SP2','SP3','SP4',key='SP',value='Organism')%>%
  select(-SP)%>%
  filter(!is.na(Organism))%>%
  filter(Organism%in%BSI_sp)%>%
  group_by(ID,Organism)%>%
  summarise()%>%
  mutate(val=T)%>%
  spread(key='Organism',value='val',fill=F)
BSI<-BSI[,c('ID',BSI_sp)]
names(BSI)<-gsub(" ","_",names(BSI))


BSI%>%nrow()
anal_data%>%
  filter(Blood_Cx)%>%
  nrow()

anal_data%>%
  filter(Blood_Cx)%>%
  select(Basic_Hospital_ID,Outcome_Death)%>%
  left_join(.,BSI,by=c('Basic_Hospital_ID'='ID'))%>%
  mytable(Outcome_Death~.-Basic_Hospital_ID,data=.,show.total=T)%>%
  compress(add.label=F)%>%
  ztable()



