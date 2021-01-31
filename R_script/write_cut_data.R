cut_data%>%
  mutate(ID=c(1:nrow(cut_data)),Outcome_Weaning_success=(Outcome_Weaning_success==1),Outcome_Death=(Outcome_Death==1))%>%
  select(ID,ECMO_duration,Outcome_Weaning_success,Outcome_Death,Surv_duration=surv_duration,BSI_Cx=BSI,BSI_interval=interval)%>%
  write.csv('ECMO_sepsis.csv',row.names = F)

tdCox<-cut_data%>%
  mutate(ID=c(1:nrow(cut_data)),Outcome_Weaning_success=(Outcome_Weaning_success==1),Outcome_Death=(Outcome_Death==1))%>%
  select(ID,ECMO_duration,Outcome_Weaning_success,Outcome_Death,Surv_duration=surv_duration,BSI_Cx=BSI,BSI_interval=interval)


View(anal_data)

cut_data%>%
  filter(BSI)%>%.$interval%>%as.numeric()%>%summary()
  hist(as.numeric(.$interval))
?hist

  
anal_data%>%
  mutate(ID=Basic_Hospital_ID,Name=Basic_이름,Start=Insertion_ECMO_시술일,End=Outcome_ECMO_제거일)%>%
  select(ID,Name,Start,End)%>%
  write.csv('Lab_list.txt',row.names=F)
