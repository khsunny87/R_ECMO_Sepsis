

#Group 0 - control
#Group 1 - PreECMO sepsis - 삽입이유 5
#Group 2 - ECMO sepsis - ECMO 기간 Cx(+)


replace_na2<-function(data,col_vector){
  tmp<-rep(F,length(col_vector))
  names(tmp)<-col_vector
  replace_na(data,map(tmp,~.))
}
Grouping<-function(Dx,ECMO_type,Cause,Cx_during_ECMO){
  return(if_else(str_detect(Dx,"(?i)pneumonia|VAP|HAP")&(ECMO_type=='VV-ECMO')|Cause=='Septic shock',1,if_else(Cx_during_ECMO,2,0)))
}


#anal_data<-left_join(inc_data,sum_Cx,by=c('Basic_Hospital_ID'='ID'))%>%
#  replace_na(list(Cx_before_ECMO=F,Cx_during_ECMO=F,LM_Cx=F,Blood_Cx=F,LM_Blood=F,Respi_Cx=F,LM_Respi=F))

raw_TF_data<-read_csv('Input/TF_count.csv')


anal_data<-left_join(inc_data,sum_Cx,by=c('Basic_Hospital_ID'='ID'))%>%
  replace_na2(Cx_list)%>%
  mutate(ECMO_duration=(Outcome_ECMO_제거일-Insertion_ECMO_시술일)/ddays(1))%>%
  mutate(Basic_성별=(Basic_성별==1))%>%
  left_join(.,raw_TF_data,by=c('Basic_Case_ID'='Case_ID'))

anal_data$OTT_BSI<-anal_data%>%
  select(ends_with('_Cx'),-Blood_Cx)%>%
  apply(.,1,any)

anal_data$Group<-Grouping(anal_data$Basic_Primary_Dx,anal_data$Insertion_ECMO_type,anal_data$Insertion_삽입이유,anal_data$Cx_during_ECMO)




#anal_data$ECMO_duration=(anal_data$Outcome_ECMO_제거일-anal_data$Insertion_ECMO_시술일)/ddays(1)





