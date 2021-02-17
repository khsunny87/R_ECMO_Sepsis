anal_data%>%
  mutate(start_date=as.Date(Insertion_ECMO_시술일-ddays(10)),end_date=as.Date(Outcome_ECMO_제거일))%>%
  select(Basic_Hospital_ID,start_date,end_date)%>%
  write.csv(.,"ECMO_fever.csv")
  
