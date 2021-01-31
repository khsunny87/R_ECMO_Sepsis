raw_Cx$lab_ID<-c(1:nrow(raw_Cx))


A<-raw_Cx%>%filter(Cx_result)%>%
  select(-sp1,-sp2,-sp3,-sp4)%>%
  gather('SP1','SP2','SP3','SP4',key='SP',value='Organism')%>%
  select(-SP)%>%
  filter(!is.na(Organism))
#View(A) 
B<-raw_Cx%>%filter(!Cx_result)%>%
  select(-sp1,-sp2,-sp3,-sp4,-SP1,-SP2,-SP3,-SP4)%>%
  mutate(Organism=NA)
#View(B)
r_Cx2<-rbind(A,B)






