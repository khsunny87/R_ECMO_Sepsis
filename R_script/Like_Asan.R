VA_data<-anal_data%>%
  filter(Insertion_ECMO_type==1)%>%
  filter(Insertion_삽입이유%in% c(2,5))%>%
  mutate(surv_duration=(Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1))

VA_data$TS=Surv(VA_data$surv_duration,VA_data$Outcome_Death)
fit<-survfit(TS~Insertion_삽입이유,data=VA_data)

print(fit)
summary(fit)
#ggsurvplot(fit,data=surv_data,risk.table=T,pval=T,fun='event')
ggsurvplot(fit,data=VA_data,risk.table=T,pval=T)
