library(pROC)

init_BSI<-r_Cx2%>%
  filter(Cx_result,Cate_sp=='BSI')%>%
  left_join(.,anal_data%>%select(Basic_Hospital_ID,Insertion_ECMO_시술일,Outcome_ECMO_제거일),by=c('ID'='Basic_Hospital_ID'))%>%
  filter(lab_performed_time<=Outcome_ECMO_제거일)%>%
  mutate(interval=(lab_performed_time-Insertion_ECMO_시술일)/ddays(1))%>%
  filter(interval>=0)%>%
  group_by(ID)%>%
  summarise(interval=min(interval))
max(init_BSI$interval)

?max
anal_data%>%
  mutate(surv_duration=pmax((Outcome_Last_FU_date-Insertion_ECMO_시술일)/ddays(1),ECMO_duration))%>%
  select(Basic_Hospital_ID,Outcome_Death,Outcome_Weaning_success,ECMO_duration,surv_duration)%>%
  left_join(.,init_BSI,by=c('Basic_Hospital_ID'='ID'))%>%
  mutate(BSI=!is.na(interval))->cut_data #
cut_data$id=(1:nrow(cut_data))
tddf<-tmerge(cut_data,cut_data,id=id,
       death=event(surv_duration,Outcome_Death),
       tdBSI=tdc(interval)
       )

summary(coxph(Surv(tstart,tstop,death)~tdBSI,data=tddf))

tddf$TS<-Surv(tddf$tstart,tddf$tstop,tddf$death)
fit<-survfit(TS~tdBSI,data=tddf)
ggsurvplot(fit)


library(RcmdrPlugin.EZR)

Mantel.Byar(Group = tddf$tdBSI, Event = tddf$death,  StartTime = tddf$tstart, StopTime = tddf$tstop, 
            method = c("SAS", "Tominaga"), plot=1, landmark=0)

View(tddf)
?tmerge

mcut<-7
anal_data%>%
  select(Basic_Hospital_ID,Outcome_Death,Outcome_Weaning_success)%>%
  left_join(.,init_BSI,by=c('Basic_Hospital_ID'='ID'))%>%
  mutate(res=if_else(is.na(interval),FALSE,interval<=mcut))%>%
  mytable(Outcome_Death~res,show.total=T,data=.)


cut_data%>%
  ggplot()+
    geom_point(aes(x=as.factor(Outcome_Death),y=interval))
#View(cut_data)
b1<-cut_data%>%roc(1-Outcome_Death~interval,data=.,ci=T,percent=T)
b1<-cut_data%>%roc(Outcome_Weaning_success~interval,data=.,ci=T,percent=T)
plot.roc(b1,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   # 기준치(cut-off value)에 대한 출력, 포인트, 색상을 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 
