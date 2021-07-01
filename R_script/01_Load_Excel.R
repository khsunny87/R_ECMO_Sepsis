library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(moonBook)


#에크모 데이터 로딩
fname<-'Input/ECMO_Sepsis_YHC_data.xlsx'

#변수설정
read_excel(fname,n_max=2,col_names=F)%>%t()%>%as.data.frame()->tmp
var_name<-paste0(tmp[[1]],'_',gsub(" ","_",tmp[[2]]))


tmp_data<-read_excel(fname,skip=2,col_names=F,na=c('ND','입원중','타원','NA'))
names(tmp_data)<-var_name

eff_var<-var_name[!is.na(tmp[[1]])]

ECMO_cause<-c('Cardiopulmonary arrest','Cardiogenic shock',
             'Respiratory failure','Weaning failure of CPB',
             'Septic shock','Others')
ECMO_type<-c('VA-ECMO','VV-ECMO','E-LVAD','E-RVAD','Others')

raw_data<-tmp_data%>%
  select(eff_var)%>%
  replace_na(list(Outcome_Death=0))%>%
  mutate(Insertion_삽입이유=factor(ECMO_cause[Insertion_삽입이유],levels = ECMO_cause))%>%
  mutate(Insertion_ECMO_type=factor(ECMO_type[Insertion_ECMO_type],levels=ECMO_type))

rm(tmp_data)




Update_survival<-function(){
update<-read_csv('Input/survival_update.txt',col_types=cols(ID=col_character()))


tmp_data<-raw_data%>%
  select(Basic_Hospital_ID,Outcome_Death,Outcome_Death_date,Outcome_Last_FU_date)%>%
  left_join(.,update,by=c('Basic_Hospital_ID'='ID'))

raw_data$Outcome_Death<<-if_else(is.na(tmp_data$Death),tmp_data$Outcome_Death,tmp_data$Death)
raw_data$Outcome_Death_date<<-if_else(!is.na(tmp_data$Death)&tmp_data$Death==1,as.POSIXct(tmp_data$Last_day),tmp_data$Outcome_Death_date)
raw_data$Outcome_Last_FU_date<<-if_else(is.na(tmp_data$Death),tmp_data$Outcome_Last_FU_date,as.POSIXct(tmp_data$Last_day))

}

Update_survival()

BM_lab<-read_csv('Input/BM_lab.txt',col_types=cols(ID=col_character()))

raw_data<-left_join(raw_data,BM_lab,by=c('Basic_Hospital_ID'='ID'))


