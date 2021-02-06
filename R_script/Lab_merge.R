library(readr)
library(dplyr)
library(tidyr)
library(stringr)

path<-'Input/Lab_txt/'
flist<-dir(path)


Load_Labfile<-function(path,fname){

raw_txt<-read_csv(paste0(path,fname))
ID<-str_match(raw_txt[2,1],"환자번호: (\\d{8})")[1,2]

raw2<-raw_txt%>%slice(-1,-2,-3)
names(raw2)<-raw_txt%>%slice(3)%>%as.character()

rendered<-raw2%>%
  pivot_longer(c(4:length(raw2)),names_to='검사시간',values_to='values')%>%
  na.omit()%>%
  mutate(ID=ID)%>%
  select(ID,everything())
return(as.data.frame(rendered))
}

merged<-lapply(flist,function(x) Load_Labfile(path=path,fname=x))%>%
  do.call(rbind,.)
View(merged)
