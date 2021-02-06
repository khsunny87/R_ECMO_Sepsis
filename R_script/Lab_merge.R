library(readr)
library(dplyr)
library(tidyr)
library(stringr)

path<-'Input/Lab_txt/'

fname<-'00130241_lab.txt'
raw_txt<-read_csv(paste0(path,fname))

ID<-str_match(raw_txt[2,1],"환자번호: (\\d{8})")[1,2]


raw2<-raw_txt%>%slice(-1,-2,-3)
names(raw2)<-raw_txt%>%slice(3)%>%as.character()

rendered<-raw2%>%
  pivot_longer(c(4:length(raw2)),names_to='time',values_to='values')%>%
  na.omit()%>%
  mutate(ID=ID)%>%
  select(ID,everything())

View(rendered)
?pivot_longer
