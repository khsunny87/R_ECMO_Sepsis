library(ztable)


myt_output<-'viewer' #default

print_mytable<-function(myt){

if(myt_output=='viewer'){
  options(ztable.type='viewer')
  myt%>%ztable()%>%print()

} else if(myt_output=='HTML'){
  options(ztable.type='HTML')
  myt%>%ztable()%>%print()
} else if(myt_output=='PPT'){
  myt%>%mytable2df()%>%knitr::kable(format='pipe',row.names=F)%>%print()
}

}

