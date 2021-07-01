library(Microsoft365R)

od <- get_personal_onedrive(auth_type="device_code") #Ubunbu
#od <- get_personal_onedrive() #Mac


od$list_items()
fpath<-'My journal/OA/ECMO_Sepsis_YHC/Data/'
od$list_items(fpath)
fname<-'ECMO_Sepsis_YHC_data.xlsx'


input_path<-'Input/'
destfile <- "ECMO_Sepsis_YHC_data.xlsx"

od$download_file(paste0(fpath,fname),paste0(input_path,destfile),overwrite=T)

