library(dplyr)
library(RMariaDB)

connect.DB<- function(){
  db.connect <- dbConnect(MariaDB(),dbname='cacaoDB',host='127.0.0.1',
                          user='root', password='root',
                          client.flag= CLIENT_MULTI_STATEMENTS)
  return(db.connect)
}

# library(readxl)
# Filter_tables_Microarray <- read_excel("Datas/02_Datasets_Microarray/00_Metadata/Filter_tables_Microarray_JSS.xlsx")
# Filter_tables_Microarray <- filter(Filter_tables_Microarray,Code_Cacao>0)
# Sample_annotation_Microarray <- read_excel("Datas/02_Datasets_Microarray/00_Metadata/Sample_annotation_JSS_SHINY.xlsx")


mysqlconnection <- connect.DB()

 # dbWriteTable(mysqlconnection, 'filter_tables_Microarray',Filter_tables_Microarray) 
 # dbWriteTable(mysqlconnection, 'sample_annotations_Microarray',Sample_annotation_Microarray) 

library(stringr)
DE_files <- dir('./Datas/02_Datasets_Microarray/ArquivosDE')

DE_files <- str_replace(DE_files,'.txt','')

Series_Matrix <- dir('./Datas/02_Datasets_Microarray/00_Metadata/Series_Matrix_files')

Series_Matrix <- str_replace(Series_Matrix,'.tsv','')





for(file in 1:24){
  De_temp <- read.delim(paste('./Datas/02_Datasets_Microarray/ArquivosDE/',DE_files[2],'.txt',sep=''),sep = '\t',comment.char = '!')
  names(De_temp) <- c('name','gene','padj','pvalue','log2FoldChange')
  De_temp[which(is.na(De_temp$log2FoldChange)),2:5] <- 0
  # dbWriteTable(mysqlconnection, DE_files[file],De_temp) 
  Series_mtx_temp  <- read.delim(paste('./Datas/02_Datasets_Microarray/00_Metadata/Series_Matrix_files/',Series_Matrix[2],'.tsv',sep=''),sep = '\t',comment.char = '!',header= T)
  unique(De_temp$gene %in% Series_mtx_temp$ID_REF )
  
  
  # dbWriteTable(mysqlconnection, Series_Matrix[file],Series_mtx_temp) 
  
  }
