library(RMariaDB)
library(stringr)
library(dplyr)
library(ggplot2)
connect.DB<- function(){
  db.connect <- dbConnect(MariaDB(),dbname='cacaoDB',host='127.0.0.1',
                          user='root', password='root',
                          client.flag= CLIENT_MULTI_STATEMENTS)
  
  return(db.connect)
}


mysqlconnection <- connect.DB()



deg_query <- dbSendQuery(mysqlconnection, 'SELECT * FROM R01_DE_MUSCLE_200123310_Zimmers_2021_MusMusculus;') 
chunk.2 <- dbFetch(deg_query)
#select_gene_DE<-as.data.frame(chunk.1)
Ddeg_data <- as.data.frame(chunk.2)

Ddeg_data$log2FoldChange <- as.numeric(Ddeg_data$log2FoldChange)
Ddeg_data <- Ddeg_data%>%
  mutate(DEG = case_when(log2FoldChange > 1 & pvalue < 0.05 ~ 'Up', 
                                             log2FoldChange < -1 & pvalue < 0.05 ~ 'Down',
                                              pvalue > 0.05 ~ "NO"))


deg_colors <- c('Up' ='#ef233c','Down'='#023e8a','NO'='black')
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

p1<-ggplot(data=Ddeg_data, aes(x=log2FoldChange, y=-log10(pvalue),col=DEG)) +
  geom_point() + 
  theme_classic()+ 
  ggtitle("")+
 # geom_text_repel( max.overlaps  =20, size = 1)+
  scale_color_manual(values=deg_colors) +
  geom_vline(xintercept=c(-1, 1), col="#d3d3d3df") +
  geom_hline(yintercept=-log10(0.05), col="#c3c3c3df")





count_query <- dbSendQuery(mysqlconnection, 'SELECT * FROM R01_RLOG_MUSCLE_200123310_Zimmers_2021_MusMusculus;') 
chunk.3 <- dbFetch(count_query)
#select_gene_DE<-as.data.frame(chunk.1)
count_data <- as.matrix(chunk.3)

row.names(count_data) <- as.data.frame(count_data)$name
degs <- filter(Ddeg_data, DEG != "NO")$name
count_data <- filter(as.data.frame(count_data), name %in% degs)
count_data <- as.data.frame(count_data) %>% select(-c('gene','name'))

for(col in names(count_data)){
  print(col)
  count_data[,col] <- as.double(count_data[,col])
}

count_data <- as.matrix(count_data)
patientcolors <- c('Control_Sham_muscle_1'="#007474","Control_Sham_muscle_2"="#007474","Control_Sham_muscle_3"="#007474",
                   "Cachexia_KPC_muscle_1"="#9E002F","Cachexia_KPC_muscle_2"="#9E002F","Cachexia_KPC_muscle_3"="#9E002F","Cachexia_KPC_muscle_4"="#9E002F")
cols <- c('Control_Sham_muscle_1'="Control.1","Control_Sham_muscle_2"="Control.2","Control_Sham_muscle_3"="Control.3",
          "Cachexia_KPC_muscle_1"="Cachexia.1","Cachexia_KPC_muscle_2"="Cachexia.2","Cachexia_KPC_muscle_3"="Cachexia.3","Cachexia_KPC_muscle_4"="Cachexia.4")




library("tidyr")
df.count<-as.data.frame(count_data)
names(df.count)<-c('Control_Sham_muscle_1'="Control.1","Control_Sham_muscle_2"="Control.2","Control_Sham_muscle_3"="Control.3",
                   "Cachexia_KPC_muscle_1"="Cachexia.1","Cachexia_KPC_muscle_2"="Cachexia.2","Cachexia_KPC_muscle_3"="Cachexia.3","Cachexia_KPC_muscle_4"="Cachexia.4")





library(ggpubr)

table <- as.data.frame(t(as.data.frame(t(c('Control_Sham_muscle_1'="Control.1","Control_Sham_muscle_2"="Control.2","Control_Sham_muscle_3"="Control.3",
                                           "Cachexia_KPC_muscle_1"="Cachexia.1","Cachexia_KPC_muscle_2"="Cachexia.2","Cachexia_KPC_muscle_3"="Cachexia.3","Cachexia_KPC_muscle_4"="Cachexia.4")))))
names(table )<-'Samples'
tab <- ggtexttable(table, theme = ttheme("blank"))
p3 <- tab %>% 
  tab_add_hline(at.row = c(1, 2), row.side = "top", linewidth = 3, linetype = 1) %>%
  # tab_add_title(  text='R01_RLOG_MUSCLE_200123310_Zimmers_2021_MusMusculus',face = 'bold',size = 7.5, padding = unit(0.5, "line")) %>% 
  # tab_add_hline(at.row = c(7), row.side = "bottom", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = 2:tab_ncol(tab), column.side = "left", from.row = 2, linetype = 2)

pca <- prcomp(t(count_data), scale=TRUE)

pca_data <- data.frame(sample=rownames(pca$x),#1 column with sample ids
                       
                       x=pca$x[,1],#2 columns for the X and Y 
                       
                       y=pca$x[,2])#coordinates for each sample


  #displaying the data

pca_data
pca.var <- pca$sdev^2

pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

pca_data$sample[grep('Control',pca_data$sample)] <- 'Control'
pca_data$sample[-grep('Control',pca_data$sample)] <- 'Cachexia'
sample_colors <- c('Control'='#007474','Cachexia'='#9E002F')
p4<- ggplot(data=pca_data,aes(x=x,y=y,col=sample,fill=sample,label=row.names(pca_data))) + 
  geom_point(size=1.5) +
  ggtitle("") +
  geom_label_repel(box.padding = 1,label.size = 0.05,colour="white",segment.colour="black")+
  theme_classic()+
  scale_fill_manual(values = sample_colors)+
  xlab(paste("PC1: ",pca.var.per[1],"%",sep="")) +
  ylab(paste("PC2: ",pca.var.per[2],"%",sep=""))



library(patchwork)


pp<- p4+p3
library("gridExtra")
library(cowplot)
ggdraw() +
  draw_plot(p1, 0, .5, 1, .5) +
  draw_plot(p4, 0, 0, .5, .5) +
  draw_plot(p3, .5, 0, .5, .5) +
  draw_plot_label(c("Volcano", "PCA","SAMPLES"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15)


patientcolors <- c('Control_Sham_muscle_1'="#007474","Control_Sham_muscle_2"="#007474","Control_Sham_muscle_3"="#007474",
                   "Cachexia_KPC_muscle_1"="#9E002F","Cachexia_KPC_muscle_2"="#9E002F","Cachexia_KPC_muscle_3"="#9E002F","Cachexia_KPC_muscle_4"="#9E002F")
cols <- c('Control_Sham_muscle_1'="Control.1","Control_Sham_muscle_2"="Control.2","Control_Sham_muscle_3"="Control.3",
          "Cachexia_KPC_muscle_1"="Cachexia.1","Cachexia_KPC_muscle_2"="Cachexia.2","Cachexia_KPC_muscle_3"="Cachexia.3","Cachexia_KPC_muscle_4"="Cachexia.4")

heatmap(count_data,
                 ColSideColors =  patientcolors,
                 col=rev(c('#e56b6f', '#b56576','#6d597a','#355070')),
                 labCol = cols,
                 margins = c(10, 2),
                 main = 'HeatMap')




  


# library(ggplot2)
# p <- get_Rlog_filtered_datas(gene_select = 'Depp1')
# p2 <- get_DE_filtered_datas(gene_select = 'Depp1')
# 
# library(patchwork)
# pp_list <-list()
# pp_list[[1]]<- p
# pp_list[[2]]<- p2[[1]]
# pp_list[[3]]<- p2[[2]]
# pp_list[[4]]<- p2[[3]]
# pp <- wrap_plots(pp_list,ncol=4)+plot_layout(widths =c(2.8,1.2,0.25,0.25), heights = 1, ncol=4)
# pp
# 
# 
# get_DE_filtered_datas <- function(De_table_names=c(), gene_select=''){
#   if(gene_select!=''){
#     mysqlconnection <- connect.DB()
#     filters <- dbSendQuery(mysqlconnection, 'SELECT * FROM cacaoDB.filter_tables;') 
#     chunk.2 <- dbFetch(filters)
#     #select_gene_DE<-as.data.frame(chunk.1)
#     filters <- as.data.frame(chunk.2)
#     
#     library(dplyr)
#     temp <- data.frame()
#     for(i in 1:length(filters$DE_Table_Name)){
#       
#       temp<- rbind(temp,as.data.frame(rep(filters$DE_Table_Name[i],8)))
#       
#     }
#     names(temp)<- 'file'
#     
#     temp <- filter(temp, !grepl("HomoSapiens",temp$file))
#     query <- paste("call cacaoDB.selectDEgeneMmusculusRNAseq('",gene_select,"');", sep='')
#     
#     res <- dbSendQuery(mysqlconnection, query)
#     
#     while(!dbHasCompleted(res)){
#       chunk.1 <- dbFetch(res)
#       #select_gene_DE<-as.data.frame(chunk.1)
#       deg_data_mysql <- as.data.frame(t(chunk.1))
#     }
#     
#     deg_data_mysql <- cbind(deg_data_mysql, temp)
#     deg_data_mysql <-dplyr::filter(deg_data_mysql, !grepl("name",row.names(deg_data_mysql)))
#     deg_data_mysql <-dplyr::filter(deg_data_mysql, !grepl("gene",row.names(deg_data_mysql)))
#     deg_data_mysql$calc <-  gsub("\\..*","",row.names(deg_data_mysql) )
#     box_data_LogFC <- filter(deg_data_mysql,calc =='log2FoldChange')
#     
#     names(box_data_LogFC)[1] <- 'log2FC'
#     
#     box_data_LogFC$log2FC <- as.numeric(box_data_LogFC$log2FC)
#     box_data_LogFC <- box_data_LogFC%>%
#       mutate(Differential_expression = case_when(log2FC >0 ~ 'Up', log2FC <0 ~ 'Down'))
#     box_data_LogFC$Differential_expression <- as.factor(box_data_LogFC$Differential_expression)
#     deg_colors <- c('Up' ='#ef233c','Down'='#023e8a')
#     p <- ggplot(box_data_LogFC, aes(x = file, y = log2FC,fill=Differential_expression)) +
#       geom_bar(stat = "identity") +
#       geom_text(aes(label = round(log2FC, 2),
#                     hjust = ifelse(log2FC < 0, 1.5, -1),
#                     vjust = 0.5),
#                 size = 3) +
#       theme(axis.text.y = element_blank(),
#             legend.position="bottom",
#             legend.title = element_text(size=8,face='bold'),
#             legend.text = element_text(size=8),
#             panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a2',linetype = "dotted"),
#             panel.background = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.title.x = element_text(size = 10,face='bold'))+
#       scale_fill_manual(values = deg_colors)+
#       xlab("") +
#       ylab("log2FC") +
#       guides(fill=guide_legend(title="DEG's"))+
#       labs(colour='Differential_expression')+
#       coord_flip()+ 
#       geom_hline(yintercept = c(-1,1), col = "#cdb4db",show.legend = T)+
#       scale_y_continuous(breaks= seq(round(min(box_data_LogFC$log2FC) - 0.5), 
#                                      round(max(box_data_LogFC$log2FC) + 0.5), by = 1),
#                          limits = c(min(box_data_LogFC$log2FC) - 0.5,
#                                     max(box_data_LogFC$log2FC) + 0.5)) 
#     box_data_adjp <- filter(deg_data_mysql,calc =='padj')
#     
#     names(box_data_adjp)[1] <- 'P.adj'
#     
#     box_data_adjp$P.adj <- as.numeric(box_data_adjp$P.adj)
#     box_data_adjp$value <-0
#     #-----------------------
#     
#     p1 <- ggplot(box_data_adjp, aes(x = value, y = file))+
#       geom_text(
#         fill='#edede9',
#         size=3,
#         label.size = 0.025,
#         label.padding = unit(0.25, "lines"),
#         label= round(box_data_adjp$P.adj,4)
#       )+
#       theme(axis.text.y = element_blank(),
#             axis.text.x = element_blank(),
#             legend.position="none",
#             panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
#             panel.background = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.title.x = element_text(size = 10,face='bold'))+  
#       xlab("adj P.value") +
#       ylab("") + xlim(-0.0025,0.0025)
#     
#     
#     
#     box_data_pval <- filter(deg_data_mysql,calc =='pvalue')
#     
#     names(box_data_pval)[1] <- 'P.value'
#     
#     box_data_pval$P.value <- as.numeric(box_data_pval$P.value)
#     box_data_pval$value <-0
#     #-----------------------
#     
#     p2 <- ggplot(box_data_pval, aes(x = value, y = file))+
#       geom_text(
#         fill='#edede9',
#         size=3,
#         label.size = 0.025,
#         label.padding = unit(0.25, "lines"),
#         label= round(box_data_pval$P.value,4)
#       )+
#       theme(axis.text.y = element_blank(),
#             axis.text.x = element_blank(),
#             legend.position="none",
#             panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
#             panel.background = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.title.x = element_text(size = 10,face='bold'))+  
#       xlab("P.value") +
#       ylab("") + xlim(-0.0025,0.0025)
#     
#     
#     plot_list<- list()
#     plot_list[[1]] <-p
#     plot_list[[2]] <-p1
#     plot_list[[3]] <-p2
#   }
#   return(plot_list)
# }
# 
# get_Rlog_filtered_datas <- function(gene_select=''){
#   mysqlconnection <- connect.DB()
#   query <- paste("call cacaoDB.selectGeneExpressionMmusculusRNAseq('",gene_select,"');",sep='')
#   print(query)
#   res <- dbSendQuery(mysqlconnection, query)
#   
#   while(!dbHasCompleted(res)){
#     chunk <- dbFetch(res)
#     #select_gene_rlog[[select]]<-as.data.frame(chunk)
#   }
#   test_df <- as.data.frame(t(as.data.frame(chunk)))
#   test_df <-dplyr::filter(test_df, !grepl("name",row.names(test_df)))
#   test_df <-dplyr::filter(test_df, !grepl("gene",row.names(test_df)))
#   #test_df$samples <- row.names(test_df)
#   test_df$gene <- gene_select
#   
#   res.anno <- dbSendQuery(mysqlconnection, 'SELECT * FROM cacaoDB.Sample_annotations;')
#   
#   while(!dbHasCompleted(res.anno)){
#     chunk.anno <- dbFetch(res.anno)
#     sample_anno<-as.data.frame(chunk.anno)
#     
#   }
#   dbClearResult(res)
#   dbDisconnect(mysqlconnection)
#   names(test_df)[1] <- 'expression'
#   #test_df$samples <- gsub("\\..*","",test_df$samples)
#   test_df <- cbind(test_df,sample_anno)
#   
#   test_df$condition <- as.factor(test_df$condition)
#   test_df$code <- as.factor(test_df$code)
#   test_df$expression <-as.numeric(test_df$expression)
#   conditions_colors <- c('Control'='#80ed99','Cachexia'='#fb5607')
#   p<-ggplot(test_df, aes(y=file, x=expression,fill=condition, color=condition))+
#     geom_boxplot(alpha=75,outlier.size = 0.5) +
#     theme(legend.position="bottom",
#           legend.title = element_text(size=8,face='bold'),
#           legend.text = element_text(size=8),
#           axis.text.y = element_text(size = 10,face = 'bold'),
#           panel.background = element_blank(),
#           panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a2df',linetype = "dotted"),
#           axis.title.x = element_text(size = 10,face='bold')
#           # panel.grid.minor.x = element_line(size = 0.75,color='#454545',linetype = "dotted")
#     ) +
#     scale_fill_manual(values = conditions_colors)+
#     scale_color_manual(values = conditions_colors)+
#     xlab("Expression levels") +
#     ylab("Study") +
#     labs(title=unique(test_df$gene), colour='condition')
#   return(p)
# }
# 
# generate_plots <- function(expression_datas= data.frame(),deg_data_mysql=data.frame()){
#   
#   conditions_colors <- c('Control'='#80ed99','Cachexia'='#fb5607')
#   p<-ggplot(expression_datas, aes(y=file, x=expression,fill=condition, color=condition))+
#     geom_boxplot(alpha=75,outlier.size = 0.5) +
#     theme(legend.position="bottom",
#           legend.title = element_text(size=8,face='bold'),
#           legend.text = element_text(size=8),
#           axis.text.y = element_text(size = 6.8,face = 'bold'),
#           panel.background = element_blank(),
#           panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
#           axis.title.x = element_text(size = 6,face='bold')
#           # panel.grid.minor.x = element_line(size = 0.75,color='#454545',linetype = "dotted")
#     ) +
#     scale_fill_manual(values = conditions_colors)+
#     scale_color_manual(values = conditions_colors)+
#     xlab("Expression levels") +
#     ylab("Experiment") +
#     labs(title=unique(expression_datas$gene), colour='condition')
#   
#   
#   
#   box_data_LogFC <- filter(deg_data_mysql,calc =='log2FoldChange')
#   
#   names(box_data_LogFC)[1] <- 'log2FC'
#   
#   box_data_LogFC$log2FC <- as.numeric(box_data_LogFC$log2FC)
#   box_data_LogFC <- box_data_LogFC%>%
#     mutate(Differential_expression = case_when(log2FC >0 ~ 'Up', log2FC <0 ~ 'Down'))
#   box_data_LogFC$Differential_expression <- as.factor(box_data_LogFC$Differential_expression)
#   
#   
#   deg_colors <- c('Up' ='#ef233c','Down'='#023e8a')
#   p2 <- ggplot(box_data_LogFC, aes(x = file, y = log2FC,fill=Differential_expression)) +
#     geom_bar(stat = "identity") +
#     geom_text(aes(label = round(log2FC, 2),
#                   hjust = ifelse(log2FC < 0, 1.5, -1),
#                   vjust = 0.5),
#               size = 3) +
#     theme(axis.text.y = element_blank(),
#           legend.position="bottom",
#           legend.title = element_text(size=8,face='bold'),
#           legend.text = element_text(size=8),
#           panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
#           panel.background = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.ticks.y = element_blank(),
#           axis.title.x = element_text(size = 6,face='bold'))+
#     scale_fill_manual(values = deg_colors)+
#     xlab("") +
#     ylab("log2FC") +
#     labs(colour='Differential_expression')+
#     coord_flip()+ 
#     geom_hline(yintercept = c(-1,1), col = "#cdb4db60",show.legend = T)+
#     scale_y_continuous(breaks= seq(round(min(box_data_LogFC$log2FC) - 0.5), 
#                                    round(max(box_data_LogFC$log2FC) + 0.5), by = 1),
#                        limits = c(min(box_data_LogFC$log2FC) - 0.5,
#                                   max(box_data_LogFC$log2FC) + 0.5))
#   p2
#   
#   
#   
#   
#   box_data_adjp <- filter(deg_data_mysql,calc =='padj')
#   
#   names(box_data_adjp)[1] <- 'P.adj'
#   
#   box_data_adjp$P.adj <- as.numeric(box_data_adjp$P.adj)
#   box_data_adjp$value <-0
#   #-----------------------
#   
#   p3 <- ggplot(box_data_adjp, aes(x = value, y = file))+
#     geom_text(
#       fill='#edede9',
#       size=3,
#       label.size = 0.025,
#       label.padding = unit(0.25, "lines"),
#       label= round(box_data_adjp$P.adj,4)
#     )+
#     theme(axis.text.y = element_blank(),
#           axis.text.x = element_blank(),
#           legend.position="none",
#           panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
#           panel.background = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.ticks.y = element_blank(),
#           axis.title.x = element_text(size = 6,face='bold'))+  
#     xlab("adj P.value") +
#     ylab("") + xlim(-0.0025,0.0025)
#   
#   
#   
#   box_data_pval <- filter(deg_data_mysql,calc =='pvalue')
#   
#   names(box_data_pval)[1] <- 'P.value'
#   
#   box_data_pval$P.value <- as.numeric(box_data_pval$P.value)
#   box_data_pval$value <-0
#   #-----------------------
#   
#   p4 <- ggplot(box_data_pval, aes(x = value, y = file))+
#     geom_text(
#       fill='#edede9',
#       size=3,
#       label.size = 0.025,
#       label.padding = unit(0.25, "lines"),
#       label= round(box_data_pval$P.value,4)
#     )+
#     theme(axis.text.y = element_blank(),
#           axis.text.x = element_blank(),
#           legend.position="none",
#           panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
#           panel.background = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.ticks.y = element_blank(),
#           axis.title.x = element_text(size = 6,face='bold'))+  
#     xlab("P.value") +
#     ylab("") + xlim(-0.0025,0.0025)
#   
#   
#   plot_list<- list()
#   plot_list[[1]] <-p
#   plot_list[[2]] <-p2
#   plot_list[[3]] <-p3
#   plot_list[[4]] <-p4
#   
#   library(patchwork)
#   pp <- wrap_plots(plot_list,ncol=4)+plot_layout(widths =c(2.8,1.2,0.25,0.25), heights = 1, ncol=4)
#   return(pp)
#   
# }
# 
# get_filter_tables <- function(){
#   
#   res <- dbSendQuery(mysqlconnection, paste("SELECT * FROM cacaoDB.`filter_tables` ;",sep=''))
#   
#   while(!dbHasCompleted(res)){
#     chunk <- dbFetch(res)
#     table_to_filter <- as.data.frame(chunk)
#   }
#   dbClearResult(res)
#   dbDisconnect(mysqlconnection)
#   
#   return(table_to_filter)
# }
# 
# filter_table <- function(df_tables_info,
#                          year_list=c(),
#                          author_list=c(),
#                          tumor_list=c(),
#                          tissue_list=c(),
#                          tissue_names_list=c(),
#                          time_days_list=c(),
#                          pmid_code_list=c(),
#                          gse_code_list=c(),
#                          gender_list=c(),
#                          organism=''){
#   final_df <- data.frame()
#   if(length(year_list)>0 || is.null(year_list)==F ){
#     df_filter_year <- filter(df_tables_info,  Year %in% year_list)
#     final_df <- rbind(final_df,df_filter_year)
#   }
#   
#   if(length(author_list)>0  || is.null(author_list)==F ){
#     df_filter_author <- filter(df_tables_info,  Autor %in% author_list)
#     final_df <- rbind(final_df,df_filter_author)
#   }
#   
#   if(length(gse_code_list)>0   || is.null(gse_code_list)==F){
#     df_filter_gse <- filter(df_tables_info,  Code_GSE %in% gse_code_list)
#     final_df <- rbind(final_df,df_filter_gse)
#   }
#   
#   if(length(gender_list)>0 || is.null(gender_list)==F){
#     df_filter_gender <- filter(df_tables_info,  Sex %in% gender_list)
#     final_df <- rbind(final_df,df_filter_gender)
#   }
#   
#   if(length(time_days_list)>0 || is.null(time_days_list)==F){
#     df_filter_time_days <- filter(df_tables_info,  Time_days %in% time_days_list)
#     final_df <- rbind(final_df,df_filter_time_days)
#   }
#   
#   if(length(tumor_list)>0 || is.null(tumor_list)==F){
#     df_filter_tumor <- filter(df_tables_info,  Tumor %in% tumor_list)
#     final_df <- rbind(final_df,df_filter_tumor)
#   }
#   
#   if(length(tissue_list)>0 || is.null(tissue_list)==F){
#     df_filter_tissue <- filter(df_tables_info,  Tissue %in% tissue_list)
#     final_df <- rbind(final_df,df_filter_tissue)
#   }
#   
#   if(length(tissue_names_list)>0 || is.null(tissue_names_list)==F){
#     df_filter_tissue_name <- filter(df_tables_info,  Year %in% tissue_names_list)
#     final_df <- rbind(final_df,df_filter_tissue_name)
#   }
#   
#   if(length(pmid_code_list)>0 || is.null(pmid_code_list)==F){
#     df_filter_pmid <- filter(df_tables_info,  PMID %in% pmid_code_list)
#     final_df <- rbind(final_df,df_filter_pmid)
#   }
#   
#   df<- data.frame(rlog_tables= unique(final_df$RLOG_Table_Name),
#                   deg_tables=unique(final_df$DE_Table_Name))
#   df$rlog_tables<- str_replace_all(df$rlog_tables,' ','')
#   df$deg_tables<- str_replace_all(df$deg_tables,' ','')
#   return(df)
#   
# }


