library(Seurat)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gridExtra)

seurat_barplot <- function(object,ident.use,features_list,ident.colors,
         show_percentage_legend=T,
         percentage_legend_size=2,path_to_save=getwd(),width=10, height=14, ncol=1,plot_name='Bar_plot_percentage_features'){
  Idents(object) <- object[[ident.use]]
  idents <- as.data.frame(Idents(object))
  colnames(idents) <- c(ident.use)
  idents$barcode <- row.names(idents)
  count_matrix <- as.data.frame(GetAssayData(object = object, slot = "counts"))[paste(features_list,sep=''),]
  
  data <- as.data.frame(t(count_matrix))
  data$barcode <- rownames(data)
  # print(names(data))
  ident.cluster <-as.data.frame(Idents(object))
  colnames(ident.cluster) <- 'Cell.Type'
  ident.cluster$barcode <- rownames(ident.cluster)
  data.to.plot <- merge(data, ident.cluster,by='barcode')
  data.to.plot$Cell.Type <- as.factor(data.to.plot$Cell.Type)
  #print(table(data.to.plot))
  plot_list <- list()
  for(gene in features_list){
    data_new <- data.to.plot[,c(gene,'Cell.Type')]
    data_new <-  data_new %>% 
      filter(get(gene)>0) %>% 
      group_by(get('Cell.Type')) %>% 
      summarise(cnt=n()) %>% 
      mutate(Percentage=cnt/sum(cnt)*100, geneName= gene)
    
    colnames(data_new) <- c('Cell.type','count','perc','geneName')
    # print(data_new)
    dd <- ggplot(data_new, aes(y=perc, x= Cell.type ,fill =Cell.type)) +
      geom_bar(stat='identity',position = "stack")+
      coord_cartesian(ylim = c(0,105))+
      theme_cowplot()+
      theme(text = element_text(face='bold'),
            axis.title.y =  element_text(size =12,face = 'bold'),
            axis.text.x= element_text(face="bold",
                                      vjust = 1,
                                      hjust = 1,
                                      size = 12,
                                      angle = 45),
            axis.title.x=  element_blank(),
            legend.text = element_text(size = 12,face = 'plain'),
            legend.title = element_text(size =15,face ='bold'),
            # title = element_text(size = 25,face = 'bold'),
            legend.position = "none",
            plot.title = element_text(size =20,face = 'bold') )+
      scale_fill_manual(values = ident.colors)+
      labs(title = gene ,y='Percentage (%)',color="grey60")
    # dd <-  dd +coord_flip()
    if(show_percentage_legend == T){
      dd<- dd+ geom_text(aes(label=round(perc, digits = 2)), vjust=-0.3, size=percentage_legend_size)
    }
    plot_list[[gene]] = dd
  }
  pp <- print(wrap_plots(plot_list)+plot_layout(heights=1, ncol=ncol, guides = 'collect', widths = 2))
  pp <- pp + plot_layout(guides = "collect")
  ggsave(paste(path_to_save,'/',plot_name,'.png',sep=''),plot =pp,width = width, height = height)
  ggsave(paste(path_to_save,'/',plot_name,'.pdf',sep=''),plot =pp,width = width, height = height)
  ggsave(paste(path_to_save,'/',plot_name,'.tiff',sep=''),plot =pp,width = width, height = height)
  print(pp)
  print(paste('save file in : ',path_to_save,'/',plot_name,' (.pdf,.png,.tiff)',sep=''))
  return(pp)
}




make_violin_plot <- function(object,ident.use,ident.colors, features_list, path_to_save=getwd(), width=10,height=14,ncol=1,plot_name='Violin_plot'){
  Idents(object) <- object[[ident.use]]
  count_matrix <- as.data.frame(GetAssayData(object = object, slot = "counts")[features_list,])
  if(length(features_list)==1){
    names(count_matrix) <- paste(features_list,sep='')
  }
  if(nrow(count_matrix)==nrow(object@assays$RNA@counts)){
    count_matrix_t <- as.data.frame(t(count_matrix))
    rm(count_matrix)
    gc()
  }else{
    count_matrix_t <- count_matrix
    rm(count_matrix)
    gc()
  }

  count_matrix_t$barcode <- row.names(count_matrix_t)
  
  ident_meta.annotations <- data.frame(barcode=c(colnames(object)),celltype=object@meta.data[[ident.use]])
  df_markers <- merge(count_matrix_t, ident_meta.annotations,by='barcode')
  #print(names(df_markers))
  
  plot_list <- list()
  for(gene in features_list){
    filter_test <- df_markers[,c(gene,'celltype')]
    filter_test <- filter(filter_test, filter_test[,gene]!=0 )
    colnames(filter_test) <- c('Marker','celltype')
    filter_test[,'Marker'] <- log( filter_test[,'Marker'],2)
    
    dd <- ggplot(filter_test, aes(y=Marker , x=factor(celltype),fill =celltype)) +
      geom_violin()+
      theme_cowplot()+
      theme(text = element_text(face='bold'),
            axis.text = element_text(size =10,face = 'bold'),
            axis.text.x= element_text(face="bold",
                                      vjust = 1,
                                      hjust = 1, 
                                      size = 10,
                                      angle = 45),
            axis.title.x=element_blank(),
            legend.text = element_text(size = 8,face = 'plain'),
            legend.title = element_text(size =10,face ='bold'),
            # title = element_text(size = 25,face = 'bold'),
            legend.position = "none",
            plot.title = element_text(size =20,face = 'bold') )+
      scale_fill_manual(values = ident.colors)+
      labs(title = gene ,y='Expression levels log(2)',color="grey60") 
    
    plot_list[[gene]] = dd
    
  }
  
  
  pp <- wrap_plots(plot_list,ncol=ncol)+plot_layout(heights=0.5, ncol=ncol)
  
  ggsave(paste(path_to_save,'/',plot_name,'.pdf',sep=''),plot =  ,width = width, height = height)
  ggsave(paste(path_to_save,'/',plot_name,'.tiff',sep=''),plot =  ,width = width, height = height)
  ggsave(paste(path_to_save,'/',plot_name,'.png',sep=''),plot =  ,width = width, height = height)
  return(pp)
}


unique(peng$cell.type.CaCao)
cell.colors <-  c("Endothelial cell"= "#1f77b4","Stellate cell"= "#ff7f0e", "Ductal cell"= "#2ca02c","Fibroblast"= "#e7969c","Macrophage"= "#9467bd",
                             "Acinar cell"=  "#8c564b","T cell"= "#e377c2","Epithelial cell"= "#e7ba52","B cell"= "#bcbd22")
colors <- c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
            "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
            "#393b79", "#637939", "#8c6d31", "#d6616b", "#7b4173",
            "#843c39", "#0392cf", "#7fc97f", "#fccde5", "#ffffb3",
            "#c6dbef", "#fdae6b", "#e7ba52", "#ce6dbd", "#bd9e39",
            "#6b6ecf", "#9c9ede", "#cedb9c", "#e7cb94", "#f4a582",
            "#b5cf6b", "#8ca252", "#bd9e39", "#e7ba52", "#e7969c",
            "#de9ed6", "#9c9ede", "#cedb9c", "#e7cb94", "#fdd0a2",
            "#ef8a62", "#f7f7f7", "#bcbddc", "#c7c7c7", "#d9d9d9",
            "#969696", "#bdbdbd", "#525252", "#737373", "#252525")




dim <- DimPlot(peng, reduction = "umap",cols = cell.colors,label = TRUE, pt.size = 0.5,label.box = T, raster.dpi = c(512, 512),repel = T)

feature <- FeaturePlot(peng, features = 'CXCL2')

bar <- seurat_barplot(object = peng, 
               features_list = 'CXCL2', #genes
               ident.use = 'cell.type.CaCao',
               show_percentage_legend =  T,
               percentage_legend_size= 3.6,
               path_to_save = getwd(),
               ncol = 1,
               width = 5,
               ident.colors = cell.colors,
               height = 7,
               plot_name = 'my_barplot_test_Cacao.celltypes')

vio <- make_violin_plot(peng,ident.use = 'cell.type.CaCao',ident.colors=cell.colors, features_list = "CXCL2",width = 8, height = 10, plot_name = "Violin_markers_cell_types_to_papper")


 
  
png('temp_fig.png', height=10,width=10,dpi=300)
(feature | dim) / (bar | vio )
dev.off()



dir()
library(base64enc)

enc <- base64encode("temp_fig.png")
conn <- file("w.bin","wb")
writeBin(enc, conn)
close(conn)




# library(RMariaDB)

# connect.DB<- function(){
#   db.connect <- dbConnect(MariaDB(),dbname='singleCellFig',host='127.0.0.1',
#                           user='root', password='root',
#                           client.flag= CLIENT_MULTI_STATEMENTS)
  
#   return(db.connect)
# }

# con <- connect.DB()
# query <- paste("INSERT INTO Figures (gene, figure, path) VALUES('CXCL2' ,'",enc,"','",enc,"')", sep="")

# res <- dbSendQuery(con, query) 
# data<-dbFetch(res,n=2) 
# data 
# dbHasCompleted(res) 
# dbClearResult(res) 
# dbDisconnect(con)

# start_time <- Sys.time()
# con <- connect.DB()
# deg_query <- dbSendQuery(con, "SELECT path FROM singleCellFig.Figures where gene='CXCL2';") 
# chunk <- dbFetch(deg_query)

# write.table(chunk$path,'bin.txt',row.names = F,col.names = F)
# dbHasCompleted(deg_query) 
# dbClearResult(deg_query) 
# dbDisconnect(con)
# end_time <- Sys.time()
# end_time - start_time
