# All codes for RNA-seq datas


render_filter_ui_microarray <- function(input,output,session ){
  body_filter <- box(
    width = 12,
    #background = "gray", #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black, gray
    fluidRow(
      width= 12,
      column(4,align="left",
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_YEAR_slider_mca",
                                  label = tags$b("YEAR"), 
                                  status = "success",
                                  fill = TRUE
                    )
             ),
             prettyCheckboxGroup(
               selected = choice_years_microarray,
               inputId = "ipt_YEAR_mca",
               label = "",
               choices = choice_years_microarray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TISSUE_NAME_slider_mca",
                                  label = tags$b("TISSUE NAME"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_TISSUE_NAME_mca",
               label = "",
               selected = choice_tissue_names_microarray,
               choices = choice_tissue_names_microarray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TIME_slider_mca",
                                  label = tags$b("TIME (DAYS)"), 
                                  status = "success",
                                  fill = TRUE
                    )
             ),
             prettyCheckboxGroup(
               inputId = "ipt_TIME_mca",
               selected = choice_time_days_microarray,
               label = "",
               choices = choice_time_days_microarray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr()
      ),
      
      
      
      column(4,align="left",
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_GENDER_slider_mca",
                                  label = tags$b("GENDER"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_GENDER_mca",
               label = "",
               selected = choice_genders_mca,
               choices = choice_genders_mca,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_GSE_slider_mca",
                                  label = tags$b("GSE"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_GSE_mca",
               label = "",
               selected = choice_gse_codes_microArray,
               choices = choice_gse_codes_microArray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_AUTHORS_slider_mca",
                                  label = tags$b("AUTHORS"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_AUTHORS_mca",
               label = "",
               selected = choice_authors_microarray,
               choices = choice_authors_microarray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr()
      ),
      column(4,align="left",
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TISSUE_slider_mca",
                                  label = tags$b("TISSUE"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_TISSUE_mca",
               label = "",
               selected = choice_tissues_microarray,
               choices = choice_tissues_microarray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TUMOR_slider_mca",
                                  label = tags$b("TUMOR"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_TUMOR_mca",
               label = "",
               selected=choice_tumors_microarray,
               choices = choice_tumors_microarray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_PMID_slider_mca",
                                  label = tags$b("PMID"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_PMID_mca",
               label = "",
               selected = choice_pmid_codes_microArray,
               choices = choice_pmid_codes_microArray,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr()
      )
    )
  )
  return(body_filter)
}

filter_year_microarray <- function(input,output,session){
  if(input$ipt_YEAR_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_YEAR_mca",
      label = "",
      selected=NULL,
      choices = choice_years_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"),
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_YEAR_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_YEAR_mca",
      label = "",
      selected=choice_years_microarray,
      choices = choice_years_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"),
                           'animation' = "jelly")
    )
  }
}

filter_tumor_microarray <- function(input,output,session){
  if(input$ipt_TUMOR_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TUMOR_mca",
      label = "",
      selected=NULL,
      choices = choice_tumors_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TUMOR_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TUMOR_mca",
      label = "",
      selected=choice_tumors_microarray,
      choices = choice_tumors_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_tissue_name_microarray <- function(input,output,session){
  if(input$ipt_TISSUE_NAME_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TISSUE_NAME_mca",
      label = "",
      selected=NULL,
      choices = choice_tissue_names_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TISSUE_NAME_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TISSUE_NAME_mca",
      label = "",
      selected=choice_tissue_names_microarray,
      choices = choice_tissue_names_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_PMID_microarray <- function(input,output,session){
  if(input$ipt_PMID_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_PMID_mca",
      label = "",
      selected=NULL,
      choices = choice_pmid_codes_microArray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_PMID_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_PMID_mca",
      label = "",
      selected=choice_pmid_codes_microArray,
      choices = choice_pmid_codes_microArray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_time_days_microarray <- function(input,output,session){
  if(input$ipt_TIME_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TIME_mca",
      label = "",
      selected=NULL,
      choices = choice_time_days_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TIME_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TIME_mca",
      label = "",
      selected=choice_time_days_microarray,
      choices = choice_time_days_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_gender_microarray <- function(input,output,session){
  if(input$ipt_GENDER_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_GENDER_mca",
      label = "",
      selected=NULL,
      choices = choice_genders,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_GENDER_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_GENDER_mca",
      label = "",
      selected=choice_genders,
      choices = choice_genders,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_GSE_microarray <- function(input,output,session){
  if(input$ipt_GSE_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_GSE_mca",
      label = "",
      selected=NULL,
      choices = choice_gse_codes_microArray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_GSE_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_GSE_mca",
      label = "",
      selected=choice_gse_codes_microArray,
      choices = choice_gse_codes_microArray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_authors_microarray <- function(input,output,session){
  if(input$ipt_AUTHORS_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_AUTHORS_mca",
      label = "",
      selected=NULL,
      choices = choice_authors_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_AUTHORS_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_AUTHORS_mca",
      label = "",
      selected=choice_authors_microarray,
      choices = choice_authors_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_tissue_microarray <- function(input,output,session){
  if(input$ipt_TISSUE_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TISSUE_mca",
      label = "",
      selected=NULL,
      choices = choice_tissues_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TISSUE_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TISSUE_mca",
      label = "",
      selected=choice_tissues_microarray,
      choices = choice_tissues_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_model_microarray <- function(input,output,session){
  if(input$ipt_TISSUE_slider_mca==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_MODEL_mca",
      label = "",
      selected=NULL,
      choices = choice_model_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TISSUE_slider_mca==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_MODEL_mca",
      label = "",
      selected=choice_model_microarray,
      choices = choice_model_microarray,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}


generate_plots_Microarray <- function(organism,filter_tables,gene_select=''){

  filter_tables <- Sample_annotation_mca_SHINY[which(Sample_annotation_mca_SHINY$cacaoStudyID %in% filter_tables$cacaoStudyID),]
  if(gene_select!='' | !is.null(gene_select)){
    
    gene_select <- toupper(gene_select)
      library(ggplot2)
      
      filtered_deg_combined_df <- do.call(cbind, lapply(Deg_data_list_Microarray, function(df) {
        filtered_df <- df[df$Gene.symbol == gene_select, ]
        filtered_df <- na.omit(filtered_df)
        filtered_df <- filtered_df[,-which(names(filtered_df) %in% c('Gene.symbol',2))]
        
        filtered_df <- as.data.frame(t(filtered_df))
      }))
      
      
      names(filtered_deg_combined_df) <- filtered_deg_combined_df['file_deg',]
      
      filtered_deg_combined_df <- as.data.frame(t(filtered_deg_combined_df))
      
      
     
      
      filtered_deg_combined_df <- merge(filtered_deg_combined_df, Sample_annotation_mca_SHINY,by='file_deg', all.x = T)
      filtered_deg_combined_df <- na.omit(filtered_deg_combined_df)
      filtered_deg_combined_df <- filtered_deg_combined_df[!duplicated(filtered_deg_combined_df$file_deg),]
      log_Fc_col <- which(str_detect(names(filtered_deg_combined_df),'logFC_'))
      filtered_deg_combined_df[,log_Fc_col] <- as.numeric(filtered_deg_combined_df[,log_Fc_col])
      
      pval_Fc_col <- which(str_detect(names(filtered_deg_combined_df),'P.Value'))
      padjval_Fc_col <- which(str_detect(names(filtered_deg_combined_df),'adj.P'))
       
      names(filtered_deg_combined_df)[log_Fc_col] <- 'log2FoldChange'
      names(filtered_deg_combined_df)[pval_Fc_col] <- 'pvalue'
      names(filtered_deg_combined_df)[padjval_Fc_col] <- 'padj'
      
      
      
      filtered_deg_combined_df <- filtered_deg_combined_df%>%
        mutate(Differential_expression = case_when(log2FoldChange >0 ~ 'Up', log2FoldChange <0 ~ 'Down'))
      filtered_deg_combined_df$Differential_expression <- as.factor(filtered_deg_combined_df$Differential_expression)
      filtered_deg_combined_df <- filter(filtered_deg_combined_df, cacaoStudyID %in% filter_tables$cacaoStudyID)
      
      
      
      deg_colors <- c('Up' ='#ef233c','Down'='#023e8a')
      pp <- ggplot(filtered_deg_combined_df, aes(x = cacaoStudyID, y = log2FoldChange,fill=Differential_expression)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(log2FoldChange, 2),
                      hjust = ifelse(log2FoldChange < 0, 1.5, -1),
                      vjust = 0.5),
                  size = 3) +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10,face = 'bold'),
          legend.position="bottom",
          legend.title = element_text(size=10,face='bold'),
          legend.text = element_text(size=10),
          panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a2',linetype = "dotted"),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 10,face='bold'))+
        scale_fill_manual(values = deg_colors)+
        xlab("") +
        ylab("log2FoldChange") +
        guides(fill=guide_legend(title="DEG's"))+
        labs(colour='Differential_expression')+
        coord_flip()+ 
        geom_hline(yintercept = c(-1,1), col = "#cdb4db",show.legend = T)+
        scale_y_continuous(breaks= seq(round(min(filtered_deg_combined_df$log2FoldChange) - 0.5), 
                                       round(max(filtered_deg_combined_df$log2FoldChange) + 0.5), by = 1),
                           limits = c(min(filtered_deg_combined_df$log2FoldChange) - 0.5,
                                      max(filtered_deg_combined_df$log2FoldChange) + 0.5)) 
      
      
      
      
      filtered_combined_df <- do.call(rbind, lapply(count_data_list_Microarray, function(df) {
        filtered_df <- df[which(df$Gene.symbol == gene_select),which(names(df) %in% c('Gene.symbol','ID',filter_tables$samples)) ]
        
        if(nrow(filtered_df) >0 ){
          filtered_df <- na.omit(filtered_df)
          filtered_df <- filtered_df[,-which(names(filtered_df) %in% c('Gene.symbol','ID'))]
          rownames(filtered_df) <- gene_select
          filtered_df <- as.data.frame(t(filtered_df))}
      }))
      
      
      names(filtered_combined_df) <- 'expression'
      

     
      
      filtered_combined_df$sample <- unlist(lapply(rownames(filtered_combined_df), function(x){str_split(x,'tsv.')[[1]][2] }))
      filtered_combined_df$file_matrix <- unlist(lapply(rownames(filtered_combined_df), function(x){paste(unlist(str_split(x,'tsv.')[[1]][1]),'tsv',sep='') }))
      
      filtered_combined_df$cacaoID = ''
      
      for (row in 1:nrow(filtered_combined_df)) {
        rsample= filtered_combined_df[row,'sample']
        rfile= filtered_combined_df[row,'file_matrix']
        filtered_combined_df$cacaoID[row]<-  filter(Sample_annotation_mca_SHINY, samples==rsample & file_matrix ==rfile)$cacaoID
      }
      
      
      filtered_combined_df <- merge(filtered_combined_df, Sample_annotation_mca_SHINY,by=c('cacaoID','file_matrix'))
      

      
      
      filtered_combined_df$condition <- as.factor(filtered_combined_df$condition)
      filtered_combined_df$code <- as.factor(filtered_combined_df$code)
      filtered_combined_df$expression <-as.numeric(filtered_combined_df$expression)
      filtered_combined_df <- filter(filtered_combined_df, cacaoStudyID %in% filter_tables$cacaoStudyID)
      
      
      row.names(filtered_combined_df) <- filtered_combined_df$cacaoID
      
      conditions_colors <- c('Control'='#4361ee','Cachexia'='#f72585')
      p<-ggplot(filtered_combined_df, aes(y=cacaoStudyID, x=expression,fill=condition, color=condition))+
        geom_boxplot(alpha=75,outlier.size = 0.5,size = 0.5) +
        theme(legend.position="bottom",
              title = element_text(face='bold'),
              legend.title = element_text(size=10,face='bold'),
              legend.text = element_text(size=10),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 10,face = 'bold'),
              panel.background = element_blank(),
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a2df',linetype = "dotted"),
              axis.title.x = element_text(size = 10,face='bold')
              # panel.grid.minor.x = element_line(size = 0.75,color='#454545',linetype = "dotted")
        ) +
        scale_fill_manual(values = conditions_colors)+
        scale_color_manual(values = conditions_colors)+
        xlab("Expression levels") +
        ylab("") +
        labs( colour='condition')
      
      
      
      p
      
      
      
      #-----------------------
      
      filtered_deg_combined_df$value <-0
      filtered_deg_combined_df$padj <- as.numeric(filtered_deg_combined_df$padj)
      filtered_deg_combined_df$pvalue <- as.numeric(filtered_deg_combined_df$pvalue)
      
      p1 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= round(filtered_deg_combined_df$padj,4)
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("adj P.val") +
        ylab("") + xlim(-0.0025,0.0025)
      p1
      
      p2 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= round(filtered_deg_combined_df$pvalue,4)
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("P.value") +
        ylab("") + xlim(-0.0025,0.0025)
      p2
      
      p00 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= filtered_deg_combined_df$author
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("Author") +
        ylab("") + xlim(-0.0025,0.0025)
      p00
      
      p0 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= filtered_deg_combined_df$year
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("Year") +
        ylab("") + xlim(-0.0025,0.0025)
      p0
      
      p01 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= filtered_deg_combined_df$tissue
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("Tissue") +
        ylab("") + xlim(-0.0025,0.0025)
      p01
      
      p02 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= filtered_deg_combined_df$code
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("GEO Code") +
        ylab("") + xlim(-0.0025,0.0025)
      p02
      
      
      p03 <- ggplot(filtered_deg_combined_df, aes(x = value, y = cacaoStudyID))+
        geom_text(
          fill='#edede9',
          size=3,
          label.size = 0.025,
          label.padding = unit(0.25, "lines"),
          label= filtered_deg_combined_df$cacaoStudyID
        )+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position="none",
              panel.grid.major.y = element_line(linewidth = 0.25,color='#a2a2a260',linetype = "dotted"),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_text(size = 10,face='bold'))+  
        xlab("CaCaO ID") +
        ylab("") + xlim(-0.0025,0.0025)
      p03
      
      pp_list <- list(p00,p0,p01,p02,p03,p,pp,p1,p2)
      pp3 <- wrap_plots(pp_list,ncol=9)+plot_layout(widths =c(0.8,0.4,1.2,0.8,1.2,2.8,2.8,0.5,0.5), heights = 1, ncol=9)+
        plot_annotation(gene_select, caption = 'CacaO Lab',theme=theme(plot.title=element_text(hjust=0.5, size = 15, face='bold')))
      print(pp3)
      return(pp3)
    
    
  }
  
  
}




# generate_plots_Microarray(  organism = 'Homo sapiens',
#                             filter_tables = Sample_annotation_JSS_SHINY_microarray,
#                             gene_select = 'DPP3')

#return filter tables from mysql
get_filter_tables_mca <- function(){
  return(table_to_filter_Microarray)
}

#set filter to RNA-seq
filter_table_mca <- function(df_tables_info,
                         year_list=c(),
                         author_list=c(),
                         tumor_list=c(),
                         tissue_list=c(),
                         tissue_names_list=c(),
                         time_days_list=c(),
                         pmid_code_list=c(),
                         gse_code_list=c(),
                         gender_list=c(),
                         organism=''){

  final_df <- data.frame()
  if(length(year_list)>0 ){
    df_filter_year <- filter(df_tables_info,  Year %in% year_list)
    final_df <- rbind(final_df,df_filter_year)
  }
  
  if(length(author_list)>0 ){
    df_filter_author <- filter(df_tables_info,  Autor %in% author_list)
    final_df <- rbind(final_df,df_filter_author)
  }
  
  if(length(gse_code_list)>0 ){
    df_filter_gse <- filter(df_tables_info,  Code_GSE %in% gse_code_list)
    final_df <- rbind(final_df,df_filter_gse)
  }
  
  if(length(gender_list)>0 ){
    df_filter_gender <- filter(df_tables_info,  Sex %in% gender_list)
    final_df <- rbind(final_df,df_filter_gender)
  }
  
  if(length(time_days_list)>0 ){
    df_filter_time_days <- filter(df_tables_info,  Time_days %in% time_days_list)
    final_df <- rbind(final_df,df_filter_time_days)
  }
  
  if(length(tumor_list)>0 ){
    df_filter_tumor <- filter(df_tables_info,  Tumor %in% tumor_list)
    final_df <- rbind(final_df,df_filter_tumor)
  }
  
  if(length(tissue_list)>0 ){
    df_filter_tissue <- filter(df_tables_info,  Tissue %in% tissue_list)
    final_df <- rbind(final_df,df_filter_tissue)
  }
  
  if(length(tissue_names_list)>0 ){
    df_filter_tissue_name <- filter(df_tables_info,  Tissue_name %in% tissue_names_list)
    final_df <- rbind(final_df,df_filter_tissue_name)
  }
  
  if(length(pmid_code_list)>0 ){
    df_filter_pmid <- filter(df_tables_info,  PMID %in% pmid_code_list)
    final_df <- rbind(final_df,df_filter_pmid)
  }
  
  
  df<- data.frame(file_matrix= unique(final_df$Series_Matrix_File_Table_Name),
                  file_deg=unique(final_df$DE_Table_Name),
                  cacaoStudyID=unique(final_df$cacaoStudyID))
  df$file_matrix<- str_replace_all(df$file_matrix,' ','')
  df$file_deg<- str_replace_all(df$file_deg,' ','')
  
  return(df)
  
}


choices_mmus_RNA_seq <- function(input,output,session){
  
}
