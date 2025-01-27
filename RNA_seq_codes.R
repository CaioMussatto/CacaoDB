# All codes for RNA-seq datas


render_filter_ui <- function(input,output,session ){
  body_filter <- box(
    width = 12,
    fluidRow(
      width= 12,
      column(4,align="left",
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_YEAR_slider",
                                  label = tags$b("YEAR"), 
                                  status = "success",
                                  fill = TRUE
                    )
             ),
             prettyCheckboxGroup(
               selected = choice_years,
               inputId = "ipt_YEAR",
               label = "",
               choices = choice_years,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TISSUE_NAME_slider",
                                  label = tags$b("TISSUE NAME"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_TISSUE_NAME",
               label = "",
               selected = choice_tissue_names,
               choices = choice_tissue_names,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TIME_slider",
                                  label = tags$b("TIME (DAYS)"), 
                                  status = "success",
                                  fill = TRUE
                    )
             ),
             prettyCheckboxGroup(
               inputId = "ipt_TIME",
               selected = choice_time_days,
               label = "",
               choices = choice_time_days,
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
                                  inputId = "ipt_GENDER_slider",
                                  label = tags$b("GENDER"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_GENDER",
               label = "",
               selected = choice_genders,
               choices = choice_genders,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_GSE_slider",
                                  label = tags$b("GSE"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_GSE",
               label = "",
               selected = choice_gse_codes,
               choices = choice_gse_codes,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_AUTHORS_slider",
                                  label = tags$b("AUTHORS"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_AUTHORS",
               label = "",
               selected = choice_authors,
               choices = choice_authors,
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
                                  inputId = "ipt_TISSUE_slider",
                                  label = tags$b("TISSUE"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_TISSUE",
               label = "",
               selected = choice_tissues,
               choices = choice_tissues,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_TUMOR_slider",
                                  label = tags$b("TUMOR"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_TUMOR",
               label = "",
               selected=choice_tumors,
               choices = choice_tumors,
               icon = icon("check"), 
               status = "primary",
               outline = TRUE,
               animation = "jelly"
             ),
             hr(),
             column(12,align="left",
                    prettySwitch( value=T,
                                  inputId = "ipt_PMID_slider",
                                  label = tags$b("PMID"), 
                                  status = "success",
                                  fill = TRUE
                    )),
             prettyCheckboxGroup(
               inputId = "ipt_PMID",
               label = "",
               selected = choice_pmid_codes,
               choices = choice_pmid_codes,
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

filter_year <- function(input,output,session){
  if(input$ipt_YEAR_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_YEAR",
      label = "",
      selected=NULL,
      choices = choice_years,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"),
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_YEAR_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_YEAR",
      label = "",
      selected=choice_years,
      choices = choice_years,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"),
                           'animation' = "jelly")
    )
  }
}

filter_tumor <- function(input,output,session){
  if(input$ipt_TUMOR_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TUMOR",
      label = "",
      selected=NULL,
      choices = choice_tumors,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TUMOR_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TUMOR",
      label = "",
      selected=choice_tumors,
      choices = choice_tumors,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_tissue_name <- function(input,output,session){
  if(input$ipt_TISSUE_NAME_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TISSUE_NAME",
      label = "",
      selected=NULL,
      choices = choice_tissue_names,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TISSUE_NAME_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TISSUE_NAME",
      label = "",
      selected=choice_tissue_names,
      choices = choice_tissue_names,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_PMID <- function(input,output,session){
  if(input$ipt_PMID_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_PMID",
      label = "",
      selected=NULL,
      choices = choice_pmid_codes,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_PMID_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_PMID",
      label = "",
      selected=choice_pmid_codes,
      choices = choice_pmid_codes,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_time_days <- function(input,output,session){
  if(input$ipt_TIME_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TIME",
      label = "",
      selected=NULL,
      choices = choice_time_days,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TIME_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TIME",
      label = "",
      selected=choice_time_days,
      choices = choice_time_days,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_gender <- function(input,output,session){
  if(input$ipt_GENDER_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_GENDER",
      label = "",
      selected=NULL,
      choices = choice_genders,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_GENDER_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_GENDER",
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

filter_GSE <- function(input,output,session){
  if(input$ipt_GSE_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_GSE",
      label = "",
      selected=NULL,
      choices = choice_gse_codes,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_GSE_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_GSE",
      label = "",
      selected=choice_gse_codes,
      choices = choice_gse_codes,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_authors <- function(input,output,session){
  if(input$ipt_AUTHORS_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_AUTHORS",
      label = "",
      selected=NULL,
      choices = choice_authors,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_AUTHORS_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_AUTHORS",
      label = "",
      selected=choice_authors,
      choices = choice_authors,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}

filter_tissue <- function(input,output,session){
  if(input$ipt_TISSUE_slider==F){
    updatePrettyCheckboxGroup(
      session = session,
      inputId = "ipt_TISSUE",
      label = "",
      selected=NULL,
      choices = choice_tissues,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
      
    )
  }
  else if(input$ipt_TISSUE_slider==TRUE){
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "ipt_TISSUE",
      label = "",
      selected=choice_tissues,
      choices = choice_tissues,
      prettyOptions = list('status' = "primary",
                           'outline' = TRUE,
                           'icon' = icon("check"), 
                           'animation' = "jelly")
    )
  }
}


generate_plots_RNAseq <- function(organism,filter_tables,gene_select=''){
  if(gene_select!='' | !is.null(gene_select)){
    
    if(organism=='Mus Musculus'){
      library(ggplot2)
      
      
      
      filtered_deg_combined_df <- do.call(cbind, lapply(Deg_data_list_rnaseq, function(df) {
        filtered_df <- df[df$name == gene_select, ]
        filtered_df <- na.omit(filtered_df)
        filtered_df <- filtered_df[,-which(names(filtered_df) %in% c('gene','name'))]
        
        filtered_df <- as.data.frame(t(filtered_df))
      }))
      
      names(filtered_deg_combined_df) <- filtered_deg_combined_df['file_deg',]
      
      filtered_deg_combined_df <- as.data.frame(t(filtered_deg_combined_df))
      
      
      filtered_deg_combined_df <- merge(filtered_deg_combined_df, Sample_annotation_rnaseq_SHINY,by='file_deg', all.x = T)
      filtered_deg_combined_df <- filtered_deg_combined_df[!duplicated(filtered_deg_combined_df$file_deg),]
      filtered_deg_combined_df$log2FoldChange <- as.numeric(filtered_deg_combined_df$log2FoldChange)
      filtered_deg_combined_df[filtered_deg_combined_df$cacaoStudyID == c('CCSTID2024016'),'log2FoldChange'] <- 
        filtered_deg_combined_df[filtered_deg_combined_df$cacaoStudyID == c('CCSTID2024016'),'log2FoldChange']  * -1
      
      filtered_deg_combined_df[filtered_deg_combined_df$cacaoStudyID == c('CCSTID2024015'),'log2FoldChange'] <- 
        filtered_deg_combined_df[filtered_deg_combined_df$cacaoStudyID == c('CCSTID2024015'),'log2FoldChange']  * -1
      
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
      
      
      
      
      filtered_combined_df <- do.call(rbind, lapply(count_data_list_rnaseq, function(df) {
        
        filtered_df <- df[which(df$name == gene_select), ]
        if(nrow(filtered_df) >0 ){
          filtered_df <- na.omit(filtered_df)
          filtered_df <- filtered_df[,-which(names(filtered_df) %in% c('gene','name'))]
          rownames(filtered_df) <- gene_select
          filtered_df <- as.data.frame(t(filtered_df))}
      }))
      
      
      names(filtered_combined_df) <- 'expression'
      
      
      
      
      
      filtered_combined_df$sample <- unlist(lapply(rownames(filtered_combined_df), function(x){str_split(x,'tsv.')[[1]][2] }))
      filtered_combined_df$file_rlog <- unlist(lapply(rownames(filtered_combined_df), function(x){paste(unlist(str_split(x,'tsv.')[[1]][1]),'tsv',sep='') })
      )
      
      
      
      filtered_combined_df$cacaoID = ''
      
      length(which(filtered_combined_df$sample %in% Sample_annotation_rnaseq_SHINY$samples))
      for (row in 1:nrow(filtered_combined_df)) {
        rsample= filtered_combined_df[row,'sample']
        rfile= filtered_combined_df[row,'file_rlog']
        filtered_combined_df$cacaoID[row]<-  filter(Sample_annotation_rnaseq_SHINY, samples==rsample & file_rlog ==rfile)$cacaoID
      }
      
      
      filtered_combined_df <- merge(filtered_combined_df, Sample_annotation_rnaseq_SHINY,by=c('cacaoID','file_rlog'))
      
      
      
      
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
      pp3 <- wrap_plots(pp_list,ncol=9)+plot_layout(widths =c(0.6,0.4,0.7,0.8,1.2,2.8,2.8,0.5,0.5), heights = 1, ncol=9)+
        plot_annotation(gene_select, caption = 'CacaO Lab',theme=theme(plot.title=element_text(hjust=0.5, size = 15, face='bold')))
      pp3
      return(pp3)
    }else if(organism=='Homo Sapiens'){
      #CODE FOR HOMO SAPIENS
    }
    
  }
  
  
}

#return filter tables from mysql
get_filter_tables <- function(){
  return(table_to_filter_rnaseq)
}

#set filter to RNA-seq
filter_table <- function(df_tables_info,
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
    df_filter_tissue_name <- filter(df_tables_info,  Year %in% tissue_names_list)
    final_df <- rbind(final_df,df_filter_tissue_name)
  }
  
  if(length(pmid_code_list)>0 ){
    df_filter_pmid <- filter(df_tables_info,  PMID %in% pmid_code_list)
    final_df <- rbind(final_df,df_filter_pmid)
  }
  
  df<- data.frame(rlog_tables= unique(final_df$RLOG_Table_Name),
                  deg_tables=unique(final_df$DE_Table_Name),
                  cacaoStudyID=unique(final_df$cacaoStudyID))
  df$rlog_tables<- str_replace_all(df$rlog_tables,' ','')
  df$deg_tables<- str_replace_all(df$deg_tables,' ','')
  
  print(df)
  return(df)
  
}


choices_mmus_RNA_seq <- function(input,output,session){
  
}
