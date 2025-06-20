#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyalert)
library(shinyWidgets)
source(paste(getwd(),'/run_analysis_ui.R',sep=''))
source(paste(getwd(),'/SC_seq_codes.R',sep=''))



shinyServer(function(input, output,session) {
  
  #RNASEQ Filtering ----
  filtered_controls_rnaseq <- reactiveVal(F)
  choice_gse_codes_render_MM_rnaseq <- reactiveVal(choice_gse_codes)
  choice_authors_render_MM_rnaseq <- reactiveVal(choice_authors)
  choice_years_render_MM_rnaseq <- reactiveVal(choice_years)
  choice_pmid_codes_render_MM_rnaseq <- reactiveVal(choice_pmid_codes)
  choice_genders_render_MM_rnaseq <- reactiveVal(choice_genders)
  choice_tissues_render_MM_rnaseq <- reactiveVal(choice_tissues)
  choice_tissue_names_render_MM_rnaseq <- reactiveVal(choice_tissue_names)
  choice_time_days_render_MM_rnaseq <- reactiveVal(choice_time_days)
  choice_tumors_render_MM_rnaseq <- reactiveVal(choice_tumors)
  
  df_filter <- reactive({
    filter(get_filter_tables(),  Organism %in% input$organism)
  })
  
  
  filtered_names_rnaseq <- reactive({
    filter_table(df_tables_info = df_filter(), 
                 author_list = choice_authors_render_MM_rnaseq(),
                 year_list = choice_years_render_MM_rnaseq(),
                 gse_code_list = choice_gse_codes_render_MM_rnaseq(),
                 pmid_code_list = choice_pmid_codes_render_MM_rnaseq(),
                 gender_list = choice_genders_render_MM_rnaseq(),
                 tissue_list = choice_tissues_render_MM_rnaseq(),
                 tissue_names_list = choice_tissue_names_render_MM_rnaseq(),
                 time_days_list = choice_time_days_render_MM_rnaseq(),
                 tumor_list = choice_tumors_render_MM_rnaseq() )
  })
  
  observeEvent(input$organism, {
    if (input$organism == 'Mus Musculus') {
      shinyjs::show("gene.m") 
      shinyjs::hide('gene.h') # Reseta o valor do gene
    } else if (input$organism == 'Homo Sapiens') {
      shinyjs::hide("gene.m") 
      shinyjs::show('gene.h') # Reseta o valor do gene
    } else {
      shinyjs::hide("gene.m") 
      shinyjs::hide('gene.h')
    }
  }, ignoreInit = TRUE, once = FALSE, ignoreNULL = FALSE)
  
  observeEvent(input$ipt_TUMOR_slider,{
    filter_tumor(input,output,session)
  })
  observeEvent(input$ipt_YEAR_slider,{
    filter_year(input,output,session)
  })
  observeEvent(input$ipt_TISSUE_NAME_slider,{
    filter_tissue_name(input,output,session)
  })
  observeEvent(input$ipt_PMID_slider,{
    filter_PMID(input,output,session)
  })
  observeEvent(input$ipt_TIME_slider,{
    filter_time_days(input,output,session)
  })
  observeEvent(input$ipt_GENDER_slider,{
    filter_gender(input,output,session)
  })
  observeEvent(input$ipt_GSE_slider,{
    filter_GSE(input,output,session)
  })
  observeEvent(input$ipt_AUTHORS_slider,{
    filter_authors(input,output,session)
  })
  observeEvent(input$ipt_TISSUE_slider,{
    filter_tissue(input,output,session)
  })
  
  
  set_filtered_controls_rnaseq <- function(value) {
    filtered_controls_rnaseq(value)
  }
  

  
  
  observeEvent(input$ok_filter,{
    
    set_filtered_controls_rnaseq(TRUE )
    arg <-c()
    if(length(input$ipt_AUTHORS)>0){
      choice_authors_render_MM_rnaseq(input$ipt_AUTHORS)
    }else{
      choice_authors_render_MM_rnaseq(c())
    }
    if(length(input$ipt_YEAR)>0){
      choice_years_render_MM_rnaseq(input$ipt_YEAR)
    }else{
      choice_years_render_MM_rnaseq(c())
    }
    if(length(input$ipt_GSE)>0){
      choice_gse_codes_render_MM_rnaseq(input$ipt_GSE)
    }else{
      choice_gse_codes_render_MM_rnaseq(c())
    }
    if(length(input$ipt_PMID)>0){
      choice_pmid_codes_render_MM_rnaseq(input$ipt_PMID)
    }else{
      choice_pmid_codes_render_MM_rnaseq(c())
    }
    if(length(input$ipt_GENDER)>0){
      choice_genders_render_MM_rnaseq(input$ipt_GENDER)
    }else{
      choice_genders_render_MM_rnaseq(c())
    }
    if(length(input$ipt_TISSUE)>0){
      choice_tissues_render_MM_rnaseq(input$ipt_TISSUE)
    }else{
      choice_tissues_render_MM_rnaseq(c())
    }
    if(length(input$ipt_TISSUE_NAME)>0){
      choice_tissue_names_render_MM_rnaseq(input$ipt_TISSUE_NAME)
    }else{
      choice_tissue_names_render_MM_rnaseq(c())
    }
    if(length(input$ipt_TIME)>0){
      choice_time_days_render_MM_rnaseq(input$ipt_TIME)
    }else{
      choice_time_days_render_MM_rnaseq(c())
    }
    if(length(input$ipt_TUMOR)>0){
      choice_tumors_render_MM_rnaseq(input$ipt_TUMOR)
    }else{
      choice_tumors_render_MM_rnaseq(c())
    }
    
  })
  
  #RNA seq plot render----
  rna_seq_plot <- reactive({
    shinyjs::show(id='spinner')
    renderPlot({
      Sys.sleep(1)
      gene_input <- if (input$organism == 'Mus Musculus') {
        input$gene.m
      } else if (input$organism == 'Homo Sapiens') {
        input$gene.h
      }
      plot <- tryCatch(generate_plots_RNAseq(input$organism,filtered_names_rnaseq(),gene_select=gene_input), 
                       error = function(e) e, finally = h3(paste('Has no data for ',gene_input,' gene !')))
      tryCatch(print(plot), error = function(e) e, finally = h3(paste('Has no data for ',gene_input,' gene !')))
    })
  })

  observeEvent(input$render_plot,{
    shinyjs::show(selector = 'div.shiny-spinner-hideui')
    output$main_plot_rnaseq <- rna_seq_plot()
    
  })
  
  #RNA seq plot download----
  output$download_plot_rnaseq <- downloadHandler(
    filename = function() {
      gene_input <- str_to_title(input$gene.m)
      organims <- str_to_title(input$organism)
      paste0(gene_input, "_", organims, ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating plot...',
        detail = 'Please wait while the file is being prepared.',
        value = 0, {
          # Increment progress before generating the plot
          incProgress(0.3)
          
          gene_input <- input$gene.m
          
          # Gera o gráfico utilizando generate_plots_RNAseq
          plot <- generate_plots_RNAseq(input$organism, filtered_names_rnaseq(), gene_select = gene_input)
          
          # Increment progress during saving the file
          incProgress(0.6)
          
          # Salva o gráfico
          ggsave(file, plot, dpi = 600, width = 15, height = 10)
          
          # Increment para finalizar
          incProgress(1)
        }
      )
    }
  )
  #------------#
  
  #ScRNA-seq----
  
  observeEvent(c(input$tissue_sc, input$organism_sc),  {
    if (input$organism_sc == 'Mus Musculus') {
      tissue_data <- tissue_list[[input$tissue_sc]]
      shinyjs::show(id = "gene.m.sc") # Mostra o pickerInput caso esteja oculto
      updatePickerInput(
        session = session,
        inputId = "gene.m.sc",
        choices = colnames(tissue_data), # Atualiza com as colunas disponíveis no tissue_data
        selected = NULL # Reseta a seleção atual
      )}
    else if (input$organism_sc == 'Human'){
      tissue_data <- tissue_list[[input$tissue_sc]]
      shinyjs::show(id = "gene.h.sc") # Mostra o pickerInput caso esteja oculto
      updatePickerInput(
        session = session,
        inputId = "gene.h.sc",
        choices = colnames(tissue_data), # Atualiza com as colunas disponíveis no tissue_data
        selected = NULL # Reseta a seleção atual
      )}
  })
  
  observeEvent(input$sc_breast_Human,
               {
                 shinyjs::hide("gene.m.sc") 
                 shinyjs::show('gene.h.sc')
                 updateTextInput(session, 'organism_sc', label = '', value = 'Human')
                 updateTextInput(session, 'tissue_sc', label = '', value = 'Breast')
                 output$tissue_selected <- renderText("Tissue: Breast")
                 
               })
  observeEvent(input$sc_colon_Human,
               {
                 shinyjs::hide("gene.m.sc") 
                 shinyjs::show('gene.h.sc')
                 updateTextInput(session, 'organism_sc', label = '', value = 'Human')
                 updateTextInput(session, 'tissue_sc', label = '', value = 'Colon')
                 output$tissue_selected <- renderText("Tissue: Colon")
                 
               })
  observeEvent(input$sc_brain_Human,
               {
                 shinyjs::hide("gene.m.sc") 
                 shinyjs::show('gene.h.sc')
                 updateTextInput(session, 'organism_sc', label = '', value = 'Human')
                 updateTextInput(session, 'tissue_sc', label = '', value = 'Brain')
                 output$tissue_selected <- renderText("Tissue: Brain")
                 
               })
  observeEvent(input$sc_kidney_Human,
               {
                 shinyjs::hide("gene.m.sc") 
                 shinyjs::show('gene.h.sc')
                 updateTextInput(session, 'organism_sc', label = '', value = 'Human')
                 updateTextInput(session, 'tissue_sc', label = '', value = 'Kidney')
                 output$tissue_selected <- renderText("Tissue: Kidney")
                 
               })
  observeEvent(input$sc_lung_Human,
               {
                 shinyjs::hide("gene.m.sc") 
                 shinyjs::show('gene.h.sc')
                 updateTextInput(session, 'organism_sc', label = '', value = 'Human')
                 updateTextInput(session, 'tissue_sc', label = '', value = 'Lung')
                 output$tissue_selected <- renderText("Tissue: Lung")
                 
               })
  
  observeEvent(input$hematoligc,
               {
                 shinyjs::hide("gene.m.sc") 
                 shinyjs::show('gene.h.sc')
                 updateTextInput(session, 'organism_sc', label = '', value = 'Human')
                 updateTextInput(session, 'tissue_sc', label = '', value = 'Hematoligc')
                 output$tissue_selected <- renderText("Tissue: Hematoligc")
                 
               })
  
  observeEvent(input$sc_mouse_CT26, {
    shinyjs::hide('gene.h.sc')
    shinyjs::show("gene.m.sc") 
    updateTextInput(session, 'organism_sc', label = '', value = 'Mus Musculus')
    updateTextInput(session, 'tissue_sc', label = '', value = 'CT26')
    output$tissue_selected <- renderText("Tissue: CT26")
    
  })
  
  observeEvent(input$sc_mouse_LL2, {
    shinyjs::hide('gene.h.sc')
    shinyjs::show("gene.m.sc") 
    updateTextInput(session, 'organism_sc', label = '', value = 'Mus Musculus')
    updateTextInput(session, 'tissue_sc', label = '', value = 'LL2')
    output$tissue_selected <- renderText("Tissue: LL2")
  })
  
  observeEvent(input$sc_mouse_B16F_10, {
    shinyjs::hide('gene.h.sc')
    shinyjs::show("gene.m.sc") 
    updateTextInput(session, 'organism_sc', label = '', value = 'Mus Musculus')
    updateTextInput(session, 'tissue_sc', label = '', value = 'B16F10')
    output$tissue_selected <- renderText("Tissue: B16F10")
  })
  
  observeEvent(input$sc_mouse_MC_38, {
    shinyjs::hide('gene.h.sc')
    shinyjs::show("gene.m.sc") 
    updateTextInput(session, 'organism_sc', label = '', value = 'Mus Musculus')
    updateTextInput(session, 'tissue_sc', label = '', value = 'MC38')
    output$tissue_selected <- renderText("Tissue: MC38")
  })
  
  observeEvent(input$sc_mouse_SA1N, {
    shinyjs::hide('gene.h.sc')
    shinyjs::show("gene.m.sc") 
    updateTextInput(session, 'organism_sc', label = '', value = 'Mus Musculus')
    updateTextInput(session, 'tissue_sc', label = '', value = 'SA1N')
    output$tissue_selected <- renderText("Tissue: SA1N")
  })
  
  observeEvent(input$sc_mouse_EMT6, {
    shinyjs::hide('gene.h.sc')
    shinyjs::show("gene.m.sc") 
    updateTextInput(session, 'organism_sc', label = '', value = 'Mus Musculus')
    updateTextInput(session, 'tissue_sc', label = '', value = 'EMT6')
    output$tissue_selected <- renderText("Tissue: EMT6")
  })
  
  
  # ScRNA seq plot render----
  Scrna_seq_plot <- reactive({
    shinyjs::show(id = 'spinnersc')
    
    renderPlot({
      Sys.sleep(1)
      gene_input <- if (input$organism_sc == 'Mus Musculus') {
        input$gene.m.sc
      } else {
        input$gene.h.sc
      }
      gene_input<- str_replace_all(gene_input,'-','.')
      cat("Debug: Using gene input:", gene_input, "\n")
      plot <- tryCatch(
        generate_plots_ScRNAseq(
          tissue = input$tissue_sc,
          organism = input$organism_sc,
          gene_select = gene_input
        ),
        error = function(e) {
          cat("Error:", conditionMessage(e), "\n")
          return(h3(paste('Has no data for', gene_input, 'gene!')))
        },
        finally = {
          h3(paste('Has no data for', gene_input, 'gene!'))
        }
      )
      plot
    })
  })

  observeEvent(input$render_plot_SC,{
    shinyjs::show(selector = 'div.shiny-spinner-hideui')
    output$main_plot_SCrnaseq <- Scrna_seq_plot()
  })
  
  #ScRNA-seq plot download----
  
  output$download_plot <- downloadHandler(
    filename = function() {
      gene_input <- if (input$organism_sc == 'Mus Musculus') {
        input$gene.m.sc
      } else {
        input$gene.h.sc
      }
      gene_input <- str_to_title(gene_input)
      
      tissue_name <- tryCatch({
        input$tissue_sc
      }, error = function(e) {
        cat("Error in tissue name extraction:", conditionMessage(e), "\n")
        return("UnknownTissue")
      })
      
      paste0(gene_input, "_", tissue_name, ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating plot...',
        detail = 'Please wait while the file is being prepared.',
        value = 0, {
          # Incrementa o progresso antes da geração do gráfico
          incProgress(0.3)
          
          gene_input <- if (input$organism_sc == 'Mus Musculus') {
            input$gene.m.sc
          } else {
            input$gene.h.sc
          }
          gene_input <- str_replace_all(gene_input, '-', '.')
          
          # Gera o gráfico
          plot <- plot_gene_analysis(
            tissue_list[[input$tissue_sc]],
            gene_name = gene_input,
            cell.colors = cell.colors
          )
          
          # Incrementa o progresso durante o salvamento do arquivo
          incProgress(0.6)
          
          # Salva o gráfico
          ggsave(file, plot, dpi = 600, width = 15, height = 10)
          
          # Incrementa para finalizar
          incProgress(1)
        }
      )
    }
  )
  
  #Microarray----
  
  observeEvent(input$organism_mca,
               {
                 if(input$organism_mca=='Mus Musculus'){
                   shinyjs::show("gene.m_mca") 
                   shinyjs::hide('gene.h_mca')
                 }else if(input$organism_mca=='Homo Sapiens'){
                   shinyjs::hide("gene.m_mca") 
                   shinyjs::show('gene.h_mca')
                 }else{
                   shinyjs::hide("gene.m_mca") 
                   shinyjs::hide('gene.h_mca')
                 } },ignoreInit = T, once = F,ignoreNULL=FALSE )
  
  
  observeEvent(input$ipt_YEAR_slider_mca,{
    filter_year_microarray(input,output,session)
  })
  observeEvent(input$ipt_TISSUE_NAME_slider_mca,{
    filter_tissue_name_microarray(input,output,session)
  })
  observeEvent(input$ipt_PMID_slider_mca,{
    filter_PMID_microarray(input,output,session)
  })
  observeEvent(input$ipt_TIME_slider_mca,{
    filter_time_days_microarray(input,output,session)
  })
  observeEvent(input$ipt_GENDER_slider_mca,{
    filter_gender_microarray(input,output,session)
  })
  observeEvent(input$ipt_GSE_slider_mca,{
    filter_GSE_microarray(input,output,session)
  })
  observeEvent(input$ipt_AUTHORS_slider_mca,{
    filter_authors_microarray(input,output,session)
  })
  observeEvent(input$ipt_TISSUE_slider_mca,{
    filter_tissue_microarray(input,output,session)
  })
  
  observeEvent(input$ipt_TUMOR_slider_mca,{
    filter_tumor_microarray(input,output,session)
  })
  
  
  set_filtered_controls_mca <- function(value) {
    filtered_controls_mca(value)
  }
  
  
  
  
  filtered_names_mca <- reactive({
    filter_table_mca(df_tables_info_mca = df_filter_mca(), 
                     author_list = choice_authors_render_MM_mca(),
                     year_list = choice_years_render_MM_mca(),
                     gse_code_list = choice_gse_codes_render_MM_mca(),
                     pmid_code_list = choice_pmid_codes_render_MM_mca(),
                     gender_list = choice_genders_render_MM_mca(),
                     tissue_list = choice_tissues_render_MM_mca(),
                     tissue_names_list = choice_tissue_names_render_MM_mca(),
                     time_days_list = choice_time_days_render_MM_mca(),
                     tumor_list = choice_tumors_render_MM_mca() )
  })
  
  
  df_filter_mca <- reactive({
    selected_organism <- case_when(
      input$organism_mca == "Homo Sapiens" ~ "Homo sapiens",
      input$organism_mca == "Mus Musculus" ~ "Mus Musculus",
      TRUE ~ input$organism_mca
    )
    filter(get_filter_tables_mca(),  Organism == selected_organism)
  })
  
  filtered_controls_mca <- reactiveVal(F)
  choice_gse_codes_render_MM_mca <- reactiveVal(choice_gse_codes_microArray)
  choice_authors_render_MM_mca <- reactiveVal(choice_authors_microarray)
  choice_years_render_MM_mca <- reactiveVal(choice_years_microarray)
  choice_pmid_codes_render_MM_mca <- reactiveVal(choice_pmid_codes_microArray)
  choice_genders_render_MM_mca <- reactiveVal(choice_genders_mca)
  choice_tissues_render_MM_mca <- reactiveVal(choice_tissues_microarray)
  choice_tissue_names_render_MM_mca <- reactiveVal(choice_tissue_names_microarray)
  choice_time_days_render_MM_mca <- reactiveVal(choice_time_days_microarray)
  choice_tumors_render_MM_mca <- reactiveVal(choice_tumors_microarray)
  
  
  observeEvent(input$ok_filter_mca,{
    
    set_filtered_controls_mca(TRUE )
    arg <-c()
    if(length(input$ipt_AUTHORS_mca)>0){
      choice_authors_render_MM_mca(input$ipt_AUTHORS_mca)
    }else{
      choice_authors_render_MM_mca(c())
    }
    if(length(input$ipt_YEAR_mca)>0){
      choice_years_render_MM_mca(input$ipt_YEAR_mca)
    }else{
      choice_years_render_MM_mca(c())
    }
    if(length(input$ipt_GSE_mca)>0){
      choice_gse_codes_render_MM_mca(input$ipt_GSE_mca)
    }else{
      choice_gse_codes_render_MM_mca(c())
    }
    if(length(input$ipt_PMID_mca)>0){
      choice_pmid_codes_render_MM_mca(input$ipt_PMID_mca)
    }else{
      choice_pmid_codes_render_MM_mca(c())
    }
    if(length(input$ipt_GENDER_mca)>0){
      choice_genders_render_MM_mca(input$ipt_GENDER_mca)
    }else{
      choice_genders_render_MM_mca(c())
    }
    if(length(input$ipt_TISSUE_mca)>0){
      choice_tissues_render_MM_mca(input$ipt_TISSUE_mca)
    }else{
      choice_tissues_render_MM_mca(c())
    }
    if(length(input$ipt_TISSUE_NAME_mca)>0){
      choice_tissue_names_render_MM_mca(input$ipt_TISSUE_NAME_mca)
    }else{
      choice_tissue_names_render_MM_mca(c())
    }
    if(length(input$ipt_TIME_mca)>0){
      choice_time_days_render_MM_mca(input$ipt_TIME_mca)
    }else{
      choice_time_days_render_MM_mca(c())
    }
    if(length(input$ipt_TUMOR_mca)>0){
      choice_tumors_render_MM_mca(input$ipt_TUMOR_mca)
    }else{
      choice_tumors_render_MM_mca(c())
    }
    
  })
  
  #Microarray Plot----
  microarray_plot <- reactive({
    shinyjs::show(id='spinner-mca')
    renderPlot({
      Sys.sleep(1)
      gene_input <- if (input$organism_mca == 'Mus Musculus') {
        str_to_upper(input$gene.m_mca)
      } else if (input$organism_mca == 'Homo Sapiens')  {
        str_to_upper(input$gene.h_mca)
      }
      plot <- tryCatch(generate_plots_Microarray(input$organism_mca,filtered_names_mca(), gene_input), 
                       error = function(e) e, finally = h3(paste('Has no data for ',gene_input,' gene !')))
      tryCatch(print(plot), error = function(e) e, finally = h3(paste('Has no data for ',gene_input,' gene !')))
    })
  })
  
  observeEvent(input$render_plot_mca, {
    shinyjs::show(selector = 'div.shiny-spinner-hideui')
    output$main_plot_mca <- microarray_plot()
  })
  
  #Microarray plot download----

  output$download_plot_mca <- downloadHandler(
    filename = function() {
      gene_input <- str_to_title(input$gene.m_mca)
      organims <- str_to_title(input$organism_mca)
      paste0(gene_input, "_", organims, ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating plot...',
        detail = 'Please wait while the file is being prepared.',
        value = 0, {
          # Increment progress before generating the plot
          incProgress(0.3)
          
          gene_input <- input$gene.m_mca
          
          # Gera o gráfico utilizando generate_plots_RNAseq
          plot <- generate_plots_Microarray(input$organism_mca, filtered_names_mca(), gene_select = gene_input)
          # Increment progress during saving the file
          incProgress(0.6)
          
          # Salva o gráfico
          ggsave(file, plot, dpi = 600, width = 15, height = 10)
          
          # Increment para finalizar
          incProgress(1)
        }
      )
    }
  )
  
  
  #Metadata ----
  filtered_metadata <- eventReactive(input$run_metadata, {
    req(input$metadata_platform)
    
    df <- metadata_values %>%
      filter(Platform == input$metadata_platform) %>%
      select(-Platform)
    
    # Criar botões de download com IDs únicos para PCA e Volcano
    df$`Download PCA` <- sapply(df$cacaoStudyID, function(id) {
      as.character(
        tags$a(
          href = "#",
          class = "download-btn",
          "Download PCA",
          onclick = sprintf("Shiny.setInputValue('download_pca_id', '%s', {priority: 'event'})", id)
        )
      )
    })
    
    df$`Download Volcano` <- sapply(df$cacaoStudyID, function(id) {
      as.character(
        tags$a(
          href = "#",
          class = "download-btn",
          style = "background-color: #28a745;",  # Cor diferente para diferenciar
          "Download Volcano",
          onclick = sprintf("Shiny.setInputValue('download_volcano_id', '%s', {priority: 'event'})", id)
        )
      )
    })
    
    return(df)
  })
  
  
  # Renderizar tabela
  output$metadata_table <- renderDT({
    req(filtered_metadata())
    df <- filtered_metadata()
    n <- ncol(df)
    
    datatable(
      df,
      escape = FALSE,
      selection = 'none',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = n-2, className = "dt-center"),  # Coluna Download PCA
          list(targets = n-1, className = "dt-center")   # Coluna Download Volcano
        )
      ),
      rownames = FALSE
    )
  })
  
  # Observador para cliques nos botões de download
  observeEvent(input$download_pca_id, {
    req(input$download_pca_id, input$metadata_platform)
    
    tryCatch({
      # Determinar qual tabela usar
      tbl <- if (input$metadata_platform == "RNAseq") {
        Sample_annotation_rnaseq_SHINY
      } else {
        Sample_annotation_mca_SHINY
      }
      
      # Obter informações do estudo
      study_info <- tbl %>% 
        filter(cacaoStudyID == input$download_pca_id)
      
      if (nrow(study_info) > 0) {
        # Obter o identificador do arquivo (não é um caminho real)
        file_label <- if (input$metadata_platform == "RNAseq") {
          as.character(study_info$file_rlog[1])
        } else {
          as.character(study_info$file_matrix[1])
        }
        print(file_label)
        # Nome para o arquivo de download
        download_name <- paste0(file_label, ".png")
        
        # Gerar plot
        withProgress(message = 'Generating PCA plot...', value = 0.5, {
          p <- pca_plot(
            plataform = input$metadata_platform,
            df_name = file_label  # Passamos o rótulo, não um caminho de arquivo
          )
          
          # Salvar temporariamente
          temp_file <- tempfile(fileext = ".png")
          
          # Verificar se o plot é válido
          if (!inherits(p, "ggplot")) {
            stop("O objeto gerado não é um gráfico ggplot válido")
          }
          
          ggsave(temp_file, plot = p, device = "png", width = 12, height = 12, dpi = 600)
          
          # Forçar download
          shinyjs::runjs(sprintf(
            "var link = document.createElement('a');
          link.href = '%s';
          link.download = '%s';
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);",
            paste0("data:image/png;base64,", base64enc::base64encode(temp_file)),
            download_name
          ))
        })
      }
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error", duration = 10)
    })
  })
  # Volcano plot ----
  observeEvent(input$download_volcano_id, {
    req(input$download_volcano_id, input$metadata_platform)
    
    tryCatch({
      tbl <- if (input$metadata_platform == "RNAseq") {
        Sample_annotation_rnaseq_SHINY
      } else {
        Sample_annotation_mca_SHINY
      }
      
      study_info <- tbl %>% 
        filter(cacaoStudyID == input$download_volcano_id)
      
      if (nrow(study_info) > 0) {
        file_label <- if (input$metadata_platform == "RNAseq") {
          as.character(study_info$file_deg[1])
        } else {
          as.character(study_info$file_deg[1])
        }
        print(file_label)
        download_name <- paste0(file_label, "_volcano.png")
        
        withProgress(message = 'Generating Volcano plot...', value = 0.5, {
          # SUPONDO QUE VOCÊ TENHA UMA FUNÇÃO volcano_plot()
          v <- volcano_plot(
            plataform = input$metadata_platform,
            nome = file_label
          )
          
          temp_file <- tempfile(fileext = ".png")
          ggsave(temp_file, plot = v, device = "png", width = 12, height = 12, dpi = 600)
          
          shinyjs::runjs(sprintf(
            "var link = document.createElement('a');
          link.href = '%s';
          link.download = '%s';
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);",
            paste0("data:image/png;base64,", base64enc::base64encode(temp_file)),
            download_name
          ))
        })
      }
    }, error = function(e) {
      showNotification(paste("Erro no Volcano Plot:", e$message), type = "error", duration = 10)
    })
  })
  
  
 
  #Observer event ----
  observeEvent(input$homeLink,{
    print('link to Home page !!!')
  })
  
  observeEvent(input$microArrayLink,{
    print('link to microArray page !!!')
  })
  
  observeEvent(input$rnaSeqLink,{
    print('link to RNA-Seq page !!!')
  })
  
  observeEvent(input$scRnaSeqLink, {
    print('Link to SCRNA-Seq page !!!')
    
    # Resetar os valores dos inputs de gene
    updatePickerInput(session, "gene.m.sc", selected = NULL)
    updatePickerInput(session, "gene.h.sc", selected = NULL)
    
    # Resetar o gráfico (limpando o output relacionado)
    output$main_plot_SCrnaseq <- renderPlot(NULL)
  })

  observeEvent(input$sc_page, {
    print('Link to SCRNA-Seq page !!!')
    
    # Resetar os valores dos inputs de gene
    updatePickerInput(session, "gene.m.sc", selected = NULL)
    updatePickerInput(session, "gene.h.sc", selected = NULL)
    
    # Resetar o gráfico (limpando o output relacionado)
    output$main_plot_SCrnaseq <- renderPlot(NULL)
  })
  
  observeEvent(input$rna_page, {
    print('Link to RNA-seq page !!!')
    output$main_plot_rnaseq <- renderPlot(NULL)
  })
  
  observeEvent(input$micro_page, {
    print('Link to Microarray  page !!!')
  })
  
  observeEvent(input$metada_page, {
    print('Link to Metadata  page !!!')
  })

})