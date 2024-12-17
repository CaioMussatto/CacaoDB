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
  
  # RNA seq plot render 
  rna_seq_plot <- reactive({
    shinyjs::show(id='spinner')
    renderPlot({
      Sys.sleep(2)
      plot <- tryCatch(generate_plots_RNAseq(input$organism,filtered_names_rnaseq(),gene_select=input$gene.m), 
                       error = function(e) e, finally = h3(paste('Has no data for ',input$gene.m,' gene !')))
      tryCatch(print(plot), error = function(e) e, finally = h3(paste('Has no data for ',input$gene.m,' gene !')))
    })
  })
  
  # ScRNA seq plot render 
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
      
      # Caso haja um erro, ele será tratado acima.
      plot
    })
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
  # render plot for RNAseq
  observeEvent(input$render_plot,{
    shinyjs::show(selector = 'div.shiny-spinner-hideui')
    output$main_plot_rnaseq <- rna_seq_plot()
    
  })
  
  
  observeEvent(input$render_plot_SC,{
    shinyjs::show(selector = 'div.shiny-spinner-hideui')
    output$main_plot_SCrnaseq <- Scrna_seq_plot()
  })
  
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
          ggsave(file, plot, dpi = 500)
          
          # Incrementa para finalizar
          incProgress(1)
        }
      )
    }
  )
  
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
          ggsave(file, plot, dpi = 500, width = 12, height = 12)
          
          # Increment para finalizar
          incProgress(1)
        }
      )
    }
  )
  
  
  
  
  
  set_filtered_controls_rnaseq <- function(value) {
    filtered_controls_rnaseq(value)
  }
  
  get_filtered_controls_rnaseq <- reactive({filtered_controls()})
  
  # observeEvent(input$filter_btn,{
  #   showModal( session = getDefaultReactiveDomain(),
  #              modalDialog(
  #     title = 'Filter',
  #   render_filter_ui(),
  #   'Text modal ',
  #      footer = tagList(
  #        modalButton("Cancel"),
  #        actionButton("ok_filter", "OK")
  #      ),
  #   easyClose = TRUE,
  #   fade = TRUE
  #    )%>%
  #     tagAppendAttributes(class='w-75 h-75 bg-light'),
  #   )
  #   showModal(
  #     session = getDefaultReactiveDomain(),
  #     modalDialog(
  #     title = "Somewhat important message",
  #     "This is a somewhat import  ant message.",
  #     easyClose = TRUE,
  #     footer = NULL
  #   )%>%
  #     tagAppendAttributes(class='w-75 h-75 bg-light')
  #   )
  # })
  # 
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
  
  observeEvent(input$organism, {
    if (input$organism == 'Mus Musculus') {
      shinyjs::show("gene.m") 
      shinyjs::hide('gene.h')
      updateTextInput(session, "gene.m", value = "") # Reseta o valor do gene
    } else if (input$organism == 'Homo Sapiens') {
      shinyjs::hide("gene.m") 
      shinyjs::show('gene.h')
      updateTextInput(session, "gene.h", value = "") # Reseta o valor do gene
    } else {
      shinyjs::hide("gene.m") 
      shinyjs::hide('gene.h')
    }
  }, ignoreInit = TRUE, once = FALSE, ignoreNULL = FALSE)
  
  
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
  
  
  
  # observeEvent(input$alert_filter,{
  #   
  #   loseSweetAlert(session = shiny::getDefaultReactiveDomain())
  # })
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
  
  
})

