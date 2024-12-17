ScSeq.ui <- function() {
  ui <- div(
    div(class = 'centered-filter-option',
        fluidRow(
          column(width = 4, 
                 div(verbatimTextOutput("tissue_selected"))
          ),
          column(width = 4, 
                 shinyjs::hidden(
                   pickerInput(
                     inputId = "gene.m.sc",
                     label = "Select gene",
                     choices = NULL, # Inicialmente vazio
                     options = list(
                       'style' = "btn btn-primary p-2 mt-1",
                       'live-search' = TRUE)
                   )),
                 shinyjs::hidden(pickerInput(
                   inputId = "gene.h.sc",
                   label = "Select gene",
                   choices = NULL,
                   options = list(
                     'style' = "btn btn-primary p-2 mt-1",
                     'live-search' = TRUE)
                 ))
          ),
          column(width = 4, 
                 div(
                   fluidRow(
                     column(width = 6, class = 'mt-2', 
                            h5(tags$b('Run')),
                            actionButton(inputId = 'render_plot_SC', 
                                         'Plot', 
                                         icon = icon('bar-chart'), 
                                         class = "btn-success mt-1")
                     ),
                     column(width = 6, class = 'mt-2', 
                            h5(tags$b('Download')),
                            downloadButton(outputId = 'download_plot', 
                                           label = 'Download Image', 
                                           class = "btn-primary mt-1")
                     )
                   )
                 )
          )
        )
    ),
    br(),
    br(),
    fluidRow(
      class = "justify-content-center align-items-center", # Centraliza horizontal e verticalmente
      div(
        class = "col-12 d-flex justify-content-center align-items-center", # Centralização adicional para a div
        shinyjs::hidden(
          shinycssloaders::withSpinner(
            plotOutput(
              outputId = "main_plot_SCrnaseq", 
              width = "1050px", 
              height = "700px"
            ),
            type = 6, # Tipo do spinner
            id = 'spinnersc' # ID do spinner
          )
        )
      )
    ),
    br(),
    br()
  )
  
  return(ui)
}


generate_plots_ScRNAseq <- function(tissue='', organism='', gene_select='') {
  cat("Debug: generate_plots_ScRNAseq called with:\n")
  cat(" - Tissue:", tissue, "\n")
  cat(" - Organism:", organism, "\n")
  cat(" - Gene:", gene_select, "\n")
  
  if (organism == 'Mus Musculus') {
    data_exp <- tissue_list[[tissue]]
    colnames(data_exp) <- str_replace_all(colnames(data_exp), '-', '.')
    plot <- plot_gene_analysis(data_exp, gene_select, cell.colors)
    cat("Debug: Plot generated successfully.\n")
    gc()
    print(plot)
    return(plot)
  } 
  if (organism == 'Human') {
    data_exp <- tissue_list[[tissue]]
    colnames(data_exp) <- str_replace_all(colnames(data_exp), '-', '.')
    plot <- plot_gene_analysis(data_exp, gene_select, cell.colors)
    cat("Debug: Plot generated successfully.\n")
    gc()
    print(plot)
    return(plot)
  }
}

  
#}