ScSeq.ui <- function() {
  ui <- div(
    # Barra de seleção e botões
    div(
      class = "bg-light p-3 mb-4 mx-auto", # Fundo claro, espaçamento interno e centralizado
      style = "width: 80%; border-radius: 8px; text-align: center;", # Centraliza a barra e define a largura
      fluidRow(
        column(
          width = 4,
          div(
            class = "d-flex flex-column align-items-center", # Centraliza verticalmente
            h5(tags$b("Tissue Selected")),
            verbatimTextOutput("tissue_selected")
          )
        ),
        column(
          width = 4,
          div(
            class = "d-flex flex-column align-items-center",
            h5(tags$b("Select Gene")),
            shinyjs::hidden(
              pickerInput(
                inputId = "gene.m.sc",
                label = NULL,
                choices = NULL, # Inicialmente vazio
                options = list(
                  'style' = "btn btn-primary p-2 mt-1",
                  'live-search' = TRUE
                )
              )
            ),
            shinyjs::hidden(
              pickerInput(
                inputId = "gene.h.sc",
                label = NULL,
                choices = NULL,
                options = list(
                  'style' = "btn btn-primary p-2 mt-1",
                  'live-search' = TRUE
                )
              )
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "d-flex justify-content-around align-items-center", # Alinha horizontalmente os botões
            div(
              class = "text-center",
              h5(tags$b("Run")),
              actionButton(
                inputId = 'render_plot_SC',
                'Plot',
                icon = icon('bar-chart'),
                class = "btn-success mt-1"
              )
            ),
            div(
              class = "text-center",
              h5(tags$b("Download")),
              downloadButton(
                outputId = 'download_plot',
                label = 'Download Image',
                class = "btn-primary mt-1"
              )
            )
          )
        )
      )
    ),
    
    # Gráfico centralizado com a mesma largura
    fluidRow(
      div(
        class = "d-flex justify-content-center align-items-center mx-auto", # Centraliza o container
        style = "width: 80%; margin: 0 auto;", # Centraliza e define largura igual à barra branca
        column(
          width = 12, # Largura total dentro do contêiner
          shinyjs::hidden(
            shinycssloaders::withSpinner(
              plotOutput(
                outputId = "main_plot_SCrnaseq",
                width = "100%", # Ajusta para respeitar o container
                height = "650px"
              ),
              type = 6, # Tipo do spinner
              id = 'spinnersc' # ID do spinner
            )
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