library(shiny)
library(shinyalert)
library(shinyWidgets)

page1.ui <- function(){
  
  ui<-fluidPage( 
    fluidRow(
      column(width = 9,
             actionButton("show", "Show",class='btn-info btn')
             #\dropdown_parameters()
      )),
    fluidRow(
      column(width = 12,
             plotOutput(width = 1000, height = 450,"main_plot_rnaseq.1")
      )
    )
  )
  
  return(ui)   
}

rnaSeq.ui <- function(){
  ui <- div(
    div(class = 'centered-filter-option',
        fluidRow(
          column(width = 4, 
                 pickerInput(inputId = 'organism',
                             label = 'Organism',
                             choices = organism_list[1:2],
                             options = list(
                               `style` = "btn btn-primary p-2 mt-1")
                 )
          ),
          column(width = 4,
                 shinyjs::hidden(pickerInput(inputId = "gene.m",
                                             label = paste("Select gene"),
                                             choices = gene_anno[['Mus musculus (mouse)']],
                                             selected = 'Gm29299',
                                             options = list(
                                               `style` = "btn btn-primary p-2 mt-1",
                                               `live-search` = TRUE)
                 )),
                 shinyjs::hidden(pickerInput(inputId = "gene.h",
                                             label = paste("Select gene"),
                                             choices = gene_anno[['Homo sapiens (human)']],
                                             options = list(
                                               `style` = "btn btn-primary p-2 mt-1",
                                               `live-search` = TRUE)
                 ))
          ),
          column(width = 4, 
                 div(
                   fluidRow(
                     column(width = 4, class = 'mt-2',
                            h5(tags$b('Run')),
                            actionButton(inputId = 'render_plot',
                                         'Plot',
                                         icon = icon('bar-chart'),
                                         class = "btn-success mt-1")
                     ), 
                     column(width = 4, 
                            div(class = 'float-right mt-2', 
                                h5(tags$b('Filter')),
                                actionButton('filter_btn', '',
                                             icon = icon('filter'),
                                             class = "btn btn-primary mt-1",
                                             `data-bs-toggle` = "modal",
                                             `data-bs-target` = "#exampleModalCenter"))
                     ),
                     column(width = 4, class = 'mt-2', 
                            h5(tags$b('Download')),
                            downloadButton(outputId = 'download_plot_rnaseq', 
                                           label = 'Download Image', 
                                           class = "btn-primary mt-1")
                     )
                   ),
                   style = "margin-right: 10px;" # Adiciona um espaçamento interno
                 )
          )
        )
    ),
    br(),
    br(),
    
    # Centraliza o gráfico
    fluidRow(
      div(
        class = "d-flex justify-content-center align-items-center", # Ajusta alinhamento do container
        column(
          width = 8, # Define um limite no tamanho para centralizar o gráfico
          shinyjs::hidden(
            shinycssloaders::withSpinner(
              id = 'spinner',
              type = 6,
              ui_element = plotOutput(
                outputId = "main_plot_rnaseq",
                width = "100%", # Ajusta a largura para respeitar o container
                height = "450px"
              )
            )
          )
        ),
        style = "width: 100%; margin: 0 auto;" # Centraliza a div principal
      )
    ),
    br(),
    br()
  )
  
  return(ui)  
}





