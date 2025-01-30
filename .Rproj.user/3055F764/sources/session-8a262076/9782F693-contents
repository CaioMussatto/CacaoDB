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

rnaSeq.ui <- function() {
  ui <- div(
    div(
      class = "bg-light p-3 mb-4 mx-auto", 
      style = "width: 80%; border-radius: 8px; text-align: center;", 
      fluidRow(
        column(
          width = 3, 
          div(
            class = "d-flex flex-column align-items-center", 
            h5(tags$b("Organism")),
            pickerInput(
              inputId = 'organism',
              label = NULL,
              choices = organism_list[1:2],
              options = list(`style` = "btn btn-primary p-2 mt-1")
            )
          )
        ),
        column(
          width = 3,
          div(
            class = "d-flex flex-column align-items-center",
            h5(tags$b("Select Gene")),
            shinyjs::hidden(pickerInput(
              inputId = "gene.m",
              label = NULL,
              choices = gene_anno[['Mus musculus (mouse)']],
              selected = 'Dpp9',
              options = list(`style` = "btn btn-primary p-2 mt-1", `live-search` = TRUE)
            )),
            shinyjs::hidden(pickerInput(
              inputId = "gene.h",
              label = NULL,
              choices = gene_anno[['Homo sapiens (human)']],
              options = list(`style` = "btn btn-primary p-2 mt-1", `live-search` = TRUE)
            ))
          )
        ),
        column(
          width = 6,
          div(
            class = "d-flex justify-content-around align-items-center", 
            div(
              class = "text-center",
              h5(tags$b("Run")),
              actionButton(
                inputId = 'render_plot',
                'Plot',
                icon = icon('bar-chart'),
                class = "btn-success mt-1"
              )
            ),
            div(
              class = "text-center",
              h5(tags$b("Filter")),
              actionButton(
                'filter_btn',
                '',
                icon = icon('filter'),
                class = "btn btn-primary mt-1",
                `data-bs-toggle` = "modal",
                `data-bs-target` = "#exampleModalCenter"
              )
            ),
            div(
              class = "text-center",
              h5(tags$b("Download")),
              downloadButton(
                outputId = 'download_plot_rnaseq',
                label = 'Download Image',
                class = "btn-primary mt-1"
              )
            )
          )
        )
      )
    ),
    
    
    fluidRow(
      div(
        class = "d-flex justify-content-center align-items-center mx-auto", 
        style = "width: 80%; margin: 0 auto;", 
        column(
          width = 12, 
          shinyjs::hidden(
            shinycssloaders::withSpinner(
              id = 'spinner',
              type = 6,
              ui_element = plotOutput(
                outputId = "main_plot_rnaseq",
                width = "100%", 
                height = "650px"
              )
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



microArray.ui <- function() {
  ui <- div(
    div(
      class = "bg-light p-3 mb-4 mx-auto", 
      style = "width: 90%; border-radius: 8px; text-align: center;", 
      fluidRow(
        column(
          width = 3, 
          div(
            class = "d-flex flex-column align-items-center", 
            h5(tags$b("Organism")),
            pickerInput(
              inputId = 'organism_mca',
              label = NULL,
              choices = organism_list[1:2],
              options = list(`style` = "btn btn-primary p-2 mt-1")
            )
          )
        ),
        column(
          width = 3,
          div(
            class = "d-flex flex-column align-items-center",
            h5(tags$b("Select Gene")),
            shinyjs::hidden(pickerInput(
              inputId = "gene.m_mca",
              label = NULL,
              choices = gene_anno[['Mus musculus (mouse)']],
              options = list(`style` = "btn btn-primary p-2 mt-1", `live-search` = TRUE),
              selected = 'Dpp9'
            )),
            shinyjs::hidden(pickerInput(
              inputId = "gene.h_mca",
              label = NULL,
              choices = gene_anno[['Homo sapiens (human)']],
              options = list(`style` = "btn btn-primary p-2 mt-1", `live-search` = TRUE)
            ))
          )
        ),
        column(
          width = 6,
          div(
            class = "d-flex justify-content-around align-items-center", 
            div(
              class = "text-center",
              h5(tags$b("Run")),
              actionButton(
                inputId = 'render_plot_mca',
                'Plot',
                icon = icon('bar-chart'),
                class = "btn-success mt-1"
              )
            ),
            div(
              class = "text-center",
              h5(tags$b("Filter")),
              actionButton(
                'filter_btn_mca',
                '',
                icon = icon('filter'),
                class = "btn btn-primary mt-1",
                `data-bs-toggle` = "modal",
                `data-bs-target` = "#modal_mca"
              )
            ),
            div(
              class = "text-center",
              h5(tags$b("Download")),
              downloadButton(
                outputId = 'download_plot_mca',
                label = 'Download Image',
                class = "btn-primary mt-1"
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      div(
        class = "d-flex justify-content-center align-items-center mx-auto", 
        style = "width: 90%; margin: 0 auto;", 
        column(
          width = 12, 
          shinyjs::hidden(
            shinycssloaders::withSpinner(
              id = 'spinner_mca',
              type = 6,
              ui_element = plotOutput(
                outputId = "main_plot_mca",
                width = "100%", 
                height = "650px"
              )
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