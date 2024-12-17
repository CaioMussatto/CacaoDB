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
  ui<-div(
    div(class='centered-filter-option',
        fluidRow(
          column(width=4, pickerInput(inputId = 'organism',
                                      label = 'Organism',
                                      choices = organism_list[1:2],
                                      options = list(
                                        
                                        `style` = "btn btn-primary p-2 mt-1")
          )),
          column(width=4,  
                 shinyjs::hidden(pickerInput(inputId="gene.m",
                                             label = paste("Select gene"),
                                             choices = gene_anno[['Mus musculus (mouse)']],
                                             options = list(
                                               `style` = "btn btn-primary p-2 mt-1",
                                               `live-search` = TRUE)
                 )),
                 shinyjs::hidden(pickerInput(inputId="gene.h",
                                             label = paste("Select gene"),
                                             choices = gene_anno[['Homo sapiens (human)']],
                                             options = list(
                                               `style` = "btn btn-primary p-2 mt-1",
                                               `live-search` = TRUE)
                 ))
          ),
          column(width=4,div(
            fluidRow(column(width=6,class='mt-2',h5(tags$b('Run')),
              actionButton(inputId = 'render_plot','Plot',icon=icon('bar-chart'),class = "btn-success mt-1")), 
              column(width=6,div(class='float-right mt-2 ', h5(tags$b('Filter')),
                                actionButton('filter_btn','',icon=icon('filter'),class = "btn btn-primary mt-1", `data-bs-toggle`="modal",`data-bs-target`="#exampleModalCenter")))
              ))
          
          )
        ),
    ),
    br(),
    br(),
    
    fluidRow(
      div(class='image-div d-flex align-items-center justify-content-center',
      column(width = 12,
             shinyjs::hidden( shinycssloaders::withSpinner(id='spinner',type=6,ui_element = 
             plotOutput(outputId = "main_plot_rnaseq", width="1250px", height="775px"  )
             ))
             , style = "width: 100vw; background-color: #ffffff00; display: block;  margin-left: auto;  margin-right: auto;"
      )
      )
    )
    ,
    br(),
    br()
  )
  
  return(ui)  
}
