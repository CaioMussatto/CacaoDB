metadata.ui <- function() {
    library(DT)
    library(shiny)
    
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      .btn-primary { background-color: #007bff; border-color: #007bff; }
      .btn-success { background-color: #28a745; border-color: #28a745; }
      .download-btn { 
        background-color: #007bff; 
        color: white; 
        padding: 3px 8px;
        border-radius: 4px;
        text-decoration: none;
        display: inline-block;
      }
      /* Adicionado para alinhar os elementos */
      .inline-form { 
        display: flex;
        align-items: center;
        gap: 10px;
        width: 100%;
      }
      .inline-form .form-group { 
        flex-grow: 1;
        margin-bottom: 0 !important; 
      }
    "))
    ),
    
    div(
      class = "bg-light p-3 mb-4 mx-auto",
      style = "width: 95%; border-radius: 8px;",
      
      fluidRow(
        column(
          width = 12,
          div(
            class = "d-flex flex-column align-items-center",
            h4(tags$b("Select the Platform"), class = "mb-3"),
            
            # Container modificado para alinhamento
            div(
              class = "inline-form",
              selectizeInput(
                inputId = "metadata_platform",
                label = NULL,
                choices = unique(metadata_values$Platform),
                width = "100%"
              ),
              actionButton(
                inputId = "run_metadata",
                label = "Run",
                class = "btn btn-success",
                style = "height: 38px; white-space: nowrap;"
              )
            )
          )
        )
      ),
      
      hr(style = "border-top: 1px solid #ddd; margin: 20px 0;"),
      
      DTOutput("metadata_table")
    )
  )
    
    return(ui)
  }