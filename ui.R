library(shinyalert)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(stringr)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(patchwork)
library(patchwork)
library(shinycssloaders)
library(shinydashboard)

source(paste(getwd(),'/run_analysis_ui.R',sep=''))
source(paste(getwd(),'/SC_seq_codes.R',sep=''))
source(paste(getwd(),'/global.R',sep=''))

jsSetImg <- "shinyjs.setImg = function(params){document.getElementById('main_plot_SCrnaseq').setAttribute('src', params);}"
# jsStopLoader <- "shiny.stopLoader = function(){document.getElementById('loader').add('hidden');alert('stopping ?');}"
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(disable = T, title = tags$h4("CaCao database", class = "text-secondary mb-5")),
    dashboardSidebar(disable = F, width = 0, collapsed = F),
    dashboardBody(
      shinyjs::useShinyjs(),
      extendShinyjs(text = jsSetImg, functions = c("setImg")),
      # extendShinyjs(text = jsStopLoader, functions = c("stopLoader")),
      tags$script(src = 'https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js'),
      includeCSS('https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.min.css'),
      tags$script('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/js/all.min.js'),
      tags$script('//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-glyphicons.css'),
      includeCSS("https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200"),
      includeCSS('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.8.1/css/all.min.css'),
      includeCSS('https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/css/bootstrap.min.css'),
      includeCSS("www/css/bootstrap-select.css"),
      includeCSS("www/css/style.css"),
      includeCSS("www/css/imagemSingleCell.css"),
      includeScript('www/js/main.js'),
      includeScript('www/js/bootstrap-select.js'),
      shinyjs::hidden(textInput("tissue_sc",
                                "",
                                value = "Breast")),
      
      shinyjs::hidden(textInput("organism_sc",
                                "",
                                value = "Human")),
      # useShinyalert(force = TRUE),
      htmlTemplate("www/Home.html",
                   footer = htmlTemplate("www/Footer.html"),
                   homeBody= htmlTemplate("www/Home_body.html",
                                          sc_button = actionBttn(
                                            inputId = "sc_page", 
                                            label = img(src = "svg/scrnaseq.svg"), 
                                            class = 'home-button', 
                                            onclick = "openPage(event, 'ScrnaSeq-tab-pane')"
                                          ),
                                          rna_button = actionBttn(
                                          inputId = 'rna_page',
                                          label = img(src='svg/rnaseq.svg'),
                                          class = 'home-button',
                                          onclick = "openPage(event, 'rnaSeq-tab-pane')"
                                          ),
                                          micro_button = actionBttn(
                                            inputId = 'micro_page',
                                            label = img(src='svg/Microarray.svg'),
                                            class = 'home-button',
                                            onclick = "openPage(event, 'microArray-tab-pane')"
                                          )
                   ),
                   render_filter_ui_microarray = render_filter_ui_microarray(),
                   microArrayBody=   microArray.ui(),
                   micro_array_link = actionLink(inputId = "microArrayLink", "Micro-Array", class = "nav-link", onclick = "openPage(event, 'microArray-tab-pane')"), 
                   ok_btn_filter_mca = actionButton('ok_filter_mca','OK',icon=icon('OK'),class = "btn btn-success", `data-bs-dismiss`="modal"),
                   rnaSeqBody = rnaSeq.ui(),
                   AboutUsBody = htmlTemplate('www/About.html'), 
                   render_filter_ui = render_filter_ui(),
                   SingleCellBody = htmlTemplate("www/singlecell.html", 
                                                 # breast_human_btn = actionButton(
                                                 #   inputId='sc_breast_Human',label=img(src="svg/BREAST.svg"), class='element-button',
                                                 #   onclick = "openPage(event, 'sc_analysis-tab-pane'); setTissue('Breast'); setOrganism('Human')"
                                                 # ),
                                                 colon_human_btn = actionButton(
                                                   inputId='sc_colon_Human',label=img(src="svg/COLORECTAL.svg"), class='element-button',
                                                   onclick = "openPage(event, 'sc_analysis-tab-pane'); setTissue('Colon'); setOrganism('Human')")
                                                 , brain_human_btn = actionButton(
                                                   inputId='sc_brain_Human',label=img(src="svg/BRAIN.svg"), class='element-button',
                                                   onclick = "openPage(event, 'sc_analysis-tab-pane'); setTissue('Brain'); setOrganism('Human')"),
                                                 kidney_human_btn = actionButton(
                                                   inputId='sc_kidney_Human',label=img(src="svg/KIDNEY.svg"), class='element-button',
                                                   onclick = "openPage(event, 'sc_analysis-tab-pane'); setTissue('Kidney'); setOrganism('Human')"),
                                                 lung_human_btn = actionButton(
                                                   inputId='sc_lung_Human',label=img(src="svg/LUNG.svg"), class='element-button',
                                                   onclick = "openPage(event, 'sc_analysis-tab-pane'); setTissue('Lung'); setOrganism('Human')"),
                                                 hematoligc_human_btn = actionButton(
                                                   inputId='sc_hematologic_Human',label=img(src="svg/HEMATOLOGIC.svg"), class='element-button',
                                                   onclick = "openPage(event, 'sc_analysis-tab-pane'); setTissue('Hematoligc'); setOrganism('Human')"),
                                                 mouse_ct26_btn = actionButton(
                                                   inputId='sc_mouse_CT26', label=img(src="svg/CT26.svg"), class='element-button',
                                                   onclick="openPage(event, 'sc_analysis-tab-pane'); setTissue('CT26'); setOrganism('Mus Musculus')"),
                                                 mouse_ll2_btn = actionButton(
                                                   inputId='sc_mouse_LL2', label=img(src="svg/LL2.svg"), class='element-button',
                                                   onclick="openPage(event, 'sc_analysis-tab-pane'); setTissue('LL2'); setOrganism('Mus Musculus')"),
                                                 mouse_b16f_10_btn = actionButton(
                                                   inputId='sc_mouse_B16F_10', label=img(src="svg/B16F-10.svg"), class='element-button',
                                                   onclick="openPage(event, 'sc_analysis-tab-pane'); setTissue('B16F10'); setOrganism('Mus Musculus')"),
                                                 mouse_mc_38_btn = actionButton(
                                                   inputId='sc_mouse_MC_38', label=img(src="svg/MC-38.svg"), class='element-button',
                                                   onclick="openPage(event, 'sc_analysis-tab-pane'); setTissue('MC38'); setOrganism('Mus Musculus')"),
                                                 mouse_mc_sa1n_btn = actionButton(
                                                   inputId='sc_mouse_SA1N', label=img(src="svg/SA1N.svg"), class='element-button',
                                                   onclick="openPage(event, 'sc_analysis-tab-pane'); setTissue('SA1N'); setOrganism('Mus Musculus')"),
                                                 mouse_mc_emt6_btn = actionButton(
                                                   inputId='sc_mouse_EMT6', label=img(src="svg/EMT6.svg"), class='element-button',
                                                   onclick="openPage(event, 'sc_analysis-tab-pane'); setTissue('EMT6'); setOrganism('Mus Musculus')"),
                   ),
                   sc_analysis_body= ScSeq.ui(),
                   ok_btn_filter = actionButton('ok_filter','OK',icon=icon('OK'),class = "btn btn-success", `data-bs-dismiss`="modal"),        # links
                   home_link = actionLink(href = "#", "Home", class = "nav-link ", inputId = "homeLink", onclick = "openPage(event, 'home-tab-pane')"),
                   rna_seq_link = actionLink(href = "#", "RNA-seq", class = "nav-link ", inputId = "rnaSeqLink", onclick = "openPage(event, 'rnaSeq-tab-pane')"), #
                   sc_rna_seq_link = actionLink(href = "#", "ScRNA-seq", class = "nav-link ", inputId = "scRnaSeqLink", onclick = "openPage(event, 'ScrnaSeq-tab-pane')"), #
                   about_us_link = actionLink(href = "#", "Our team", class = "nav-link ", inputId = "aboutLink", onclick = "openPage(event, 'About-tab-pane')") #
      ),
      # htmlTemplate('modal.html')
      
    )
  )
)