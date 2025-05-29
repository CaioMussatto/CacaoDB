
pacotes = c("shiny","shinyWidgets", "shinyjs","shinythemes","stringr","dplyr","ggrepel","ggplot2",
            "cowplot","gridExtra","patchwork","shinycssloaders","shinyalert", "factoextra", "matrixStats",
            "ggpubr", "FactoMineR", "purrr")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

library(shinyalert)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(stringr)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(patchwork)
library(patchwork)
library(shinycssloaders)
library(factoextra)
library(matrixStats)
library(ggpubr)
library(FactoMineR)
library(purrr)

organism_list <- c('None','Mus Musculus','Homo Sapiens')
#source(paste(getwd(),'/Mysql_datas.R',sep=''))
gene_RNA <- readRDS('gene_RNA.rds')
gene_anno <- readRDS('gene_list.rds')
source(paste(getwd(),'/Microarray_codes.R',sep=''))
#source(paste(getwd(),'/homepage.R',sep=''))
# if(exists('gene_list')){
#   gene_list <- readRDS('gene_list.rds')
# }else{
#   gene_list <- readRDS('gene_list.rds')
# }
# gene_anno$RNA_SEQ$`Mus musculus (mouse)` <- gsub("\\..*","",gene_anno$`Mus musculus (mouse)`)

choices_RNAseq <- readRDS('choices_RNAseq.rds')

list2env(choices_RNAseq, envir = .GlobalEnv)


if(!exists('Deg_data_list_rnaseq')){
  Deg_data_list_rnaseq <- readRDS('Deg_data_list_rnaseq.rds')
}
if(!exists('count_data_list_rnaseq')){
  count_data_list_rnaseq <- readRDS('count_data_list_rnaseq.rds')
}

if(!exists('Sample_annotation_rnaseq_SHINY')){
  Sample_annotation_rnaseq_SHINY <- readRDS('Sample_annotation_rnaseq_SHINY.rds')
}

if(!exists('table_to_filter_rnaseq')){
  table_to_filter_rnaseq <- readRDS('table_to_filter_rnaseq.rds')
}


if(!exists('Deg_data_list_Microarray')){
  Deg_data_list_Microarray <- readRDS('Deg_data_list_Microarray.rds')
}

if(!exists('count_data_list_Microarray')){
  count_data_list_Microarray <- readRDS('count_data_list_Microarray.rds')
}

if(!exists('Sample_annotation_mca_SHINY')){
  Sample_annotation_mca_SHINY <- readRDS('Sample_annotation_mca_SHINY.rds')
}

if(!exists('table_to_filter_Microarray')){
  table_to_filter_Microarray <- readRDS('table_to_filter_Microarray.rds')
}



choice_gse_codes_microArray <- unique(table_to_filter_Microarray$Code_GSE)
choice_authors_microarray <- unique(table_to_filter_Microarray$Autor)
choice_pmid_codes_microArray <- unique(table_to_filter_Microarray$PMID)
choice_tissue_names_microarray <- unique(table_to_filter_Microarray$Tissue_name)
choice_time_days_microarray <- unique(table_to_filter_Microarray$Time_days)
choice_time_days_microarray <- choice_time_days_microarray[-which(is.na(choice_time_days_microarray))]
choice_tissues_microarray <- unique(table_to_filter_Microarray$Tissue)
choice_tumors_microarray <- unique(table_to_filter_Microarray$Tumor)
choice_model_microarray <- unique(table_to_filter_Microarray$Model)
choice_organism_microarray <- unique(table_to_filter_Microarray$Organism)
choice_years_microarray <- unique(table_to_filter_Microarray$Year)
choice_genders_mca <- unique(table_to_filter_Microarray$Sex)


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

source('RNA_seq_codes.R')
source('Microarray_codes.R')

 if(!exists('tissue_list')){
 tissue_list <- readRDS('todos.RDS')
 }



#mouse_scrnaseq <- list(CT26 = readRDS('/home/hipatia/Documentos/CT26_scRNASeq.RDS'))

cell.colors <- c( "Malignant"= "#ff7f0e"
                  ,"Macrophage" = "#1f77b4",
                  "Fibroblast" = "#2ca02c",
                  "DC"= "#e7969c",
                  "Mesothelial cells" = '#ffafcc',
                  "T cell"= "#9467bd",
                  "Endothelial"=  "#8c564b",
                  "Plasm cell" = "#e377c2",
                  "Epithelial"= "#e7ba52",
                  "Pericyte"= "#bcbd22",
                  "NK"="#7fc97f",
                  "B cell" ="#bcbddc",
                  "Mast cell" ="#cedb9c",
                  "Unknown"="#843c39",
                  "Filtered-out"="#fdd0a2",
                  "Myeloid"="#bd9e39" ,
                  "Lymphiod"="#d6616b", "Monocyte" = "#f768a1", "NK" = "#fee391",
                  "Microglia" = "#8c6bb1", "Astrocyte" = "#238443", "Oligodendrocyte" = "#c51b8a", "Unassigned" = "#737373",
                  "CAF" = "#619CFF", pDC = "#00B4F0")


colors <- c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
             "#393b79", "#637939", "#8c6d31", "#d6616b", "#7b4173",
             "#843c39", "#0392cf", "#7fc97f", "#fccde5", "#ffffb3",
             "#c6dbef", "#fdae6b", "#e7ba52", "#ce6dbd", "#bd9e39",
             "#6b6ecf", "#9c9ede", "#cedb9c", "#e7cb94", "#f4a582",
             "#b5cf6b", "#8ca252", "#bd9e39", "#e7ba52", "#e7969c",
             "#de9ed6", "#9c9ede", "#cedb9c", "#e7cb94", "#fdd0a2",
             "#ef8a62", "#f7f7f7", "#bcbddc", "#c7c7c7", "#d9d9d9",
             "#969696", "#bdbdbd", "#525252", "#737373", "#252525")


#scRNAseq plot ----

plot_umap <- function(expr_data) {
  a <- ggplot(expr_data, aes(x = UMAP_1, y = UMAP_2, color = cell_types)) +
    geom_point(size = 0.8, shape = 16) +
    scale_color_manual(
      values = cell.colors,
      guide = guide_legend(
        title = "Cell Types",
        title.position = "top",
        title.hjust = 0.5,     
        label.theme = element_text(size = 12), 
        key.height = unit(5, "cm"), 
        key.width = unit(5, "cm"),
        override.aes = list(size = 2.5)
      )
    ) +
    theme_classic() +
    ggtitle(paste("UMAP")) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, face = 'bold'),
      legend.text = element_text(size = 10), 
      axis.title.x = element_text(size = 10, face = 'bold'),
      axis.title.y = element_text(size = 10, face = 'bold'),
      axis.text.x = element_text(size = 8, face = 'bold'),
      axis.title = element_text(size = 10, face = 'bold'),
      axis.text.y = element_text(size = 8, face = 'bold'),
      plot.title = element_text(
        hjust = 0.5, size = 20, face = 'bold'
      )
    )
  return(a)
}


plot_percentage <- function(expr_data, features_list) {
  plots <- list()
  
  for(gene in features_list) {
    
    data_new <- expr_data[, c(gene, 'cell_types')]  
    data_new <- data_new %>%
      filter(get(gene) > 0) %>%  
      group_by(cell_types) %>%   
      summarise(cnt = n()) %>%   
      mutate(Percentage = cnt / sum(cnt) * 100, geneName = gene) 
    colnames(data_new) <- c('Cell.Type', 'count', 'perc', 'geneName')
    dd <- ggplot(data_new, aes(x = Cell.Type, y = perc, fill = Cell.Type)) +
      geom_bar(stat = 'identity', position = "stack") +
      coord_cartesian(ylim = c(0, 105)) +  
      theme_cowplot() +
      ggtitle(paste("Percentage of", gene)) +
      theme(text = element_text(face = 'bold'),
            axis.title.y = element_text(size = 12, face = 'bold'),
            axis.text.x = element_text(face = "bold", vjust = 1, hjust = 1, size = 12, angle = 45),
            axis.title.x = element_blank(),
            legend.text = element_text(size = 12, face = 'plain'),
            legend.title = element_text(size = 15, face = 'bold'),
            legend.position = "none",
            plot.title = element_text(size = 20, face = 'bold')) +
      labs(title = gene, y = 'Percentage (%)') +
      scale_fill_manual(values = cell.colors) 
    plots[[gene]] <- dd
  }
  return(plots)
}

plot_feature <- function(data, genes, x_col = 'UMAP_1', y_col = 'UMAP_2') {
  plots <- lapply(genes, function(gene) {
    ggplot(data, aes_string(x = x_col, y = y_col, color = gene)) +
      geom_point(size = 0.5, alpha = 0.8) +
      scale_color_gradient(low = "lightblue", high = "darkblue") +
      theme_classic() +
      labs(title = gene, color = NULL, x = x_col, y = y_col) +
      ggtitle(paste("Feature plot of", genes)) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "right"
      )
  })
  combined_plot <- wrap_plots(plots, ncol = length(genes))
  return(combined_plot)
}

plot_violin <- function(expr_data, gene_expr, cell_colors) {
  expr_data[, gene_expr] <- log2(expr_data[, gene_expr] + 1)
  a <- ggplot(expr_data, aes(x = cell_types, y = .data[[gene_expr]], fill = cell_types)) +
    geom_violin(scale = "width", trim = TRUE, alpha = 0.7)  +
    scale_fill_manual(values = cell_colors) +
    theme_classic() +
    ggtitle(paste("Expression of", gene_expr)) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 12, face = 'bold'),
      axis.title.y = element_text(size = 12, face = 'bold'),
      axis.text.x = element_text(size = 10, face = 'bold', angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10, face = 'bold'),
      plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')
    ) +
    labs(x = "Cell Types", y = paste("Log2(Expression of", gene_expr))
  
  return(a)
}


plot_gene_analysis <- function(expr_data, gene_name, cell.colors) {
  colnames(expr_data) <- str_replace_all(colnames(expr_data), '-', '.')
  umap_plot <- plot_umap(expr_data)
  percentage_plot <- plot_percentage(expr_data, gene_name)
  feature_plot <- plot_feature(expr_data, gene_name)
  violin_plot <- plot_violin(expr_data, gene_name, cell.colors)
  todos <- umap_plot + percentage_plot + feature_plot + violin_plot + plot_annotation(gene_name, caption = 'CaCaO Lab', 
                                                                                      theme = theme(
                                                                                        plot.title = element_text(
                                                                                          hjust = 0.5, size = 24, face = 'bold'
                                                                                        )
                                                                                      ))
  print(todos)
  return(todos)
}



# Metadata functions ----

metadata_filter <- function(df_rnaseq, df_microarray) {
  select_common <- function(df) {
    required_cols <- c("cacaoStudyID", "ID_GEO", "Code_GSE", "Autor", "Year", 
                       "Organism", "Tissue_name", "Platform")
    existing_cols <- intersect(required_cols, names(df))
    missing_cols <- setdiff(required_cols, existing_cols)
    if(length(missing_cols) > 0) {
      warning(paste("Colunas faltantes no dataframe:", paste(missing_cols, collapse = ", ")))
    }
    
    df %>% select(all_of(existing_cols))
  }
  df_rnaseq_clean <- select_common(df_rnaseq)
  df_microarray_clean <- select_common(df_microarray)
  
  # Combinar os dados verticalmente
  final <- bind_rows(
    "RNAseq" = df_rnaseq_clean,
    "Microarray" = df_microarray_clean,
    .id = "Platform"
  )
  
  return(final)
}

metadata_values <- metadata_filter(table_to_filter_rnaseq, table_to_filter_Microarray)

pca_plot <- function(plataform, df_name) {
  require(ggplot2)
  require(FactoMineR)
  require(factoextra)
  require(matrixStats)
  require(viridis)
  require(dplyr)
  
  tryCatch({
    # 1. Carregar dados
    if(plataform == "RNAseq") {
      df_samples <- Sample_annotation_rnaseq_SHINY %>% 
        filter(file_rlog == df_name) %>% 
        select(samples, condition)
      
      df_counts <- count_data_list_rnaseq[[df_name]] %>%
        select(-gene, -name)
      
    } else {
      df_samples <- Sample_annotation_mca_SHINY %>% 
        filter(file_matrix == df_name) %>% 
        select(samples, condition)
      
      df_counts <- count_data_list_Microarray[[df_name]] %>%
        select(-ID, -Gene.symbol)
      print(df_counts)
    }
    
    # 2. Verificar correspondência de amostras
    if(!all(colnames(df_counts) %in% df_samples$samples)) {
      stop("Amostras não correspondem entre dados e anotações!")
    }
    
    # 3. Pré-processamento
    df_samples <- df_samples[match(colnames(df_counts), df_samples$samples), ]
    df_samples$Mix <- factor(df_samples$condition, 
                             levels = c("Control", "Cachexia"),
                             labels = c("Control", "Cachexia"))
    
    # 4. Cálculo da PCA
    df_numeric <- as.matrix(df_counts)
    rv <- matrixStats::rowVars(df_numeric)
    ntop <- 500
    select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
    mat <- t(df_numeric[select, ])
    pca <- prcomp(mat)
    
    # 5. Construção do gráfico (mantendo estrutura original)
    plot_pca <- fviz_pca_ind(pca, 
                             invisible = "quali",
                             col.ind = df_samples$Mix,
                             addEllipses = FALSE,
                             pointsize = 5,
                             labelsize = 4,
                             geom = c("text", "point"),
                             legend.title = "Condition",
                             repel = TRUE,
                             col.var = "black",
                             title = df_name,
                             ellipse.type = "confidence",
                             ellipse.level = 0.95) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      ) +
      scale_shape_manual(values = c(19, 19)) +
      scale_size_manual(values = c(14, 14, 28, 28)) +
      scale_color_manual(values = c("Control" = "gray", "Cachexia" = "black")) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm")
      ) +
      stat_ellipse(
        aes(x = x, y = y, color = df_samples$Mix, group = df_samples$Mix),
        type = "t",
        alpha = 1,
        level = 0.8
      )
    
    # 6. Saída do gráfico
    print("Deu certo")
    plot_pca
    # 7. Salvamento opcional (descomentar se necessário)
    # ggsave(paste0("PCA_", df_name, ".png"), plot_pca, 
    #        width = 24, height = 10, units = "in", dpi = 600)
    
  }, error = function(e) {
    message("Erro durante a execução:")
    message(e$message)
    return(NULL)
  })
}

volcano_plot <- function(plataform, nome) {
  if(plataform == "RNAseq") {
    df <- Deg_data_list_rnaseq[[nome]]
  } else {
    df <- Deg_data_list_Microarray[[nome]]
    colnames(df) <- c("name", "ID_001", "padj", "pvalue", "log2FoldChange", "file_deg")
    df <- na.omit(df)
    is.na(df)
  }
  if (!"Differential_expression" %in% colnames(df)) {
    print("Creating a Differential expression column")
    df <- df %>%
      mutate(Differential_expression = case_when(
        padj < 0.05 & log2FoldChange > 0 ~ 'Up',
        padj < 0.05 & log2FoldChange < 0 ~ 'Down',
        TRUE ~ 'Not significant'  # Define casos não significativos
      ))
  }
  
  # Corrige a referência ao dataframe e coluna de genes
  df$delabel <- ifelse(
    df$name %in% head(df[order(df$padj), "name"], 20),  # Usa 'df' e corrige a coluna
    df$name, 
    NA
  )
  
  # Cria o gráfico
  # Calcular limites dinâmicos
  x_max <- max(abs(df$log2FoldChange), na.rm = TRUE) * 1.10  # Adiciona 10% ao valor absoluto máximo
  y_max <- max(-log10(df$padj), na.rm = TRUE) * 1.10        # Adiciona 10% ao valor máximo
  
  plot <- ggplot(data = df, aes(x = log2FoldChange, y = -log10(padj), 
                                col = Differential_expression, label = delabel)) +
    geom_vline(xintercept = 0, col = "gray", linetype = 'dashed') +
    geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') + 
    geom_point(size = 1) + 
    scale_color_manual(
      values = c("Down" = "#00AFBB", "Not significant" = "grey", "Up" = "#bb0c00"),
      labels = c("Downregulated", "Not significant", "Upregulated")
    ) +
    coord_cartesian(ylim = c(0, y_max), xlim = c(-x_max, x_max)) +  # Limites dinâmicos
    labs(
      color = 'Differential Expression',
      x = expression("log"[2]*"FC"), 
      y = expression("-log"[10]*"p-value")
    ) + 
    scale_x_continuous(breaks = seq(floor(-x_max), ceiling(x_max), by = 2)) +  # Quebras dinâmicas
    ggtitle(paste0(nome)) +
    geom_text_repel(max.overlaps = Inf) +
    theme_classic()
  
  print(plot)
  return(plot)
}



