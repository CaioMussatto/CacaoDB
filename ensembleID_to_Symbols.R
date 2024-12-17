library('biomaRt')
library(dplyr)
homosap <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))

reshsap <- getBM(attributes = c("ensembl_gene_id_version","external_gene_name", "gene_biotype","chromosome_name",'strand','transcript_start','transcript_end'), mart = homosap)




mouse <- useDataset("mmusculus_gene_ensembl", useMart("ensembl"))
res <- getBM(attributes = c("ensembl_gene_id_version", "external_gene_name","gene_biotype","chromosome_name",'strand','transcript_start','transcript_end'), mart = mouse)


mmusculus <- res %>% select(c("ensembl_gene_id_version", "external_gene_name")) %>% filter (! duplicated(external_gene_name))
hsapiens <- reshsap %>% select(c("ensembl_gene_id_version", "external_gene_name")) %>% filter (! duplicated(external_gene_name))

gene_list$`Mus musculus (mouse)` <- mmusculus$external_gene_name


saveRDS(gene_list,'gene_list.rds')
