# Inpupt

proteoInput <- read.delim('~/Downloads/proteinGroups(1)/proteinGroups.txt')


# experiment design

experiment_design <- read.delim('~/Downloads/experiment_design(1).txt')


# summarized experiment

columns = grep('LFQ', colnames(proteoInput))

data_unique <- DEP::make_unique(proteoInput, 'Gene.names', 'Protein.IDs', delim = ';')

data_se <- DEP::make_se(data_unique, columns = columns, expdesign=experiment_design)

# filter NAs

data_filt <- DEP::filter_missval(data_se,thr = 0)

# data normalization

data_norm <- normalize_vsn(data_filt)

# imputation

data_imp <-DEP::impute(data_norm, fun = "man", shift = 1.8, scale = 0.3)


# DEP

data_diff_all_contrasts <- MQanalyser::test_limma(data_imp, type = "all")

dep <- DEP::add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))

data_results <- DEP::get_results(dep)

# enrichment

geneListObject <- MQanalyser::create_geneList(
    data_results = data_results,
    comparison_samples = 'HEK_vs_TM',
    organism = 'org.Hs.eg.db')

geneList <- geneListObject$geneList$ratio

names(geneList) <- geneListObject$geneList$ENTREZID

geneList <- geneList[geneList > log2(1.5)]

diffExpress <- names(geneList)

edo <- DOSE::enrichDGN(diffExpress)

edo2 <- DOSE::gseDO(geneList)
