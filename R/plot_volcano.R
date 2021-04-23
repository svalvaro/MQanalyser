#' Volacno Plot
#'
#' @param proteomics_results
#' @param gene_list
#'
#' @return
#' @export
#'
#' @examples
plot_volcano <- function(proteomics_results = NULL,
                         sample_comparison = NULL,
                         gene_list = NULL,
                         log2_cutoff = 1.5,
                         p_value_cutoff = 0.05){

    results <- proteomics_results %>% select(contains(c('name','ID', sample_comparison, 'p.adj', 'ratio')))

    pvalue_adj <- paste0(sample_comparison, '_p.adj')

    foldchange <- paste0(sample_comparison, '_ratio')



    #Applying the -log10 to the pvalue_adj
    results$fold_change <- results[,which(names(results)==foldchange)]

    results$`log10_p.adj` <- -log10(results[,which(names(results)==pvalue_adj)])


    #Plot
    ggplot(results,  aes(x = fold_change, y = `log10_p.adj`))+
        geom_point()


}
