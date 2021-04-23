#' Volacno Plot
#'
#' @param proteomics_results
#' @param gene_list
#' @param sample_comparison
#' @param foldchange_cutoff
#' @param p_value_cutoff
#'
#' @import plotly
#'
#' @return
#' @export
#'
#' @examples
plot_volcano <- function(proteomics_results = NULL,
                         sample_comparison = NULL,
                         gene_list = NULL,
                         foldchange_cutoff = 1.5,
                         p_value_cutoff = 0.05){

    results <- proteomics_results %>% select(contains(c('name','ID', sample_comparison, 'p.adj', 'ratio')))

    pvalue_adj <- paste0(sample_comparison, '_p.adj')

    foldchange <- paste0(sample_comparison, '_ratio')



    #Applying the -log10 to the pvalue_adj
    results$fold_change <- results[,which(names(results)==foldchange)]

    results$log10_p.adj <- -log10(results[,which(names(results)==pvalue_adj)])

    #Color for the right side significant
    results$color[results$fold_change > log2(foldchange_cutoff) & results$log10_p.adj > -log10(p_value_cutoff)] <- 'brown2'


    #Color for the left side significant

    results$color[results$fold_change < -log2(foldchange_cutoff) & results$log10_p.adj > -log10(p_value_cutoff)] <- 'cyan3'

    #Color for the non significant

    results$color[is.na(results$color)] <- 'grey'


    #Plot
    p <- ggplot(results,  aes(x = fold_change, y = log10_p.adj,  key = name))+
                geom_point(aes(color = color))+
                geom_vline(xintercept=c(-log2(foldchange_cutoff), log2(foldchange_cutoff)), color = "black",lwd=1.0,alpha=0.5,lty=3)+
                geom_hline(yintercept=-log10(p_value_cutoff), color = "black",lwd=1.0,alpha=0.5,lty=3)+
                #labs(x=expression('Log'[2]*'(Fold-change)'), y=expression('-Log'[10]*'(P-value)'))+
                theme_bw()+
                theme(legend.position = 'none')+
                scale_color_identity()

  plotly::ggplotly(p = p ,
                   tooltip = c("fold_change", "log10_p.adj", 'name'))

  }
