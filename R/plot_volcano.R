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
                         alpha = 0.8,
                         foldchange_cutoff = 1.5,
                         p_value_cutoff = 0.05,
                         color_down = 'cyan3',
                         color_up = 'brown2',
                         p_adj = TRUE,
                         show_genes_user = FALSE,
                         user_genes_de = NULL,
                         color_genes_de = '#800080',
                         coord_x = NULL,
                         coord_y = NULL,
                         show_genes_names = FALSE,
                         brushed_Points = NULL,
                         font_gene_names = NULL
                         ){

    results <- proteomics_results %>% select(
      contains(c('name','ID', sample_comparison, 'p.adj', 'ratio'))
      )

    pvalue_adj <- paste0(sample_comparison, '_p.adj')

    pval <- paste0(sample_comparison, '_p.val')

    foldchange <- paste0(sample_comparison, '_ratio')

    #Applying the -log10 to the pvalue_adj
    results$fold_change <- results[,which(names(results)==foldchange)]

    if (p_adj == TRUE) {
      results$log10_pvalues <- -log10(results[,which(names(results)==pvalue_adj)])
      ylab = '-Log10 (Adjusted P-Value)'
    } else{
      results$log10_pvalues <- -log10(results[,which(names(results)==pval)])
      ylab = '-Log10 (P-Value)'
    }

    #Color for the right side significant
    results$color[results$fold_change > log2(foldchange_cutoff) & results$log10_pvalues > -log10(p_value_cutoff)] <- color_up

    #Color for the left side significant

    results$color[results$fold_change < -log2(foldchange_cutoff) & results$log10_pvalues > -log10(p_value_cutoff)] <- color_down

    #Color for the non significant

    results$color[is.na(results$color)] <- 'grey'


    colnames(results)[colnames(results) == "name"] <- "Gene"

    # comparison names

    name1 <- gsub("_vs_.*", "", sample_comparison)
    name2 <- gsub(".*_vs_", "", sample_comparison)

    # Reducing number of digits

    #results$fold_change <- format(round(results$fold_change, 2), nsmall = 2)
    #results$log10_pvalues <- format(results$log10_pvalues, format = "e", digits = 2)

    #Plot
    p <- ggplot(results,  aes(x = fold_change, y = log10_pvalues,  key = Gene))+

                geom_point(aes(color = color), alpha = alpha)+

                geom_vline(xintercept=c(-log2(foldchange_cutoff),
                                        log2(foldchange_cutoff)),
                           color = "black",
                           lwd=1.0,
                           alpha=0.2,
                           lty=3)+

                ggtitle(sample_comparison)+

                ylab(ylab)+

                xlab('Log2(Fold Change)')+

                geom_hline(yintercept=-log10(p_value_cutoff),
                           color = "black",
                           lwd=1.0,
                           alpha=0.2,
                           lty=3)+

                theme_bw()+

                theme(panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.line = element_line(colour = "black"))+

                theme(legend.position = 'none')+

                scale_color_identity()+

                annotate("text",
                         x = max(results$fold_change),
                         y = 0,
                         label = name1,
                         size = 5,
                         fontface = 'bold')+

                annotate("text",
                         x = min(results$fold_change)+0.1,
                         y = 0,
                         label = name2,
                         size = 5,
                         fontface = 'bold')

    # If the user wants to see their genes:

    if(!is.null(user_genes_de) && show_genes_user ==TRUE){
      p <- p+geom_point(data = results[which(results$Gene %in% user_genes_de),],
                        color = color_genes_de,
                        alpha = alpha)
    }

    if(!is.null(coord_x) && !is.null(coord_y)){

      p <- p+coord_cartesian(xlim = coord_x, ylim = coord_y)
    }

    # Return the static figure no plotly

    if (show_genes_names == TRUE) {
      message('No plotly required for volcan')

      if (!is.null(brushed_Points)) {

        brushed <- shiny::brushedPoints(results, brushed_Points)

        # Add a text repel label of the selected genes with the brush
        p <- p +ggrepel::geom_text_repel(data = brushed,
                                         mapping = aes(
                                           x = fold_change,
                                           y = log10_pvalues,
                                           label = Gene),
                                        size = font_gene_names,
                                        max.overlaps = 100)+
          # Increase the size a bit of the selected points
          geom_point(data = brushed,
                      mapping = aes(x = fold_change,
                                    y = log10_pvalues,
                                    color = color),
                     alpha = alpha, size = 2.5)
      }

      return(p)
    }

  # Return plotly version.

  if (show_genes_names == FALSE) {

    volcan_Plotly <-  plotly::ggplotly(
      p = p ,
      tooltip = c("fold_change", "log10_pvalues", 'Gene'))

    return(volcan_Plotly)
  }
}
