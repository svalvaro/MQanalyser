#' Plot correlation interactively
#'
#' @param dep
#' @param intensity_type
#'
#' @return
#' @export
#'
#' @examples
plot_correlationly <- function(dep, interactive = TRUE){

    cor_mat <- cor(assay(dep))

    couls <-grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlGn"))

    p <- heatmaply::heatmaply(cor_mat,
                         colors =  couls(256),
                         key.title = 'Pearson Corr.',
                         label_names = c('row', 'column', 'correlation')
                         )
    if (interactive == FALSE) {
        return(
            gplots::heatmap.2(cor_mat,
                              col = couls(20),
                              trace = 'none',
                              key.title =  'Pearson corr.',
                              density.info = 'none'
            )
        )

    }else{
        return(p)
    }

}
