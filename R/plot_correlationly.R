#' Plot correlation interactively
#'
#' @param dep
#' @param intensity_type
#'
#' @return
#' @export
#'
#' @examples
plot_correlationly <- function(dep){

    cor_mat <- cor(assay(dep))

    couls <-grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))

    p <- heatmaply::heatmaply(cor_mat,
                         colors =  couls(256),
                         key.title = 'Pearson Corr.',
                         label_names = c('row', 'column', 'correlation')
                         )
    return(p)
}
