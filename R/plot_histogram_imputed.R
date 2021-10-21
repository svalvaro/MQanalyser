#' Plot_histogram
#'
#' @param data_to_be_imputed
#'
#' @return
#' @export
#'
#' @examples
plot_histogram_imputed <- function(data_to_be_imputed,
                                   combined = FALSE){

    colnames(data_to_be_imputed) <- c('Protein.ID',
                                      'Label',
                                      'Log2 Intensity',
                                      'Imputed')

    p <-     ggplot(data_to_be_imputed, aes( x = `Log2 Intensity`,
                                             fill = Imputed))+
                    geom_histogram(alpha = 0.4,
                                   bins = 30,
                                   position = 'identity')+
                    theme_bw()+
                    ylab('Frequency')+
                    xlab('Log 2  Intensity')+
                    theme(legend.position = 'bottom')+
                    scale_fill_manual(values = c('#00539CFF','red'))

    if (combined == FALSE) {

        p <- p + facet_wrap(.~ Label)
    }

    fig <- ggplotly(p)
    return(fig)
}
