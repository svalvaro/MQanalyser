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

    p <-     ggplot(data_to_be_imputed, aes( x = value, fill = Imputed))+
                    geom_histogram(alpha = 0.4,
                                   bins = 30,
                                   position = 'identity')+
                    theme_bw()+
                    ylab('Frequency')+
                    xlab('Log 2  Intensity')+
                    theme(legend.position = 'bottom')+
                    scale_fill_manual(values = c('#00539CFF','red'))

    if (combined == FALSE) {

        p <- p + facet_wrap(.~ variable)

    }

    return(ggplotly(p))

}
