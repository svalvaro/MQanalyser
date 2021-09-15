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
    df <- data_to_be_imputed %>% select(-contains(c('group','group_name')))

    df_melted <- melt(df, id.vars = 'Imputed')

#
#     p <-  ggplot(df_melted, aes(y = value, fill = Imputed))  +
#         facet_wrap(.~variable)+
#         geom_histogram(bins = 30, color = 'black',alpha = 0.8)+
#         coord_flip()+
#         theme_bw()+
#         ylab('Frequency')+
#         xlab(expression('Log'[2]*' Intensity'))+
#         theme(legend.position = 'bottom')



    p <-  ggplot(df_melted, aes(x = value, fill = Imputed))+
          geom_histogram(data = subset(df_melted, Imputed == TRUE),
                         alpha = 0.4,
                         #color = 'black',
                         bins = 30)+
        geom_histogram(data = subset(df_melted, Imputed == FALSE),
                       alpha = 0.4,
                       #color = 'black',
                       bins = 30)+
        ylab('Frequency')+
        xlab('Log 2  Intensity')+
        theme_bw()+
        theme(legend.position = 'bottom')+
        scale_fill_manual(values = c('#00539CFF','#EEA47FFF'))

    if (combined == FALSE) {

        p <- p + facet_wrap(.~ variable)

    }

    return(ggplotly(p))

}
