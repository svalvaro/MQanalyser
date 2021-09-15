#' Plot_histogram
#'
#' @param data_to_be_imputed
#'
#' @return
#' @export
#'
#' @examples
plot_histogram_imputed <- function(data_to_be_imputed){
    df <- data_to_be_imputed %>% select(-contains(c('group','group_name')))

    df_melted <- melt(df, id.vars = 'Imputed')


    p <-  ggplot(df_melted, aes(y = value, fill = Imputed))  +
        facet_wrap(.~variable)+
        geom_histogram(bins = 30, color = 'black',alpha = 0.8)+
        coord_flip()+
        theme_bw()+
        ylab('Frequency')+
        xlab(expression('Log'[2]*' Intensity'))+
        theme(legend.position = 'bottom')


    return(p)

}
