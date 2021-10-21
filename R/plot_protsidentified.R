#' Title
#'
#' @param data_filt
#'
#' @return
#' @export
#'
#' @examples
plot_protsidentified <- function(data_filt){

    df <- as.data.frame(data_filt@assays@data@listData[[1]])

    df2 <- data.frame(
        Label = colnames(df),
        `Missing Values` = colSums(is.na(df)),
        `Valid Values` = colSums(!is.na(df))
                     )

    df2 <- melt(df2, id.vars = 'Label')

    p <- ggplot(df2, aes(x = Label, y = value, fill = variable))+
        geom_col(color = 'black')+
        theme_bw()+
        ylab('Frequency')+
        theme(legend.title = element_blank(),
              legend.position = 'bottom')+
        scale_fill_manual(values = c('#A4DDED', '#FFDFD3'))

    return(ggplotly(p))

}
