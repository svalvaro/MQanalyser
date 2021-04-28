#' Profile Plot
#'
#' @param dep
#'
#' @return
#' @export
#'
#' @examples
plot_profilely <- function(dep,
                           intensity_type = 'LFQ',
                           color = '#56B4E9',
                           angle_labels = 45){

    row_data <- rowData(dep, use.names = FALSE)

    col_data <- colData(dep) %>%
        as.data.frame()

    # Filter for significant proteins only
    filtered <- dep[row_data$significant, ]



    # Get centered intensity values ('centered')
#
#     rowData(filtered)$mean <- rowMeans(assay(filtered), na.rm = TRUE)
#
#     df <- as.data.frame(assay(filtered) - rowData(filtered, use.names = FALSE)$mean)

    df <- as.data.frame(assay(filtered))
#
    df$name <- rownames(df)

    df_melt <- melt(df, id.vars = 'name')

    colnames(df_melt) <- c('Gene', 'Label', 'Intensity') #It is centered intenseity (removed)


    p <- ggplot(df_melt, aes(x=Label,
                             y= Intensity,
                             text = paste('\nGene:', Gene,
                                          '\nLabel:', Label,
                                          paste0('\nLog 2 ', intensity_type,': '), format(round(Intensity,1),nsmall =1)
                                          )))+
            geom_line(aes(group=Gene), color= color)+
            ylab(paste0('Log2  ', intensity_type))+
            theme(axis.text.x = element_text(angle = angle_labels))

    ggplotly(p,tooltip = c('text'))

}
