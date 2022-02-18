#' Title
#'
#' @param dep
#' @param intensity_type
#'
#' @return
#' @export
#'
#' @examples
plot_heatmaply <- function(dep,
                           intensity_type = 'LFQ',
                           top_contributors = NULL,
                           interactive = TRUE){

    row_data <- rowData(dep, use.names = FALSE)

    col_data <- colData(dep) %>%
        as.data.frame()

    # Filter for significant proteins only
    filtered <- dep[row_data$significant, ]

    #Groupings <- filtered %>% select(contains(c('ID','condition')))

    Groupings <- cbind( filtered$replicate, filtered$condition)

    colnames(Groupings) <- c('replicate', 'condition')

    # Get centered intensity values ('centered')

    rowData(filtered)$mean <- rowMeans(assay(filtered), na.rm = TRUE)

    df <- assay(filtered) - rowData(filtered, use.names = FALSE)$mean


    # Filter for only the not contributors

    if (!is.null(top_contributors)) {

        df <- df[rownames(df) %in% top_contributors,]
    }


    p <- heatmaply::heatmaply(as.matrix(df),
              colors =  rev(RColorBrewer::brewer.pal(11, "RdBu")),
              col_side_colors = Groupings,
              label_names = c('Gene', 'ID', 'Log2 FC'),
              key.title = 'Log 2 \nFold Change',
              dendrogram = 'both',
              plot_method = 'plotly')

    if (interactive == FALSE) {
        couls <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))

        return(

            gplots::heatmap.2(as.matrix(df),
                              col = couls(100),
                              trace = 'none',
                              key.title =  'Log2 FC.',
                              density.info = 'none')
        )
    }else{
        return(p)
    }
}
