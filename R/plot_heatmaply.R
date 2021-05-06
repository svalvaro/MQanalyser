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
                           #type = 'centered',
                           dendogram = 'both',
                           k_row = 0,
                           k_col = 0){

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


    heatmaply(as.matrix(df),
              colors =  rev(RColorBrewer::brewer.pal(11, "RdBu")),
              col_side_colors = Groupings, #Maybe its possible to add a title?
              label_names = c('Gene', 'ID', 'Log2 FC'),
              key.title = 'Log 2 \nFold Change',
              k_row = k_row,
              k_col = k_col,
              dendrogram = dendogram,
              plot_method = 'plotly')+

       annotate('rect',
                xmin = 0,
                xmax = 20,
                ymin = 200,
                ymax = 250,
                alpha=0.3)


}
