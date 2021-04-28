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
              plot_method = 'plotly')






    # df <- as.data.frame(dep@assays@data@listData[[1]])
    #
    # df$name <- rownames(df)
    #
    #
    # heatmaply(as.matrix(df),
    #           colors =  rev(RColorBrewer::brewer.pal(11, "RdBu")),
    #           #col_side_colors = Groupings, #Maybe its possible to add a title?
    #           label_names = c('Gene', 'ID', 'Log2 FC'),
    #           key.title = 'Log 2 \nFold Change',
    #           k_row = k_row,
    #           k_col = k_col,
    #           dendrogram = dendogram,
    #           plot_method = 'plotly')
   #
   #
   #
   #  df <- proteinGroups %>% select(contains(c('Gene.',paste0(intensity_type, '.')))) %>%
   #      select(-contains('names'))
   #
   #
   #
   #  #Fix this since it is obtaining the wrong data. I believe.
   #
   #
   #  df <- as.data.frame(dep@elementMetadata) %>% select(contains(c('name',paste0(intensity_type, '.')))) %>%
   #        select(-contains('names'))
   #  df[-1] <- log2(df[-1])
   #
   #  df[df == '-Inf'] = 0
   #
   #  rownames(df) <- df$name
   #  df$name <- NULL
   #
   #  colnames(df) <- gsub(paste0(intensity_type, '.'), '', colnames(df))
   #
   #  #heatmaply(as.matrix(df), colors = coul)
   #
   #  # Decide whether to do it from the beginning or use the plot_volcano from DEP::
   #  # it might be interesting to check the centered parameter.
   #
   #  # Extract row and col data
   #  row_data <- SummarizedExperiment::rowData(dep, use.names = FALSE)
   #
   #
   #  col_data <- SummarizedExperiment::colData(dep) %>%
   #      as.data.frame()
   #
   #  filtered <- dep[row_data$significant, ]
   #
   #
   #  #Groupings <- filtered %>% select(contains(c('ID','condition')))
   #
   #  Groupings <- cbind( filtered$replicate, filtered$condition)
   #
   #  colnames(Groupings) <- c('replicate', 'condition')
   #  #The centered parameter for the intensities is explained in the DEP proteomics package.
   #
   # # if(type == "centered") {
   #      rowData(filtered)$mean <- rowMeans(assay(filtered), na.rm = TRUE)
   #      df <- assay(filtered) - rowData(filtered, use.names = FALSE)$mean
   #  # }
   #  # # Get contrast fold changes ('contrast')
   #  # if(type == "contrast") {
   #  #     df <- rowData(filtered, use.names = FALSE) %>%
   #  #         data.frame() %>%
   #  #         column_to_rownames(var = "name") %>%
   #  #         select(ends_with("_diff"))
   #  #     colnames(df) <-
   #  #         gsub("_diff", "", colnames(df)) %>%
   #  #         gsub("_vs_", " vs ", .)
   #  #     df <- as.matrix(df)
   #  # }
   #
   #
   #  heatmaply(as.matrix(df),
   #            colors =  rev(RColorBrewer::brewer.pal(11, "RdBu")),
   #            col_side_colors = Groupings, #Maybe its possible to add a title?
   #            label_names = c('Gene', 'ID', 'Log2 FC'),
   #            key.title = 'Log 2 \nFold Change',
   #            k_row = k_row,
   #            k_col = k_col,
   #            dendrogram = dendogram,
   #            plot_method = 'plotly')



}
