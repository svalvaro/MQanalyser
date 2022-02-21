#' Title
#'
#' @param dep
#'
#' @return
#' @export
#'
#' @examples
plot_heatmap_missvals <- function(data_filt){

    row_data <- rowData(data_filt, use.names = FALSE)

    col_data <- colData(data_filt) %>%
        as.data.frame()

    Groupings <- cbind( data_filt$replicate, data_filt$condition)

    colnames(Groupings) <- c('replicate', 'condition')


    se_assay <- SummarizedExperiment::assay(data_filt)
    # Show error if there are no missing values
    if(!any(is.na(se_assay))) {
        stop("No missing values in '", deparse(substitute(se_assay)), "'",
             call. = FALSE)
    }

    # Make assay data binary (1 = valid value, 0 = missing value)
    df <- se_assay %>% data.frame(.)
    missval <- df[apply(df, 1, function(x) any(is.na(x))), ]
    missval <- ifelse(is.na(missval), 0, 1)


    p <- heatmaply::ggheatmap(
        x = missval,
        colors  = c("white", "#71a873"),
        col_side_colors = Groupings,
        hide_colorbar = TRUE,
        dendrogram = 'both',
        show_dendrogram = c(FALSE, TRUE),
        showticklabels = c(TRUE, FALSE))

}
