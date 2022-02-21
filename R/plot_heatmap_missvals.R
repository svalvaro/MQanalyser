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

    Groupings <- cbind(data_filt$replicate, data_filt$condition)

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


    n <- nrow(Groupings)
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    col_vector <- col_vector[1:n]

    p <- gplots::heatmap.2(x = missval, col = c("white", "#71a873"),
                      trace = 'none',
                      #key = FALSE,
                      dendrogram = 'col',
                      #lwid=c(0.1,4),
                      labRow =  FALSE,
                      ColSideColors = col_vector,
                      key.ylab = 'Freq',
                      key.title = 'Valid Values',
                      key.xlab = '',
                      tracecol = 'black',
                      keysize = 1.5,
                      key.xtickfun = function() {
                          list(at = parent.frame()$scale01(c(0,1)),
                                labels = c('Missing','Valid'))
                          }

                    )



}
