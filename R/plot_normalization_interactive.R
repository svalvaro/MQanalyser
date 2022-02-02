#' Plot Distribution of the data before an after normalization
#'
#' @param se
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_normalization_interactive <- function(se, ...) {
    # Get arguments from call
    call <- match.call()
    arglist <- lapply(call[-1], function(x) x)
    var.names <- vapply(arglist, deparse, character(1))
    arglist <- lapply(arglist, eval.parent, n = 2)
    names(arglist) <- var.names

    # Show error if inputs are not the required classes
    lapply(arglist, function(x) {
        assertthat::assert_that(inherits(x,
                                         "SummarizedExperiment"),
                                msg = "input objects need to be of class 'SummarizedExperiment'")
        if (any(!c("label", "ID", "condition", "replicate") %in% colnames(colData(x)))) {
            stop("'label', 'ID', 'condition' and/or 'replicate' ",
                 "columns are not present in (one of) the input object(s)",
                 "\nRun make_se() or make_se_parse() to obtain the required columns",
                 call. = FALSE)
        }
    })

    # Function to get a long data.frame of the assay data
    # annotated with sample info
    gather_join <- function(se) {
        assay(se) %>%
            data.frame() %>%
            gather(ID, val) %>%
            left_join(., data.frame(colData(se)), by = "ID")
    }

    df <- map_df(arglist, gather_join, .id = "var") %>%
        mutate(var = factor(var, levels = names(arglist)))

    # Boxplots for conditions with facet_wrap
    # for the original and normalized values
    p <-  ggplot(df, aes(x = ID, y = val, fill = condition)) +
        geom_boxplot(notch = TRUE, na.rm = TRUE) +
        coord_flip() +
        facet_wrap(~var, ncol = 1) +
        labs(x = "", y = 'Log 2 Intensity')+
        theme_bw()+
        scale_fill_brewer(palette = 'Set3')


    #ggplotly(p)
    return(p)

}
