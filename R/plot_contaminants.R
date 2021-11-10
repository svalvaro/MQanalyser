#' Title
#'
#' @param proteoInput
#' @param intensityType
#'
#'
#' @return
#' @export
#'
#' @examples
plot_contaminants <- function(proteoInput,
                              intensityType = 'LFQ',
                              interactive = FALSE
                              ){

    # what_to_plot <- c('frequency', 'intensity')

    df <- proteoInput %>% select(contains(
        c('Experiment', 'Contaminant', 'LFQ','Gene.names')))

    df2 <- reshape2::melt(df, id.var = c('Potential.contaminant','Gene.names'))

    # Change the + for TRUE or FALSEs
    df2$Potential.contaminant <- ifelse(df2$Potential.contaminant == '+',
                                        'Contaminant', 'Not Contaminant')

    # Log2 Value, and removed -Inf
    df2$value <- log2(df2$value)

    df2 <- df2[!df2$value == '-Inf',]

    # Gsub the intensity type

    df2$variable <- gsub(pattern = 'LFQ.intensity.', replacement = '',
                         x = df2$variable)

    colnames(df2)[colnames(df2)=='value'] <- 'Log2.Intensity'

    # Colors for the plot

    if (length(unique(df2$Potential.contaminant)) == 2) {
        colors <- c('#A4DDED', '#FFDFD3')
    }else{
        colors <- '#FFDFD3'
    }


    p <- ggplot(df2, aes( y = Log2.Intensity,  key = Gene.names))+
            #gghalves::geom_half_point()+
            geom_jitter(aes(x = Potential.contaminant,
                            colour = Potential.contaminant))+
            facet_wrap(.~variable)+
            ggtitle('Intensity distribution')+
            ylab('Log2 Intensity')+
            xlab('')+
            theme_bw()+
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.title = element_blank()

                )+
            scale_colour_manual(values = colors)

    if (interactive == TRUE) {
        return(
            ggplotly(p, tooltip = c('y', 'key', 'colour'))
        )
    }else{
        return(
            p
        )
    }
}
