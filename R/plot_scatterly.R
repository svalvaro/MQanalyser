#' Scatter plot
#'
#' @param dep
#' @param x_sample
#' @param y_sample
#' @param intensity_type
#' @param alpha
#' @param show_lm
#' @param color
#' @param show_genes_user
#' @param user_genes_de
#' @param color_genes_de
#'
#' @return
#' @export
#'
#' @examples
plot_scatterly <- function(dep = NULL,
                           x_sample = NULL,
                           y_sample = NULL,
                           intensity_type = NULL,
                           alpha = 0.5,
                           show_lm = TRUE,
                           color = '#56B4E9',
                           show_genes_user = FALSE,
                           user_genes_de = NULL,
                           color_genes_de = 'dc143c'
                           ){

    df <- as.data.frame(dep@assays@data@listData[[1]])

    df$name <- rownames(df)

    df <- df[,c('name', x_sample, y_sample)]

    colnames(df) = c('Gene', 'X_sample', 'Y_sample')

     p <- ggplot(df, aes(x = X_sample,
                         y = Y_sample))+
         theme_bw()+
         theme(panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))+
         geom_point(alpha = alpha,
                    size = 2,
                    color = color,
                    aes(x = X_sample,
                        y = Y_sample,
                        text = paste(x_sample,':', format(round(X_sample, 1), nsmall = 1),
                                      paste0('\n',y_sample),':', format(round(Y_sample, 1), nsmall = 1),
                                      '\nGene:', Gene)))+
         ggtitle(label = paste0('Log2 ', intensity_type))+
         xlab(label = x_sample)+
         ylab(label = y_sample)

    if(show_lm == TRUE){
        p <- p+geom_smooth(method = 'lm' )
    }

    if(!is.null(user_genes_de) & show_genes_user ==TRUE){
        p <- p+geom_point(data = df[which(df$Gene %in% user_genes_de),],
                          aes(x = X_sample,
                              y = Y_sample,
                              text = paste(x_sample,':', format(round(X_sample, 1), nsmall = 1),
                                           paste0('\n',y_sample),':', format(round(Y_sample, 1), nsmall = 1),
                                           '\nGene:', Gene)
                          ),
                          alpha = alpha,
                          size = 2,
                          color = color_genes_de)
    }
    # return(
    #     plotly::ggplotly(p = p, tooltip = c('text'))
    # )

     return(
         p
     )
}
