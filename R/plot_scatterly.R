plot_scatterly <- function(dep = NULL,
                           x_sample = NULL,
                           y_sample = NULL,
                           intensity_type = NULL,
                           gene_list = NULL,
                           alpha = 0.5,
                           show_lm = TRUE,
                           color = '#56B4E9'){

    df <- as.data.frame(dep@assays@data@listData[[1]])

    df$name <- rownames(df)

    df <- df[,c('name', x_sample, y_sample)]

    colnames(df) = c('Gene', 'X_sample', 'Y_sample')


    # x_sample = 'Benign_1'
    # y_sample = 'Malignant_1'

     p <- ggplot(df, aes(x = X_sample,
                         y = Y_sample))+
         theme_bw()+
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


     # p <- ggplot(df, aes(x = X_sample,
     #                     y = Y_sample,
     #                     text = paste(x_sample,':', format(round(X_sample, 1), nsmall = 1),
     #                                  paste0('\n',y_sample),':', format(round(Y_sample, 1), nsmall = 1),
     #                                  '\nGene:', Gene)))+
     #     theme_bw()+
     #     geom_point(alpha = alpha, size = 2,
     #                color = color)+
     #     ggtitle(label = paste0('Log2 ', intensity_type))+
     #     xlab(label = x_sample)+
     #     ylab(label = y_sample)
     #
#
#      p <- df %>%
#          ggplot(aes(x = X_sample,
#                 y = Y_sample,
#                 text = paste(x_sample,':', format(round(X_sample, 1), nsmall = 1),
#                              paste0('\n',y_sample),':', format(round(Y_sample, 1), nsmall = 1),
#                              '\nGene:', Gene)))+
#         theme_bw()+
#         geom_point(alpha = alpha, size = 2,
#                    color = color)+
#         ggtitle(label = paste0('Log2 ', intensity_type))+
#         xlab(label = x_sample)+
#         ylab(label = y_sample)+
#         geom_smooth()








    if(show_lm == TRUE){
        p <- p+geom_smooth(method = 'lm' )
        # p <- p+geom_smooth(method = "lm", se = 0, color ='black')
    }



    plotly::ggplotly(p = p, tooltip = c('text'))


}
