plot_scatterly <- function(dep,
                           log_base = 10, # 10, 2, 'none',
                           x_sample = NULL,
                           y_sample = NULL,
                           intensity_type = 'Intensity',
                           gene_list = NULL){


    df <- as.data.frame(dep@elementMetadata) %>% select(contains(c('name',paste0(intensity_type, '.')))) %>%
        select(-contains('names'))


    colnames(df) <- gsub(paste0(intensity_type, '.'), '', colnames(df))


    # df only with the samples selected

    x_sample = 'Total_309M'
    y_sample = 'Total_309B'

    df <- df %>% select(contains(c('name', x_sample, y_sample)))

    # Remove rows with zero in one of the two rows, or both.
    df[df == 0] <- NA
    df <- na.omit(df)


    # Change the name of the columns to be able to use key = name in aes(), rather than using aes_string()

    colnames(df) = c('Gene', 'X_sample', 'Y_sample')

    # plot_ly(data = df, x = ~log10(Total_309B), y = ~log10(Total_309M), key = ~name ,
    #         marker = list(size = 10,
    #                       color = 'rgba(255, 182, 193, .9)',
    #                       line = list(color = 'rgba(152, 0, 0, .8)',
    #                                   width = 2)))


    if(log_base == 10){
        df[-1] = log10(df[-1])




    } else if(log_base == 2){

        df[-1] = log2(df[-1])


    } else if(log_base == 'none') {
        next
    }





    p <- ggplot(df, aes(x = X_sample, y = Y_sample, key = Gene))+
        geom_point()+
        xlab(label = x_sample)+
        ylab(label = y_sample)


    plotly::ggplotly(p = p, tooltip = 'Gene')
}
