plot_scatterly <- function(dep = NULL,
                           log_base = 10, # 10, 2, 'none',
                           x_sample = NULL,
                           y_sample = NULL,
                           intensity_type = 'LFQ',
                           gene_list = NULL,
                           alpha = 0.5,
                           color = '#56B4E9'){


    df <- as.data.frame(dep@elementMetadata) %>% select(contains(c('name',paste0(intensity_type, '.')))) %>%
        select(-contains('names'))



    if(intensity_type == 'Intensity'){

        colnames(df) <- gsub(paste0(intensity_type, '.'), '', colnames(df))
    } else{#For LFQ/iBAQ
        colnames(df) <- gsub(paste0(intensity_type, '.intensity.'), '', colnames(df))
    }




    # Match the column name with the id

    names_to_match <- data.frame(label = dep$label,
                                 ID = dep$ID)


    names(df) <- plyr::mapvalues(names(df), from = names_to_match$label, to = names_to_match$ID)




    # df only with the samples selected


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

        tittle = paste0('Log10 ', intensity_type)

    } else if(log_base == 2){

        df[-1] = log2(df[-1])
        tittle = paste0('Log2 ', intensity_type)

    } else if(log_base == 'none') {
        tittle = paste0('Raw ', intensity_type, ' values')
        next
    }


    p <- ggplot(df, aes(x = X_sample,
                        y = Y_sample,
                        text = paste(x_sample,':', format(round(X_sample, 1), nsmall = 1),
                                     paste0('\n',y_sample),':', format(round(Y_sample, 1), nsmall = 1),
                                     '\nGene:', Gene)))+
        geom_point(alpha = alpha, size = 2,
                   color = color)+
        ggtitle(tittle)+
        xlab(label = x_sample)+
        ylab(label = y_sample)


    plotly::ggplotly(p = p, tooltip = c('text'))
}
