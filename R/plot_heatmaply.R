plot_heatmaply <- function(dep,
                           intensity_type = 'Intensity'){

    df <- as.data.frame(dep@elementMetadata) %>% select(contains(c('name',paste0(intensity_type, '.')))) %>%
          select(-contains('names'))
    df[-1] <- log2(df[-1])

    df[df == '-Inf'] = 0

    rownames(df) <- df$name
    df$name <- NULL

    colnames(df) <- gsub(paste0(intensity_type, '.'), '', colnames(df))

    #coul <- reactive(colorRampPalette(c(input$col_heatdown,input$col_heatmedium,input$col_heatup))(75))
    coul <- colorRampPalette(c('dodgerblue2','white','red2'))(75)
    dodgerblue2

    #heatmaply(as.matrix(df), colors = coul)

    # Decide whether to do it from the beginning or use the plot_volcano from DEP::
    # it might be interesting to check the centered parameter.

    #The centered parameter for the intensities is explained in the DEP proteomics package.
}
