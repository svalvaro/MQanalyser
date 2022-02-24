#  Chose the parameter scale if imputation selected == Manual

output$manual_imputation_scale  <- renderUI({

    if (input$input_imputation == 'Manual') {

        sliderInput(inputId = 'input_scale',
                    label = h5('Sets the width of the distribution relative
                                   to the standard deviation of the original distribution.'),
                    min = 0.05,
                    max = 0.95,
                    value = 0.3,
                    step = 0.05)
    }else{
        return(NULL)
    }
})

# Chose the parameter shift if imputation selected == Manual

output$manual_imputation_shift  <- renderUI({

    if (input$input_imputation == 'Manual') {

        sliderInput(inputId = 'input_shift',
                    label = h5(' Sets the left-shift of the distribution
                                   (in standard deviations) from the median of
                                   the original distribution.'),
                    min = 0,
                    max = 10,
                    value = 1.8,
                    step = 0.1)
    }else{
        return(NULL)
    }
})

data_imp <- reactive({

    if(input$input_imputation == 'Manual'){
        data_imp <-DEP::impute(data_norm(),
                               fun = "man",
                               shift = input$input_shift,
                               scale = input$input_scale)
    }else if(input$input_imputation == 'MinProb'){
        data_imp <- DEP::impute(data_norm(),
                                fun = "MinProb", q = 0.05)
    } else if(input$input_imputation == 'knn'){
        data_imp <- DEP::impute(data_norm(),
                                fun = "knn",
                                k = 10, rowmax = 0.9)
    }else if(input$input_imputation == 'MLE'){
        data_imp <- DEP::impute(data_norm(), fun = "MLE")
    }else if(input$input_imputation == 'none'){
        data_imp <- data_norm()
    } else{
        data_imp <- DEP::impute(data_norm(),
                                fun = input$input_imputation)
    }

    return(data_imp)

})

data_to_be_imputed <- reactive({

    # Obtain the data before being imputed
    filtered <- as.data.frame(data_filt()@assays@data)

    filtered$Protein.ID <- rownames(filtered)

    filtered <- filtered %>% select(-contains('group'))

    filtered_melt <- reshape2::melt(filtered, id.vars = 'Protein.ID')

    # Create a new column calledd imputed
    filtered_melt$Imputed <- FALSE

    # If the value is NA, it will be imputed in the next step
    filtered_melt$Imputed[is.na(filtered_melt$value)] <- TRUE

    if (input$input_imputation == 'none') {
        message('No imputation')
        filtered_melt$Imputed <- FALSE
    }

    # Now obtain the already imputed values:

    # imputed <- as.data.frame(data_imp@assays@data)
    imputed <- as.data.frame(data_imp()@assays@data)

    imputed <- imputed %>% select(-contains('group'))

    imputed$Protein.ID <- rownames(imputed)

    imputed_melt <- reshape2::melt(imputed, id.vars = 'Protein.ID')

    # The column imputed_melt contained the data of the imputed table
    # and added a column specifying whether a protein in a specific group
    # has been imputed or not. This is done by matching it to the
    # filtered table.
    # In the filtered table it is possible to know which one will be imputed
    # since it is in the form of NAs.

    imputed_melt$Imputed <- filtered_melt$Imputed[
        imputed_melt$Protein.ID == filtered_melt$Protein.ID &&
            imputed_melt$variable == filtered_melt$variable]


    return(imputed_melt)
})

imputation_plot <- reactive({

    p <- MQanalyser::plot_histogram_imputed(

        data_to_be_imputed = data_to_be_imputed(),
        bins = input$imputation_bins,
        combined = input$combined_imputation)

    return(p)
})

output$imputation <- renderPlotly(

    plotly::ggplotly(
        imputation_plot()
    ) %>%
        layout(height = 900, width = 1400)
)
