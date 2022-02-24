# Contaminants

contaminants_report <- reactive({
    if ("Contaminants" %in% input$preprocessingReport) {

        message('Contaminants added to the report')
        return(contaminants_non_interactive())
    }else{
        return(NULL)
    }
})

# Missing Values

missingValues_report <- reactive({
    if ("missing" %in% input$preprocessingReport) {

        message('Missing values added to the report')
        return(missvalues_plot())

    }else{
        return(NULL)
    }
})

# Normalization

normalization_report <- reactive({

    if ("Normalization" %in% input$preprocessingReport) {

        message('Normalized plot added to the report')
        return(normalized_plot())

    }else{
        return(NULL)
    }
})

# Imputation

imputation_report <- reactive({

    if ("Imputation" %in% input$preprocessingReport) {

        message('Imputation plot added to the report')
        return(imputation_plot())

    }else{
        return(NULL)
    }

})
