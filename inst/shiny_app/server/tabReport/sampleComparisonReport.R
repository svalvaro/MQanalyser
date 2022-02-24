# Scatter Plot

scatter_report <- reactive({

    if ("scatter" %in% input$sampleReport) {

        message('Scatter plot added to the report')
        return(scatter_plot())

    }else{
        return(NULL)
    }
})

# Correlation plot

correlation_report <- reactive({
    # Much more trickier since the function does not return a plot, so
    # it will return the dep() and the function plot_correlationly will
    # be called from the Rmd

    if ("Correlation" %in% input$sampleReport) {

        message('Correlation plot added to the report')
        return(dep())

    }else{
        return(NULL)
    }
})

# PCA

PCA_report <- reactive({

    if ("PCA" %in% input$sampleReport) {

        message('PCA plot added to the report')
        return(pca_reactive())

    }else{
        return(NULL)
    }
})
