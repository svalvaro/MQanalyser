# Selec the NAs allowed

output$na_threshold  <- renderUI({

    # Check number of replicates

    n_replicates <- max(ed_final$data$replicate)

    message(paste0('The number of replicates is: ', n_replicates))

    if(n_replicates < 3){
        threshold <-0 #If there are two replicates, NA accepted is 0.
    } else  if(n_replicates == 3){
        threshold <-1 #If there are three replicates,  NA accepted is 1.
    } else if(n_replicates < 6 ){
        threshold <-2 #If there are 4 or 5 replicates,  NA accepted is 2.
    } else if (n_replicates >= 6){
        threshold <- trunc(n_replicates / 2) #If there are 6 or more. NA accepted is half of the max.
    }

    sliderInput(inputId = 'nas_threshold',
                label = h4('Select the number of missing values allowed in each group'),
                min = 0,
                max = n_replicates,
                value = threshold,
                step = 1)
})

data_filt <- reactive({

    if (is.null(data_se())) {
        return(NULL)
    }

    # Avoid warning message while rendering
    shiny::req(input$nas_threshold)

    #Imputation should not be done for proteins with too many NAs
    #We set a threshold for the allowed number of missing values per condition
    # Threshold is per group.
    # So if threshold is 0 it means that for a given protein, at least
    # all samples in one of the groups must have non NAs. In the other group,
    # for that given protein NAs are allowed.

    data_filt <- DEP::filter_missval(data_se(),
                                     thr = as.integer(input$nas_threshold))

    return(data_filt)
})


missvalues_plot <- reactive({
    p <- MQanalyser::plot_protsidentified(data_filt())

    return(p)
})

output$barplot_missvals <- renderPlotly(
    plotly::ggplotly(missvalues_plot())%>%
        layout(height = 800, width = 850)
    #MQanalyser::plot_protsidentified(data_filt)
)


output$heatmap_nas <- renderPlot(height = 800,width = 700,{

    if (is.null(data_filt())) {
        return(NULL)
    }

    if (nrow(data_filt())==nrow(data_se())) {
        message("No missing values, the heatmap will not be plotted")
        return(NULL)
    }

    # Make it into plotly and iteractive!
    # DEP::plot_missval(data_filt())
    MQanalyser::plot_heatmap_missvals(data_filt())
})
