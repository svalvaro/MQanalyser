output$plot_before_normalization <- renderPlotly({
    `Before normalization` = data_filt()
    plotly::ggplotly(
        MQanalyser::plot_normalization_interactive(`Before normalization`)
    )

})

normalized_plot <- reactive({
    `After normalization` = data_norm()

    if (input$normalize_input == TRUE) {

        MQanalyser::plot_normalization_interactive(`After normalization`)

    }else{
        return(NULL)
    }
})

output$plot_after_normalization <- renderPlotly({

    plotly::ggplotly(
        normalized_plot()
    )
})

data_norm <- reactive({

    # If the user wants to normalize

    if (input$normalize_input == TRUE) {

        data_norm <- DEP::normalize_vsn(data_filt())
    } else{
        data_norm <- data_filt()
    }
})
