# Select the sample to plot in the scatter plot

output$x_sample_selector <- renderUI({

    selectInput(inputId = 'x_sample_input',
                label = h4('Select the sample to plot in the x_axis:'),

                choices = unlist(dep()$ID),selected = unlist(dep()$ID)[1])
})


output$y_sample_selector <- renderUI({

    selectInput(inputId = 'y_sample_input',
                label = h4('Select the sample to plot in the y_axis:'),

                choices = unlist(dep()$ID),selected = unlist(dep()$ID)[2])
})


scatter_plot <- reactive({

    MQanalyser::plot_scatterly(dep = dep(),
                               x_sample = input$x_sample_input,
                               y_sample = input$y_sample_input,
                               color = input$color_scatter,
                               show_genes_user = input$showgenes,
                               user_genes_de = user_genes_de(),
                               color_genes_de = input$color_de_scatter,
                               alpha = input$input_alpha,
                               intensity_type = input$IntensityType,
                               show_lm = input$input_lm)
})

output$scatterplot <- renderPlotly(
    plotly::ggplotly(
        scatter_plot(), tooltip = c('text')
    ) %>%
        layout(height = 1000, width = 1000 )
)
