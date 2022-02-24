profile_reactive <- reactive({

    p <- MQanalyser::plot_profilely(dep = dep(),
                                    centered = input$prof_centered,
                                    intensity_type = input$IntensityType,
                                    color = input$input_col_prof,
                                    angle_labels = input$input_angle_samples,
                                    selected_genes = input$plot_profile_table_rows_selected,
                                    color_selected = input$input_col_sel,
                                    alpha = input$profile_alpha,
                                    plot = TRUE,
                                    clear_selection = input$clear_selection,
                                    prof_genes_de = input$prof_genes_de,
                                    user_genes_de = user_genes_de(),
                                    color_genes_de = input$input_col_prof_de)
    return(p)
})

output$plot_profile <- renderPlotly(

    plotly::ggplotly(
        profile_reactive(),
        tooltip = c('text')
    )%>%

        layout(height = 800, width = 1200)
)


output$plot_profile_table <- DT::renderDataTable({

    if(input$clear_selection == TRUE){
        DT::datatable(
            MQanalyser::plot_profilely(
                dep = dep(),
                intensity_type = input$IntensityType,
                color = NULL,
                angle_labels = NULL,
                selected_genes = NULL,
                color_selected = NULL,
                plot = FALSE),

            selection = 0,
            extensions = 'Scroller',

            options = list(scrollY=500, scrollX=100)
        )
    }else{
        DT::datatable(
            MQanalyser::plot_profilely(
                dep = dep(),
                intensity_type = input$IntensityType,
                color = NULL,
                angle_labels = NULL,
                selected_genes = NULL,
                color_selected = NULL,
                plot = FALSE),
            extensions = 'Scroller',

            options = list(scrollY=500, scrollX=100)
        )
    }
})
