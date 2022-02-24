#just for the report, non interactive
# correlation_plot <- reactive({
#
#
#     p <- MQanalyser::plot_correlationly(dep(), interactive = FALSE)
#
#
# })

output$plot_correlation <- renderPlotly(

    MQanalyser::plot_correlationly(dep()) %>%
        layout(height = 800, width = 800)

)
