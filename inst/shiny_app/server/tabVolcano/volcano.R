# Comparison to check in the volcano plot

comparisons <- reactive({

    comparisons <- data_results() %>%
        select(contains('vs') & contains('significant'))

    comparisons <- gsub(pattern = '_significant',
                        replacement = '',
                        colnames(comparisons))
})

output$comparisons_out  <- renderUI({
    selectInput(inputId = 'comparison_input',
                label = h4('Select the comparison:'),
                choices = unlist(comparisons()),
                selected = comparisons()[1])
})

output$font_gene_labels <- renderUI({
    if (input$showGeneNames == FALSE) {
        return(NULL)
    }

    shiny::sliderInput(
        inputId = 'font_gene_names',
        label = 'Adjust the font size of the labels:',
        min = 1,
        max = 10,
        value = 5
    )
})

# Create the volcano, it has two options: To be reactive with plotly,
# Or to allow selection of points with the brush.
# Each option has to be rendered in a diferent way with :
# renderPlot or renderPlotly

volcano_Plot <- reactive({

    coord_x  <- NULL
    coord_y <- NULL

    if(input$modify_axis == TRUE){
        coord_x  <- input$range_fc
        coord_y <- input$range_pvalue
    }

    if (input$showGeneNames == FALSE) {

        volcano_Plot <- MQanalyser::plot_volcano(
            proteomics_results = data_results(),
            sample_comparison = input$comparison_input,
            foldchange_cutoff = input$input_fc,
            p_value_cutoff = input$input_pvalue,
            color_up = input$col_upregulated,
            color_down = input$col_downregulated,
            p_adj = input$p_adj_input,
            show_genes_user = input$showgenes_volcano,
            user_genes_de = user_genes_de(),
            color_genes_de = input$col_selected,
            alpha = input$volc_alpha,
            coord_x = coord_x,
            coord_y = coord_y,
            show_genes_names = FALSE
        )
    }else{

        volcano_Plot <- MQanalyser::plot_volcano(
            proteomics_results = data_results(),
            sample_comparison = input$comparison_input,
            foldchange_cutoff = input$input_fc,
            p_value_cutoff = input$input_pvalue,
            color_up = input$col_upregulated,
            color_down = input$col_downregulated,
            p_adj = input$p_adj_input,
            show_genes_user = input$showgenes_volcano,
            user_genes_de = user_genes_de(),
            color_genes_de = input$col_selected,
            alpha = input$volc_alpha,
            coord_x = coord_x,
            coord_y = coord_y,
            show_genes_names = TRUE,
            brushed_Points = input$brush_volcano,
            font_gene_names = input$font_gene_names)
    }

    return(volcano_Plot)
})

# Volcano for the report, not plotly:

volcano_non_interactive <- reactive({

    coord_x  <- NULL
    coord_y <- NULL

    if(input$modify_axis == TRUE){
        coord_x  <- input$range_fc
        coord_y <- input$range_pvalue
    }

    volcano_Plot <- MQanalyser::plot_volcano(
        proteomics_results = data_results(),
        sample_comparison = input$comparison_input,
        foldchange_cutoff = input$input_fc,
        p_value_cutoff = input$input_pvalue,
        color_up = input$col_upregulated,
        color_down = input$col_downregulated,
        p_adj = input$p_adj_input,
        show_genes_user = input$showgenes_volcano,
        user_genes_de = user_genes_de(),
        color_genes_de = input$col_selected,
        alpha = input$volc_alpha,
        coord_x = coord_x,
        coord_y = coord_y,
        show_genes_names = TRUE,
        brushed_Points = input$brush_volcano,
        font_gene_names = input$font_gene_names)

    return(volcano_Plot)
})

output$volcano_plot_plotly <- renderPlotly({

    coord_x  <- NULL
    coord_y <- NULL

    if(input$modify_axis == TRUE){
        coord_x  <- input$range_fc
        coord_y <- input$range_pvalue
    }

    return(
        volcano_Plot() %>%
            layout(height = 1000, width = 1000)
    )
})

output$volcano_plot_genes <- renderPlot(height = 1000, width = 1000,{

    coord_x  <- NULL
    coord_y <- NULL

    if(input$modify_axis == TRUE){
        coord_x  <- input$range_fc
        coord_y <- input$range_pvalue
    }

    return(
        volcano_Plot()

    )
})

output$volcano_final <- renderUI({

    if (input$showGeneNames == TRUE) {
        return(
            plotOutput('volcano_plot_genes',
                       brush = 'brush_volcano')
        )
    }

    if (input$showGeneNames == FALSE) {
        return(

            plotlyOutput('volcano_plot_plotly')
        )
    }
})


# Download the volcano plot when not using plotly

output$downloaderVolcano <- renderUI({
    if (input$showGeneNames == TRUE) {
        shiny::downloadButton('downloadvolcano',
                              'Download the Volcano Plot')
    }
})

output$downloadvolcano <- downloadHandler(

    filename = function(){
        'volcano.png'
    },

    content = function(file){
        ggplot2::ggsave(file, volcano_Plot())
    }
)

