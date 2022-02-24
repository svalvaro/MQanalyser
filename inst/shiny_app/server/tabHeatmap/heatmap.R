heatmapInteractive <- reactive({
    p <- MQanalyser::plot_heatmaply(dep(),
                                    intensity_type = input$IntensityType,
                                    top_contributors = topContributors())

    return(p)
})
output$heatmaply <- renderPlotly({

    heatmapInteractive() %>%
        layout(height = 1000, width = 1000)

}
)


heatmapPlot <- reactive({
    p <- DEP::plot_heatmap(dep = dep(),
                           type = 'centered')
    return(
        p
    )
})

output$heatMapNonInteractive <- renderPlot(height = 1000, width = 1000,{

    heatmapPlot()

}
)


output$heatmapDownloader <- downloadHandler(

    filename = function(){
        'heatmap.png'
    },

    content = function(file){
        ggplot2::ggsave(file, heatmapPlot())
    }
)



output$heatmapUI <- renderUI({

    message(paste0('Show top contributors: ', input$topContInput))

    if (input$topContInput == TRUE) {
        return(

            shinycssloaders::withSpinner(
                plotlyOutput('heatmaply'),
                image = 'images/logoTransparentSmall.gif',
                image.width = '200px')
        )
    }else{

        return(
            shinycssloaders::withSpinner(

                plotOutput('heatMapNonInteractive'),
                image = 'images/logoTransparentSmall.gif',
                image.width = '200px')
        )
    }
})


output$heatmapContributors <- renderUI({

    if (is.null(dep()) || input$topContInput == FALSE) {
        return(NULL)
    }

    sliderInput(inputId = 'heatMaxContributors',
                label = h4("Select the maximum number of contributors"),
                min = 2,
                max = 200,
                value = 50,
                step = 1)

})

comparisonsHeatmap <- reactive({

    comparisons <- data_results() %>%
        select(contains('vs') & contains('ratio'))

    comparisons <- gsub(pattern = '_ratio',
                        replacement = '',
                        colnames(comparisons))
})

output$comparisonsHeatmap_out  <- renderUI({

    if (is.null(dep()) || input$topContInput == FALSE) {
        return(NULL)
    }

    selectInput(inputId = 'inpComparisonHeatmap',
                label = h4('Select the comparison for the Top Contributors:'),
                choices = unlist(comparisonsHeatmap()),
                selected = comparisonsHeatmap()[1])
})

topContributors <- reactive({
    # It's going to be a vector of N gene names with the highest FC difference

    df <- data_results() %>% select(contains(c("name", paste0(input$inpComparisonHeatmap, '_ratio'))))

    # df <- data_results %>% select(contains(c('name', 'Ctrl_vs_Tumor_ratio')))

    colnames(df)[colnames(df) != 'name' ] <- 'ratioSelectedComparison'

    # Obtain the absolute top contributors
    df$ratioSelectedComparison <- abs(as.numeric(df$ratioSelectedComparison))

    # Sort in descending order and obtain the N highest

    df <- df[order(df$ratioSelectedComparison ,decreasing = TRUE),]

    topContributors <- df[1:input$heatMaxContributors,]$name

    # topContributors <- df[1:50,]$name


    return(topContributors)

})
