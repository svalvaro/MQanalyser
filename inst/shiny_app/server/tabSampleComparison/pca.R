# Number of proteins that are selected to calculate the PCA
output$pca_number_proteins  <- renderUI({

    var <- apply(assay(dep()), 1, sd)

    if (length(var)>500) {
        value = 500
    }else{
        value = length(var)
    }

    sliderInput(inputId = 'pca_proteins',
                label = h4('Select the number of proteins:'),
                min = 2,
                max = length(var),
                value = value,
                step = 1)
})

# PCA plot

pca_reactive <- reactive({

    req(input$pca_proteins)

    pca_reactive <- MQanalyser::plot_pca_improved(dep = dep(),
                                                  PC_x = 1,
                                                  PC_y = 2,
                                                  label_name = input$pca_label,
                                                  n = input$pca_proteins)
    return(pca_reactive)

})

output$pca_plot <- renderPlot(height = 800, width = 1200,{

    pca_reactive()
})

# Download button for the PCA Plot
output$downloadPCA <- downloadHandler(

    filename = function(){
        'pca_plot.png'
    },

    content = function(file){
        ggplot2::ggsave(file, pca_reactive())
    }
)
