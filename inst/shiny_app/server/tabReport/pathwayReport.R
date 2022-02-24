pathway_report <- reactive({
    if (input$pathwayReport == TRUE) {
        message('Pathway Plot added to the report')
        return(pathway_plot())
    }else{
        return(NULL)
    }
})
