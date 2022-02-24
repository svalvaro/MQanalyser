interactions_report  <- reactive({
    if (input$interactionReport == TRUE) {
        message('Interactions Plot added to the report')

        return(interactionsTrimmed())
    }else{
        return(NULL)
    }
})
