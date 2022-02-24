expdesign_report <- reactive({
    if (input$experimentReport == TRUE) {
        message('Experiment design added to the report')
        return(ed_final$data)
    }else{
        return(NULL)
    }
})
