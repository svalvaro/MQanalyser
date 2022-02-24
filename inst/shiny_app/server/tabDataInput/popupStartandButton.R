observeEvent(input$start_input, {

    if(is.null(input$proteinInput) & demo$start == FALSE & IntensityFound() == TRUE){
        shinyalert::shinyalert("Analysis not started", "protein table not uploaded",
                               type="error",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 6000)

    }else if(all(ed_final$data$condition == ' ')){
        shinyalert::shinyalert("Analysis not started",
                               "Provide an Experiment Design",
                               type="error",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 6000)

    }else if(IntensityFound() == FALSE){

        shinyalert::shinyalert("Analysis not started",
                               "Intensity selected was not found. \nSelect another type of Intensity.",
                               type="error",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 6000)
    }else{

        shinyalert::shinyalert("Analysis Started!",
                               "You can now move to the next tab.",
                               type="success",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 10000)
    }
})


output$start_analysis <- renderUI({

    if (is.null(proteoInput())) {
        return(NULL)
    }
    shinyWidgets::actionBttn(
        inputId = "start_input",
        label = "Start Analysis",
        style = "unite",
        color = "primary",
        size = 'lg',
        icon = icon("play")
    )
})
