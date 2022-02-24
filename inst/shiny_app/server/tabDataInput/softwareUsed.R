software_used <- reactive({

    software_used <- MQanalyser::check_software_input(proteoInput())

    message(paste0('Software used: \n', software_used))

    return(software_used)

})

output$softwareUsedBox <- renderInfoBox({

    if (is.null(proteoInput())) {
        return(NULL)
    }

    icon <- "check-square"

    color <- 'green'

    info <- infoBox(
        'Software used:',
        paste0('\n', software_used()),
        #icon = icon("stats", lib = "glyphicon"))
        icon = icon(icon),
        color = color)
    return(info)
})
