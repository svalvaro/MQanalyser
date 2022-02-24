demo <- reactiveValues(start = FALSE)

observeEvent(input$Demo, {
    req(input$Demo)
    demo$start <-  TRUE

    shinyalert::shinyalert("Demo Data Loaded", "Press Start Analysis when you are ready!",
                           type="success",
                           closeOnClickOutside = TRUE,
                           closeOnEsc = TRUE,
                           timer = 60000)
})
