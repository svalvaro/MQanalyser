observeEvent(input$tabs_menu, {

    req(input$enrich_organism)


    if (! input$enrich_organism == 'org.Hs.eg.db' &&
        input$tabs_menu == 'disease-tab') {

        message('alert launch')
        return(
            shinyalert::shinyalert("Disease Analaysis Unavailable",
                                   "This part is only available with human species",
                                   type="error",
                                   closeOnClickOutside = TRUE,
                                   closeOnEsc = TRUE,
                                   timer = 6000)
        )
    }
})
