experiment_design <- reactive({

    if (is.null(proteoInput())) {
        return(NULL)
    }

    inFile <- input$optional_exp_design

    if (is.null(inFile) && demo$start == FALSE){



        df <- data.frame(label = experiment_names(),
                         condition = '',
                         replicate = NA_integer_,
                         Include = TRUE)

    } else if(demo$start == TRUE){

        df <- read.delim('www/data/experiment_design_example.txt')
    }else{

        df <- read.delim(inFile$datapath)
    }

    return(df)
})


expDesignEditable <- reactiveVal()

observeEvent(experiment_design(),{

    if (is.null(proteoInput())) {
        return(NULL)
    }

    print(experiment_design())

    expDesignEditable(experiment_design())

})

output$ed_out <- renderRHandsontable({


    if (is.null(proteoInput())) {
        return(NULL)
    }

    print(paste0('Intensity found: ', IntensityFound()))


    if(IntensityFound() == FALSE){
        return(NULL)
    }

    df <- expDesignEditable()

    rhandsontable(
        df,
        height =  500) %>%
        hot_col('replicate', format = '0a') %>%

        rhandsontable::hot_col('label', readOnly = TRUE)%>%

        hot_table(highlightRow = TRUE) %>%

        hot_col(col = "Include", halign = 'htCenter',
                renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);

                        var col_value = instance.getData()[row][3]

                        if (col_value === false) {

                            td.style.background = 'pink';
                        }
                    }
                ") %>%

        hot_col(col = c("label", "condition", "replicate"),halign = 'htCenter',
                renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);

                        var col_value = instance.getData()[row][3]

                        if (col_value === false) {

                            td.style.background = 'pink';
                        }
                    }
                ") %>%
        hot_col(col = 'replicate', readOnly = TRUE)

})


observeEvent(input$ed_out, {
    userDT <- rhandsontable::hot_to_r(input$ed_out)
    data.table::setDT(userDT)
    userDT[, replicate := seq_len(.N), by = condition][is.na(condition) | condition == "", replicate := NA_integer_]
    expDesignEditable(userDT)
})


ed_final <- reactiveValues()

observeEvent(input$start_input, {

    if (is.null(input$proteinInput) && is.null(input$optional_exp_design) &&
        demo$start == FALSE) {
        return(NULL)
    } else{

        ed_final$data <-  rhandsontable::hot_to_r(input$ed_out)

        # Remove the rows containing FALSE in include.
        print(ed_final$data$Include)

        ed_final$data <- ed_final$data[! ed_final$data$Include == FALSE,]
    }

    print('The experiment design is:')
    print(ed_final$data)

})

output$download_experiment_design <- downloadHandler(

    filename = function(){'experiment_design.txt'},
    content = function(fname){
        write_delim(ed_final$data, fname,delim = '\t')
    }
)

# Dynamic download

output$downloaderExperimentUI <- renderUI({
    if (is.null(ed_final$data)) {
        return(NULL)
    }

    downloadButton(
        outputId = 'download_experiment_design',
        label = 'Download ')

})
