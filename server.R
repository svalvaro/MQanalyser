function(input, output) {


    # exp_design_na <- data.frame(label = experiment_names(),
    #                             condition = NA,
    #                             replicate = NA)


    options(shiny.maxRequestSize=100*1024^2)## Set maximum upload size to 100MB

    #proteinGroups.txt input

    proteinGroups <- reactive({

        inFile <- input$proteinGroups

        if (is.null(inFile))
            return(NULL)

        df <- read.delim(inFile$datapath)

        #df <- read.delim('/home/alvaro/Downloads/proteinGroups_example(2).txt')

        #Remove reverse and reverse and contaminants and only identified by site


        df <- df[(df$Potential.contaminant == '') & (df$Reverse == '')  & (df$Only.identified.by.site==''),]

        #Separate the Protein IDs into different rows separated by ;

        # proteinGroups <- df
        # df <- NULL

        #df_separated <- df %>%  separate_rows(c(`Protein IDs`), sep = ';')

        return(df)

    })

    experiment_names <- reactive({

        experiment_names <- proteinGroups() %>%
                            select(contains('Intensity.')) %>%
                            select(-contains('LFQ'))

        experiment_names <- colnames(experiment_names)

        experiment_names <- gsub('Intensity.', '', experiment_names)

        return(experiment_names)

    })

    experiment_design <- reactive({

        #experiment_design <- read.delim('/home/alvaro/Downloads/experimental_design_example(2).txt')

        inFile <- input$optional_exp_design

        if (is.null(inFile)){

            df <- data.frame(label = experiment_names(),
                             condition = ' ',
                             replicate = ' ')
        } else{
            df <- read.delim(inFile$datapath)
        }

        return(df)
    })


    ed_final <- reactiveValues()

    output$ed_out <- renderRHandsontable({
        rhandsontable(experiment_design()) %>%
            hot_col('label', readOnly = TRUE) %>%
            hot_col('replicate', format = '0a')
    })


    observeEvent(input$runButton, {
        ed_final$data <-  rhandsontable::hot_to_r(input$ed_out)

    })

    output$IntensityFound <- renderText({

        columns = grep(paste0(input$IntensityType,'.'), colnames(proteinGroups()))

        # columns = grep('LFQ.', colnames(proteinGroups))

        if (length(columns) == 0) {

            print(paste0(input$IntensityType, ' was not found. \nSelect another type of intensity.'))

        } else{
            print(paste0(input$IntensityType, ' was found. \nContinue with the analysis.'))


        }
        })


    data_se <- reactive({

        columns = grep(paste0(input$IntensityType,'.'), colnames(proteinGroups()))

        # Adds two columns at the end with an unique gene and protein name.
        data_unique <- DEP::make_unique(proteinGroups(), 'Gene.names', 'Protein.IDs', delim = ';')

        # data_unique <- DEP::make_unique(proteinGroups, 'Gene.names', 'Protein.IDs', delim = ';')

        # Creates a SummarizedExperiment,
        data_se <- DEP::make_se(data_unique, columns = columns, ed_final$data)
        # data_se <- DEP::make_se(data_unique, columns = columns, experiment_design)
        #View(as.data.frame(data_se@elementMetadata))

    })



    data_filt <- reactive({

        #Imputation should not be done for proteins with too many NAs
        #We set a threshold for the allowed number of missing values per condition

        # Check number of replicates
        if(max(ed_final$data$replicate)<3){
            threshold <-0 #If there are two replicates, NA accepted is 0.
        } else  if(max(ed_final$data$replicate)==3){
            threshold <-1 #If there are three replicates,  NA accepted is 1.
        } else if(max(ed_final$data$replicate)<6 ){
            threshold <-2 #If there are 4 or 5 replicates,  NA accepted is 2.
        } else if (max(ed_final$data$replicate)>=6){
            threshold<-trunc(max(ed_final$data$replicate)/2) #If there are 6 or more. NA accepted is half of the max.
        }

        data_filt <- DEP::filter_missval(data_se(),thr = threshold)

        # data_filt <- DEP::filter_missval(data_se,thr = threshold)

        #plot_missval(filter_missval(data_se,thr = 5))

        #plot_detect(data_filt)
        #plot_coverage(data_filt)
    })

    data_norm <- reactive({

        data_norm <- DEP::normalize_vsn(data_filt())

        # data_norm <- DEP::normalize_vsn(data_filt)

        #meanSdPlot(data_norm)
        #data_norm <- normalize_vsn(data_filt)
        #plot_normalization(data_filt, data_norm)
    })

    data_imp <- reactive({

        if(input$input_imputation == 'MinProb'){
            data_imp <- DEP::impute(data_norm(), fun = "MinProb", q = 0.05)
        } else if(input$input_imputation == 'knn'){
            data_imp <- DEP::impute(data_norm(), fun = "knn", k = 10, rowmax = 0.9)
        }else if(input$input_imputation == 'MLE'){
            data_imp <- DEP::impute(data_norm(), fun = "MLE")
        }else if(input$input_imputation == 'none'){
            data_imp <- data_norm()
        } else{
            data_imp <- DEP::impute(data_norm(), fun = input$input_imputation)
        }

        # data_imp <- DEP::impute(data_norm, fun = 'MinProb', q = 0.05)

        #plot_imputation(data_norm, data_imp)

        #data_imp <- impute(data_norm, fun = "knn", rowmax = 0.9)
    #  plot_imputation(data_norm, data_imp)

    })

    dep <- reactive({



        # data_diff_all_contrasts <- MQanalyser::test_limma(data_imp, type = "all")

        data_diff_all_contrasts <- MQanalyser::test_limma(data_imp(), type = "all")


        #data_diff_all_contrasts <- DEP::test_diff(data_imp(), type = "all")

        # dep <- add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))

        dep <- DEP::add_rejections(data_diff_all_contrasts, alpha = input$input_pvalue, lfc = log2(input$input_fc))


    })

    data_results <- reactive({

        # Generate a results table
        data_results <- DEP::get_results(dep())
    })

    output$significant_proteins <- renderText({
        # Number of significant proteins
        significant_proteins <- data_results() %>% filter(significant) %>% nrow()
        HTML(
            paste0("There are: ", "<b>",significant_proteins," out of ",nrow(data_results()),"</b>"," signifcant proteins.")
        )
    })

    output$proteomics_results <- DT::renderDataTable({

        DT::datatable(data_results())
    })

    # Download the proteomics_results

    output$download_proteomics <- downloadHandler(
        filename = function(){'proteomics_results.csv'},
        content = function(fname){
            write.csv(data_results(), fname)
        }
    )


    # Heatmap

    output$heatmaply <- renderPlotly(MQanalyser::plot_heatmaply(dep(),
                                                                intensity_type = input$IntensityType,
                                                                dendogram = input$dendogram_input,
                                                                k_row = input$k_row_input,
                                                                k_col = input$k_col_input

                                                                ) %>%
                                   layout(height = 1000, width = 1000)

    )



    #Comparison to check.

    comparisons <- reactive({

        comparisons <- data_results() %>% select(contains('vs') & contains('significant'))

        comparisons <- gsub(pattern = '_significant', replacement = '',colnames(comparisons))


    })


    output$comparisons_out  <- renderUI({
        selectInput(inputId = 'comparison_input',
                    label = h4('Select the comparison:'),
                    choices = unlist(comparisons()),
                    selected = comparisons()[1])
    })


    # Select the sample to plot in the scatter plot

    output$x_sample_selector <- renderUI({

        selectInput(inputId = 'x_sample_input',label = h4('Select the sample to plot in the x_axis:'),

                    choices = unlist(dep()$ID),selected = unlist(dep()$ID)[1])
    })


    output$y_sample_selector <- renderUI({

        selectInput(inputId = 'y_sample_input',label = h4('Select the sample to plot in the y_axis:'),

                    choices = unlist(dep()$ID),selected = unlist(dep()$ID)[2])
    })


    output$scatterplot <- renderPlotly(MQanalyser::plot_scatterly(dep = dep(),
                                                                    # log_base =2,
                                                                     x_sample = input$x_sample_input,
                                                                     y_sample = input$y_sample_input,
                                                                     gene_list = NULL,
                                                                     alpha = input$input_alpha,
                                                                     intensity_type = input$IntensityType) %>%

                                                                      layout(height = 1000, width = 1000 )
                                       )

    # output$scatterplot <- renderPlotly(MQanalyser::plot_scatterly(dep = dep(),
    #                                                               log_base =2,
    #                                                               x_sample = input$x_sample_input,
    #                                                               y_sample = input$y_sample_input,
    #                                                               gene_list = NULL,
    #                                                               alpha = 0.05,
    #                                                               intensity_type = input$IntensityType
    # ) %>%
    #
    #     layout(height = 1000, width = 1000))
    #





    output$volcano_plot <- renderPlotly(MQanalyser::plot_volcano(proteomics_results = data_results(),
                                                                 sample_comparison = input$comparison_input,
                                                                 foldchange_cutoff = input$input_fc,
                                                                 p_value_cutoff = input$input_pvalue) %>%

                                                                layout(height = 1000, width = 1000))


    output$plot_correlation <- renderPlotly(MQanalyser::plot_correlationly(dep()) %>%

                                               layout(height = 1000, width = 1000))




}
