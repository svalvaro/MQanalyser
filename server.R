function(input, output) {

    options(shiny.maxRequestSize=100*1024^2)## Set maximum upload size to 100MB

    #proteinGroups.txt input

    proteinGroups <- reactive({

        inFile <- input$proteinGroups

        if (is.null(inFile))
            return(NULL)

        df <- read_delim(inFile$datapath, "\t", escape_double = FALSE, trim_ws = TRUE)

        # df <- read_delim("www/proteinGroups_example.txt",
        #                                   "\t", escape_double = FALSE, trim_ws = TRUE)

        #Remove reverse and reverse and contaminants and only identified by site
        df <- df[is.na(df$`Potential contaminant`) & is.na(df$Reverse)  & is.na(df$`Only identified by site`),]

        #Separate the Protein IDs into different rows separated by ;

        #df_separated <- df %>%  separate_rows(c(`Protein IDs`), sep = ';')

        return(df)

    })

    experiment_names <- reactive({

        proteinGroups <- proteinGroups()

        experiment_names <- proteinGroups %>%
                             select(contains('Intensity ')) %>%
                             select(-contains('LFQ'))

        experiment_names <- colnames(experiment_names)

        experiment_names <- gsub('Intensity', '', experiment_names)

        return(experiment_names)

    })

    experiment_design <- reactive({

        inFile <- input$optional_exp_design

        if (is.null(inFile)){

            df <- data.frame(label = experiment_names(),
                             condition = NA,
                             replicate = NA)
        } else{
            df <- read_delim(inFile$datapath,"\t", escape_double = FALSE, trim_ws = TRUE)
        }

        return(df)
    })


    output$experiment_design_out <- DT::renderDataTable({

         DT::datatable(experiment_design(), editable = TRUE)

    })


    data_se <- reactive({

        if (input$IntensityType == 'Intensity') {

            columns = grep('Intensity.', colnames(proteinGroups()))

        } else if (input$IntensityType == 'LFQ') {

            columns = grep('LFQ.', colnames(proteinGroups()))

        } else if (input$IntensityType == 'iBAQ'){
            columns = grep('iBAQ.', colnames(proteinGroups()))
        }

        data_unique <- DEP::make_unique(proteinGroups(), 'Gene names', 'Protein IDs', delim = ';')

        data_se <- DEP::make_se(data_unique, columns = columns, experiment_design())

    })

    data_filt <- reactive({

        #Filter for proteins that are identified in all replicates of at least one condition
        data_filt <- filter_missval(data_se(),thr = 0)
        #plot_missval(data_filt)
    })

    data_norm <- reactive({

        data_norm <- normalize_vsn(data_filt())

        #plot_normalization(data_filt, data_norm)
    })

    data_imp <- reactive({

        data_imp <- impute(data_norm(), fun = "knn", rowmax = 0.9)

        #plot_imputation(data_norm, data_imp)

    })

    data_results <- reactive({
        # Test every sample versus control
        data_diff <- test_diff(data_imp(), type = "control", control = "Ctrl")




        dep <- add_rejections(data_diff, alpha = 0.05, lfc = log2(1.5))

        data_results <- get_results(dep)
    })


    output$proteomics_results <- DT::renderDataTable({

        DT::datatable(data_results())

    })





}
