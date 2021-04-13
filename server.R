function(input, output) {

    options(shiny.maxRequestSize=100*1024^2)## Set maximum upload size to 100MB

    #proteinGroups.txt input

    proteinGroups <- reactive({

        inFile <- input$proteinGroups

        if (is.null(inFile))
            return(NULL)

        df <- read_delim(inFile$datapath, "\t", escape_double = FALSE, trim_ws = TRUE)

        #df <- read_delim("www/proteinGroups_example.txt",
         #                                  "\t", escape_double = FALSE, trim_ws = TRUE)

        #Remove reverse and reverse and contaminants and only identified by site
        df <- df[is.na(df$`Potential contaminant`) & is.na(df$Reverse)  & is.na(df$`Only identified by site`),]

        #Separate the Protein IDs into different rows separated by ;

        #df_separated <- df %>%  separate_rows(c(`Protein IDs`), sep = ';')

        return(df)

    })

    experiment_names <- reactive({

        experiment_names <- proteinGroups() %>%
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



    output$IntensityFound <- renderText({

        columns = grep(paste0(input$IntensityType,'.'), colnames(proteinGroups()))

        if (length(columns) == 0) {

            print(paste0(input$IntensityType, ' was not found. \nSelect another type of intensity.'))

        } else{
            print(paste0(input$IntensityType, ' was found. \nContinue with the analysis.'))

        }




        })


    data_se <- reactive({

        if (input$IntensityType == 'Intensity') {

            columns = grep('Intensity.', colnames(proteinGroups()))

        } else if (input$IntensityType == 'LFQ') {

            columns = grep('LFQ.', colnames(proteinGroups()))

        } else if (input$IntensityType == 'iBAQ'){
            columns = grep('iBAQ.', colnames(proteinGroups()))
        }

        #Adds two columns at the end with an unique gene and protein name.
        data_unique <- DEP::make_unique(proteinGroups(), 'Gene names', 'Protein IDs', delim = ';')


        #Creates a SummarizedExperiment
        data_se <- DEP::make_se(data_unique, columns = columns, experiment_design())

    })

    data_filt <- reactive({

        #Imputation should not be done for proteins with too many NAs
        #We set a threshold for the allowed number of missing values per condition

        # Check number of replicates
        if(max(experiment_design()$replicate)<3){
            threshold <-0 #If there are two replicates, NA accepted is 0.
        } else  if(max(experiment_design()$replicate)==3){
            threshold <-1 #If there are three replicates,  NA accepted is 1.
        } else if(max(experiment_design()$replicate)<6 ){
            threshold <-2 #If there are 4 or 5 replicates,  NA accepted is 2.
        } else if (max(experiment_design()$replicate)>=6){
            threshold<-trunc(max(experiment_design()$replicate)/2) #If there are 6 or more. NA accepted is half of the max.
        }

        data_filt <- filter_missval(data_se(),thr = threshold)
        #plot_missval(filter_missval(data_se,thr = 5))

        #plot_detect(data_filt)
        #plot_coverage(data_filt)
    })

    data_norm <- reactive({

        data_norm <- normalize_vsn(data_filt())
        #meanSdPlot(data_norm)
        #data_norm <- normalize_vsn(data_filt)
        #plot_normalization(data_filt, data_norm)
    })

    data_imp <- reactive({

        data_imp <- impute(data_norm(), fun = "MinProb", q = 0.01)

        #plot_imputation(data_norm, data_imp)

        #data_imp <- impute(data_norm, fun = "knn", rowmax = 0.9)
    #   plot_imputation(data_norm, data_imp)

    })

    dep <- reactive({
        # Test every sample versus control
        #data_diff <- test_diff(data_imp, type = "control", control = "Benign")

        #data_diff_all_contrasts <- test_diff(data_imp, type = "all")

        data_diff_all_contrasts <- MQanalyser::test_limma(data_imp(), type = "all")
        #dep <- add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))

        dep <- add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))


    })

    output$significant_proteins <- renderText({

        # Generate a results table
        data_results <- get_results(dep())

        # Number of significant proteins
        significant_proteins <- data_results %>% filter(significant) %>% nrow()
        HTML(
            paste0("There are: ", "<b>",significant_proteins," out of ",nrow(data_results),"</b>"," signifcant proteins.")
        )
    })


    output$proteomics_results <- DT::renderDataTable({

        # Generate a results table
        data_results <- get_results(dep())

        DT::datatable(data_results)
    })





}
