function(input, output) {

    options(shiny.maxRequestSize=100*1024^2)## Set maximum upload size to 100MB

    #proteinGroups.txt input

    proteinGroups <- reactive({

        inFile <- input$proteinGroups

        if (is.null(inFile))
            return(NULL)

        df <- read_delim(inFile$datapath, "\t", escape_double = FALSE, trim_ws = TRUE)

        # df <- read_delim("www/proteinGroups.txt",
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

    output$experiment_design <- DT::renderDataTable({

        df <- data.frame(Sample = experiment_names(),
                         Group = NA,
                         Replicate = NA)

        DT::datatable(df, editable = TRUE)

    })



}
