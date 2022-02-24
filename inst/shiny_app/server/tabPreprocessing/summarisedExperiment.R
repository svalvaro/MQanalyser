data_se <- reactive({

    df <- proteoInputClean()

    exp_design <- ed_final$data

    if (software_used() == 'MaxQuant') {

        df$iBAQ.peptides <- NULL

        # columns = grep('LFQ', colnames(df))

        columns = grep(paste0(input$IntensityType,'.'), colnames(df))

        # Adds two columns at the end with an unique gene and protein name.
        data_unique <- DEP::make_unique(df, 'Gene.names', 'Protein.IDs', delim = ';')

        # data_unique <- DEP::make_unique(proteoInput, 'Gene.names', 'Protein.IDs', delim = ';')

        if (input$IntensityType == 'iBAQ') {
            # Remove the iBAQ. from the column names: iBAQ.sample1, iBAQ.sample2

            names(data_unique)[columns] <- gsub('iBAQ.','',names(data_unique)[columns])
        }


    } else if (software_used() == 'Spectronaut'){
        # Find the columns with the LFQ or iBAQ intensity

        if (input$IntensityType == 'LFQ') {
            columns <-  grep('PG.Quantity', colnames(df))

            # remove the ending of the names
            colnames(df)[columns] <-  gsub(pattern = '.raw.PG.Quantity','',base::colnames(df)[columns])
        }else{

            columns <-  grep('PG.IBAQ', colnames(df))
            # remove the ending of the names
            colnames(df)[columns] <-  gsub(pattern = '.raw.PG.IBAQ','',base::colnames(df)[columns])


            # the IBAQ values in Spectronaut contains multiple values separated by a semicolon
            # For now I will just take the first value.

        }
        # Remove [1], [2], [3] from the column names

        colnames(df)[columns] <-  gsub(pattern = '\\[.*\\] ','',base::colnames(df)[columns])

        # Make unique
        data_unique <- DEP::make_unique(df,'Gene.names', 'Protein.IDs', delim = ';')

    }

    message('The experiment names are:\n')
    print(colnames(data_unique[columns]))
    # Creates a SummarizedExperiment,
    data_se <- DEP::make_se(data_unique, columns = columns, expdesign = exp_design)

    message('Summarized Experiment created')

    return(data_se)

})
