experiment_names <- reactive({

    if (is.null(proteoInput())) {
        return(NULL)
    }

    if (is.null(software_used())) {
        return(NULL)
    }

    shiny::req(input$IntensityType)

    print(paste0('user uploaded proteoInput: ', !is.null(proteoInput())))

    #shiny::req(!is.null(proteoInput()))

    if (software_used() == 'MaxQuant') {

        if(input$IntensityType == 'Intensity'){

            experiment_names <- proteoInput() %>%
                select(contains('Intensity.')) %>%
                select(-contains('LFQ'))

            experiment_names <- base::colnames(experiment_names)
            experiment_names <- gsub('Intensity.', '', experiment_names)
        }

        if(input$IntensityType == 'LFQ'){

            experiment_names <- proteoInput() %>%
                select(contains('LFQ'))

            experiment_names <- base::colnames(experiment_names)
            experiment_names <- gsub('LFQ.intensity.', '', experiment_names)

            print('exp names')
            print(experiment_names)
        }

        if(input$IntensityType == 'iBAQ'){

            experiment_names <- proteoInput() %>%
                select(contains('iBAQ.')) %>%
                select(-contains('peptides'))

            experiment_names <- base::colnames(experiment_names)
            experiment_names <- gsub('iBAQ.', '', experiment_names)
        }

    } else if (software_used() == 'Spectronaut'){

        # proteinGroups <- prot_quant
        experiment_names <- proteoInput() %>%
            select(contains('PG.Quantity'))
        experiment_names <- base::colnames(experiment_names)

        experiment_names <- gsub('.raw.PG.Quantity', '', experiment_names)

        experiment_names <- gsub('\\[.*\\] ', '', experiment_names)

    } else if (software_used() == 'MSFragger'){

        experiment_names <- colnames(
            proteoInput() %>% select(contains('Intensity'),
                                     -contains(c('Total','Unique')))
                                     )

        experiment_names <- gsub(' Intensity', '', experiment_names)

    }

    message(paste0('The experiment names are:', experiment_names))

    return(experiment_names)
})
