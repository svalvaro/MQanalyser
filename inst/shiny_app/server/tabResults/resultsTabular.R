data_results <- reactive({

    # Generate a results table
    data_results <- DEP::get_results(dep())

    # data_results <- get_results(dep)

    # Remove centered columns

    data_results <- data_results %>% select(-contains('centered'))

    # Format the _p.val column to two decimals
    pval_to_format <- which(base::endsWith(names(data_results), '_p.val'))

    data_results[,pval_to_format] <- format(data_results[,pval_to_format], digits =  3)

    # Imputed proteins

    imputed_proteins <- data_to_be_imputed() %>%
        group_by(Protein.ID) %>%
        summarise(Imputed = sum(Imputed == TRUE))

    imputed_proteins$Imputed <- ifelse(imputed_proteins$Imputed == FALSE, FALSE, TRUE)

    colnames(imputed_proteins) <- c('name', 'Imputed')

    #data_results$Imputed <- FALSE

    # Add column with wether the join the data_results with the imputed proteins

    results <- dplyr::full_join(data_results, imputed_proteins, by = "name")

    return(results)
})



# Info box with the number of diff expressed proteins

output$significant_proteins <- renderInfoBox({
    # Number of significant proteins
    significant_proteins <- data_results() %>%
        filter(significant) %>% nrow()
    total_proteins <- data_results() %>% nrow()
    info <- infoBox(
        'Differentially expressed proteins',
        paste0(significant_proteins, ' out of ', total_proteins, ' proteins.'),
        icon = icon("info"))
    return(info)
})

# User genes diff expressed

user_genes_de <- reactive({

    if (is.null(user_genes())) {
        return(NULL)
    }

    significant_proteins <- data_results() %>% filter(significant)

    # significant_proteins <- data_results %>% filter(significant)

    user_genes_de <- user_genes()[which(user_genes()$Gene %in% significant_proteins$name),]

    # user_genes_de <- user_genes[which(user_genes$Gene %in% significant_proteins$name),]
    return(user_genes_de)
})

# info box with the number of diff expressed proteins
output$significant_user_genes <- renderInfoBox({

    if (is.null(user_genes())) {
        return(NULL)
    }

    info <- infoBox(
        'From your selected proteins, there are:',
        paste0(length(user_genes_de()), ' out of ', nrow(user_genes()), ' proteins.'),
        icon = icon("bullseye"),
        color = 'green')
    return(info)
})

# table with the significant user genes
output$table_user_genes <- renderUI({

    if (is.null(user_genes())) {
        return(NULL)
    }

    selectInput(inputId = 'selected_df_user_prots',
                label = h4('These are the differentially expressed \nproteins that you provided:'),
                choices = unlist(user_genes_de()),
                selected = user_genes_de()[1])
})

# Render the table with the results

output$proteomics_results <- DT::renderDataTable({

    DT::datatable(data_results(),
                  extensions = 'Scroller',

                  options = list(scrollY=500,
                                 scrollX=30),
                  width = '400px')
})

# Download the button proteomics_results

output$download_proteomics <- downloadHandler(
    filename = function(){'proteomics_results.csv'},
    content = function(fname){
        write.csv(data_results(), fname)
    }
)
