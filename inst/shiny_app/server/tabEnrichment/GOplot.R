geneOntologyTable <- reactive({

    df <-  clusterProfiler::groupGO(gene = diffExpress(),
                                    keyType = 'ENTREZID',
                                    OrgDb = input$enrich_organism,
                                    ont = input$go_ontology,
                                    level = input$go_level) %>%
        as.data.frame()

    rownames(df) <- NULL

    df[df == 0] <- NA

    df <- drop_na(df)

    df <- df[order(df$Count, decreasing = TRUE),]

    # Create a new column with the gene names not only the IDs
    # For that I need to map the column geneId, which the genes are separated
    # by / , to the geneListObject, that contains the gene names

    df$geneNames <- stringi::stri_replace_all_fixed(
        str = df$geneID,
        pattern = geneListObject()$geneList$ENTREZID,
        replacement = geneListObject()$geneList$SYMBOL,
        vectorize_all = F
    )

    return(df)
})


geneOntologyReactive <- reactive({

    if(input$go_ontology == 'CC'){
        title = 'Cellular Component'
    } else if (input$go_ontology == 'MF'){
        title = 'Molecular Function'
    } else{
        title = 'Biological Function'
    }

    df <- geneOntologyTable() %>%
        select(contains(c('Description', 'count')))

    mycolors <- grDevices::colorRampPalette(brewer.pal(8, "Set2"))(nrow(df))

    p <- ggplot(df, aes(x = Count,
                        y = reorder(Description, Count),
                        fill = Description))+
        geom_bar(stat = 'identity')+
        theme_bw()+
        ylab('Description')+
        ggtitle(title)+
        theme(legend.position = 'none')+
        scale_fill_manual(values = mycolors)

    return(p)
})

output$go_classification_plot <- renderPlotly({



    ggplotly(geneOntologyReactive())%>%

        layout(height = 1000, width = 1200)
})
