function(input, output) {

    options(shiny.maxRequestSize=100*1024^2)## Set maximum upload size to 100MB


    # FILE INPUTS

    proteinGroups <- reactive({

        inFile <- input$proteinGroups

        if (is.null(inFile))
            return(NULL)

        df <- read.delim(inFile$datapath)

        # df <- read.delim('/home/alvaro/Downloads/proteinGroups_example(2).txt')

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

        # experiment_design <- read.delim('/home/alvaro/Downloads/experimental_design_example(2).txt')

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


    observeEvent(input$start_input, {

        if (is.null(input$proteinGroups) && is.null(input$optional_exp_design) ) {
            return(NULL)
        } else{

        ed_final$data <-  rhandsontable::hot_to_r(input$ed_out)
        }

    })


    observeEvent(input$start_input, {

        if(is.null(input$proteinGroups)){
            shinyalert::shinyalert("Analysis not started", "proteinGroups.txt not uploaded",
                                   type="error",
                                   closeOnClickOutside = TRUE,
                                   closeOnEsc = TRUE,
                                   timer = 6000)

        }else if(all(ed_final$data$condition == ' ')){
            shinyalert::shinyalert("Analysis not started",
                                   "Provide an Experiment Design",
                                   type="error",
                                   closeOnClickOutside = TRUE,
                                   closeOnEsc = TRUE,
                                   timer = 6000)

        }else{

            shinyalert::shinyalert("Analysis Started!",
                                   "You can now move to the next tab.",
                                   type="success",
                                   closeOnClickOutside = TRUE,
                                   closeOnEsc = TRUE,
                                   timer = 10000)
        }
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



    ## User Genes

    user_genes <- reactive({

        inFile <- input$user_genes

        if (is.null(inFile))
            return(NULL)

        df <- read.csv(inFile$datapath, col.names = 'Gene')

        # user_genes <- read.csv("inst/shiny_app/www/user_genes_examples.txt", col.names = 'Gene')

        return(df)

    })


    # DEP ANALYSIS

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
        if(input$input_imputation == 'Perseus'){
            data_imp <-DEP::impute(data_norm(),
                                   fun = "man",
                                   shift = 1.8,
                                   scale = 0.3)
        }else if(input$input_imputation == 'MinProb'){
            data_imp <- DEP::impute(data_norm(),
                                    fun = "MinProb", q = 0.05)
        } else if(input$input_imputation == 'knn'){
            data_imp <- DEP::impute(data_norm(),
                                    fun = "knn",
                                    k = 10, rowmax = 0.9)
        }else if(input$input_imputation == 'MLE'){
            data_imp <- DEP::impute(data_norm(), fun = "MLE")
        }else if(input$input_imputation == 'none'){
            data_imp <- data_norm()
        } else{
            data_imp <- DEP::impute(data_norm(),
                                    fun = input$input_imputation)
        }

        # data_imp <-DEP::impute(data_norm, fun = "man", shift = 1.8, scale = 0.3)

        #plot_imputation(data_norm, data_imp)

        #data_imp <- impute(data_norm, fun = "knn", rowmax = 0.9)
    #  plot_imputation(data_norm, data_imp)

    })

    dep <- reactive({

        # data_diff_all_contrasts <- MQanalyser::test_limma(data_imp, type = "all")

         data_diff_all_contrasts <- MQanalyser::test_limma(data_imp(),
                                                           type = "all")


         #data_diff_all_contrasts <- DEP::test_diff(data_imp(), type = "all")

        # dep <- add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))

        dep <- DEP::add_rejections(data_diff_all_contrasts,
                                   alpha = input$input_pvalue,
                                   lfc = log2(input$input_fc))


    })

    data_results <- reactive({

        # Generate a results table
        data_results <- DEP::get_results(dep())
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
                         icon = icon("stats", lib = "glyphicon"))
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
            icon = icon("stats", lib = "glyphicon"))
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


    # PLOTS

    # heatmap

    output$heatmaply <- renderPlotly(MQanalyser::plot_heatmaply(dep(),
                                       intensity_type = input$IntensityType,
                                       dendogram = input$dendogram_input,
                                       k_row = input$k_row_input,
                                       k_col = input$k_col_input) %>%
                layout(height = 1000, width = 1000)

        )


    # correlation
    output$plot_correlation <- renderPlotly(

        MQanalyser::plot_correlationly(dep()) %>%
            layout(height = 1000, width = 1000)

    )

    # scatterplot
    output$scatterplot <- renderPlotly(
        MQanalyser::plot_scatterly(dep = dep(),
                                   x_sample = input$x_sample_input,
                                   y_sample = input$y_sample_input,
                                   color = input$color_scatter,
                                   show_genes_user = input$showgenes,
                                   user_genes_de = user_genes_de(),
                                   color_genes_de = input$color_de_scatter,
                                   alpha = input$input_alpha,
                                   intensity_type = input$IntensityType,
                                   show_lm = input$input_lm) %>%
                        layout(height = 1000, width = 1000 )
    )



    # volcano
    output$volcano_plot <- renderPlotly(

        if(input$modify_axis == TRUE){

            MQanalyser::plot_volcano(proteomics_results = data_results(),
                                     sample_comparison = input$comparison_input,
                                     foldchange_cutoff = input$input_fc,
                                     p_value_cutoff = input$input_pvalue,
                                     color_up = input$col_upregulated,
                                     color_down = input$col_downregulated,
                                     p_adj = input$p_adj_input,
                                     show_genes_user = input$showgenes_volcano,
                                     user_genes_de = user_genes_de(),
                                     color_genes_de = input$col_selected,
                                     alpha = input$volc_alpha,
                                     coord_x = input$range_fc,
                                     coord_y = input$range_pvalue) %>%

                layout(height = 1000, width = 1000)

        } else{

            MQanalyser::plot_volcano(proteomics_results = data_results(),
                                     sample_comparison = input$comparison_input,
                                     foldchange_cutoff = input$input_fc,
                                     p_value_cutoff = input$input_pvalue,
                                     color_up = input$col_upregulated,
                                     color_down = input$col_downregulated,
                                     p_adj = input$p_adj_input,
                                     show_genes_user = input$showgenes_volcano,
                                     user_genes_de = user_genes_de(),
                                     color_genes_de = input$col_selected,
                                     alpha = input$volc_alpha,
                                     coord_x = NULL,
                                     coord_y = NULL)%>%

                layout(height = 1000, width = 1000)

        }




    )

    # profile

    output$plot_profile <- renderPlotly(

        MQanalyser::plot_profilely(dep = dep(),
                                   centered = input$prof_centered,
                                   intensity_type = input$IntensityType,
                                   color = input$input_col_prof,
                                   angle_labels = input$input_angle_samples,
                                   selected_genes = input$plot_profile_table_rows_selected,
                                   color_selected = input$input_col_sel,
                                   plot = TRUE,
                                   clear_selection = input$clear_selection,
                                   prof_genes_de = input$prof_genes_de,
                                   user_genes_de = user_genes_de(),
                                   color_genes_de = input$input_col_prof_de) %>%

            layout(height = 800, width = 1200)
    )

    # Comparison to check.

    comparisons <- reactive({

        comparisons <- data_results() %>%
            select(contains('vs') & contains('significant'))

        comparisons <- gsub(pattern = '_significant',
                            replacement = '',
                            colnames(comparisons))


    })


    output$comparisons_out  <- renderUI({
        selectInput(inputId = 'comparison_input',
                    label = h4('Select the comparison:'),
                    choices = unlist(comparisons()),
                    selected = comparisons()[1])
    })


    # Select the sample to plot in the scatter plot

    output$x_sample_selector <- renderUI({

        selectInput(inputId = 'x_sample_input',
                    label = h4('Select the sample to plot in the x_axis:'),

                    choices = unlist(dep()$ID),selected = unlist(dep()$ID)[1])
    })


    output$y_sample_selector <- renderUI({

        selectInput(inputId = 'y_sample_input',
                    label = h4('Select the sample to plot in the y_axis:'),

                    choices = unlist(dep()$ID),selected = unlist(dep()$ID)[2])
    })





    output$plot_profile_table <- DT::renderDataTable({

        if(input$clear_selection == TRUE){
            DT::datatable(MQanalyser::plot_profilely(dep = dep(),
                                                     intensity_type = input$IntensityType,
                                                     color = NULL,
                                                     angle_labels = NULL,
                                                     selected_genes = NULL,
                                                     color_selected = NULL,
                                                     plot = FALSE),
                          selection = 0,
                          extensions = 'Scroller',

                          options = list(scrollY=500,
                                         scrollX=100)
            )
        }else{
            DT::datatable(MQanalyser::plot_profilely(dep = dep(),
                                                     intensity_type = input$IntensityType,
                                                     color = NULL,
                                                     angle_labels = NULL,
                                                     selected_genes = NULL,
                                                     color_selected = NULL,
                                                     plot = FALSE),
                          extensions = 'Scroller',

                          options = list(scrollY=500,
                                         scrollX=100)
            )
        }


    })


    #### ENRICHMENT PLOTS


    output$comparisons_enrichment  <- renderUI({
        selectInput(inputId = 'comparison_enrch',
                    label = h4('Select the comparison:'),
                    choices = unlist(comparisons()),
                    selected = comparisons()[1])
    })

    geneList <- reactive({

        # organisms <- org.Mm.eg.db - Mouse
        #               org.Hs.eg.db - Human
        #            org.Hs.eg.db - Rat
        # yeast = org.Sc.sgd.db - not working

        geneList <- MQanalyser::create_geneList(data_results = data_results(),
                                                comparison_samples = input$comparison_enrch,
                                                organism = input$enrich_organism) # adapt it to more organisms.

        # geneList <- MQanalyser::create_geneList(data_results = data_results,
        #                             comparison_samples = 'Benign_vs_Malignant',
        #                             organism = 'org.Hs.eg.db')


        # apply log2fc cut off:

        geneList <- geneList[abs(geneList) > log2(input$fc_enrichment)]
    })


    diffExpress <- reactive({


        # de <-  names(geneList())[abs(geneList()) > 2]


        # de <-  names(geneList())[abs(geneList()) > log2(input$fc_enrichment)]


        # diffExpress <- geneList[abs(geneList) > log2(2.5)]

        de <- names(geneList())

        # diffExpress <- names(geneList)[abs(geneList) > log2(3.5)]
    })



    output$diffExpress_number <- renderText({

        a <- length(diffExpress())

        print(paste0('There are: ', a, ' proteins that will
                      be used for the enrichment, gene
                      network and pathway analysis.'))

    })


    edo <- reactive({

        edo <- DOSE::enrichDGN(diffExpress())

        # edo <- DOSE::enrichDGN(diffExpress)

    })


    # GO terms plots

    output$go_classification_plot <- renderPlotly({

       df <-  clusterProfiler::groupGO(gene = diffExpress(),
                                 keyType = 'ENTREZID',
                                 OrgDb = input$enrich_organism,
                                 ont = input$go_ontology,
                                 level = input$go_level) %>%
           as.data.frame() %>%
           select(contains(c('Description', 'count')))


       # df <-  clusterProfiler::groupGO(gene = diffExpress,
       #                                 keyType = 'ENTREZID',
       #                                 OrgDb = org.Hs.eg.db,
       #                                 ont = 'CC',
       #                                 level = 10) %>%
       #     as.data.frame() %>%
       #     select(contains(c('Description', 'count')))


       if(input$go_ontology == 'CC'){
           title = 'Cellular Component'
       } else if (input$go_ontology == 'MF'){
           title = 'Molecular Function'
       } else{
           title = 'Biological Function'
       }


       df[df == 0] <- NA

       df <- drop_na(df)

       df <- df[order(df$Count, decreasing = TRUE),]



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



       ggplotly(p)%>%

            layout(height = 1000, width = 1200)
    })



    # Enrichment for gsea
    edo2 <- reactive({
        # edo2 <- DOSE::gseDO(geneList)
        edo2 <- DOSE::gseDO(geneList())
        return(edo2)
    })


    output$enr_gseaplot <- renderPlot(height = 800, {

        if(input$runscore == 'all'){
            enrichplot::gseaplot2(edo2(), geneSetID = 1)
        } else{
           enrichplot::gseaplot(edo2(), geneSetID = 1, by = input$runscore)
        }
    })


    # DISEASE TAB

    # Disease Enrichment
    output$enr_dotplot <- renderPlot(height = 1000,{

        enrichplot::dotplot(edo(),showCategory = 25)

    })

    # Disease GSEA

    output$enr_gseadotplot <- renderPlot(height = 1000,{

        dotplot(edo2(), showCategory=20) + ggtitle("dotplot for GSEA")

    })

    # Disease plot of enriched terms

    output$heatmapnrich <- renderPlotly({

        ggplotly(heatplot(edox() ,foldChange=geneList())) %>%

            layout(height = 800, width = 1200)
    })



    #Output overlapping distributions
    output$enr_ridgeplot <- renderPlot(height = 800, width =1200,{


        ridgeplot(edo2())
    })


    # Disease Association

    output$upset <- renderPlot(height = 800, width = 1200,{

        enrichplot::upsetplot(edo())

    })




    #### GENE NETWORK


    # Biological Comparison

    output$bio_comparison <- renderPlot(height = 900, {
        #bp2 <- pairwise_termsim(simplify(bp, cutoff=0.7, by="p.adjust", select_fun=min))

        bp <- enrichplot::pairwise_termsim(enrichGO(diffExpress(),
                                                    ont="BP",
                                                    OrgDb = 'org.Hs.eg.db'))
        enrichplot::emapplot(bp)

    })



    edox <- reactive({
        edox <- clusterProfiler::setReadable(edo(), input$organism_input, 'ENTREZID')

         # edox <- clusterProfiler::setReadable(edo, org.Hs.eg.db, 'ENTREZID')
         #
         # edox <- clusterProfiler::setReadable(edo, org.Sc.sgd.db, 'ENTREZID')
         #
         # edox <- clusterProfiler::setReadable(edo, org.Rn.eg.db, 'ENTREZID')

        # edox <- clusterProfiler::setReadable(edo, org.Mm.eg.db, 'ENTREZID')
         #
         # DOSE::setReadable(edo, org.Rn.eg.db, 'ENTREZID')


        # TO DO: other species

        # cellular component, biological function,
        # ggo <- groupGO(gene     = names(geneList),
        # OrgDb    = org.Mm.eg.db,
        # ont      = "CC",
        # level    = 3,
        # readable = TRUE)
        # #




        return(edox)
    })

    # Circus PLot

    output$enr_circusplot <- renderPlot(height = 1000,{

        cnetplot(edox(),  circular = TRUE, colorEdge = TRUE)

    })

    #Gene Network

    output$enr_networkplot <- renderPlot(height = 900, width = 800, {

        cnetplot(edox(), node_label = "all")
    })


    #Enrichment Map


    output$enr_mapplot <- renderPlot(height = 1000, width = 900, {

        enrichplot::emapplot(pairwise_termsim(edo())#, node_scale=input$enrich_nodes
                             ,layout="kk")
    })




    #### PATHWAY ANALYSIS


    #KEGG analysis1

    kegg_react1 <- reactive({

        kk <- clusterProfiler::enrichKEGG(gene=diffExpress(),
                                          organism = 'hsa',
                                          #minGSSize = 120,
                                          pvalueCutoff = 0.05,
                                          #verbose = FALSE
        )
        return(kk)
    })

    output$enr_kegg1 <- renderPlot(height = 900,{


        print(kegg_react1())
        dotplot(kegg_react1(),showCategory =20)
    })



    pathways_id <- reactive({

        patwaisID_vec <- kegg_react1()$ID

        names(patwaisID_vec) <- kegg_react1()$Description

        return(patwaisID_vec)

    })

    output$pathway_selector <- renderUI({

        selectInput(inputId = 'pathselec',label = h4('Select the pathway to check:'),
                    choices = as.list(pathways_id()),selected = as.list(pathways_id()[1]))

    })


    output$network_selector <- renderUI({

        selectInput(inputId = 'netselec',label = h4('Select the pathway to check:'),
                    choices = as.list(pathways_id()),selected = as.list(pathways_id()[1]))

    })



    # If pressed the button, it will open a new tab.
    observeEvent(input$GoToPathway, {

        clusterProfiler::browseKEGG(kegg_react1(),input$pathselec)

    })








}
