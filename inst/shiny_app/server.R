function(input, output) {

    #### Options and DEMO ####

    options(shiny.maxRequestSize=100*1024^2)## Set maximum upload size to 100MB

    demo <- reactiveValues(start = FALSE)

    observeEvent(input$Demo, {
        req(input$Demo)
        demo$start <-  TRUE


        shinyalert::shinyalert("Demo Data Loaded", "Press Start Analysis when you are ready!",
                               type="success",
                               closeOnClickOutside = TRUE,
                               closeOnEsc = TRUE,
                               timer = 6000)


    })


    #### protein Input MaxQuant or Spectronaut ####

    proteoInput <- reactive({

        inFile <- input$proteinInput

        # If no file is loaded and no pressed demo
        if (is.null(inFile) & demo$start == FALSE){
            return(NULL)

        # If a file is loaded (MaxQuant or spectronaut)

        }else if(!is.null(inFile)){

            # If the file ends in txt is from MaxQuant, read accordingly

            if (endsWith(as.character(inFile$datapath),suffix = '.txt')) {
                #message(paste0(inFile, 'ends with .txt'))

                df <- read.delim(inFile$datapath)

                # df <- read.delim('./inst/shiny_app/www/proteinGroups_example.txt')

                #Remove reverse and reverse and contaminants and only identified by site


                df <- df[(df$Reverse == '')  & (df$Only.identified.by.site==''),]

                # Remove the contaminants if checkbox is pressed
                if (input$contaminantsInput) {

                    df <- df[(df$Potential.contaminant == ''),]
                }

                # If the file ends in csv is from Spectronaut, read accordingly

            } else if (endsWith(as.character(inFile$datapath), suffix = '.csv')){

                #message(paste0(inFile, 'ends with .csv'))

                df <- read_csv(inFile$datapath, na = 'NaN')

                 #df <- read_csv('www/Pivot_ProteinQuant_example.csv',na = 'NaN')

                # Remove the contamintants is only if checkbox is pressed.
                # Need to find a file with contaminants first.

            }

        # If they press DEMO
        } else if(demo$start == TRUE){

            df <- read.delim('www/proteinGroups_example.txt')

            #Remove reverse and reverse and contaminants and only identified by site

            df <- df[(df$Potential.contaminant == '') & (df$Reverse == '')  & (df$Only.identified.by.site==''),]

        }

        return(df)
    })

    #### Software used ####

    software_used <- reactive({

        software_used <- MQanalyser::check_software_input(proteoInput())

        message(paste0('Software used:', software_used))

        return(software_used)

    })

    output$sw_used <- renderText({
        if (is.null(proteoInput())) {
            print("")
        }else{
            print(paste0('Software used: ', software_used()))
        }

    })

    #### Experiment Design ####
    experiment_names <- reactive({

        if (software_used() == 'MaxQuant') {

            experiment_names <- proteoInput() %>%
                select(contains('Intensity.')) %>%
                select(-contains('LFQ'))

            experiment_names <- base::colnames(experiment_names)
            experiment_names <- gsub('Intensity.', '', experiment_names)

        } else if (software_used() == 'Spectronaut'){

            # proteinGroups <- prot_quant
            experiment_names <- proteoInput() %>%
                select(contains('PG.Quantity'))
            experiment_names <- base::colnames(experiment_names)

            experiment_names <- gsub('.raw.PG.Quantity', '', experiment_names)

            experiment_names <- gsub('\\[.*\\] ', '', experiment_names)

            # gsub('\\[.*\\] ','',exp_design[,'label'])
        }

        message(paste0('The experiment names are:', experiment_names))


        return(experiment_names)

    })

    experiment_design <- reactive({

        # experiment_design <- read.delim('/home/alvaro/Documents/R/proteomics/MQanalyser/inst/shiny_app/www/experiment_design_example.txt')

        # experiment_design <- read.delim('www/experiment_design_example_spectronaut.txt')

        inFile <- input$optional_exp_design

        if (is.null(inFile) & demo$start == FALSE){

            df <- data.frame(label = experiment_names(),
                             condition = ' ',
                             replicate = as.numeric(' '))
        } else if(demo$start == TRUE){
            df <- read.delim('www/experiment_design_example.txt')
        }else{
            df <- read.delim(inFile$datapath)
        }

        return(df)
    })




    output$ed_out <- renderRHandsontable({

        rhandsontable(experiment_design(), height =  500) %>%
            hot_col('replicate', format = '0a')
    })




    ed_final <- reactiveValues()

    observeEvent(input$start_input, {

        if (is.null(input$proteinInput) && is.null(input$optional_exp_design) && demo$start == FALSE) {
            return(NULL)
        } else{

        ed_final$data <-  rhandsontable::hot_to_r(input$ed_out)
        }

    })

    # Button to download the experiment design once the user has finished it.


    output$download_experiment_design <- downloadHandler(

        filename = function(){'experiment_design.txt'},
        content = function(fname){
            write_delim(ed_final$data, fname,delim = '\t')
        }
    )



    #### Pop-up message when pressed start analysis ####


    observeEvent(input$start_input, {

        if(is.null(input$proteinInput) & demo$start == FALSE & IntensityFound() == TRUE){
            shinyalert::shinyalert("Analysis not started", "protein table not uploaded",
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

        }else if(IntensityFound() == FALSE){

            shinyalert::shinyalert("Analysis not started",
                                   "Intensity selected was not found. \nSelect another type of Intensity.",
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



    #### Intensity type depending on the proteoInput ####


    # Radio buttons will appear in the UI depending on the software used.

    output$intensity_selector  <- renderUI({


        if (software_used() == 'MaxQuant') {

            radioButtons(inputId = "IntensityType",
                         h4("Intensity type to analyze:"),
                         choices = c("Raw Intensity" = 'Intensity',
                                     "LFQ" = 'LFQ',
                                     "iBAQ" = 'iBAQ'),
                         selected = 'LFQ')

        } else if ( software_used() == 'Spectronaut'){

            radioButtons(inputId = "IntensityType",
                         h4("Intensity type to analyze:"),
                         choices = c("LFQ" = 'LFQ',
                                     "iBAQ" = 'iBAQ'),
                         selected = 'LFQ')
        }
    })

    # Check if the type of intensity is found in the proteoInput


    IntensityFound <- reactive({

        intensityToUse <- input$IntensityType

        if (is.null(software_used())) {
            return(NULL)

            # If the software used was MaxQuant

        } else if (software_used() == 'MaxQuant') {

            columns = grep(paste0(input$IntensityType,'.'), colnames(proteoInput()))

        # For Spectronaut Software

        } else if (software_used() == 'Spectronaut'){

            # If the intensity selected is LFQ, the column name is PG.Quantity
            if (input$IntensityType == 'LFQ') {
                columns <-  grep('PG.Quantity', colnames(proteoInput()))

            # If IBAQ Intensity is selected
            }else{
                columns <-  grep('PG.IBAQ', colnames(proteoInput()))
            }
        }

        if (length(columns) == 0) {

            IntensityFound <- FALSE
        } else{

            IntensityFound <- TRUE
        }

        return(IntensityFound)
    })


    output$IntensityFound_message <- renderText({
        if (IntensityFound() == TRUE) {
            print(paste0(input$IntensityType, '  was found. \nYou can continue with the analysis.'))
        }else{
            print(paste0(input$IntensityType, ' was not found. \nPlease select another intensity.'))
        }
    })



    #### User Genes ####

    user_genes <- reactive({

        inFile <- input$user_genes

        if (is.null(inFile) & demo$start == FALSE){
            return(NULL)
        } else if (demo$start == TRUE){
            df <- read.csv('www/user_genes_examples.txt', col.names = 'Gene')
        } else{
            df <- read.csv(inFile$datapath, col.names = 'Gene')
        }

        # user_genes <- read.csv("inst/shiny_app/www/user_genes_examples.txt", col.names = 'Gene')

        return(df)

    })


    #### DEP ANALYSIS ####

    data_se <- reactive({

        df <- proteoInput()

        exp_design <- ed_final$data

        if (software_used() == 'MaxQuant') {

            # columns = grep('LFQ', colnames(df))

            # iBAQ failing here, check why.
            columns = grep(paste0(input$IntensityType,'.'), colnames(df))

            # Adds two columns at the end with an unique gene and protein name.
            data_unique <- DEP::make_unique(df, 'Gene.names', 'Protein.IDs', delim = ';')

            # data_unique <- DEP::make_unique(df, 'Gene.names', 'Protein.IDs', delim = ';')



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

                # df[columns] <- gsub(';*', '', df[columns])
            }

            # Remove [1], [2], [3] from the column names

            colnames(df)[columns] <-  gsub(pattern = '\\[.*\\] ','',base::colnames(df)[columns])

            # Make unique
            data_unique <- DEP::make_unique(df,'PG.Genes', 'PG.ProteinGroups', delim = ';')

            # Remove the brackets [1], [2], from the experiment design if there.

            #exp_design[,'label'] <- gsub('\\[.*\\] ','',exp_design[,'label'])

        }

        # Creates a SummarizedExperiment,
        data_se <- DEP::make_se(data_unique, columns = columns, expdesign = exp_design)

        # data_se <- DEP::make_se(data_unique, columns = columns, experiment_design)
        #View(as.data.frame(data_se@elementMetadata))
        })



    #### Filtering of NAs ####

    # Selec the NAs allowd

    output$na_threshold  <- renderUI({

        # Check number of replicates

        n_replicates <- max(ed_final$data$replicate)

        message(paste0('The number of replicates is: ', n_replicates))

         if(n_replicates < 3){
             threshold <-0 #If there are two replicates, NA accepted is 0.
         } else  if(n_replicates == 3){
             threshold <-1 #If there are three replicates,  NA accepted is 1.
         } else if(n_replicates < 6 ){
             threshold <-2 #If there are 4 or 5 replicates,  NA accepted is 2.
         } else if (n_replicates >= 6){
             threshold<- trunc(n_replicates / 2) #If there are 6 or more. NA accepted is half of the max.
         }



        sliderInput(inputId = 'nas_threshold',
                    label = h4('Select the number of missing values allowed in each group'),
                    min = 0,
                    max = n_replicates,
                    value = threshold,
                    step = 1)


    })

    data_filt <- reactive({

        #Imputation should not be done for proteins with too many NAs
        #We set a threshold for the allowed number of missing values per condition
        # Threshold is per group.
        # So if threshold is 0 it means that for a given protein, at least
        # all samples in one of the groups must have non NAs. In the other group,
        # for that given protein NAs are allowed.

        data_filt <- DEP::filter_missval(data_se(),
                                         thr = as.integer(input$nas_threshold))

        # data_filt <- DEP::filter_missval(data_se,thr = 0)

        # heatmaply(plot_missval(filter_missval(data_se,thr = 0)))

        # plot_detect(data_filt)
        # plot_coverage(data_filt)
        return(data_filt)
    })


    output$heatmap_nas <- renderPlot(height = 800,{


        # Make it into plotly and iteractive!
        DEP::plot_missval(data_filt())
    })

    ####  DATA normalization ####

    output$plot_before_normalization <- renderPlotly({
        `Before normalization` = data_filt()
        #ggplotly(DEP::plot_normalization(`Before normalization`))

        MQanalyser::plot_normalization_interactive(`Before normalization`)
    })

    output$plot_after_normalization <- renderPlotly({
        `After normalization` = data_norm()

        if (input$normalize_input == TRUE) {
            # ggplotly(
            #     (`After normalization`)
            #
            # )
            MQanalyser::plot_normalization_interactive(`After normalization`)

        }else{
            return(NULL)
        }

    })

    data_norm <- reactive({

        # If the user wants to normalize

        if (input$normalize_input == TRUE) {

            data_norm <- DEP::normalize_vsn(data_filt())
        } else{
            data_norm <- data_filt()
        }
         #data_norm <- DEP::normalize_vsn(data_filt)

        #meanSdPlot(data_norm)
        #data_norm <- normalize_vsn(data_filt)
        # ggplotly( plot_normalization( data_norm))
    })

    #### Imputation ####
    #  Chose the parameter scale if imputation selected == Manual

    output$manual_imputation_scale  <- renderUI({

        if (input$input_imputation == 'Manual') {

            sliderInput(inputId = 'input_scale',
                        label = h5('Sets the width of the distribution relative
                                   to the standard deviation of the original distribution.'),
                        min = 0.05,
                        max = 0.95,
                        value = 0.3,
                        step = 0.05)
        }else{
            return(NULL)
        }
    })

    # Chose the parameter shift if imputation selected == Manual

    output$manual_imputation_shift  <- renderUI({

        if (input$input_imputation == 'Manual') {

            sliderInput(inputId = 'input_shift',
                        label = h5(' Sets the left-shift of the distribution
                                   (in standard deviations) from the median of
                                   the original distribution.'),
                        min = 0,
                        max = 10,
                        value = 1.8,
                        step = 0.1)
        }else{
            return(NULL)
        }
    })




    data_imp <- reactive({
        if(input$input_imputation == 'Manual'){
            data_imp <-DEP::impute(data_norm(),
                                   fun = "man",
                                   shift = input$input_shift,
                                   scale = input$input_scale)
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


         #data_imp <- DEP::impute(data_norm , fun = 'zero')
        # data_imp <-DEP::impute(data_norm, fun = "man", shift = 1.8, scale = 0.3)

        #plot_imputation(data_norm, data_imp)

        #data_imp <- impute(data_norm, fun = "knn", rowmax = 0.9)
    #  plot_imputation(data_norm, data_imp)

    })


    data_to_be_imputed <- reactive({

        # filtered <- as.data.frame(data_filt@assays@data)



        # Obtain the data before being imputed
        filtered <- as.data.frame(data_filt()@assays@data)

        filtered$Protein.ID <- rownames(filtered)

        filtered <- filtered %>% select(-contains('group'))

        filtered_melt <- melt(filtered, id.vars = 'Protein.ID')

        # Create a new column calledd imputed
        filtered_melt$Imputed <- FALSE

        # If the value is NA, it will be imputed in the next step
        filtered_melt$Imputed[is.na(filtered_melt$value)] <- TRUE

        # imputed <-  which(rowSums(is.na(
        #     filtered %>% select(-contains(c('Group', 'Group_name')))
        # ))>0)
        #
        # df$Imputed <- FALSE
        #
        # df$Imputed[imputed] <- TRUE


        # Now obtain the already imputed values:

        # imputed <- as.data.frame(data_imp@assays@data)
        imputed <- as.data.frame(data_imp()@assays@data)

        imputed <- imputed %>% select(-contains('group'))

        imputed$Protein.ID <- rownames(imputed)

        imputed_melt <- melt(imputed, id.vars = 'Protein.ID')


        # The column imputed_melt contained the data of the imputed table
        # and added a column specifying whether a protein in a specific group
        # has been imputed or not. This is done by matching it to the
        # filtered table.
        # In the filtered table it is possible to know which one will be imputed
        # since it is in the form of NAs.

        imputed_melt$Imputed <- filtered_melt$Imputed[
            imputed_melt$Protein.ID == filtered_melt$Protein.ID &&
            imputed_melt$variable == filtered_melt$variable]


        return(imputed_melt)
    })




    dep <- reactive({

        # data_diff_all_contrasts <- MQanalyser::test_limma(data_imp, type = "all")

        data_diff_all_contrasts <- MQanalyser::test_limma(data_imp(),
                                                           type = "all")


         #data_diff_all_contrasts <- DEP::test_diff(data_imp(), type = "all", design_formula = formula(~ 0 + condition))

        # dep <- add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))

        dep <- DEP::add_rejections(data_diff_all_contrasts,
                                   alpha = input$input_pvalue,
                                   lfc = log2(input$input_fc))


    })

    data_results <- reactive({

        # Generate a results table
        data_results <- DEP::get_results(dep())

        # data_results <- get_results(dep)


        # Remove centered columns

        data_results <- data_results %>% select(-contains('centered'))


        # Imputed proteins

        imputed_proteins <- data_to_be_imputed() %>%
                                group_by(Protein.ID) %>%
                                summarise(Imputed = sum(Imputed == TRUE))

        imputed_proteins$Imputed <- ifelse(imputed_proteins$Imputed > 0, FALSE, TRUE)

        colnames(imputed_proteins) <- c('name', 'Imputed')

        #data_results$Imputed <- FALSE

        # Add column with wether the

        # Join the data_results with the imputed proteins


        results <- dplyr::full_join(data_results, imputed_proteins, by = "name")

        return(results)
    })



    output$imputation <- renderPlotly(

        MQanalyser::plot_histogram_imputed(

            data_to_be_imputed = data_to_be_imputed(),
            combined = input$combined_imputation) %>%
            layout(height = 900, width = 1400)
    )



    #### RESULTS TABULAR ####

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

        DT::datatable(data_results(),
                      extensions = 'Scroller',

                      options = list(scrollY=500,
                                     scrollX=30),width = 400)
    })

    # Download the proteomics_results

    output$download_proteomics <- downloadHandler(
        filename = function(){'proteomics_results.csv'},
        content = function(fname){
            write.csv(data_results(), fname)
        }
    )


    #### Heatmap plot ####

    output$heatmaply <- renderPlotly(
        MQanalyser::plot_heatmaply(dep(),
                   intensity_type = input$IntensityType,
                   dendogram = input$dendogram_input,
                   k_row = input$k_row_input,
                   k_col = input$k_col_input) %>%
                layout(height = 1000, width = 1000)

        )


    #### Correlation plot ####
    output$plot_correlation <- renderPlotly(

        MQanalyser::plot_correlationly(dep()) %>%
            layout(height = 800, width = 800)

    )

    #### Scatter plot ####

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



    #### Volcano plot ####

    # Comparison to check in the volcano plot

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


    #### PCA pot ####

    output$pca_number_proteins  <- renderUI({

        var <- apply(assay(dep()), 1, sd)

        if (length(var)>500) {
            value = 500
        }else{
            value = length(var)
        }

        sliderInput(inputId = 'pca_proteins',
                    label = h4('Select the number of proteins:'),
                    min = 2,
                    max = length(var),
                    value = value,
                    step = 1)
        })


    output$pca_plot <- renderPlot(height = 800, width = 1200,{
        MQanalyser::plot_pca_improved(dep = dep(),
                                      PC_x = 1,
                                      PC_y = 2,
                                      label_name = input$pca_label,
                                      n = input$pca_proteins)
    })

    #### Profile Plot ####

    output$plot_profile <- renderPlotly(

        MQanalyser::plot_profilely(dep = dep(),
                                   centered = input$prof_centered,
                                   intensity_type = input$IntensityType,
                                   color = input$input_col_prof,
                                   angle_labels = input$input_angle_samples,
                                   selected_genes = input$plot_profile_table_rows_selected,
                                   color_selected = input$input_col_sel,
                                   alpha = input$profile_alpha,
                                   plot = TRUE,
                                   clear_selection = input$clear_selection,
                                   prof_genes_de = input$prof_genes_de,
                                   user_genes_de = user_genes_de(),
                                   color_genes_de = input$input_col_prof_de) %>%

            layout(height = 800, width = 1200)
    )


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


    #### Enrichment Analysis ####


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



    output$enr_gseaplot <- renderPlot(height = 800, {

        if(input$runscore == 'all'){
            enrichplot::gseaplot2(edo2(), geneSetID = 1)
        } else{
           enrichplot::gseaplot(edo2(), geneSetID = 1, by = input$runscore)
        }
    })


    #### Disease Analysis ####

    ## DISEASE DATA
    output$comparisons_diseases  <- renderUI({
        selectInput(inputId = 'comparison_disease',
                    label = h4('Select the comparison:'),
                    choices = unlist(comparisons()),
                    selected = comparisons()[1])
    })



    geneList_disease <- reactive({


        geneList <- MQanalyser::create_geneList(data_results = data_results(),
                                                comparison_samples = input$comparison_disease,
                                                organism = input$disease_organism) # adapt it to more organisms.

        geneList <- geneList[abs(geneList) > log2(input$fc_disease)]

    })


    edo <- reactive({
        diffExpress <- names(geneList_disease())




        edo <- DOSE::enrichDGN(diffExpress)

        # edo <- DOSE::enrichDGN(diffExpress)

    })

    # Enrichment for gsea
    edo2 <- reactive({
        # edo2 <- DOSE::gseDO(geneList)
        edo2 <- DOSE::gseDO(geneList_disease ())
        return(edo2)
    })

    edox <- reactive({
        edox <- clusterProfiler::setReadable(edo(),
                                             input$disease_organism,
                                             'ENTREZID')
        return(edox)
    })



    output$diff_disease_number <- renderText({

        a <- length(names(geneList_disease()))

        print(paste0('There are: ', a, ' proteins that will
                      be used for the disease analysis.'))

    })



    ## DISEASE PLOTS

    # Disease Enrichment
    output$enr_dotplot <- renderPlot(height = 1000,{

        enrichplot::dotplot(edo(),showCategory = 25)

    })



    # Disease GSEA

    output$enr_gseadotplot <- renderPlot(height = 1000,{

        if(nrow(edo()) <1){
            print('Cant print')
        }else{
            dotplot(edo2(), showCategory=20) + ggtitle("dotplot for GSEA")
        }



    })

    # Disease plot of enriched terms

    output$heatmapnrich <- renderPlotly({

        ggplotly(heatplot(edox() ,foldChange=geneList_disease())) %>%

            layout(height = 800, width = 1400)
    })



    #Output overlapping distributions
    output$enr_ridgeplot <- renderPlot(height = 800, width =1200,{


        ridgeplot(edo2())
    })


    # Disease Association

    output$upset <- renderPlot(height = 800, width = 1200,{

        enrichplot::upsetplot(edo())

    })




    #### GENE NETWORK ####

    ## Data

    output$comparisons_network  <- renderUI({
        selectInput(inputId = 'comparison_network',
                    label = h4('Select the comparison:'),
                    choices = unlist(comparisons()),
                    selected = comparisons()[1])
    })



    geneList_network <- reactive({

        geneList <- MQanalyser::create_geneList(data_results = data_results(),
                                                comparison_samples = input$comparison_network,
                                                organism = input$network_organism) # adapt it to more organisms.



        geneList <- geneList[abs(geneList) > log2(input$fc_network)]
    })


    diffExpress_network <- reactive({

        de <- names(geneList_network())
    })

    edo_network <- reactive({

        edo <- DOSE::enrichDGN(diffExpress_network())
    })


    edox_network <- reactive({
        edox <- clusterProfiler::setReadable(edo_network(),
                                             input$network_organism,
                                             'ENTREZID')
        return(edox)
    })


    output$diff_network_number <- renderText({

        a <- length(diffExpress_network())

        print(paste0('There are: ', a, ' proteins that will
                      be used for gene network.'))

    })



    ## Plots gene network

    # Biological Comparison

    output$bio_comparison <- renderPlot(height = 900, {
        #bp2 <- pairwise_termsim(simplify(bp, cutoff=0.7, by="p.adjust", select_fun=min))

        # bp <- enrichplot::pairwise_termsim(enrichGO(diffExpress_network(),
        #                                             ont="BP",
        #                                             OrgDb = 'org.Hs.eg.db'))

        bp <- enrichplot::pairwise_termsim(enrichGO(diffExpress_network(),
                                                    ont="BP",
                                                    OrgDb = 'org.Hs.eg.db'))
        enrichplot::emapplot(bp)

    })



    # Circus PLot

    output$enr_circusplot <- renderPlot(height = 1000,{

        cnetplot(edox_network(),  circular = TRUE, colorEdge = TRUE)

    })

    #Gene Network

    output$enr_networkplot <- renderPlot(height = 900, width = 800, {

        cnetplot(edox_network(), node_label = "all")
    })


    #Enrichment Map


    output$enr_mapplot <- renderPlot(height = 1000, width = 900, {

        enrichplot::emapplot(pairwise_termsim(edo_network())#, node_scale=input$enrich_nodes
                             ,layout="kk")
    })


    #### PATHWAY ANALYSIS ####

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
