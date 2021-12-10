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

        }

        if(!is.null(inFile)){

            # If the file ends in txt is from MaxQuant, read accordingly

            if (endsWith(as.character(inFile$datapath),suffix = '.txt')) {
                #message(paste0(inFile, 'ends with .txt'))

                df <- read.delim(inFile$datapath)

                # proteoInput <- read.delim('./inst/shiny_app/www/data/proteinGroups_example.txt')

                #Remove reverse and reverse and contaminants and only identified by site
                # The user might have modified the proteinGroups.txt and this columns are not present



            # If the file ends in csv is from Spectronaut, read accordingly

            } else if (endsWith(as.character(inFile$datapath), suffix = '.csv')){

                df <- read_csv(inFile$datapath, na = 'NaN')

                # df <- read_csv('./inst/shiny_app/www/data/Pivot_ProteinQuant_example.csv',na = 'NaN')
            }

        # If they press DEMO
        } else if(demo$start == TRUE){

            df <- read.delim('www/data/proteinGroups_example.txt')

            #Remove reverse and reverse and contaminants and only identified by site

            df <- df[(df$Reverse == '')  & (df$Only.identified.by.site==''),]

            # Remove the contaminants if checkbox is pressed
            if (input$removeContaminantsInput) {

                df <- df[(df$Potential.contaminant == ''),]
            }
        }

        return(df)
    })

    #### Software used ####

    software_used <- reactive({

        software_used <- MQanalyser::check_software_input(proteoInput())

        message(paste0('Software used:', software_used))

        return(software_used)

    })

    output$softwareUsedBox <- renderInfoBox({

        if (is.null(proteoInput())) {
            return(NULL)
        }

        icon <- "check-square"

        color <- 'green'


        info <- infoBox(
            'Software used:',
            paste0('\n', software_used()),
            #icon = icon("stats", lib = "glyphicon"))
            icon = icon(icon),
            color = color)
        return(info)
    })

    #### Experiment Design ####

    # Addition of a fourth column containing boolean values (TRUE/FALSE) to
    # include that experiment or not.

    experiment_names <- reactive({

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
        }

        message(paste0('The experiment names are:', experiment_names))

        return(experiment_names)
    })

    experiment_design <- reactive({

        inFile <- input$optional_exp_design

        if (is.null(inFile) & demo$start == FALSE){

            df <- data.frame(label = experiment_names(),
                             condition = ' ',
                             replicate = as.numeric(' '))
        } else if(demo$start == TRUE){
            df <- read.delim('www/data/experiment_design_example.txt')
        }else{
            df <- read.delim(inFile$datapath)
        }

        return(df)
    })

    output$ed_out <- renderRHandsontable({

        df <- experiment_design()

        df$Include <- TRUE

        rhandsontable(
                    df,
                    height =  500) %>%
            hot_col('replicate', format = '0a') %>%

            rhandsontable::hot_col('label', readOnly = TRUE)%>%

            hot_table(highlightRow = TRUE) %>%

            hot_col(col = "Include", halign = 'htCenter',
                renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);

                        var col_value = instance.getData()[row][3]

                        if (col_value === false) {

                            td.style.background = 'pink';
                        }
                    }
                ") %>%

            hot_col(col = c("label", "condition", "replicate"),
                    renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);

                        var col_value = instance.getData()[row][3]

                        if (col_value === false) {

                            td.style.background = 'pink';
                        }
                    }
                ")

    })

    ed_final <- reactiveValues()

    observeEvent(input$start_input, {

        if (is.null(input$proteinInput) && is.null(input$optional_exp_design) &&
            demo$start == FALSE) {
            return(NULL)
        } else{

            ed_final$data <-  rhandsontable::hot_to_r(input$ed_out)

            # Remove the rows containing FALSE in include.
            print(ed_final$data$Include)

            ed_final$data <- ed_final$data[! ed_final$data$Include == FALSE,]
        }

        print('The experiment design is:')
        print(ed_final$data)

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

    #### Start Analyisis Button ####

    output$start_analysis <- renderUI({

        if (is.null(proteoInput())) {
            return(NULL)
        }
        shinyWidgets::actionBttn(
            inputId = "start_input",
            label = "Start Analysis",
            style = "unite",
            color = "primary",
            size = 'lg',
            icon = icon("play")
        )
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

        message(paste0('The user selects intensity:', intensityToUse))

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

    output$intensityBox <- renderInfoBox({

        if (IntensityFound() == TRUE) {
            title_box <- paste0(input$IntensityType, '  was found.')
            subtitle_box <- ' You can continue with the analysis.'
            icon <- "check-square"
            color <- 'green'

        }else{
            title_box <- paste0(input$IntensityType, '  was not found.')
            subtitle_box <- ' You must select another type of Intensity.'
            icon <- "exclamation-circle"
            color <- 'red'
        }

        info <- infoBox(
            title =  title_box,
            value = subtitle_box,
            #icon = icon("stats", lib = "glyphicon"))
            icon = icon(icon),
            color = color)
        return(info)
    })



    #### User Genes ####

    user_genes <- reactive({

        inFile <- input$user_genes

        if (is.null(inFile) & demo$start == FALSE){
            return(NULL)
        } else if (demo$start == TRUE){
            df <- read.csv('www/data/user_genes_examples.txt', col.names = 'Gene')
        } else{
            df <- read.csv(inFile$datapath, col.names = 'Gene')
        }

        # user_genes <- read.csv("inst/shiny_app/www/user_genes_examples.txt", col.names = 'Gene')

        return(df)
    })

    #### Filter out contaminants ####

    # If the user uploads a file from Spectronaut, I need to open a fasta file
    # and remove the proteins from the proteoInput that match the contaminants
    # inside the FASTA

    contaminants_fasta <- reactive({

        if (software_used() == 'MaxQuant') {
            return(NULL)
        }

        inFile <- input$contaminantsFastaInput

        # If no file is loaded and no pressed demo
        if (input$fastaOptions == "Use MaxQuant default"){
            message('User uses the default contaminants.fasta')
            contaminants_fasta <- seqinr::read.fasta(file = 'www/data/MaxQuant_Contaminants_Default.fasta')
        }

        if(!is.null(inFile) && input$fastaOptions == "Upload Custom"){
            message('User uploads a fasta with contaminants')
            contaminants_fasta <- seqinr::read.fasta(file = inFile$datapath )
        }

        if(is.null(inFile) && input$fastaOptions == "Upload Custom"){
            message('Waiting for the user to upload a fasta with contaminants')
            return(NULL)
        }


        # contaminants_fasta <- seqinr::read.fasta(file = 'inst/shiny_app/www/data/MaxQuant_Contaminants_Default.fasta' )
        # contaminants_fasta <- seqinr::read.fasta(file = '~/Downloads/MaxQuant_Contaminants_Default.fasta')

        return(contaminants_fasta)
    })

    output$fastaSelection <- renderUI({

        if (software_used() == 'MaxQuant') {
            return(NULL)
        }


        radioGroupButtons(
            inputId = "fastaOptions",
            label = h4("Contaminants proteins"),
            choices = c("Use MaxQuant default",
                        "Upload Custom"),
        status = "success",
        checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon"),
            no = icon("remove",
                      lib = "glyphicon"))
        )

    })

    output$fastaInput <- renderUI({

        if (software_used() == 'MaxQuant') {
            return(NULL)
        }

        shiny::req(input$fastaOptions == 'Upload Custom')

        fileInput(inputId = 'contaminantsFastaInput',
                  label = h4('Upload a FASTA file containing contaminant proteins'),
                  multiple = FALSE,
                  accept = 'txt')
    })

    proteoInputClean <- reactive({

        df <- proteoInput()

        if (software_used() == 'MaxQuant') {

            if (c('Reverse', 'Only.identified.by.site',
                  'Potential.contaminant') %in% names(df)) {

                df <- df[(df$Reverse == '')  & (df$Only.identified.by.site==''),]

                # Remove the contaminants if checkbox is pressed
                if (input$removeContaminantsInput) {

                    df <- df[(df$Potential.contaminant == ''),]
                }
            }
        }

        if (software_used() == 'Spectronaut') {

            if (is.null(contaminants_fasta())) {
                message('Waiting for the user to upload a fasta file')
                return(NULL)
            }

            # Change the name of some columns to make it like proteinGroups

            colnames(df)[colnames(df) %in% c("PG.Genes", "PG.ProteinGroups")] <- c("Gene.names", "Protein.IDs")

            # Add a new column with the contaminants

            df$Potential.contaminant <- ''

            # Compare the proteinIds, with the proteinIDs of the contaminants,
            # if they match, mark it as '+', to be removed later

            # df$Potential.contaminant[which(df$Gene.names %in% names(contaminants_fasta))] <- '+'
            df$Potential.contaminant[which(df$Gene.names %in% names(contaminants_fasta()))] <- '+'

            # Remove the contaminants if checkbox is pressed
            if (input$removeContaminantsInput) {

                df <- df[(df$Potential.contaminant == ''),]
            }
        }

        return(df)
    })


    output$contaminants_box <- renderInfoBox({

        if (is.null(proteoInputClean())) {
            return(NULL)
        }

        # Number of contaminants proteins
        contaminants <- proteoInputClean()$Potential.contaminant

        total_contaminants <- length(contaminants[grep('.+',contaminants)])

        message(paste0('The number of contaminants is: ', total_contaminants))

        if(total_contaminants == 0){
            icon <- "check-square"
            color <- 'green'
        }else{
            icon <- "exclamation-circle"
            color <- 'red'
        }

        info <- infoBox(
            'Contaminant Proteins',
            paste0(total_contaminants, ' contaminants proteins found.'),
            #icon = icon("stats", lib = "glyphicon"))
            icon = icon(icon),
            color = color)
        return(info)
    })

    output$contaminantsPlot <- renderPlotly(

        MQanalyser::plot_contaminants(proteoInput = proteoInputClean(),
                                      softwareUsed = software_used(),
                                      intensityType = input$IntensityType,
                                      interactive = TRUE)%>%
        layout(height = 800, width = 800)
    )

    output$contaminantsPlotNonInteractive <- renderPlot(height = 800, width = 800,{

        if (is.null(proteoInputClean())) {
            return(NULL)
        }

        MQanalyser::plot_contaminants(proteoInput = proteoInputClean(),
                                      softwareUsed = software_used(),
                                      intensityType = input$IntensityType,
                                      interactive = FALSE)
    })

    output$contaminantsUI <- renderUI({

        if (software_used() == 'MaxQuant' && !'Potential.contaminant' %in% names(proteoInput())) {

            return('Contaminants Column is not present in the proteinGroups,
                   Contaminants might be present.
                   Did you modify the proteinGroups.txt?
                   ')
        }

        message(paste0('Value of the contaminants: ', input$contaminantsInteractive))

        if (input$contaminantsInteractive == FALSE) {
            return(
                shinycssloaders::withSpinner(

                    plotOutput('contaminantsPlotNonInteractive'),
                    image = 'images/logoTransparentSmall.gif',
                    image.width = '200px'
                )
            )


        }else{

            return(
                shinycssloaders::withSpinner(
                    plotlyOutput('contaminantsPlot'),
                    image = 'images/logoTransparentSmall.gif',
                    image.width = '200px'
                )
            )
        }
    })

    #shinyjs::onclick('filter_tab',)

    #### Make summarised experiment ####

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

                # df[columns] <- gsub(';*', '', df[columns])
            }

            # Remove [1], [2], [3] from the column names

            colnames(df)[columns] <-  gsub(pattern = '\\[.*\\] ','',base::colnames(df)[columns])



            # Make unique
            data_unique <- DEP::make_unique(df,'PG.Genes', 'PG.ProteinGroups', delim = ';')

            # Remove the brackets [1], [2], from the experiment design if there.

            #exp_design[,'label'] <- gsub('\\[.*\\] ','',exp_design[,'label'])
        }

        message('The experiment names are:\n')
        print(colnames(data_unique[columns]))
        # Creates a SummarizedExperiment,
        data_se <- DEP::make_se(data_unique, columns = columns, expdesign = exp_design)

        message('Summarized Experiment created')

        return(data_se)

        #data_se <- DEP::make_se(data_unique, columns = columns, expdesign=experiment_design)
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
             threshold <- trunc(n_replicates / 2) #If there are 6 or more. NA accepted is half of the max.
         }

        sliderInput(inputId = 'nas_threshold',
                    label = h4('Select the number of missing values allowed in each group'),
                    min = 0,
                    max = n_replicates,
                    value = threshold,
                    step = 1)
    })

    data_filt <- reactive({

        if (is.null(data_se())) {
            return(NULL)
        }

        # Avoid warning message while rendering
        shiny::req(input$nas_threshold)

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

    output$barplot_missvals <- renderPlotly(
        MQanalyser::plot_protsidentified(data_filt())%>%
            layout(height = 800, width = 850)
        #MQanalyser::plot_protsidentified(data_filt)
    )


    output$heatmap_nas <- renderPlot(height = 800,width = 700,{

        if (nrow(data_filt())==nrow(data_se())) {
            message("No missing values, the heatmap will not be plotted")
            return(NULL)
        }

        # Make it into plotly and iteractive!
        DEP::plot_missval(data_filt())
    })

    #### DATA normalization ####

    output$plot_before_normalization <- renderPlotly({
        `Before normalization` = data_filt()
        # ggplotly(DEP::plot_normalization(`Before normalization`))

        MQanalyser::plot_normalization_interactive(`Before normalization`)
    })

    output$plot_after_normalization <- renderPlotly({
        `After normalization` = data_norm()

        if (input$normalize_input == TRUE) {

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
         # data_norm <- DEP::normalize_vsn(data_filt)

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

    output$imputation <- renderPlotly(

        MQanalyser::plot_histogram_imputed(

            data_to_be_imputed = data_to_be_imputed(),
            bins = input$imputation_bins,
            combined = input$combined_imputation) %>%
            layout(height = 900, width = 1400)
    )

    #### Differential Enrichment analysis of Proteomics (DEP) ####

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

        # Format the _p.val column to two decimals
        pval_to_format <- which(base::endsWith(names(data_results), '_p.val'))



        data_results[,pval_to_format] <- format(data_results[,pval_to_format], digits =  3)

        # Format the _ratio columns to two decimals

        # ratio_to_format <- which(base::endsWith(names(data_results), '_ratio'))
        #
        # data_results[,ratio_to_format] <- format(round(data_results[,ratio_to_format], 2), nsmall = 2)


        # Imputed proteins

        imputed_proteins <- data_to_be_imputed() %>%
                                group_by(Protein.ID) %>%
                                summarise(Imputed = sum(Imputed == TRUE))

        imputed_proteins$Imputed <- ifelse(imputed_proteins$Imputed > 0, FALSE, TRUE)

        colnames(imputed_proteins) <- c('name', 'Imputed')

        #data_results$Imputed <- FALSE

        # Add column with wether the join the data_results with the imputed proteins

        results <- dplyr::full_join(data_results, imputed_proteins, by = "name")

        return(results)
    })

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

    #### Heatmap plot ####

    output$heatmaply <- renderPlotly({

        shiny::req(input$heatmapInteractive)

        if (input$heatmapInteractive == FALSE) {
            return(NULL)
        }

        MQanalyser::plot_heatmaply(dep(),
                                   intensity_type = input$IntensityType,
                                   dendogram = input$dendogram_input,
                                   k_row = input$k_row_input,
                                   k_col = input$k_col_input) %>%
            layout(height = 1000, width = 1000)

    }
    )


    heatmapPlot <- reactive({
        p <- DEP::plot_heatmap(dep = dep(),
                          type = 'centered')
        return(
            p
        )
    })

    output$heatMapNonInteractive <- renderPlot(height = 1000, width = 1000,{

        if (input$heatmapInteractive == TRUE) {
            return(NULL)
        }

        heatmapPlot()

    }
    )


    output$heatmapDownloader <- downloadHandler(

        filename = function(){
            'heatmap.png'
        },

        content = function(file){
            ggplot2::ggsave(file, heatmapPlot())
        }
    )



    output$heatmapUI <- renderUI({

        message(paste0('Value of the heatmap: ', input$heatmapInteractive))

        if (input$heatmapInteractive == FALSE) {
            return(
                shinycssloaders::withSpinner(

                plotOutput('heatMapNonInteractive'),
                    image = 'images/logoTransparentSmall.gif',
                    image.width = '200px'
                )
                )


        }else{

            return(
                shinycssloaders::withSpinner(
                    plotlyOutput('heatmaply'),
                    image = 'images/logoTransparentSmall.gif',
                    image.width = '200px'
                    )
            )
            }
    })

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

    output$font_gene_labels <- renderUI({
        if (input$showGeneNames == FALSE) {
            return(NULL)
        }

        shiny::sliderInput(
            inputId = 'font_gene_names',
            label = 'Adjust the font size of the labels:',
            min = 1,
            max = 10,
            value = 5
        )
    })

    # Create the volcano, it has two options: To be reactive with plotly,
    # Or to allow selection of points with the brush.
    # Each option has to be rendered in a diferent way with :
    # renderPlot or renderPlotly

    volcano_Plot <- reactive({

        coord_x  <- NULL
        coord_y <- NULL

        if(input$modify_axis == TRUE){
            coord_x  <- input$range_fc
            coord_y <- input$range_pvalue
        }

        if (input$showGeneNames == FALSE) {

            volcano_Plot <- MQanalyser::plot_volcano(
                proteomics_results = data_results(),
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
                coord_x = coord_x,
                coord_y = coord_y,
                show_genes_names = FALSE
                )
        }else{

            volcano_Plot <- MQanalyser::plot_volcano(
                proteomics_results = data_results(),
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
                coord_x = coord_x,
                coord_y = coord_y,
                show_genes_names = TRUE,
                brushed_Points = input$brush_volcano,
                font_gene_names = input$font_gene_names)
        }

        return(volcano_Plot)
    })

    output$volcano_plot_plotly <- renderPlotly({

        coord_x  <- NULL
        coord_y <- NULL

        if(input$modify_axis == TRUE){
            coord_x  <- input$range_fc
            coord_y <- input$range_pvalue
        }

        return(
            volcano_Plot() %>%
                layout(height = 1000, width = 1000)
            )
    })

    output$volcano_plot_genes <- renderPlot(height = 1000, width = 1000,{

        coord_x  <- NULL
        coord_y <- NULL

        if(input$modify_axis == TRUE){
            coord_x  <- input$range_fc
            coord_y <- input$range_pvalue
        }

        return(
            volcano_Plot()

        )
    })

    output$volcano_final <- renderUI({

        if (input$showGeneNames == TRUE) {
            return(
                plotOutput('volcano_plot_genes',
                           brush = 'brush_volcano')
            )
        }

        if (input$showGeneNames == FALSE) {
            return(

                plotlyOutput('volcano_plot_plotly')
            )
        }
    })


    # Download the volcano plot when not using plotly

    output$downloaderVolcano <- renderUI({
        if (input$showGeneNames == TRUE) {
            shiny::downloadButton('downloadvolcano',
                                  'Download the Volcano Plot')
        }
    })

    output$downloadvolcano <- downloadHandler(

            filename = function(){
                'volcano.png'
            },

            content = function(file){
                ggplot2::ggsave(file, volcano_Plot())
            }
    )

    #### PCA pot ####

    # Number of proteins that are selected to calculate the PCA
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

    # PCA plot

    pca_reactive <- reactive({
        pca_reactive <- MQanalyser::plot_pca_improved(dep = dep(),
                                      PC_x = 1,
                                      PC_y = 2,
                                      label_name = input$pca_label,
                                      n = input$pca_proteins)
        return(pca_reactive)

    })

    output$pca_plot <- renderPlot(height = 800, width = 1200,{
        pca_reactive()
    })

    # Download button for the PCA Plot
    output$downloadPCA <- downloadHandler(

            filename = function(){
                'pca_plot.png'
            },

            content = function(file){
                ggplot2::ggsave(file, pca_reactive())
            }
        )

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
            DT::datatable(
                MQanalyser::plot_profilely(
                    dep = dep(),
                    intensity_type = input$IntensityType,
                    color = NULL,
                    angle_labels = NULL,
                    selected_genes = NULL,
                    color_selected = NULL,
                    plot = FALSE),

                selection = 0,
                extensions = 'Scroller',

                  options = list(scrollY=500, scrollX=100)
                )
        }else{
            DT::datatable(
                MQanalyser::plot_profilely(
                    dep = dep(),
                    intensity_type = input$IntensityType,
                    color = NULL,
                    angle_labels = NULL,
                    selected_genes = NULL,
                    color_selected = NULL,
                    plot = FALSE),
                extensions = 'Scroller',

                options = list(scrollY=500, scrollX=100)
                )
            }
    })

    #### Enrichment Analysis ####
        # Comparisons -------------------------------

    output$comparisons_enrichment  <- renderUI({
        selectInput(inputId = 'comparison_enrch',
                    label = h4('Select the comparison:'),
                    choices = unlist(comparisons()),
                    selected = comparisons()[1])
    })

    output$selectUpregulatedEnrich <- renderUI({

        req(input$comparison_enrch)

        comparison <- input$comparison_enrch

        Option1 <- gsub(pattern = '_.*', '', comparison)
        Option2 <- gsub(pattern = '.*_', '', comparison)
        Option3 <- paste0('Combine: ', Option1, ' and ' , Option2)

        message(paste0('Option1: ' , Option1))
        message(paste0('Option2: ' , Option2))


        ## Unblock the other enrichment tabs
        shinyjs::runjs(
            '
                var tab = $(\'a[data-value="disease-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="network-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="pathway-tab"]\').parent();
                $(tab).removeClass("disabled");
                '
        )

        return(
            selectInput(
                inputId = 'upregulatedSelection',
                label = h4('Upregulated:'),
                choices = c(Option1,
                            Option2,
                            Option3)
                )
            )
    })

        # Enrichment elements -------------------------------

    # List containing the gene list and the failed to map number

    geneListObject <- reactive({

        message(paste0('The comparison of the samples for enrichment is: ', input$comparison_enrch))

        message(paste0('The organism  for enrichment is: ', input$enrich_organism))
        # geneListObject <- MQanalyser::create_geneList(
        #     data_results = data_results,
        #     comparison_samples = 'HEK_vs_III',
        #     organism = 'org.Hs.eg.db')

        geneListObject <- MQanalyser::create_geneList(
            data_results = data_results(),
            comparison_samples = input$comparison_enrch,
            organism = input$enrich_organism)

        # geneListObject <- MQanalyser::create_geneList(data_results = data_results,
        #                             comparison_samples = 'HEK_vs_TM',
        #                             organism = 'org.Hs.eg.db')
    })

    geneList <- reactive({

        geneList <- geneListObject()$geneList$ratio

        names(geneList) <- geneListObject()$geneList$ENTREZID

        # geneList <- geneListObject$geneList$ratio
        #
        # names(geneList) <- geneListObject$geneList$ENTREZID

        # apply log2fc cut off:

        comparison <- input$comparison_enrch

        Option1 <- gsub(pattern = '_.*', '', comparison)
        Option2 <- gsub(pattern = '.*_', '', comparison)
        Option3 <- paste0('Combine: ', Option1, ' and ' , Option2)

        req(input$upregulatedSelection)


        if (input$upregulatedSelection == Option3) {

            geneList <- geneList[abs(geneList) > log2(input$fc_enrichment)]


            # The positive values for option 1, which is Ctrl_vs_Tumor
            # Means the upregulated in Ctrl
        }else if(input$upregulatedSelection == Option1){

            geneList <- geneList[geneList > log2(input$fc_enrichment)]
            # geneList <- geneList[geneList > log2(1.5)]

            #The negative values, which means the upregulated in Tumour or
            # option2
        }else if(input$upregulatedSelection == Option2){
            geneList <- geneList[geneList < log2(input$fc_enrichment)]

        }
    })

    diffExpress <- reactive({

        # diffExpress <- names(geneList)

        de <- names(geneList())

    })

    # enrich results disease

    edo <- reactive({

        edo <- DOSE::enrichDGN(diffExpress())

        # edo <- DOSE::enrichDGN(diffExpress)

    })

    # Enrichment for gsea
    edo2 <- reactive({
        # edo2 <- DOSE::gseDO(geneList)
        message('edo2 being generated')

        edo2 <- DOSE::gseDO(geneList())

        message('edo2 created:')
        print(as.data.frame(edo2))

        return(edo2)
    })


    # Edox is the same as edo but changing the geneID for the gene Names.

    edox <- reactive({
        edox <- clusterProfiler::setReadable(edo(),
                                             input$enrich_organism,
                                             'ENTREZID')

        # edox <- clusterProfiler::setReadable(edo2,
        #                                      'org.Hs.eg.db',
        #                                      'ENTREZID')
        return(edox)
    })

    # Output box with the number of proteins

    output$differentiallyExpressedProteins <- renderInfoBox({

        a <- length(diffExpress())

        icon = "info"
        color = 'aqua'

        if (a == 0) {
            color = 'red'
            icon = "exclamation-triangle"
        }


        info <- infoBox(
            'Proteins Enriched',
            paste0(a, ' proteins used for enrichment.'),
            #icon = icon("stats", lib = "glyphicon"))
            icon = icon(icon),
            color =  color)
        return(info)
    })

    # Infobox with the genes tha failed to map
    output$failedToMapGenes <- renderInfoBox({

        failedToMap <- geneListObject()$failedToMap

        info <- infoBox(
            'Unmapped Proteins',
            paste0(failedToMap, '% were not mapped.'),
            icon = icon("exclamation-triangle"),
            color = 'yellow')
        return(info)
    })

    #### Enrichment Analysis Plots ####
        # Gene Ontology -------------------------------

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

    output$go_classification_plot <- renderPlotly({

        if(input$go_ontology == 'CC'){
            title = 'Cellular Component'
        } else if (input$go_ontology == 'MF'){
            title = 'Molecular Function'
        } else{
            title = 'Biological Function'
        }

        df <- geneOntologyTable()%>%
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

        ggplotly(p)%>%

            layout(height = 1000, width = 1200)
    })


        # prerank score plot -------------------------------
    enriched_plot_preranked <- reactive({

        if (is.null(edo2())) {

            message('Can not plot, not enough proteins')
            return(NULL)
        }

        if(input$runscore == 'all'){
            p <- enrichplot::gseaplot2(edo2(), geneSetID = 1, pvalue_table = F)
            #enrichplot::gseaplot2(edo2, geneSetID = 1)
        } else{
            p <- enrichplot::gseaplot(edo2(), geneSetID = 1, by = input$runscore, pvalue_table = F)
        }

        return(p)
    })

    output$enr_gseaplot <- renderPlot(height = 800, {

        if (is.null(edo2())) {

            message('Can not plot, not enough proteins')
            return(NULL)
        }

        rows_edo2 <- nrow(as.data.frame(edo2()))

        message(paste0('The number of rows of edo2 is:', rows_edo2))


        if (rows_edo2 == 0) {

            message('Can not plot, not enough proteins')
            return(NULL)
        }

        if (is.null(enriched_plot_preranked())) {
            message('The plot is null')
            return(NULL)
        }

        message('edo2() is not null, can the gsea enrichment plot be created?')

        return(enriched_plot_preranked())
    })

    output$preRankedPlotUI <- renderUI({

        message('Plotting network enrichment')

        rows_edo2 <- nrow(as.data.frame(edo2()))
        message(paste0('Number of rows edo2: ', rows_edo2))

        if (rows_edo2 == 0) {
            print(
                'The Gene Set Enrichment Analysis did not found any gene
                enriched under the specific p-value cut-off'
            )
        }else{

            shinycssloaders::withSpinner(
              plotOutput('enr_gseaplot'),
              image = 'images/logoTransparentSmall.gif',
              image.width = '200px'
              )
        }
    })

        # Network -------------------------------
    # Table
    network_enrich_table <- reactive({
        message('Creating the Gene Network in enrichment tab')

        bp <- enrichplot::pairwise_termsim(enrichGO(diffExpress(),
                                                    ont=input$go_ontology,
                                                    OrgDb = input$enrich_organism))
        return(bp)
    })

    # Plot
    output$network_renrichment <- renderPlot(height = 900, {

        enrichplot::emapplot(network_enrich_table())

    })

    # Table to Visualize

    # Download the table button

    netEnrichReadable <- reactive({
        message('Creating the Gene Network in enrichment tab for downloading')

        bp <- clusterProfiler::setReadable(network_enrich_table(),
                                           OrgDb = input$enrich_organism)

        bp <- as.data.frame(bp)
        return(bp)
    })


    output$netEnrichReadableOut <- DT::renderDataTable({

        DT::datatable(netEnrichReadable(),
                      extensions = 'Scroller',

                      options = list(scrollY=300,
                                     scrollX=30),
                      width = '300px',height = '200px')
    })


    output$download_network_table <- downloadHandler(

        filename = function(){ 'network_results.csv'},
        content = function(fname){
            write.csv(x = netEnrichReadable(),
                      fname,
                      row.names = FALSE)
        }

    )


    output$networkEnrichmentUI <- renderUI({

        if (length(diffExpress())== 0) {

            print('The Gene Set Enrichment Analysis did not found any gene
                enriched under the specific p-value cut-off')
        }else{

            fluidRow(
                column(
                    width = 12,

                    tabBox(height = 900, width = 900,
                        shinycssloaders::withSpinner(
                            plotOutput('network_renrichment'),
                            image = 'images/logoTransparentSmall.gif',
                            image.width = '200px'
                        )
                    ),



                    br(),

                    tabBox(height = 300, width = 600,
                        DT::dataTableOutput('netEnrichReadableOut')

                    ),
                    br(),
                    tabBox(

                        downloadButton(outputId = 'download_network_table',
                                       label = 'Download table')
                    )


                )
            )
        }

    })

        # GO table -------------------------------

    output$geneOntologyDataTable <- DT::renderDataTable({

        DT::datatable(geneOntologyTable(),
                      extensions = 'Scroller',

                      options = list(scrollY=500,
                                     scrollX=30),
                      width = '400px')
    })

    # Download the table button

    output$download_enrichment_table <- downloadHandler(

        filename = function(){ 'enrichment_results.csv'},
        content = function(fname){
            write.csv(geneOntologyTable(), fname)
        }
    )

    #### Disease Analysis Plots ####

    # Warning pop up that the disease tab is only available for human data

    observeEvent(input$tabs_menu, {

        message('user in this tab')

        req(input$enrich_organism)

        message(paste0('organism', input$enrich_organism))

        if (! input$enrich_organism == 'org.Hs.eg.db' &&
            input$tabs_menu == 'disease-tab') {

            message('alert launch')
            return(
                shinyalert::shinyalert("Disease Analaysis Unavailable",
                                       "This part is only available with human species",
                                       type="error",
                                       closeOnClickOutside = TRUE,
                                       closeOnEsc = TRUE,
                                       timer = 6000)
            )
        }
    })

    ## DISEASE PLOTS

    # Disease Enrichment
    output$enr_dotplot <- renderPlot(height = 1000,{

        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        enrichplot::dotplot(edo(),showCategory = 25)

    })

    # Disease GSEA

    output$enr_gseadotplot <- renderPlot(height = 1000,{

        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        message('Disease GSEA plot')
        message(paste0('Number of rows of edo2:', nrow(edo2())))

        if(nrow(edo2()) <1){
            print('Cant print')
        }else{
            dotplot(edo2(), showCategory=20) + ggtitle("dotplot for GSEA")
        }
    })

    # Disease plot of enriched terms

    output$heatmapnrich <- renderPlotly({
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        ggplotly(heatplot(edox() ,foldChange=geneList())) %>%

            layout(height = 800, width = 1400)
    })

    #Output overlapping distributions
    output$enr_ridgeplot <- renderPlot(height = 800, width =1200,{
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        ridgeplot(edo2())
    })

    # Disease Association

    output$upset <- renderPlot(height = 800, width = 1200,{
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        enrichplot::upsetplot(edo())

    })

    # Circus PLot

    output$enr_circusplot <- renderPlot(height = 1000,{
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        cnetplot(edox(),  circular = TRUE, colorEdge = TRUE)

    })

    #Gene Network

    output$enr_networkplot <- renderPlot(height = 900, width = 800, {
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        cnetplot(edox(), node_label = "all")
    })

    #Enrichment Map

    output$enr_mapplot <- renderPlot(height = 1000, width = 900, {
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        enrichplot::emapplot(pairwise_termsim(edo())#, node_scale=input$enrich_nodes
                             ,layout="kk")
    })

    # Disease Tabular format

    output$diseaseTable <- DT::renderDataTable({
        shiny::req(input$enrich_organism == 'org.Hs.eg.db')

        DT::datatable(as.data.frame(edox()),
                      extensions = 'Scroller',

                      options = list(scrollY=500,
                                     scrollX=30),
                      width = '400px')
    })

    # Download the table button

    output$download_disease_table <- downloadHandler(


        filename = function(){ 'disease_results.csv'},
        content = function(fname){
            write.csv(edox(), fname)
        }
    )

    #### PATHWAY ANALYSIS ####

    #KEGG analysis1

    kegg_react1 <- reactive({

        if (input$enrich_organism == 'org.Hs.eg.db' ) {
            organism <-  'hsa'
        }

        if (input$enrich_organism == 'org.Mm.eg.db' ) {
            organism <-  'mmu'
        }


        kk <- clusterProfiler::enrichKEGG(gene=diffExpress(),
                                          organism = organism,
                                          #minGSSize = 120,
                                          pvalueCutoff = 0.05,
                                          #verbose = FALSE
        )

        # kk <- clusterProfiler::enrichKEGG(gene=diffExpress,
        #                                   organism = 'hsa',
        #                                   #minGSSize = 120,
        #                                   pvalueCutoff = 0.05,
        #                                   #verbose = FALSE
        # )

        return(kk)
    })

    output$enr_kegg1 <- renderPlot(height = 900,{


        #print(kegg_react1())
        dotplot(kegg_react1(),showCategory =20)
    })

    pathways_id <- reactive({

        patwaisID_vec <- kegg_react1()$ID

        names(patwaisID_vec) <- kegg_react1()$Description

        return(patwaisID_vec)

    })


    pathway_table <- reactive({
        # Change the ids to the names
        kk <- clusterProfiler::setReadable(kegg_react1(),
                                           input$enrich_organism,
                                           'ENTREZID')
    })

    output$pathway_selector <- renderUI({

        selectInput(inputId = 'pathselec',
                    label = h4('Select the pathway to check:'),
                    choices = as.list(pathways_id()),
                    selected = as.list(pathways_id()[1])
                    )
    })


    output$pathwayTable <- DT::renderDataTable({

        # Change the ids to the names

        # kk <- clusterProfiler::setReadable(kegg_react1(),
        #                                    input$enrich_organism,
        #                                    'ENTREZID')

        DT::datatable(as.data.frame(pathway_table()),
                      extensions = 'Scroller',

                      options = list(scrollY=500,
                                     scrollX=30),
                      width = '400px', rownames = FALSE)
    })

    # Download the table button

    output$download_pathway_table <- downloadHandler(


        filename = function(){ 'pathway_results.csv'},
        content = function(fname){
            write.csv(pathway_table(), fname)
        }
    )



#     # If pressed the button, it will open a new tab.
#     observeEvent(input$GoToPathway, {
#

    output$pathwayButton <- renderUI({

        url <- paste0("http://www.kegg.jp/kegg-bin/show_pathway?",
                      input$pathselec,
                      '/',
                      kegg_react1()[input$pathselec, "geneID"])


        message(paste0('Pathway selected is: ',input$pathselec))

        message(paste0('The url is: ', url))

        shiny::a(
            actionBttn(
                inputId = 'GoToPathway',
                label = 'Browse Path',
                icon = NULL,
                style = "unite",
                color = "default",
                size = "md",
                block = FALSE,
                no_outline = TRUE
                ),
            target = "_blank",
            href = url)
    })

    #### Block the tabs ####

    # Observe which tab the user is in: Unblock the Preprocessing tab


    observeEvent(input$start_input, {

        message('Unblocking the preprocessing tab ')

         shinyjs::runjs(
         '
         var tab = $(\'a[data-value="preprocessing-tab"]\').parent();
         $(tab).removeClass("disabled");
         '
         )
    })

    # Unblock the results, heatmap, comparison, volcano, profile and enrichment
    # tabs

    observeEvent(input$preprocessing_tabset,{

        # If the user enters in one of the other three tabs creating the
        # data_se() summarized experiment object, allow them to enter in the
        # tabs
        if (! input$preprocessing_tabset == 'contaminants_tab' ) {

            message('Unblock the rest of the results and other tabs')

            shinyjs::runjs(
                '
                var tab = $(\'a[data-value="results-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="heatmap-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="comparisons-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="volcano-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="profile-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="enrichment-tab"]\').parent();
                $(tab).removeClass("disabled");

                '
            )
            }
    })

    # The pathway tabs are unblock under the enrichment section.

}
