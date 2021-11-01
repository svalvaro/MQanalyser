tagList(

#### navbar ####
navbarPage(fluid = TRUE,


    ##### style css ####
    titlePanel(
      title = tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"),


      # Show the logo in the browser tab
      windowTitle = tags$head(tags$link(rel = "icon", type = "image/png",
                                        href = "logo_small.png"),
                              tags$title("Proteomics Analyser"))
    ),
    theme = shinytheme(theme ='flatly'),


#### DATA INPUT ####

    tabPanel(h4('Data \nInput'),
            # class = 'bg-light',
            sidebarLayout(
                sidebarPanel(id = 'sidebar',
                             width = 2,
                  shinyalert::useShinyalert(),

                    fileInput(inputId = 'proteinInput',
                              label = h4('Upload the proteinGroups.txt or
                                         proteoQuant.csv'),
                              multiple = FALSE,
                              accept = 'text'),

                    verbatimTextOutput('sw_used'),

                    uiOutput('intensity_selector'),

                    verbatimTextOutput('IntensityFound_message'),
                    hr(),
                    br(),

                    fileInput(inputId = 'optional_exp_design',
                              label='Provide the experiment design (Optional)',
                              multiple= FALSE,
                              accept= 'text'),
                    br(),
                    dropdown(
                        tags$h3("Advanced Parameters"),

                        sliderInput(inputId = 'input_pvalue',
                                    label = 'Adjusted p-value cut off',
                                    value = 0.05,
                                    min = 0, max = 1),

                        numericInput(inputId = 'input_fc',
                                    label = 'Log2 fold change cut off',
                                    value = 1.5,
                                    min = 0, max = 100),

                        options = list(`style` = "btn-info"),
                        style = "unite", icon = icon("cogs"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                            )
                        ),

                    br(),

                    fileInput(inputId = 'user_genes',
                              label='Provide a list of desired genes to check
                              in the analysis: (Optional)',
                              multiple= FALSE,
                              accept= 'text'),
                    hr(),
                    br(),

                  actionBttn(inputId = 'Demo',
                             label = 'Start Demo',
                             icon = NULL,
                             style = "unite",
                             color = "default",
                             size = "md",
                             block = FALSE,
                             no_outline = TRUE
                  ),

                    #Download a file example
                    h4('Exapmles of the file(s) to be uploaded:'),
                    fluidRow(

                        column(5,a(href='proteinGroup_example.txt',
                                   'proteinGroups.txt',
                                   download=NA,
                                   target='_blank') ),

                        column(5,a(href='experiment_design_example.txt',
                                   'experiment_design.txt',
                                   download=NA,
                                   target='_blank') ),


                        column(5,a(href='user_genes_examples.txt',
                                   'Genes example',
                                   download=NA,
                                   target='_blank')
                               ),
                        )
                  ),

                mainPanel(
                    fluidRow(
                        column(8,
                               box(title = h3('Experiment Design'),
                                   width = 10,
                                   h4('Welcome to analysis of the results
                                      of LC-MS/MS'),
                                   h5('Start by uploading the proteinGroups.txt
                                      table provided.'),
                                   h5('Please Edit the next table by adding the
                                      Condition and Replicate.'),
                                   h5('Alternatively an experiment design can
                                      be uploaded. Be sure that the Sample
                                      names are correct.'),
                                   hr(),

                                   #DTOutput('experiment_design')
                                   #DT::dataTableOutput('ed_out'),
                                   rHandsontableOutput('ed_out'),
                                   br(),
                                   downloadButton(
                                    outputId = 'download_experiment_design',
                                    label = 'Download (Press Start
                                    Analysis First)'),
                                   )
                               ),

                        column(3,
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               #box(
                                 # actionButton(inputId = "start_input",
                                 #                label = "Start Analysis",
                                 #                icon = icon("play"))

                                 uiOutput('start_analysis')
                                   #)
                               )
                        )
                    )
                ),

    # Footer -------------------------------
            br(),
            hr(style = "border-color: #cbcbcb;"),

            fluidRow(
              column(9,

                     p("App created by ",
                       tags$a(href = "https://www.linkedin.com/in/svalvaro/",
                              'Alvaro Sanchez-Villalba',
                              target = '_blank'),
                       HTML("&bull;"),
                       style = "font-size: 85%"),

                     p("Have a question? Spot an error? Send an email ",
                       tags$a(href = "mailto:alvaro.sanchez@fgu.cas.cz",
                              tags$i(class = 'fa fa-envelope',
                                     style = 'color:#990000'),
                              target = '_blank'),
                       style = "font-size: 85%"),
                     p(tags$em("Last updated: October 2021"),
                       style = 'font-size:75%'))
            )
            ),

#### Preprocessing ####

    # uiOutput('preprocessing-tab'),



    tabPanel(
      title = h4("Preprocessing"),
      value = 'preprocessing-tab',
      # id = 'preprocessing-tab',
      # value = 'preprocessing-tab',
      #style = 'pointer-events: none;',

      tabsetPanel(
        type = 'tabs',
        # Contaminants -------------------------------

        tabPanel(title = 'Contaminants',

                 # Use this to be able to use the shinydashboard stuff
                 shinyWidgets::useShinydashboard(),

                 sidebarLayout(
                   sidebarPanel(
                     id = 'sidebar',
                     width = 2,

                     checkboxInput(inputId = 'removeContaminantsInput',
                                   label = h4('Remove Contaminants'),
                                   value = TRUE)
                     # Reverse and Identified by one hit will be removed
                     #automatically
                   ),
                   mainPanel(
                     column(width = 10,

                            h4('Filter out the contaminant proteins.'),
                            br(),
                            shinydashboard::infoBoxOutput('contaminants_box',
                                                          width = 8),

                            br(),
                            br(),
                            hr(),
                            box(width = 10,height = 850,
                                shinycssloaders::withSpinner(
                                  plotlyOutput('contaminantsPlot'),
                                  image = 'logoTransparentSmall.gif',
                                  image.width = '200px')
                                )
                            )
                     )
                   )
                 ),

        # Filter Missing Values -------------------------------
        tabPanel('Filter out missing values',
                 sidebarLayout(
                   sidebarPanel(id = 'sidebar',
                                width = 2,
                                uiOutput('na_threshold')),
                   mainPanel(fluid = FALSE,
                     #fluidRow(
                       column(
                         width = 8,
                         height = 800,
                         shinycssloaders::withSpinner(
                         plotlyOutput('barplot_missvals'),
                         image = 'logoTransparentSmall.gif',
                         image.width = '200px')
                         ),
                     #column(width = 1),

                       column(
                         width = 4,
                         shinycssloaders::withSpinner(
                           plotOutput('heatmap_nas'),
                           image = 'logoTransparentSmall.gif',
                           image.width = '200px')
                              )
                     )
                   )
                 ),

        # Normalization -------------------------------
                               tabPanel('Normalization',

                                        sidebarLayout(

                                          sidebarPanel(id = 'sidebar',
                                            width = 2,
                                            checkboxInput(inputId = 'normalize_input',
                                                          label = h4('Use normalized intensities by variance stabilizing transformation (VSN)'),
                                                          value = TRUE),
                                            value = TRUE
                                          ),

                                          mainPanel(

                                            print(h4('Normalization of the intensities')),
                                            br(),
                                            box(
                                              shinycssloaders::withSpinner(
                                                plotlyOutput('plot_before_normalization'),
                                                image = 'logoTransparentSmall.gif',
                                                image.width = '200px'
                                              ),

                                              shinycssloaders::withSpinner(
                                                plotlyOutput('plot_after_normalization'),
                                                image = 'logoTransparentSmall.gif',
                                                image.width = '200px'
                                                )
                                              )
                                            )
                                          )
                                        ),
        # Imputation missing values -------------------------------
                               tabPanel('Imputation of the missing values',

                                        sidebarLayout(
                                          sidebarPanel(id = 'sidebar',
                                            width = 2,

                                            selectInput(inputId = 'input_imputation',
                                                        label = h4('Imputation type'),
                                                        choices = c('Manual Imputation' = 'Manual',
                                                                    'Bayesian' = 'bpca',
                                                                    'Quantile Regression'= 'QRILC',
                                                                    'MinProb'= 'MinProb',
                                                                    'Nearest neighbour averaging' = 'knn',
                                                                    'MinProb' = 'MinProb',
                                                                    'Maximum likelihood-based ' = 'MLE',
                                                                    'Minimum Value' = 'min',
                                                                    'Replace by 0' = 'zero',
                                                                    'No imputation' = 'none'),
                                                        selected = 'MLE'),

                                            uiOutput('manual_imputation_scale'),

                                            uiOutput('manual_imputation_shift'),

                                            checkboxInput(inputId = 'combined_imputation',
                                                          label = h4('Combine the samples into one plot'),
                                                          value = FALSE)
                                                       ),

                                          mainPanel(
                                            box(
                                              shinycssloaders::withSpinner(
                                                plotlyOutput('imputation'),
                                                image = 'logoTransparentSmall.gif',
                                                image.width = '200px'
                                              )
                                            )

                                          )
                                        )
                                        )
                               )
             ),

#### Results Panel ####
    tabPanel(h4("Results"),

      includeCSS("www/styles.css"),
      box(width = 4, solidHeader = FALSE,
          shinydashboard::infoBoxOutput('significant_proteins',
                                        width = NULL)
          ),

      box(width = 4,
          shinydashboard::infoBoxOutput('significant_user_genes',
                                        width = NULL)
          ),

      box(width = 4,
          uiOutput('table_user_genes')
          ),

      br(),
      br(),
      hr(),

      #box(
        shinycssloaders::withSpinner(
          DT::dataTableOutput('proteomics_results'),
          image = 'logoTransparentSmall.gif',
          image.width = '200px'
        ),
      #),

      br(),
      downloadButton(outputId = 'download_proteomics',
                      label = 'Download'),
      ),

#### HeatMap ####
    tabPanel(h4("Heatmap"),
             sidebarLayout(
               sidebarPanel(id = 'sidebar',
                            width = 2,

                        selectInput(inputId = 'dendogram_input',
                                    label = 'Type of Clustering',
                                    choices = c('Proteins and the samples' = 'both',
                                                'Only for the genes.'= 'row',
                                                'Only for the samples.'= 'column',
                                                'No Clustering' = 'none'),
                                    selected = 'both'),

                        sliderInput(inputId = 'k_row_input',
                                    label = 'Colour clusters, genes:',
                                    value = 0,
                                    min = 0, max = 50),

                        sliderInput(inputId = 'k_col_input',
                                     label = 'Colour clusters, samples',
                                     value = 0,
                                     min = 0, max = 50)
                        ),
                mainPanel(
                    box(
                        shinycssloaders::withSpinner(
                            plotlyOutput('heatmaply'),
                            image = 'logoTransparentSmall.gif',
                            image.width = '200px'
                            )
                        )
                    )
                )
            ),


#### Sample Comparisons ####

tabPanel(h4("Sample Comparisons"),

         tabsetPanel(type = 'tabs',
    # Scatter Plot -------------------------------
                     tabPanel(title = h4('Scatter Plot'),

                              sidebarLayout(
                                sidebarPanel(id = 'sidebar',
                                             width = 2,
                                             h3("Select the adjustments"),

                                             #Sample for X axis
                                             uiOutput("x_sample_selector"),
                                             #Sample for y axis

                                             uiOutput("y_sample_selector"),

                                             #check if they want to see their genes

                                             checkboxInput(inputId = 'showgenes',
                                                           label = h4(
                                                             'Show the differentially expressed
                                  proteins of the list that you have uploaded'),
                                                           value=FALSE),
                                             dropdown(
                                               tags$h3("Advanced Parameters"),

                                               sliderInput(inputId = 'input_alpha',
                                                           label = 'Adjust the alpha parameter:',
                                                           value = 0.8,
                                                           min = 0, max = 1),

                                               #add or not a regression line
                                               checkboxInput(inputId = 'input_lm',
                                                             label = h4('Add regression line'),
                                                             value = FALSE),

                                               colourpicker::colourInput(inputId = "color_scatter",
                                                                         h4("Select colour:"),
                                                                         '#56B4E9',
                                                                         palette = "square",
                                                                         returnName = TRUE,
                                                                         showColour = c("background")),


                                               colourpicker::colourInput(
                                                 inputId = "color_de_scatter",
                                                 h4(
                                                   "Select colour for your proteins of interest:"),
                                                 '#dc143c',
                                                 palette = "square",
                                                 returnName = TRUE,
                                                 showColour = c("background")
                                               ),

                                               options = list(`style` = "btn-info"),
                                               style = "unite", icon = icon("paint-brush"),
                                               status = "success", width = "300px",
                                               animate = animateOptions(
                                                 enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutRightBig)
                                             )
                                             ),

                                mainPanel(
                                  #Plot the scatter plot  in the second tab
                                  box(height = 1200,width = 1200,
                                      shinycssloaders::withSpinner(
                                        plotlyOutput('scatterplot'),
                                        image = 'logoTransparentSmall.gif',
                                        image.width = '200px'
                                      )
                                  )
                                )
                              )
                     ),
    # Correlation Plot -------------------------------
                     tabPanel(h4('Correlation'),
                              fluidRow(
                                column(
                                  width = 12,
                                  align = 'center',
                                  print('If you would like to analyse each plot individually,
                go to the next tab "Scatter Plot"'),
                                  hr(),
                                  box(width = 11,

                                      shinycssloaders::withSpinner(
                                        ui_element = plotlyOutput('plot_correlation'),
                                        image = 'logoTransparentSmall.gif',
                                        image.width = '200px'
                                      )
                                  )
                                )
                              )
                     ),
    # PCA Plot -------------------------------
                     tabPanel(h4('PCA'),
                              sidebarLayout(
                                sidebarPanel(id = 'sidebar',
                                             width = 2,
                                             h3("Select the adjustments"),

                                             selectInput(inputId = 'pca_label',
                                                         label = 'Select the labels:',
                                                         choices =  c('label' = 'rowname',
                                                                      'experiment' = 'label',
                                                                      'replicate' = 'replicate'),
                                                         selected = 'rowname'
                                             ),
                                             uiOutput('pca_number_proteins'),
                                             # Download button for the plot
                                             downloadButton(outputId = 'downloadPCA',
                                                            label = 'Download the PCA plot')
                                             ),
                                mainPanel(
                                  box(
                                    height = 900,
                                    width = 1300,
                                    shinycssloaders::withSpinner(
                                      plotOutput('pca_plot'),
                                      image = 'logoTransparentSmall.gif',
                                      image.width = '200px'
                                    )
                                  )
                                )
                              )
                              )
    )
),
#### Volcano Plot ####
    tabPanel(h4('Volcano Plot'),
            sidebarLayout(
                sidebarPanel(id = 'sidebar',
                             width = 2,
                    h3("Select the adjustments"),

                    uiOutput("comparisons_out"),

                    dropdown(
                        tags$h3("Advanced Parameters"),


                        #check box for protein IDs
                        checkboxInput(
                          inputId = 'modify_axis',
                          label =h4(
                          'Would you prefer to modify the
                          axis values: \n(Uncheck to restore values)'),
                          value = FALSE),

                        conditionalPanel(
                          "input.modify_axis == 1",

                          sliderInput(inputId = 'range_fc',
                                      label = h4('Log2(Fold-Change) Range'),
                                      min = -10,
                                      max = 10,
                                      step = 0.1,
                                      value = c(-6,6) ),

                          sliderInput(inputId = 'range_pvalue',
                                      label = h4('-Log10(P-Value): Range'),
                                      min = 0,
                                      max = 50,
                                      step = 0.5,
                                      value = c(0,5)
                                      )
                          ),
                        br(),
                        hr(),
                        sliderInput(inputId = 'volc_alpha',
                                    label = 'Adjust the transparency of
                                    the points:',
                                    value = 0.8,
                                    min = 0, max = 1),

                        #check box for protein IDs
                        checkboxInput(inputId = 'p_adj_input',
                                      label =h4('Use adjusted P values'),
                                      value = TRUE),

                          #Select the Colour for UPregulated
                          colourpicker::colourInput("col_upregulated",
                                                    h4("Colour upregulated:"),
                                                    'brown2',
                                                    palette = "square",
                                                    returnName = TRUE,
                                                    showColour = c("background")),

                          #Select the color for Down regulated
                          colourpicker::colourInput("col_downregulated",
                                                    h4("Colour downregulated:"),
                                                    'cyan3',
                                                    palette = "square",
                                                    returnName = TRUE,
                                                    showColour = c("background")
                                                    ),

                        #Select the color for selected genes
                        colourpicker::colourInput("col_selected",
                                                  h4("Colour your proteins of interest:"),
                                                  '#800080',
                                                  palette = "square",
                                                  returnName = TRUE,
                                                  showColour = c("background")
                        ),

                        options = list(`style` = "btn-info"),
                        style = "unite", icon = icon("paint-brush"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                        )
                    ),
                    hr(),

                    tags$div(
                    title = "Press ON if you would like to see the gene names. Draw a box around the genes that you are interested in",

                      shinyWidgets::switchInput(
                        inputId = "showGeneNames",
                        label = "Gene Names",
                        labelWidth = "80px",
                        onStatus = 'success',
                        value = FALSE,
                        offStatus = 'danger')
                      ),

                    # Font size depending on the showGeneNames:

                    uiOutput('font_gene_labels'),

                    hr(),
                    #Check box for fav genes
                    checkboxInput(inputId = 'showgenes_volcano',
                                  label=h4('Show your selected genes'),
                                  value=FALSE),

                    #Download button for the volcano plot
                    #downloadButton('downloadvolcano','Download the Volcano Plot')
                    uiOutput('downloaderVolcano')
                ),

                mainPanel(
                  box(
                    shinycssloaders::withSpinner(
                      uiOutput('volcano_final'),
                      #plotlyOutput('volcano_plot'),
                      image = 'logoTransparentSmall.gif',
                      image.width = '200px'
                      )
                    )
                  )
                )
            ),


#### Profile Plot ####
    tabPanel(h4('Profile Plot'),
            sidebarLayout(
                sidebarPanel(id = 'sidebar',
                             width = 2,
                    h3("Select the adjustments"),
                    br(),
                    dropdown(
                        tags$h3("Advanced Parameters"),
                        colourpicker::colourInput(inputId = "input_col_prof",
                                                  label = h4("Colour all proteins:"),
                                                  value = "#56B4E9",
                                                  palette = "square",
                                                  returnName = TRUE,
                                                  showColour = c("background")
                                                  ),

                        colourpicker::colourInput(inputId = "input_col_sel",
                                                  label = h4("Colour for\nproteins selected in the table:"),
                                                  value = "red",
                                                  palette = "square",
                                                  returnName = TRUE,
                                                  showColour = c("background")
                                                  ),


                        colourpicker::colourInput(inputId = "input_col_prof_de",
                                                  label = h4("Colour your proteins of interest:"),
                                                  value = "#800080",
                                                  palette = "square",
                                                  returnName = TRUE,
                                                  showColour = c("background")
                        ),
                        sliderInput(inputId = 'profile_alpha',
                                    label = 'Transparency of the plot',
                                    value = 0.9,
                                    min = 0.01, max = 1),

                        sliderInput(inputId = 'input_angle_samples',
                                    label = 'Select the angle of the lables',
                                    value = 45,
                                    min = 0, max = 90),

                        checkboxInput(inputId = 'prof_centered',
                                      label = h4('Show centered intensities (remove mean)',
                                                 value=FALSE)
                        ),

                        options = list(`style` = "btn-info"),
                        style = "unite", icon = icon("paint-brush"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig)
                        ),

                    br(),

                    checkboxInput(inputId = 'prof_genes_de',
                                  label = h4('Show the genes that you have uploaded:',
                                             value=FALSE)
                                  )

                ),

                mainPanel(box(height = 800, width = 1300,

                              shinycssloaders::withSpinner(
                                plotlyOutput('plot_profile'),
                                image = 'logoTransparentSmall.gif',
                                image.width = '200px'
                                )
                              ),
                          br(),
                          # Not sure if this checbox is that useful because at the end
                          # the user has to remove the selection of the rows manually.
                          # Ideally, I can find a way that after using the checkbox,
                          # the selected rows are removed. That's would be a good solution.

                          checkboxInput(inputId = 'clear_selection',
                                        label =h4('Remove the selected genes from the table. (Press twice)'),
                                        value = FALSE),
                          br(),
                          hr(),
                          box(height = 400, width =300,
                              DT::dataTableOutput('plot_profile_table')
                              )
                          )
                )
            ),
#### Enrichment analysis ####

    tabPanel(h4('Enrichment Analysis'),
            sidebarLayout(
                sidebarPanel(id = 'sidebar',
                             width = 2,
                    h3("Select the adjustments"),

                    # Comparison to perform
                    uiOutput('comparisons_enrichment'),

                    uiOutput('selectUpregulatedEnrich'),
                    br(),

                    # Organism used,
                    selectInput(inputId = 'enrich_organism',
                                label = 'Select the species:',
                                choices = c('Human' = 'org.Hs.eg.db',
                                            'Mouse'= 'org.Mm.eg.db',
                                            'Rat'= 'org.Rn.eg.db'),
                                            #'Yeast' = 'org.Sc.sgd.db'),
                                selected = 'org.Hs.eg.db'),
                    br(),

                    # selectInput(inputId = 'enrichment_selection_genes',
                    #             label = 'Proteins to do the enrichment analyisis:',
                    #             choices = c('Upregulated' ,
                    #                         'Downregulated',
                    #                         'Both'),
                    #
                    #             selected = 'Upregulated'),




                    dropdown(
                      # Number of proteins selected:

                      sliderInput(inputId = 'fc_enrichment',
                                  h4('The Log2 Fold Change can be modified to run
                                   the enrichment analysis with proteins more significant.'),
                                  min = 1,
                                  max = 20,
                                  value = 1.5,
                                  step = 0.5),

                      sliderInput(inputId = 'go_level',
                                  h4('Degree of specificity, or GO level. The higher the more specific:'),
                                  min = 1,
                                  max = 20,
                                  value = 3,
                                  step = 1),

                      options = list(`style` = "btn-info"),
                      style = "unite", icon = icon("cogs"),
                      status = "success", width = "300px",
                      animate = animateOptions(
                        enter = animations$fading_entrances$fadeInLeftBig,
                        exit = animations$fading_exits$fadeOutRightBig
                      )
                    )
                    ),

                mainPanel(

                    box(height = 2500,
                        width = 4000,
                        shinydashboard::infoBoxOutput('differentiallyExpressedProteins',
                                                      width = 5),
                        br(),

                        tabsetPanel(type = 'tabs',

                                    tabPanel('Gene Ontology',
                                             br(),


                                             selectInput(inputId = 'go_ontology',
                                                label = 'What are you interested in?',
                                                choices = c('Cellular Component' = 'CC',
                                                            'Biological Function'= 'BP',
                                                            'Molecular Function'= 'MF'),
                                                width = '300px',
                                                selected = 'CC'),

                                             shinycssloaders::withSpinner(
                                               plotlyOutput('go_classification_plot'),
                                               image = 'logoTransparentSmall.gif',
                                               image.width = '200px'
                                               )
                                    ),


                                    tabPanel('GSEA Enrichment',
                                             br(),
                                             #Input for type of preranked score

                                             selectInput(inputId = 'runscore',
                                                         label=h4('For the Running Score, select the choice:'),
                                                         choices = c('Running Score + Ranked List' = 'all',
                                                                     'Only Running Enrhichment Score' ='runningScore',
                                                                     'Only Ranked List' = 'preranked'),
                                                         selected = 'all'),
                                             br(),

                                             shinycssloaders::withSpinner(
                                               plotOutput('enr_gseaplot'),
                                               image = 'logoTransparentSmall.gif',
                                               image.width = '200px'
                                               )
                                             )
                                    )
                        )
                    )
                )
            ),


#### Disease Analysis Tab ####
  tabPanel(title = h4('Disease Analysis'),
           sidebarLayout(
             sidebarPanel(id = 'sidebar',
                          width = 2#,
               #' uiOutput('comparisons_diseases'),
               #' selectInput(inputId = 'disease_organism',
               #'             label = 'Select the species:',
               #'             choices = c('Human' = 'org.Hs.eg.db',
               #'                         'Mouse'= 'org.Mm.eg.db',
               #'                         'Rat'= 'org.Rn.eg.db'),
               #'                         #'Yeast' = 'org.Sc.sgd.db'),
               #'             selected = 'org.Hs.eg.db'),
               #'
               #' hr(),
               #'
               #' sliderInput(inputId = 'fc_disease',
               #'             h4('The Log2 Fold Change can be modified to run
               #'                     the enrichment analysis with proteins more significant.'),
               #'             min = 1,
               #'             max = 20,
               #'             value = 1.5,
               #'             step = 0.5),
               #' verbatimTextOutput('diff_disease_number'),
             ),

             mainPanel(
               box(height = 2500, width = 2000,
                   tabsetPanel(type = 'tabs',

                               tabPanel('Disease Enrichment',
                                        hr(),
                                        # print('Dot plot is similar to bar
                                        #        plot with the capability to encode another score as dot size.'),
                                        shinycssloaders::withSpinner(
                                          plotOutput('enr_dotplot'),
                                          image = 'logoTransparentSmall.gif',
                                          image.width = '200px')
                               ),

                               tabPanel('Disease GSEA',
                                        hr(),
                                        shinycssloaders::withSpinner(
                                          plotOutput('enr_gseadotplot'),
                                          image = 'logoTransparentSmall.gif',
                                          image.width = '200px'
                                          )
                               ),

                               tabPanel('Disease Heatmap',
                                        hr(),
                                        print('The heatmap can simplify the results making it easier to
                                                   identify expression patterns.'),
                                        hr(),
                                        shinycssloaders::withSpinner(
                                          plotlyOutput('heatmapnrich'),
                                          image = 'logoTransparentSmall.gif',
                                          image.width = '200px')
                               ),

                               tabPanel('Disease density',
                                        hr(),
                                        print('The ridgeplot will visualize expression distributions
                                                    of core enriched genes for GSEA enriched categories.
                                                    It helps users to interpret up/down-regulated pathways.'),
                                        hr(),
                                        shinycssloaders::withSpinner(
                                          plotOutput('enr_ridgeplot'),
                                          image = 'logoTransparentSmall.gif',
                                          image.width = '200px')
                               ),

                               tabPanel('Disease association',
                                        hr(),
                                        print('visualizing the complex association between genes and gene sets.
                                                   It emphasizes the gene overlapping among different gene sets.'),
                                        hr(),
                                        shinycssloaders::withSpinner(
                                          plotOutput('upset'),
                                          image = 'logoTransparentSmall.gif',
                                          image.width = '200px'
                                          )
                                        )
                               )
                   )
               )
             )
           ),


#### Gene Network ####
  tabPanel(title = h4('Gene Network'),
           sidebarLayout(
               sidebarPanel(id = 'sidebar',
                            width = 2,
                 #'
                 #' uiOutput('comparisons_network'),
                 #'
                 #' selectInput(inputId = 'network_organism',
                 #'             label = 'Select the species:',
                 #'             choices = c('Human' = 'org.Hs.eg.db',
                 #'                         'Mouse'= 'org.Mm.eg.db',
                 #'                         'Rat'= 'org.Rn.eg.db'),
                 #'             #'Yeast' = 'org.Sc.sgd.db'),
                 #'             selected = 'org.Hs.eg.db'),
                 #'
                 #' hr(),
                 #'
                 #' sliderInput(inputId = 'fc_network',
                 #'             h4('The Log2 Fold Change can be modified to run
                 #'                   the enrichment analysis with proteins more significant.'),
                 #'             min = 1,
                 #'             max = 20,
                 #'             value = 1.5,
                 #'             step = 0.5),
                 #'
                 #' verbatimTextOutput('diff_network_number')
                 ),

               mainPanel(
                   box(height = 2500, width = 2000,
                       tabsetPanel(type = 'tabs',

                                   tabPanel('Biological Function',
                                            hr(),
                                            shinycssloaders::withSpinner(
                                              plotOutput('bio_comparison'),
                                              image = 'logoTransparentSmall.gif',
                                              image.width = '200px'
                                              )
                                   ),

                                   tabPanel('Circus',
                                            hr(),
                                            shinycssloaders::withSpinner(
                                              plotOutput('enr_circusplot'),
                                              image = 'logoTransparentSmall.gif',
                                              image.width = '200px'
                                              )
                                            ),

                                   tabPanel('Gene Network',
                                            hr(),
                                            shinycssloaders::withSpinner(
                                              plotOutput('enr_networkplot'),
                                              image = 'logoTransparentSmall.gif',
                                              image.width = '200px'
                                              )
                                            ),

                                   tabPanel('Enrichment Map',
                                            hr(),
                                            print('Enrichment map organizes enriched terms into a network
                                                   with edges connecting overlapping gene sets. In this way,
                                                   mutually overlapping gene sets are tend to cluster together,
                                                   making it easy to identify functional module.'),
                                            hr(),
                                            shinycssloaders::withSpinner(
                                              plotOutput('enr_mapplot'),
                                              image = 'logoTransparentSmall.gif',
                                              image.width = '200px'
                                              )
                                            )
                                   )
                       )
                   )
               )
           ),


#### Pathway Analysis ####
  tabPanel(title = h4('Pathway Analysis'),
           sidebarLayout(
               sidebarPanel(id = 'sidebar',
                            width = 2,
                  uiOutput("pathway_selector"),

                 actionBttn(inputId = 'GoToPathway',
                            label = 'Go to KEGG website',
                            icon = NULL,
                            style = "unite",
                            color = "default",
                            size = "md",
                            block = FALSE,
                            no_outline = TRUE
                            ),



                 plotOutput('enr_kegg2')
                 ),

               mainPanel(
                   box(height = 2500, width = 2000,
                        h4('KEGG Pathway'),
                        hr(),
                        shinycssloaders::withSpinner(
                          plotOutput('enr_kegg1'),
                          image = 'logoTransparentSmall.gif',
                          image.width = '200px'
                          )
                       )
                   )
           )
  ),

#### Logo panel ####
  tabPanel(id = 'logo_tab',
           position = 'right',
           title =   tags$img(
      src='logo.png',
      width = 150),


      fluidRow(
        column(9,

               p("App created by ",
                 tags$a(href = "https://www.linkedin.com/in/svalvaro/",
                        'Alvaro Sanchez-Villalba',
                        target = '_blank'),
                 HTML("&bull;"),
                 style = "font-size: 85%"),

               p("Have a question? Spot an error? Send an email ",
                 tags$a(href = "mailto:alvaro.sanchez@fgu.cas.cz",
                        tags$i(class = 'fa fa-envelope',
                               style = 'color:#990000'),
                        target = '_blank'),
                 style = "font-size: 85%"),
               p(tags$em("Last updated: October 2021"),
                 style = 'font-size:75%'))
      )
      )
  )
)
