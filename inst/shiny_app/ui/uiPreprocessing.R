uiPreprocessing <-     tabPanel(
    title = h4("Preprocessing"),
    value = 'preprocessing-tab',

    tabsetPanel(
        type = 'tabs',

        id = 'preprocessing_tabset',

        # Contaminants -------------------------------

        tabPanel(title = 'Contaminants',
                 value = 'contaminants_tab',

                 # Use this to be able to use the shinydashboard stuff
                 shinyWidgets::useShinydashboard(),

                 sidebarLayout(
                     sidebarPanel(
                         id = 'sidebar',
                         width = 2,

                         checkboxInput(inputId = 'removeContaminantsInput',
                                       label = h4('Remove Contaminants'),
                                       value = TRUE),

                         uiOutput('fastaSelection'),

                         uiOutput('fastaInput'),

                         tags$div(
                             title = "The interactive version is under development since it might slow down, or crash the application",

                             shinyWidgets::switchInput(
                                 inputId = "contaminantsInteractive",
                                 label = "Interactive",
                                 labelWidth = "80px",
                                 value = FALSE,
                                 offStatus = 'danger'),
                         )

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
                                uiOutput('contaminantsUI')
                         )
                     )
                 )
        ),

        # Filter Missing Values -------------------------------
        tabPanel('Filter out missing values',
                 value = 'filter_tab',

                 sidebarLayout(
                     sidebarPanel(id = 'sidebar',
                                  width = 2,
                                  uiOutput('na_threshold')),


                     mainPanel(fluid = FALSE,
                               column(
                                   width = 8,
                                   height = 800,
                                   shinycssloaders::withSpinner(
                                       plotlyOutput('barplot_missvals'),
                                       image = 'images/logoTransparentSmall.gif',
                                       image.width = '200px')
                               ),

                               column(
                                   width = 4,
                                   uiOutput('triggerjs'),
                                   shinycssloaders::withSpinner(
                                       plotOutput('heatmap_nas'),
                                       image = 'images/logoTransparentSmall.gif',
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
                                 image = 'images/logoTransparentSmall.gif',
                                 image.width = '200px'
                             ),

                             shinycssloaders::withSpinner(
                                 plotlyOutput('plot_after_normalization'),
                                 image = 'images/logoTransparentSmall.gif',
                                 image.width = '200px'
                             )
                         )
                     )
                 )
        ),
        # Imputation missing values -------------------------------
        tabPanel('Imputation of the missing values',
                 value = 'imputation_tab',

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
                                              selected = 'Manual'),

                                  uiOutput('manual_imputation_scale'),

                                  uiOutput('manual_imputation_shift'),

                                  checkboxInput(inputId = 'combined_imputation',
                                                label = h4('Combine the samples into one plot'),
                                                value = FALSE),

                                  sliderInput('imputation_bins',
                                              label = 'Number of bins',
                                              min = 1,
                                              max = 100,
                                              value = 30)
                     ),

                     mainPanel(
                         box(
                             shinycssloaders::withSpinner(
                                 plotlyOutput('imputation'),
                                 image = 'images/logoTransparentSmall.gif',
                                 image.width = '200px'
                             )
                         )

                     )
                 )
        )
    )
)
