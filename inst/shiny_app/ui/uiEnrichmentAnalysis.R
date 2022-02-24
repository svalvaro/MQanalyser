uiEnrichmentAnalysis <-tabPanel(h4('Enrichment Analysis'),
                                value = 'enrichment-tab',
                                id = 'enrichment-tab-id',
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

                                    mainPanel(height = 2000,

                                              fluidPage(
                                                  fluidRow(
                                                      column(6,
                                                             shinydashboard::infoBoxOutput('differentiallyExpressedProteins',
                                                                                           width = 5)
                                                      ),
                                                      column(6,
                                                             shinydashboard::infoBoxOutput('failedToMapGenes',
                                                                                           width = 5),
                                                      )
                                                  ),
                                                  br(),
                                                  fluidRow(
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
                                                                               image = 'images/logoTransparentSmall.gif',
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
                                                                                       selected = 'all',
                                                                                       width = '300px'),
                                                                           br(),

                                                                           uiOutput('preRankedPlotUI')
                                                                  ),


                                                                  tabPanel('Network',

                                                                           hr(),

                                                                           uiOutput('networkEnrichmentUI')#,

                                                                           # downloadButton(outputId = 'download_network_table',
                                                                           #                label = 'Download table')

                                                                  ),


                                                                  tabPanel('Results Table',
                                                                           br(),
                                                                           shinycssloaders::withSpinner(
                                                                               DT::dataTableOutput('geneOntologyDataTable'),
                                                                               image = 'images/logoTransparentSmall.gif',
                                                                               image.width = '200px'
                                                                           ),
                                                                           downloadButton(
                                                                               outputId = 'download_enrichment_table',
                                                                               label = 'Download Table')


                                                                  )
                                                      )
                                                  )
                                              )
                                    )
                                )
)
