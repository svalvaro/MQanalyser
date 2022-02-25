uiResults <- tabPanel(h4("Results"),
                      value = "results-tab",

                      includeCSS("www/css/styles.css"),

                      sidebarLayout(
                          sidebarPanel(

                              id = 'sidebar',
                              width = 2,

                              sliderInput(inputId = 'input_pvalue',
                                          label = 'Adjusted p-value cut off',
                                          value = 0.05,
                                          min = 0, max = 1),

                              numericInput(inputId = 'input_fc',
                                           label = 'Log2 fold change cut off',
                                           value = 1.5,
                                           min = 0, max = 100)
                          ),
                          mainPanel(
                              fluidRow(
                                  column(
                                      width = 4,
                                      shinydashboard::infoBoxOutput('significant_proteins',
                                                                    width = NULL)
                                  ),
                                  column(
                                      width = 4,
                                      shinydashboard::infoBoxOutput('significant_user_genes',
                                                                    width = NULL)
                                  ),
                                  column(
                                      width = 4,
                                      uiOutput('table_user_genes')
                                  )
                              ),
                              hr(),
                              fluidRow(
                                  column(align = 'center',
                                         width = 10,
                                         shinycssloaders::withSpinner(
                                             DT::dataTableOutput('proteomics_results'),
                                             image = 'images/logoTransparentSmall.gif',
                                             image.width = '200px'
                                         ),
                                         br(),
                                         downloadButton(outputId = 'download_proteomics',
                                                        label = 'Download')
                                  )
                              )
                          )
                      )
)
