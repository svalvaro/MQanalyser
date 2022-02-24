uiDisease  <- tabPanel(title = h4('Disease Analysis'),
                       value = 'disease-tab',

                       sidebarLayout(
                           sidebarPanel(id = 'sidebar',
                                        width = 2
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
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px')
                                               ),

                                               tabPanel('Disease GSEA',
                                                        hr(),
                                                        shinycssloaders::withSpinner(
                                                            plotOutput('enr_gseadotplot'),
                                                            image = 'images/logoTransparentSmall.gif',
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
                                                            image = 'images/logoTransparentSmall.gif',
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
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px')
                                               ),

                                               tabPanel('Disease association',
                                                        hr(),
                                                        print('visualizing the complex association between genes and gene sets.
                                                   It emphasizes the gene overlapping among different gene sets.'),
                                                        hr(),
                                                        shinycssloaders::withSpinner(
                                                            plotOutput('upset'),
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px'
                                                        )
                                               ),


                                               tabPanel('Circus Plot',
                                                        hr(),
                                                        shinycssloaders::withSpinner(
                                                            plotOutput('enr_circusplot'),
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px'
                                                        )
                                               ),


                                               tabPanel('Disease Network',
                                                        hr(),
                                                        shinycssloaders::withSpinner(
                                                            plotOutput('enr_networkplot'),
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px'
                                                        )
                                               ),

                                               tabPanel('Disease Map',
                                                        hr(),
                                                        print('Enrichment map organizes enriched terms into a network
                                                   with edges connecting overlapping gene sets. In this way,
                                                   mutually overlapping gene sets are tend to cluster together,
                                                   making it easy to identify functional module.'),
                                                        hr(),
                                                        shinycssloaders::withSpinner(
                                                            plotOutput('enr_mapplot'),
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px'
                                                        )
                                               ),

                                               tabPanel('Disease Results Table',
                                                        br(),

                                                        shinycssloaders::withSpinner(
                                                            DT::dataTableOutput('diseaseTable'),
                                                            image = 'images/logoTransparentSmall.gif',
                                                            image.width = '200px'
                                                        ),
                                                        downloadButton(
                                                            outputId = 'download_disease_table',
                                                            label = 'Download Table')

                                               )
                                   )
                               )
                           )
                       )
)
