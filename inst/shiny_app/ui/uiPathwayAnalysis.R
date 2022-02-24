uiPathwayAnalysis <-   tabPanel(title = h4('Pathway Analysis'),
                                value = 'pathway-tab',
                                sidebarLayout(
                                    sidebarPanel(id = 'sidebar',
                                                 width = 2,
                                                 uiOutput("pathway_selector"),

                                                 uiOutput('pathwayButton')
                                    ),

                                    mainPanel(

                                        tabsetPanel(type = 'tabs',

                                                    tabPanel('KEGG Pathway',
                                                             hr(),

                                                             shinycssloaders::withSpinner(
                                                                 plotOutput('enr_kegg1'),
                                                                 image = 'images/logoTransparentSmall.gif',
                                                                 image.width = '200px'
                                                             )
                                                    ),

                                                    tabPanel('Pathway Table',
                                                             hr(),

                                                             shinycssloaders::withSpinner(
                                                                 DT::dataTableOutput('pathwayTable')
                                                             ),
                                                             downloadButton(
                                                                 outputId = 'download_pathway_table',
                                                                 label = 'Download Table')
                                                    )
                                        )
                                    )
                                )
)
