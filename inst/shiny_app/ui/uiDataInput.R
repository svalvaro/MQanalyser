uiDataInput <-     tabPanel(h4('Data \nInput'),

                            sidebarLayout(
                                sidebarPanel(id = 'sidebar',
                                             width = 2,

                                             shinyjs::useShinyjs(),

                                             fileInput(inputId = 'proteinInput',
                                                       label = h4('Upload the proteinGroups.txt or
                                         proteoQuant.csv'),
                                                       multiple = FALSE,
                                                       accept = 'text'),

                                             uiOutput('intensity_selector'),

                                             hr(),

                                             fileInput(inputId = 'optional_exp_design',
                                                       label= h4('Provide the experiment design (Optional)'),
                                                       multiple= FALSE,
                                                       accept= 'text'),
                                             br(),

                                             fileInput(inputId = 'user_genes',
                                                       label= h4('Provide a list of desired genes to check
                              in the analysis: (Optional)'),
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


                                             tags$a(href='data/proteinGroups_example.txt',
                                                    h5('proteinGroups'),
                                                    download=NA,
                                                    target='_blank'),

                                             tags$a(href='data/user_genes_examples.txt',
                                                    h5('Genes example'),
                                                    download=NA,
                                                    target='_blank'),
                                             tags$a(href='data/experiment_design_example.txt',
                                                    h5('experiment Design'),
                                                    download=NA,
                                                    target='_blank')

                                ),

                                mainPanel(

                                    fluidRow(
                                        column(width = 4,
                                               shinydashboard::infoBoxOutput('softwareUsedBox', width = 12)
                                        ),

                                        column(width = 4,
                                               shinydashboard::infoBoxOutput('intensityBox',
                                                                             width = 12)
                                        ),
                                        column(4,
                                               shinydashboard::infoBoxOutput('matrixDimensions',
                                                                             width = 12),
                                        )
                                    ),

                                    fluidRow(
                                        column(width = 3),
                                        column(width = 7,
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

                                                   rHandsontableOutput('ed_out'),
                                                   br(),

                                                   uiOutput('downloaderExperimentUI')
                                               )
                                        ),
                                        column(width = 2,
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),

                                               uiOutput('start_analysis')
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
                                       p(tags$em("Last updated: January 2022"),
                                         style = 'font-size:75%'))
                            )
)
