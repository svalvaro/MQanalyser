uiInteractions <- tabPanel(title = h4('Interactions'),
                           value = 'interactions-tab',
                           sidebarLayout(
                               sidebarPanel(
                                   id = 'sidebar',
                                   width = 2,

                                   uiOutput('comparisons_interaction'),

                                   sliderInput(
                                       inputId = 'numberofInteractions',
                                       label = h4('Maximum proteins'),min = 2,max = 1999, value = 50
                                   ),

                                   uiOutput('interactionsButton')
                               ),

                               mainPanel(
                                   fluidRow(
                                       h4('Select the proteins on the table by holding ctrl/shift'),

                                       column(
                                           width = 8,
                                           shinycssloaders::withSpinner(
                                               plotOutput('stringPlot'),
                                               image = 'images/logoTransparentSmall.gif',
                                               image.width = '200px'
                                           )
                                       ),

                                       column(
                                           width = 4,

                                           actionBttn(
                                               inputId = 'refreshInteractionTable',
                                               label = h4('Update'),
                                               icon = NULL,
                                               style = "unite",
                                               color = "default",
                                               size = "md",
                                               block = FALSE,
                                               no_outline = TRUE
                                           ),

                                           br(),
                                           br(),
                                           br(),

                                           DT::dataTableOutput('interactionResults')
                                       )
                                   )
                               )
                           )
)
