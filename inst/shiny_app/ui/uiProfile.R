uiProfile <- tabPanel(h4('Profile Plot'),
                      value = 'profile-tab',
                      sidebarLayout(
                          sidebarPanel(id = 'sidebar',
                                       width = 2,
                                       h3("Select the adjustments"),
                                       br(),
                                       dropdown(
                                           tags$h3("Advanced Parameters"),
                                           colourpicker::colourInput(inputId = "input_col_prof",
                                                                     label = h4("Colour all proteins:"),
                                                                     value = "#71a873",
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
                                            image = 'images/logoTransparentSmall.gif',
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
)
