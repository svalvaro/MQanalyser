uiSampleComparisons <-     tabPanel(h4("Sample Comparisons"),
                                    value = 'comparisons-tab',

                                    tabsetPanel(
                                        type = 'tabs',
                                        # Scatter Plot -------------------------------
                                        tabPanel(
                                            title = h4('Scatter Plot'),
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
                                                                                           '#71a873',
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
                                                            image = 'images/logoTransparentSmall.gif',
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
                                                         width = 10,
                                                         align = 'center',
                                                         offset = 2,

                                                         br(),

                                                         shinycssloaders::withSpinner(
                                                             ui_element = plotlyOutput('plot_correlation'),
                                                             image = 'images/logoTransparentSmall.gif',
                                                             image.width = '200px'
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
                                                                 image = 'images/logoTransparentSmall.gif',
                                                                 image.width = '200px'
                                                             )
                                                         )
                                                     )
                                                 )
                                        )
                                    )
)
