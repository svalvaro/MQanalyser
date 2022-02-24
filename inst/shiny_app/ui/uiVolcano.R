uiVolcano <-  tabPanel(h4('Volcano Plot'),
             value = 'volcano-tab',
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
                                      value = TRUE,
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
                             image = 'images/logoTransparentSmall.gif',
                             image.width = '200px'
                         )
                     )
                 )
             )
    )
