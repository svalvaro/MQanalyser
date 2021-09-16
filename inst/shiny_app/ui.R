navbarPage(#title = "Proteomics Analyser",

          titlePanel('',
                     tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"),
                               tags$title("Proteomics Analyser")) ),

    theme = shinytheme(theme ='flatly'),

#### DATA INPUT ####

    tabPanel(h4('Data \nInput'),
            sidebarLayout(
                sidebarPanel(width = 3,
                  shinyalert::useShinyalert(),

                    fileInput(inputId = 'proteinInput',
                              label = h4('Upload the proteinGroups.txt or proteoQuant.csv'),
                              multiple = FALSE,
                              accept = 'text'),

                    verbatimTextOutput('sw_used'),

                    uiOutput('intensity_selector'),


                    verbatimTextOutput('IntensityFound_message'),
                    hr(),
                    br(),

                    fileInput(inputId = 'optional_exp_design',
                              label='Provide the experiment design (Optional)',
                              multiple= FALSE,
                              accept= 'text'),

                    br(),

                    dropdown(
                        tags$h3("Advanced Parameters"),

                        sliderInput(inputId = 'input_pvalue',
                                    label = 'Adjusted p-value cut off',
                                    value = 0.05,
                                    min = 0, max = 1),

                        numericInput(inputId = 'input_fc',
                                    label = 'Log2 fold change cut off',
                                    value = 1.5,
                                    min = 0, max = 100),

                        checkboxInput(inputId = 'contaminantsInput',
                                      label = 'Filter out the contaminants',
                                      value = TRUE),

                        options = list(`style` = "btn-info"),
                        style = "unite", icon = icon("gear"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                            )
                        ),

                    br(),

                    fileInput(inputId = 'user_genes',
                              label='Provide a list of desired genes to check in the analysis: (Optional)',
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
                    fluidRow(

                        column(5,a(href='proteinGroup_example.txt','proteinGroups.txt', download=NA, target='_blank') ),

                        column(5,a(href='experiment_design_example.txt','experiment_design.txt', download=NA, target='_blank') ),


                        column(5,a(href='user_genes_examples.txt','Genes example', download=NA, target='_blank')
                               ),
                        )
                  ),

                mainPanel(
                    fluidRow(
                        column(8,
                               box(title = h3('Experiment Design'),
                                   width = 10,
                                   h4('Welcome to analysis of the results of LC-MS/MS'),
                                   h5('Start by uploading the proteinGroups.txt table provided.'),
                                   h5('Please Edit the next table by adding the Condition and Replicate.'),
                                   h5('Alternatively an experiment design can be uploaded. Be sure that the Sample names are correct.'),
                                   hr(),

                                   #DTOutput('experiment_design')
                                   #DT::dataTableOutput('ed_out'),
                                   rHandsontableOutput('ed_out'),
                                   br(),
                                   downloadButton(outputId = 'download_experiment_design',
                                                  label = 'Download (Press Start Analysis First)'),
                                   )
                               ),


                        column(3,box(img(src="Proteomika_logo_hires.png",
                                         height = '100%',
                                         width = '100%',
                                         align = 'right')
                                     ),

                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               box(actionButton("start_input","Start Analysis")
                                   )
                               )
                        )
                    )
                )
            ),


#### Imputation Distribution ####
tabPanel(h4("Imputation"),
         sidebarLayout(
           sidebarPanel(width = 2,
             #Drop down with Parameters for heatmap



             selectInput(inputId = 'input_imputation',
                         label = 'Imputation type',
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
                         selected = 'MLE'),

              uiOutput('manual_imputation_scale'),

             uiOutput('manual_imputation_shift'),

             uiOutput('na_threshold'),

             checkboxInput(inputId = 'combined_imputation',
                           label = 'Combine the samples into one plot',
                           value = FALSE)
           ),
           mainPanel(
             box(
               shinycssloaders::withSpinner(
                 plotlyOutput('imputation')
               )
             )
           )
           )
         ),


#### Results Panel ####
    tabPanel(h4("Results"),
      includeCSS("www/info_box.css"),


      box(width = 4,
          shinydashboard::infoBoxOutput('significant_proteins',
                                        width = NULL)
          ),

      box(width = 4,
          shinydashboard::infoBoxOutput('significant_user_genes',
                                        width = NULL)
          ),

      box(width = 4,
          uiOutput('table_user_genes')
          ),

      br(),
      br(),

      hr(),

      shinycssloaders::withSpinner(
        DT::dataTableOutput('proteomics_results')
      ),

      br(),
      downloadButton(outputId = 'download_proteomics',
                      label = 'Download'),
      ),



#### HeatMap ####
    tabPanel(h4("Heatmap"),
             sidebarLayout(
               sidebarPanel(width = 2,
                    #Drop down with Parameters for heatmap
                    dropdown(
                        tags$h3("Advanced Parameters"),


                        selectInput(inputId = 'dendogram_input',
                                    label = 'Type of Clustering',
                                    choices = c('Proteins and the samples' = 'both',
                                                'Only for the genes.'= 'row',
                                                'Only for the samples.'= 'column',
                                                'No Clustering' = 'none'),
                                    selected = 'both'),

                        sliderInput(inputId = 'k_row_input',
                                    label = 'Colour clusters, genes:',
                                    value = 0,
                                    min = 0, max = 50),

                        sliderInput(inputId = 'k_col_input',
                                     label = 'Colour clusters, samples',
                                     value = 0,
                                     min = 0, max = 50),
#
#                         #Select the Colour for UPregulated
#                         colourpicker::colourInput("heat_upregulated",
#                                                   h4("Select colour upregulated:"),
#                                                   'brown2',
#                                                   palette = "square",
#                                                   returnName = TRUE,
#                                                   showColour = c("background")),
#
#                         #Select the color for Down regulated
#                         colourpicker::colourInput("heat_downregulated",
#                                                   h4("Select colour downregulated:"),
#                                                   'cyan3',
#                                                   palette = "square",
#                                                   returnName = TRUE,
#                                                   showColour = c("background")),


                        options = list(`style` = "btn-info"),
                        style = "unite", icon = icon("gear"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig)
                        )
                    ),
                mainPanel(
                    box(
                        shinycssloaders::withSpinner(
                            plotlyOutput('heatmaply')
                            )
                        )
                    )
                )
            ),


#### Correlation PLot ####
    tabPanel(h4('Correlation'),
            fluidRow(column(width = 12,
                            align = 'center',
                            print('If you would like to analyse each plot individually,
                            go to the next tab "Scatter Plot"'),
                            hr(),
                            shinycssloaders::withSpinner(
                              plotlyOutput('plot_correlation')
                              )
                            )
                     )
            ),


####  Scatter Plot ####
    tabPanel(h4("Scatter Plot"),
            sidebarLayout(
                sidebarPanel( width = 2,
                    h3("Select the adjustments"),

                    #Sample for X axis
                    uiOutput("x_sample_selector"),
                    #Sample for y axis

                    uiOutput("y_sample_selector"),

                    #check if they want to see their genes

                    checkboxInput(inputId = 'showgenes',
                                  label = h4('Show the differentially expressed proteins of the list that you have uploaded'),
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
                                                  '#56B4E9',
                                                  palette = "square",
                                                  returnName = TRUE,
                                                  showColour = c("background")),


                        colourpicker::colourInput(inputId = "color_de_scatter",
                                                  h4("Select colour for your proteins of interest:"),
                                                  '#dc143c',
                                                  palette = "square",
                                                  returnName = TRUE,
                                                  showColour = c("background")),

                        options = list(`style` = "btn-info"),
                        style = "unite", icon = icon("gear"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig)
                        ),

                    br(),

                    # Download button for the plot
                    downloadButton(outputId = 'downloadscatter',
                                   label = 'Download the Scatter Plot')

                ),

                mainPanel(
                    #Plot the scatter plot  in the second tab
                    box(height = 1200,width = 1200,
                        shinycssloaders::withSpinner(
                          plotlyOutput('scatterplot')
                          )
                        )
                    )
                )
            ),


#### Volcano Plot ####
    tabPanel(h4('Volcano Plot'),
            sidebarLayout(
                sidebarPanel(width = 2,
                    h3("Select the adjustments"),

                    uiOutput("comparisons_out"),

                    dropdown(
                        tags$h3("Advanced Parameters"),


                        #check box for protein IDs
                        checkboxInput(inputId = 'modify_axis',
                                      label =h4('Would you prefer to modify the axis values: \n(Uncheck to restore values)'),
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
                                    label = 'Adjust the transparency of the poings
                                    parameter:',
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
                        style = "unite", icon = icon("gear"),
                        status = "success", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                        )
                    ),

                    hr(),

                    # #Checkbox for Gene names
                    # checkboxInput(inputId = 'check_names',
                    #               label =h4('Show gene  names:'),
                    #               value = FALSE),
                    #
                    # #check box for protein IDs
                    # checkboxInput(inputId = 'protein_id',
                    #               label =h4('Show protein IDs:'),
                    #               value = FALSE),

                    #Check box for fav genes
                    checkboxInput(inputId = 'showgenes_volcano',
                                  label=h4('Show your selected genes'),
                                  value=FALSE),

                    #Download button for the volcano plot
                    downloadButton(outputId = 'downloadvolcano',
                                   label = 'Download the Volcano Plot')
                ),

                mainPanel(
                  box(shinycssloaders::withSpinner(
                    plotlyOutput('volcano_plot')
                    )
                    )
                  )
                )
            ),


#### Profile Plot ####
    tabPanel(h4('Profile Plot'),
            sidebarLayout(
                sidebarPanel(width = 2,
                    h3("Select the adjustments"),
                    br(),
                    dropdown(
                        tags$h3("Advanced Parameters"),
                        colourpicker::colourInput(inputId = "input_col_prof",
                                                  label = h4("Colour all proteins:"),
                                                  value = "#56B4E9",
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
                        style = "unite", icon = icon("gear"),
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
                                plotlyOutput('plot_profile')
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
            ),


#### PCA Plot ####

    tabPanel(h4('PCA'),
             sidebarLayout(
               sidebarPanel(width = 2,
                 h3("Select the adjustments"),

                 selectInput(inputId = 'pca_label',
                             label = 'Select the labels:',
                             choices =  c('label' = 'rowname',
                                          'experiment' = 'label',
                                          'replicate' = 'replicate'),
                             selected = 'rowname'
                            ),
                 uiOutput('pca_number_proteins')

                 ),

               mainPanel(box(height = 900, width = 1300,

                             shinycssloaders::withSpinner(
                               plotOutput('pca_plot'))
                             )
                         )
               )
             ),

#### Enrichment analysis ####

    tabPanel(h4('Enrichment Analysis'),
            sidebarLayout(
                sidebarPanel(width = 2,
                    h3("Select the adjustments"),

                    uiOutput('comparisons_enrichment'),

                    selectInput(inputId = 'enrich_organism',
                                label = 'Select the species:',
                                choices = c('Human' = 'org.Hs.eg.db',
                                            'Mouse'= 'org.Mm.eg.db',
                                            'Rat'= 'org.Rn.eg.db'),
                                            #'Yeast' = 'org.Sc.sgd.db'),
                                selected = 'org.Hs.eg.db'),

                    sliderInput(inputId = 'go_level',
                                h4('Degree of specificity, or GO level. The higher the more specific:'),
                                min = 1,
                                max = 20,
                                value = 3,
                                step = 1),

                    # Number of proteins selected:

                    sliderInput(inputId = 'fc_enrichment',
                                h4('The Log2 Fold Change can be modified to run
                                   the enrichment analysis with proteins more significant.'),
                                min = 1,
                                max = 20,
                                value = 1.5,
                                step = 0.5),

                    verbatimTextOutput('diffExpress_number'),
                    hr(),
                    ),

                mainPanel(
                    box(height = 2500, width = 2000,
                        tabsetPanel(type = 'tabs',

                                    tabPanel('Gene Ontology',
                                             br(),
                                             selectInput(inputId = 'go_ontology',
                                                label = 'Select the GO term',
                                                choices = c('Cellular Component' = 'CC',
                                                            'Biological Function'= 'BP',
                                                            'Molecular Function'= 'MF'),
                                                selected = 'CC'),
                                             hr(),
                                             shinycssloaders::withSpinner(plotlyOutput('go_classification_plot'))
                                    ),

                                    # tabPanel('GSEA Enrichment',
                                    #          hr(),
                                    #          shinycssloaders::withSpinner(plotOutput('enr_gsea2'))
                                    # ),


                                    tabPanel('GSEA Enrichment',
                                             hr(),
                                             #Input for type of preranked score

                                             selectInput(inputId = 'runscore',
                                                         label=h4('For the Running Score, select the choice:'),
                                                         choices = c('Running Score + Ranked List' = 'all',
                                                                     'Only Running Enrhichment Score' ='runningScore',
                                                                     'Only Ranked List' = 'preranked'),
                                                         selected = 'all'),
                                             br(),

                                             shinycssloaders::withSpinner(plotOutput('enr_gseaplot'))
                                             )
                                    )
                        )
                    )
                )
            ),


#### Disease Analysis Tab ####
  tabPanel(title = h4('Disease Analysis'),
           sidebarLayout(
             sidebarPanel(width = 2,
               uiOutput('comparisons_diseases'),
               selectInput(inputId = 'disease_organism',
                           label = 'Select the species:',
                           choices = c('Human' = 'org.Hs.eg.db',
                                       'Mouse'= 'org.Mm.eg.db',
                                       'Rat'= 'org.Rn.eg.db'),
                                       #'Yeast' = 'org.Sc.sgd.db'),
                           selected = 'org.Hs.eg.db'),

               hr(),

               sliderInput(inputId = 'fc_disease',
                           h4('The Log2 Fold Change can be modified to run
                                   the enrichment analysis with proteins more significant.'),
                           min = 1,
                           max = 20,
                           value = 1.5,
                           step = 0.5),
               verbatimTextOutput('diff_disease_number'),
             ),

             mainPanel(
               box(height = 2500, width = 2000,
                   tabsetPanel(type = 'tabs',

                               tabPanel('Disease Enrichment',
                                        hr(),
                                        # print('Dot plot is similar to bar
                                        #        plot with the capability to encode another score as dot size.'),
                                        shinycssloaders::withSpinner(plotOutput('enr_dotplot'))
                               ),

                               tabPanel('Disease GSEA',
                                        hr(),
                                        shinycssloaders::withSpinner(plotOutput('enr_gseadotplot'))
                               ),

                               tabPanel('Disease Heatmap',
                                        hr(),
                                        print('The heatmap can simplify the results making it easier to
                                                   identify expression patterns.'),
                                        hr(),
                                        shinycssloaders::withSpinner(plotlyOutput('heatmapnrich'))
                               ),

                               tabPanel('Disease density',
                                        hr(),
                                        print('The ridgeplot will visualize expression distributions
                                                    of core enriched genes for GSEA enriched categories.
                                                    It helps users to interpret up/down-regulated pathways.'),
                                        hr(),
                                        shinycssloaders::withSpinner(plotOutput('enr_ridgeplot'))
                               ),

                               tabPanel('Disease association',
                                        hr(),
                                        print('visualizing the complex association between genes and gene sets.
                                                   It emphasizes the gene overlapping among different gene sets.'),
                                        hr(),
                                        shinycssloaders::withSpinner(plotOutput('upset')
                                                                     )
                                        )
                               )
                   )
               )
             )
           ),


#### Gene Network ####
  tabPanel(title = h4('Gene Network'),
           sidebarLayout(
               sidebarPanel(width = 2,

                 uiOutput('comparisons_network'),

                 selectInput(inputId = 'network_organism',
                             label = 'Select the species:',
                             choices = c('Human' = 'org.Hs.eg.db',
                                         'Mouse'= 'org.Mm.eg.db',
                                         'Rat'= 'org.Rn.eg.db'),
                             #'Yeast' = 'org.Sc.sgd.db'),
                             selected = 'org.Hs.eg.db'),

                 hr(),

                 sliderInput(inputId = 'fc_network',
                             h4('The Log2 Fold Change can be modified to run
                                   the enrichment analysis with proteins more significant.'),
                             min = 1,
                             max = 20,
                             value = 1.5,
                             step = 0.5),

                 verbatimTextOutput('diff_network_number')
                 ),

               mainPanel(
                   box(height = 2500, width = 2000,
                       tabsetPanel(type = 'tabs',

                                   tabPanel('Biological Function',
                                            hr(),
                                            shinycssloaders::withSpinner(plotOutput('bio_comparison'))
                                   ),

                                   tabPanel('Circus',
                                            hr(),
                                            shinycssloaders::withSpinner(plotOutput('enr_circusplot'))
                                            ),

                                   tabPanel('Gene Network',
                                            hr(),
                                            shinycssloaders::withSpinner(plotOutput('enr_networkplot'))
                                            ),

                                   tabPanel('Enrichment Map',
                                            hr(),
                                            print('Enrichment map organizes enriched terms into a network
                                                   with edges connecting overlapping gene sets. In this way,
                                                   mutually overlapping gene sets are tend to cluster together,
                                                   making it easy to identify functional module.'),
                                            hr(),
                                            shinycssloaders::withSpinner(plotOutput('enr_mapplot')
                                                                         )
                                            )
                                   )
                       )
                   )
               )
           ),


#### Pathway Analysis ####
  tabPanel(title = h4('Pathway Analysis'),
           sidebarLayout(
               sidebarPanel(width = 2,
                  uiOutput("pathway_selector"),

                 actionBttn(inputId = 'GoToPathway',
                            label = 'Go to KEGG website',
                            icon = NULL,
                            style = "unite",
                            color = "default",
                            size = "md",
                            block = FALSE,
                            no_outline = TRUE
                            ),



                 plotOutput('enr_kegg2')
                 ),

               mainPanel(
                   box(height = 2500, width = 2000,
                        h4('KEGG Pathway'),
                        hr(),
                        shinycssloaders::withSpinner(plotOutput('enr_kegg1'))
                       )
                   )
           )
  ),


#### style css

    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }")
####
)
