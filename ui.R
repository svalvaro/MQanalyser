shiny_busy <- function() {
    # use &nbsp; for some alignment, if needed
    HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
        '<span data-display-if="',
        '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
        '">',
        '<i class="fa fa-spinner fa-spin fa-fw" style="color:darkblue; font-size:70px"></i>',
        '</span>'
    ))
}


navbarPage(h3("Proteomics results"),
                 theme = shinytheme(theme ='flatly'),


                 tabPanel(h4('Start'),
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput(inputId = 'proteinGroups',
                                            label = h4('Upload the proteinGroups.txt file'),
                                            multiple = FALSE,
                                            accept = 'text'),

                                  radioButtons(inputId = "IntensityType",
                                                h4("Intensity type to analyze:"),
                                                choices = c("Intensity" = 'Intensity',
                                                            "LFQ" = 'LFQ',
                                                            "iBAQ" = 'iBAQ'),
                                                selected = 'Intensity'),

                                  verbatimTextOutput('IntensityFound'),
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

                                      numericInput(inputId = 'input_log2',
                                                  label = 'Log2 fold change cut off',
                                                  value = 1.5,
                                                  min = 0, max = 100),

                                      selectInput(inputId = 'input_imputation',
                                                  label = 'Imputation',
                                                  choices = c('Bayesian' = 'bpca',
                                                              'Quantile Regression'= 'QRILC',
                                                              'MinProb'= 'MinProb',
                                                              'Nearest neighbour averaging' = 'knn',
                                                              'MinProb' = 'MinProb',
                                                              'Maximum likelihood-based ' = 'MLE',
                                                              'Minimum Value' = 'min',
                                                              'Replace by 0' = 'zero',
                                                              'No imputation' = 'none'),
                                                 selected = 'QRILC'),

                                      options = list(`style` = "btn-info"),
                                      style = "unite", icon = icon("gear"),
                                      status = "default", width = "300px",
                                      animate = animateOptions(
                                          enter = animations$fading_entrances$fadeInLeftBig,
                                          exit = animations$fading_exits$fadeOutRightBig
                                      )
                                  ),

                                  fileInput(inputId = 'user_genes',
                                            label='Provide a list of desired genes to check in the analysis: (Optional)',
                                            multiple= FALSE,
                                            accept= 'text/csv'),
                                  hr(),
                                  br(),

                                  #Download a file example
                                  h4('Exapmles of the file(s) to be uploaded:'),
                                  fluidRow(

                                      column(5,a(href='proteinGroup_example.txt','proteinGroups.txt', download=NA, target='_blank') ),

                                      column(5,a(href='experiment_design_example.txt','experiment_design.txt', download=NA, target='_blank') ),


                                      column(5,a(href='user_genes_examples.txt','Genes example', download=NA, target='_blank') ),
                                  )
                              ),

                              mainPanel(
                                  box(title = h3('Experiment Design'),
                                      width = 10,
                                      h4('Welcome to analysis of the results of LC-MS/MS'),
                                      h5('Start by uploading the proteinGroups.txt table provided.'),
                                      h5('Please Edit the next table and add the Group and Replicate.'),
                                      h5('Alternatively an experiment design can be uploaded. Be sure that the Sample names are correct.'),
                                      hr(),
                                      #DTOutput('experiment_design')
                                      #DT::dataTableOutput('ed_out'),
                                      rHandsontableOutput('ed_out'),
                                      br(),
                                      br(),
                                      actionButton("runButton","Start Analysis"),

                                  ),
                                  box(img(src='Proteomika_logo_hires.png', height = '60%', width = '60%', align = 'right'))
                              )
                          )
                 ),
                 #SHiny busy

           #First Panel is the heatmap
           tabPanel(h4("Results"),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Select the adjustments")

                        ),
                        mainPanel(uiOutput('significant_proteins'),
                                  br(),
                                  br(),
                                  br(),
                                 DT::dataTableOutput('proteomics_results'))

                        )
                    ),




                 #First Panel is the heatmap
                 tabPanel(h4("Heatmap"),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Select the adjustments"),

                                  # #Select whether the user wants the dendogram or not
                                  # selectInput('dendogram',h4('Select an option for the dendogram'), choices = c('none','row','column','both'),selected = 'both'),
                                  #
                                  uiOutput('k_row'),

                                  uiOutput('k_col'),
                                  #horizontal bar to separate the heatmap parameters with the key parameters
                                  hr(),
                                  #
                                  # #Size of the key
                                  # sliderInput('keysize',h4('Introduce the size of the key'),min = 0.1,max = 2,value = 1),
                                  #
                                  # #Density info
                                  # selectInput('density',h4('Select the density information'), choices= c('histogram','density','none'),selected = 'none'),
                                  # #Select the color for Down regulated
                                  colourpicker::colourInput("col_heatdown", h4("Select colour for downregulated:"), "dodgerblue2",palette = "square", returnName = TRUE,showColour = c("background")),
                                  #Select the Colour for UPregulated
                                  colourpicker::colourInput("col_heatup", h4("Select colour upregulated:"), "red2",palette = "square", returnName = TRUE,showColour = c("background")),
                                  #Select the Colour for non significant
                                  colourpicker::colourInput("col_heatmedium", h4("Select colour non significant:"), "white",palette = "square", returnName = TRUE,showColour = c("background")),

                                  #Download button for the plot
                                  downloadButton(outputId = 'downloadheatmap',
                                                 label = 'Download the heatmap')

                              ),
                              mainPanel(shiny_busy(),
                                        #Plot the heatmap in the first tab
                                        box(height = 2500,width = 2000, plotlyOutput('heatmap') )

                              )
                          )
                 ),


                 #Multi Scatter PLot
                 tabPanel(h4('Multi Scatter Plot'),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Select the adjustments"),

                                  #Input for selection of multi plot or correlations
                                  selectInput(inputId='multiselection',
                                              label=h4('Select what do you want to visualize:'),
                                              choices=c('Multi Scatter Plot'='multiplot','Correlation Matrix'='cor_matrix'),
                                              selected = 'multiplot')



                              ),


                              mainPanel(
                                  print('If you would like to analyse each plot individually, go to the next tab
                                    "Scatter Plot"'),
                                  hr(),
                                  shiny_busy(),
                                  plotOutput('multiscatter')

                              )
                          )
                 ),


                 #Second Panel is the ScatterPLot
                 tabPanel(h4("Scatter Plot"),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Select the adjustments"),

                                  #Sample for X axis
                                  uiOutput("names_x_selector"),
                                  #Sample for y axis

                                  uiOutput("names_y_selector"),


                                  #add or not a regression line
                                  checkboxInput(inputId = 'checkbox',
                                                label = h4('Add regression line'),
                                                value = FALSE),


                                  #check if they want to see their genes

                                  checkboxInput(inputId = 'showgenes',
                                                label = h4('Show the genes that you have uploaded:'),
                                                value=FALSE),

                                  # Download button for the plot
                                  downloadButton(outputId = 'downloadscatter',
                                                 label = 'Download the Scatter Plot')




                              ),
                              mainPanel(

                                  #Plot the scatter plot  in the second tab
                                  box(height = 900,width = 900,

                                      plotOutput('scatter')),
                                  box(height = 900, width =900,
                                      DT::dataTableOutput('table_plot'))



                              )
                          )
                 ),
                 #Panel for the Volcano Plot
                 tabPanel(h4('Volcano Plot'),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Select the adjustments"),


                                  # #Sample for y axis
                                  #
                                  uiOutput("volcano_selector"),


                                  hr(),

                                  sliderInput(inputId = 'log_input',
                                              label = h4('Log2(Fold-Change) Range'),
                                              min = -10,
                                              max = 10,
                                              step = 0.1,
                                              value = c(-6,6) ),

                                  sliderInput(inputId = 'log_p',
                                              label = h4('-Log10(P-Value): Range'),
                                              min = 0,
                                              max = 100,
                                              value = 5 ),
                                  #Select the color for Down regulated
                                  colourpicker::colourInput("col1", h4("Select colour downregulated:"), "dodgerblue3",palette = "square", returnName = TRUE,showColour = c("background")),
                                  #Select the Colour for UPregulated
                                  colourpicker::colourInput("col2", h4("Select colour upregulated:"), "red2",palette = "square", returnName = TRUE,showColour = c("background")),


                                  hr(),
                                  sliderInput(inputId = 'pvalue_thresh',
                                              label = h4('P-value Threshhold:'),
                                              min = 1,
                                              max = 15,
                                              value = 1.3,
                                              step = 0.1),

                                  sliderInput(inputId = 'fc_thresh',
                                              label = h4('log2(Fold Change) Threshold:'),
                                              min = 0,
                                              max = 10,
                                              value = 1.5
                                              ,
                                              step = 0.1),

                                  #Checkbox for Gene names
                                  checkboxInput(inputId = 'check_names',
                                                label =h4('Show gene  names:'),
                                                value = FALSE),
                                  #check box for protein IDs
                                  checkboxInput(inputId = 'protein_id',
                                                label =h4('Show protein IDs:'),
                                                value = FALSE),

                                  #Check box for fav genes
                                  checkboxInput(inputId = 'showgenes_volcano',
                                                label=h4('Show your selected genes'),
                                                value=FALSE),

                                  #Download button for the volcano plot
                                  downloadButton(outputId = 'downloadvolcano',
                                                 label = 'Download the Volcano Plot')

                              ),
                              mainPanel(
                                  #Plot the volcano plot in the third tab
                                  box(height = 2500,width = 2000,plotOutput('volcano'))

                              )
                          )
                 ),

                 #Profile
                 #Tab for Profile Plot

                 tabPanel(h4('Profile Plot'),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Select the adjustments"),

                                  checkboxInput(inputId = 'check_profiles',
                                                label =h4('Show gene  names of table selection:'),
                                                value = FALSE),

                                  checkboxInput(inputId = 'profile_favs',
                                                label = h4('Show the genes that you have uploaded:',
                                                           value=FALSE))

                              ),
                              mainPanel(shiny_busy(),
                                        box(height = 800, width = 900,

                                            plotOutput('profile')),

                                        box(height = 800, width =900,
                                            DT::dataTableOutput('profiletable'))




                              )
                          )
                 ),

                 #Tab for Enrichment analysis

                 tabPanel(h4('Enrichment Analysis'),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Select the adjustments"),

                                  uiOutput('enrichment_selector'),

                                  hr(),

                                  # #Input for choices gene network
                                  # selectInput(inputId = 'gene_choices',
                                  #             label = h4('Select choices for gene network:'),
                                  #             choices = c('category','gene','all','none'),
                                  #             selected = 'all'),

                                  #INput for enrichment map
                                  # sliderInput(inputId = 'enrich_nodes',
                                  #             h4('Select the scale of the nodes for Enrichment Map'),
                                  #             min = 0,
                                  #             max = 4,
                                  #             value = 1.5,
                                  #             step = 0.5),
                                  #Input for type of preranked score

                                  selectInput(inputId = 'runscore',
                                              label=h4('For the Running Score, select the choice:'),
                                              choices = c('runningScore','preranked'),
                                              selected = 'runningScore'),

                                  #Input for pathway to check


                                  uiOutput("pathway_selector"),

                              ),
                              mainPanel(
                                  box(height = 2500, width = 2000,
                                      tabsetPanel(type = 'tabs',

                                                  tabPanel('Bar Plot',
                                                           hr(),

                                                           print('Bar plot is the most widely used method to visualize
                                                   enriched terms. It depicts the enrichment scores
                                                  (e.g. p values) and gene count or ratio as bar height and color.'),
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_barplot')),

                                                  tabPanel('Dot plot',
                                                           hr(),
                                                           print('Dot plot is similar to bar
                                                         plot with the capability to encode another score as dot size.'),
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_dotplot')),

                                                  tabPanel('GSEA',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_gseadotplot')),

                                                  tabPanel('Circus',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_circusplot')),

                                                  tabPanel('Gene Network',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_networkplot')),

                                                  tabPanel('Heatmap',
                                                           hr(),
                                                           print('The gene-concept network may become too complicated
                                                         if user want to show a large number significant terms.
                                                         The heatmap can simplify the results making it easier to
                                                         identify expression patterns.'),
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('heatmapnrich')),

                                                  tabPanel('Enrichment Map',
                                                           hr(),
                                                           print('Enrichment map organizes enriched terms into a network
                                                         with edges connecting overlapping gene sets. In this way,
                                                         mutually overlapping gene sets are tend to cluster together,
                                                         making it easy to identify functional module.'),
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_mapplot')),



                                                  tabPanel('Biological Function',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('bio_comparison')),

                                                  tabPanel('Overlapping densities',
                                                           hr(),
                                                           print('The ridgeplot will visualize expression distributions
                                                         of core enriched genes for GSEA enriched categories.
                                                         It helps users to interpret up/down-regulated pathways.'),
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_ridgeplot')),

                                                  tabPanel('GSEA 1',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_gseaplot')),
                                                  tabPanel('GSEA 2',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_gsea2')),

                                                  #KEGG
                                                  tabPanel('KEGG Pathway 1',
                                                           hr(),
                                                           shiny_busy(),
                                                           plotOutput('enr_kegg1')),
                                                  tabPanel('Pathway WebBrowser',
                                                           hr(),
                                                           print('Select in the adjustments the pathway that you would
                                                         like to visualize and it will automatically open in a web
                                                         browser'),
                                                           shiny_busy(),
                                                           plotOutput('enr_kegg2'))
                                      )


                                  )
                              )
                          )
                 ),

                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }")
)
