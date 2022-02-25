uiReport <- tabPanel(title = h4('Report'),
         value = 'report-tab',
         fluidPage(
             fluidRow(
                 h3('What would you like to report?'),

                 column(
                     width = 2,
                     value = "checkbox-report",
                     # Experiment Design

                     shinyWidgets::prettyCheckbox(
                         inputId = "experimentReport",
                         label = "Include Experiment Design",
                         value = TRUE,
                         shape = "round",
                         status = "info",
                         outline = TRUE,
                         bigger = TRUE,
                         plain = FALSE,
                         animation = "pulse"
                     )
                 ),

                 column(
                     width = 2,

                     # Experiment Design

                     shinyWidgets::prettyCheckboxGroup(
                         inputId = "preprocessingReport",
                         label = h4("Preprocessing Plots"),
                         choices = c("Contaminants",
                                     "Filtering out Missing Values" ="missing",
                                     "Normalization",
                                     "Imputation"),
                         shape = "round",
                         outline = TRUE,
                         animation = "pulse",
                         bigger = TRUE,
                         status = "info",
                         plain = FALSE
                     )
                 ),

                 column(
                     width = 2,
                     # Sample comparisons

                     shinyWidgets::prettyCheckboxGroup(
                         inputId = "sampleReport",
                         label = h4("Visualizations"),
                         choices = c("Scatter Plot" = "scatter",
                                     "Correlation",
                                     "PCA",
                                     "Heatmap Plot" = "heatmap",
                                     "Volcano Plot" = "volcano",
                                     "Profile Plot" = "profile"),
                         shape = "round",
                         outline = TRUE,
                         animation = "pulse",
                         bigger = TRUE,
                         status = "info",
                         plain = FALSE
                     )
                 )
             ),

             fluidRow(
                 column(
                     width = 2,
                     # Enrichemnt Plots

                     shinyWidgets::prettyCheckboxGroup(
                         inputId = "enrichmentReport",
                         label = h4("Enrichment Plots"),
                         choices = c("Gene Ontology",
                                     "GSEA Enrichment",
                                     "Network"),
                         shape = "round",
                         outline = TRUE,
                         animation = "pulse",
                         bigger = TRUE,
                         status = "info",
                         plain = FALSE )
                 ),

                 column(
                     width = 2,
                     # Disease Plots

                     shinyWidgets::prettyCheckboxGroup(
                         inputId = "diseaseReport",
                         label = h4("Disease Plots"),

                         choices = c("Enrichment",
                                     "GSEA",
                                     "Heatmap",
                                     "Density",
                                     "Association",
                                     "Circus Plot",
                                     "Network",
                                     "Map"
                         ),
                         shape = "round",
                         outline = TRUE,
                         animation = "pulse",
                         bigger = TRUE,
                         status = "info",
                         plain = FALSE)
                 ),

                 column(
                     width = 2,

                     # Pathway Plot

                     shinyWidgets::prettyCheckbox(
                         inputId = "pathwayReport",
                         label = "Pathway Plot",
                         value = FALSE,
                         shape = "round",
                         outline = TRUE,
                         animation = "pulse",
                         bigger = TRUE,
                         status = "info",
                         plain = FALSE
                     )
                 ),

                 column(
                     width = 2,
                     # Interactions Plot

                     shinyWidgets::prettyCheckbox(
                         inputId = "interactionReport",
                         label = "Interactions Network",
                         value = FALSE,
                         shape = "round",
                         outline = TRUE,
                         animation = "pulse",
                         bigger = TRUE,
                         status = "info",
                         plain = FALSE
                     )
                 )
             ),

             fluidRow(
                 column(
                     width = 12,
                     # Format Plot

                     shinyWidgets::awesomeRadio(
                         inputId = "formatReport",
                         label = h4("What format would you prefer"),
                         choices = c("PDF"='pdf',
                                     "HTML"='HTML'
                                     ),
                         inline = TRUE,
                         checkbox = TRUE,
                         status = "info",
                     )
                 )
             ),


             br(),
             br(),

             fluidRow(
                 downloadButton("generateReport", "Generate report")
             )
         )
)
