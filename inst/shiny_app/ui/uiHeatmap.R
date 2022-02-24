uiHeatmap <-     tabPanel(h4("Heatmap"),
                          value = 'heatmap-tab',
                          sidebarLayout(
                              sidebarPanel(id = 'sidebar',
                                           width = 2,

                                           tags$div(
                                               title = paste0("If off, all the differentiallly expressed will be shown.
                                             If ON, only the N top contributors will."),

                                               shinyWidgets::switchInput(
                                                   inputId = "topContInput",
                                                   label = "Top Contributors",
                                                   labelWidth = "80px",
                                                   onStatus = 'success',
                                                   value = TRUE,
                                                   offStatus = 'danger')

                                           ),

                                           uiOutput('comparisonsHeatmap_out'),

                                           uiOutput('heatmapContributors')

                                           #     br(),
                                           #
                                           # selectInput(inputId = 'dendogram_input',
                                           #             label = 'Type of Clustering',
                                           #             choices = c('Proteins and the samples' = 'both',
                                           #                         'Only for the genes.'= 'row',
                                           #                         'Only for the samples.'= 'column',
                                           #                         'No Clustering' = 'none'),
                                           #             selected = 'both'),
                                           #
                                           # sliderInput(inputId = 'k_row_input',
                                           #             label = 'Colour clusters, genes:',
                                           #             value = 0,
                                           #             min = 0, max = 50),
                                           #
                                           # sliderInput(inputId = 'k_col_input',
                                           #              label = 'Colour clusters, samples',
                                           #              value = 0,
                                           #              min = 0, max = 50)#,

                                           # downloadButton(outputId = 'heatmapDownloader',
                                           #                label = 'Download the heatmap')
                              ),
                              mainPanel(

                                  uiOutput('heatmapUI')

                                  #plotlyOutput('heatmaply')

                              )
                          )
)
