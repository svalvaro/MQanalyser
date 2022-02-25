tabSetCorrelation <- tabPanel(h4('Correlation'),
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
         ))
