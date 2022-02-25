tabSetPCA <-    tabPanel(h4('PCA'),
                         sidebarLayout(
                             sidebarPanel(
                                 id = 'sidebar',
                                 width = 2,
                                 h3("Select the adjustments"),

                                 selectInput(
                                     inputId = 'pca_label',
                                     label = 'Select the labels:',
                                     choices =  c(
                                         'label' = 'rowname',
                                         'experiment' = 'label',
                                         'replicate' = 'replicate'
                                     ),
                                     selected = 'rowname'
                                 ),
                                 uiOutput('pca_number_proteins'),
                                 # Download button for the plot
                                 downloadButton(outputId = 'downloadPCA',
                                                label = 'Download the PCA plot')
                             ),
                             mainPanel(box(
                                 height = 900,
                                 width = 1300,
                                 shinycssloaders::withSpinner(
                                     plotOutput('pca_plot'),
                                     image = 'images/logoTransparentSmall.gif',
                                     image.width = '200px'
                                 )
                             ))
                         ))
