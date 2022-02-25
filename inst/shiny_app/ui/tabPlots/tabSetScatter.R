tabSetScatter <- tabPanel(
    title = h4('Scatter Plot'),
    sidebarLayout(
        sidebarPanel(
            id = 'sidebar',
            width = 2,
            h3("Select the adjustments"),

            #Sample for X axis
            uiOutput("x_sample_selector"),
            #Sample for y axis

            uiOutput("y_sample_selector"),

            #check if they want to see their genes

            checkboxInput(
                inputId = 'showgenes',
                label = h4(
                    'Show the differentially expressed
                      proteins of the list that you have uploaded'
                ),
                value =
                    FALSE
            ),
            dropdown(
                tags$h3("Advanced Parameters"),

                sliderInput(
                    inputId = 'input_alpha',
                    label = 'Adjust the alpha parameter:',
                    value = 0.8,
                    min = 0,
                    max = 1
                ),

                #add or not a regression line
                checkboxInput(
                    inputId = 'input_lm',
                    label = h4('Add regression line'),
                    value = FALSE
                ),

                colourpicker::colourInput(
                    inputId = "color_scatter",
                    h4("Select colour:"),
                    '#71a873',
                    palette = "square",
                    returnName = TRUE,
                    showColour = c("background")
                ),


                colourpicker::colourInput(
                    inputId = "color_de_scatter",
                    h4("Select colour for your proteins of interest:"),
                    '#dc143c',
                    palette = "square",
                    returnName = TRUE,
                    showColour = c("background")
                ),

                options = list(`style` = "btn-info"),
                style = "unite",
                icon = icon("paint-brush"),
                status = "success",
                width = "300px",
                animate = animateOptions(
                    enter = animations$fading_entrances$fadeInLeftBig,
                    exit = animations$fading_exits$fadeOutRightBig
                )
            )
        ),

        mainPanel(#Plot the scatter plot  in the second tab
            box(
                height = 1200,
                width = 1200,
                shinycssloaders::withSpinner(
                    plotlyOutput('scatterplot'),
                    image = 'images/logoTransparentSmall.gif',
                    image.width = '200px'
                )
            ))
    )
)
