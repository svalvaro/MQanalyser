# Heatmap --------------------------

heatmap_report <- reactive({

    if ("heatmap" %in% input$proteinReport) {
        message('Heatmap added to the report')

        # If the user has selected the top Contributors method, the heatmap
        # must be recreated on the .Rmd side.
        # It needs dep(), top Contributors, and Intensity type.
        # If it hasn't selected that, we just need the heatmap of all()

        if (input$topContInput == TRUE) {

            return(
                list(dep = dep(),
                     intensityType = input$IntensityType,
                     top_contributors = topContributors()
                )
            )

        }else{
            return(
                heatmapPlot()
            )
        }

    }else{
        return(NULL)
    }
})

# Volcano --------------------------

volcano_report <- reactive({

    if ("volcano" %in% input$proteinReport) {
        message('Volcano added to the report')
        return(volcano_non_interactive())
    }else{
        return(NULL)
    }
})

# Profile --------------------------

profile_report <- reactive({

    if ("profile" %in% input$proteinReport) {
        message('Volcano added to the report')
        return(profile_reactive())
    }else{
        return(NULL)
    }
})
