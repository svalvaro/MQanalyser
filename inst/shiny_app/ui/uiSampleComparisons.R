source("ui/tabPlots/tabSetScatter.R", local = TRUE)
source("ui/tabPlots/tabSetCorrelation.R", local = TRUE)
source("ui/tabPlots/tabSetPCA.R", local = TRUE)
source("ui/tabPlots/tabSetHeatmap.R", local = TRUE)
source("ui/tabPlots/tabSetVolcano.R", local = TRUE)
source("ui/tabPlots/tabSetProfile.R", local = TRUE)

uiSampleComparisons <-     tabPanel(
    h4("Visualizations"),
    value = 'comparisons-tab',

    tabsetPanel(
        type = 'tabs',
        # Scatter Plot

        tabSetScatter,
        # Correlation Plot
        tabSetCorrelation,

        # PCA Plot
        tabSetPCA,

        # Heat Map
        tabSetHeatmap,

        # Volcano Plot

        tabSetVolcano,

        # Profile Plot

        tabSetProfile
    )
)
