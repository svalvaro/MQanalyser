options(shiny.maxRequestSize=1000*1024^2)## Set maximum upload size to 1000MB

function(input, output) {

    #### Tab Data Input ####

    # DEMO

    source('server/tabDataInput/demo.R' , local = TRUE)

    # protein Input MaxQuant or Spectronaut

    source("server/tabDataInput/proteinInput.R", local = TRUE)

    #Software used

    source("server/tabDataInput/softwareUsed.R", local = TRUE)

    # Experiment Names

    source("server/tabDataInput/experimentNames.R", local = TRUE)

    # Experiment design
    source("server/tabDataInput/experimentDesign.R", local = TRUE)

    # Pop-up message when pressed start analysis && Button #

    source("server/tabDataInput/popupStartandButton.R", local = TRUE)

    # Intensity type depending on the proteoInput

    source("server/tabDataInput/intensityType.R", local = TRUE)

    # User Genes

    source("server/tabDataInput/userGenes.R", local = TRUE)

    #### Tab Preprocessing ####

    # Filter out contaminants

    source("server/tabPreprocessing/contaminants.R", local = TRUE)

    # Make summarised experiment

    source("server/tabPreprocessing/summarisedExperiment.R", local = TRUE)

    # Filtering of NAs

    source("server/tabPreprocessing/filteringNAs.R", local = TRUE)

    # DATA normalization

    source("server/tabPreprocessing/dataNormalization.R", local = TRUE)

    # Imputation

    source("server/tabPreprocessing/imputation.R", local = TRUE)

    #### Tab Results ####

    # Differential Enrichment analysis of Proteomics (DEP)
    source("server/tabResults/DEP.R", local = TRUE)

    # RESULTS TABULAR
    source("server/tabResults/resultsTabular.R", local = TRUE)

    #### Tab Heatmap ####

    source("server/tabHeatmap/heatmap.R", local = TRUE)

    #### Tab Sample COmparison ####

    # Scatter plot

    source("server/tabSampleComparison/scatter.R", local = TRUE)

    # Correlation plot
    source("server/tabSampleComparison/correlation.R", local = TRUE)

    # PCA pot

    source("server/tabSampleComparison/pca.R", local = TRUE)

    #### Tab Volcano plot ####

    source("server/tabVolcano/volcano.R", local = TRUE)

    #### Tab Profile Plot ####

    source("server/tabProfile/profile.R", local = TRUE)

    #### Tab Enrichment Analysis ####

    # Comparisons
    source("server/tabEnrichment/comparisons.R", local = TRUE)

    # Enrichment elements

    source("server/tabEnrichment/elements.R", local = TRUE)

    # Gene Ontology Plot

    source("server/tabEnrichment/GOplot.R", local = TRUE)

    # prerank score plot

    source("server/tabEnrichment/preRanked.R", local = TRUE)

    # Network

    source("server/tabEnrichment/network.R", local = TRUE)

    # GO table

    source("server/tabEnrichment/GOTable.R", local = TRUE)

    #### Tab Disease Analysis  ####

    # Warning pop up that the disease tab is only available for human data

    source("server/tabDisease/warning.R", local = TRUE)

    # DISEASE PLOTS

    source("server/tabDisease/allPlots.R", local = TRUE)

    #### Tab Pathway Analysi s####

    source("server/tabPathway/pathwayPlotTable.R", local = TRUE)

    source("server/tabPathway/pathwaySelector.R", local = TRUE)

    #### Tab Interactions String database ####

    source("server/tabInteractions/interactions.R", local = TRUE)

    #### Tab Report ####

    # Plots for the report

    # Experiment design

    source("server/tabReport/expDesignReport.R", local = TRUE)

    # Preprocessing Plots

    source("server/tabReport/preProcessingReport.R", local = TRUE)

    # Sample Comparison Plots
    source("server/tabReport/sampleComparisonReport.R", local = TRUE)

    # Results Plots

    source("server/tabReport/resultsPlotsReport.R", local = TRUE)

    # Enrichment Plots

    source("server/tabReport/enrichmentPlotsReport.R", local = TRUE)

    # Disease Plots

    source("server/tabReport/diseasePlotsReport.R", local = TRUE)

    # Pathway Plot

    source("server/tabReport/pathwayReport.R", local = TRUE)

    # Interactions Plot

    source("server/tabReport/interactionsReport.R", local = TRUE)

    # report downloader

    source("server/tabReport/downloadReport.R", local = TRUE)


    #### Block the tabs ####

    source("server/blockTabs/blockTabs.R", local = TRUE)

}
