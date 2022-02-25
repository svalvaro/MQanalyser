#### Sourcing the UI elements

source("ui/uiReport.R", local = TRUE)
source("ui/uiLogo.R", local = TRUE)
source("ui/uiInteractions.R", local = TRUE)
source("ui/uiPathwayAnalysis.R", local = TRUE)
source("ui/uiDataInput.R", local = TRUE)
source("ui/uiPreprocessing.R", local = TRUE)
source("ui/uiResults.R", local = TRUE)
source("ui/uiSampleComparisons.R", local = TRUE)
source("ui/uiEnrichmentAnalysis.R", local = TRUE)
source("ui/uiDisease.R", local = TRUE)

##### UI
tagList(

#### navbar ####
  navbarPage(
    fluid = TRUE,
    id = 'tabs_menu',

    ##### style css ####

    titlePanel(
      title = '',
      # Show the logo in the browser tab
      windowTitle = tags$head(
        tags$link(
          rel = "icon",
          type = "image/png",
          href = "images/logo_small.png"
          ),
        tags$title("Proteomics Analyser"))
    ),

    theme = shinytheme(theme ='flatly'),
#### DATA INPUT ####

    uiDataInput,

#### Preprocessing ####
    uiPreprocessing,

#### Results Panel ####
    uiResults,

#### Sample Comparisons ####

    uiSampleComparisons,

#### Enrichment analysis ####

    uiEnrichmentAnalysis,

#### Disease Analysis Tab ####

    uiDisease,

#### Pathway Analysis ####
    uiPathwayAnalysis,

#### Interactions STRING DB ####

  uiInteractions,

#### Report Panel ####

  uiReport,

#### Logo panel ####
  uiLogo
  ),

#### JS ####
  # Block all tabs at the start

tags$script(
  src = 'JavaScript/block-all-tabs.js'
  ),

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }")

)
