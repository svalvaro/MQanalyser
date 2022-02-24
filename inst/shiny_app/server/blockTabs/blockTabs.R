# Observe which tab the user is in: Unblock the Preprocessing tab


observeEvent(input$start_input, {

    message('Unblocking the preprocessing tab ')

    shinyjs::runjs(
        '
         var tab = $(\'a[data-value="preprocessing-tab"]\').parent();
         $(tab).removeClass("disabled");
         '
    )
})

# Unblock the results, heatmap, comparison, volcano, profile and enrichment
# tabs

observeEvent(input$preprocessing_tabset,{

    # If the user enters in one of the other three tabs creating the
    # data_se() summarized experiment object, allow them to enter in the
    # tabs
    if (! input$preprocessing_tabset == 'contaminants_tab' ) {

        message('Unblock the rest of the results and other tabs')

        shinyjs::runjs(
            '
                var tab = $(\'a[data-value="results-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="heatmap-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="comparisons-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="volcano-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="profile-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="enrichment-tab"]\').parent();
                $(tab).removeClass("disabled");

                '
        )
    }
})

# The pathway tabs are unblock under the enrichment section.
