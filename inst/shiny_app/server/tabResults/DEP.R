dep <- reactive({

    # data_diff_all_contrasts <- MQanalyser::test_limma(data_imp, type = "all")

    data_diff_all_contrasts <- MQanalyser::test_limma(data_imp(),
                                                      type = "all")

    #data_diff_all_contrasts <- DEP::test_diff(data_imp(), type = "all", design_formula = formula(~ 0 + condition))

    # dep <- add_rejections(data_diff_all_contrasts, alpha = 0.05, lfc = log2(1.5))

    dep <- DEP::add_rejections(data_diff_all_contrasts,
                               alpha = input$input_pvalue,
                               lfc = log2(input$input_fc))
})
