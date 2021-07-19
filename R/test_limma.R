#' Test_limma
#'
#' @param se
#' @param type
#' @param control
#' @param test
#' @param design_formula
#' @param paired
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import limma
#' @import rhandsontable
#' @import SummarizedExperiment
#'
#' @return
#' @export
#'
#' @examples
test_limma <- function(se, type = c("control", "all", "manual"),
                       control = NULL, test = NULL,
                       design_formula = formula(~ 0 + condition),
                       paired = FALSE){
    #require("dplyr", "tidyr", "purrr")

    # Show error if inputs are not the required classes
    assertthat::assert_that(inherits(se, "SummarizedExperiment"),
                            is.character(type),
                            class(design_formula) == "formula")
if (paired == FALSE){
    design_formula <- design_formula
}else{
    design_formula<-formula(~ 0 + condition + replicate)
}


# Show error if inputs do not contain required columns
type <- match.arg(type)

col_data <- SummarizedExperiment::colData(se)
raw <- SummarizedExperiment::assay(se)

if(any(!c("name", "ID") %in% colnames(SummarizedExperiment::rowData(se)))) {
    stop("'name' and/or 'ID' columns are not present in '",
         deparse(substitute(se)),
         "'\nRun make_unique() and make_se() to obtain the required columns",
         call. = FALSE)
}
if(any(!c("label", "condition", "replicate") %in% colnames(col_data))) {
    stop("'label', 'condition' and/or 'replicate' columns are not present in '",
         deparse(substitute(se)),
         "'\nRun make_se() or make_se_parse() to obtain the required columns",
         call. = FALSE)
}
if(any(is.na(raw))) {
    warning("Missing values in '", deparse(substitute(se)), "'")
}

if(!is.null(control)) {
    # Show error if control input is not valid
    assertthat::assert_that(is.character(control),
                            length(control) == 1)
    if(!control %in% unique(col_data$condition)) {
        stop("run test_diff() with a valid control.\nValid controls are: '",
             paste0(unique(col_data$condition), collapse = "', '"), "'",
             call. = FALSE)
    }
}

# variables in formula
variables <- terms.formula(design_formula) %>%
    attr(., "variables") %>%
    as.character() %>%
    .[-1]

# Throw error if variables are not col_data columns
if(any(!variables %in% colnames(col_data))) {
    stop("run make_diff() with an appropriate 'design_formula'")
}
if(variables[1] != "condition") {
    stop("first factor of 'design_formula' should be 'condition'")
}

# Obtain variable factors
for(var in variables) {
    temp <- factor(col_data[[var]])
    assign(var, temp)
}

# Make an appropriate design matrix
design <- model.matrix(design_formula, data = environment())
colnames(design) <- gsub("condition", "", colnames(design))

# Generate contrasts to be tested
# Either make all possible combinations ("all"),
# only the contrasts versus the control sample ("control") or
# use manual contrasts
conditions <- as.character(unique(condition))
if(type == "all") {
    # All possible combinations
    cntrst <- apply(utils::combn(conditions, 2), 2, paste, collapse = " - ")

    if(!is.null(control)) {
        # Make sure that contrast containing
        # the control sample have the control as denominator
        flip <- grep(paste("^", control, sep = ""), cntrst)
        if(length(flip) >= 1) {
            cntrst[flip] <- cntrst[flip] %>%
                gsub(paste(control, "- ", sep = " "), "", .) %>%
                paste(" - ", control, sep = "")
        }
    }

}
if(type == "control") {
    # Throw error if no control argument is present
    if(is.null(control))
        stop("run test_diff(type = 'control') with a 'control' argument")

    # Make contrasts
    cntrst <- paste(conditions[!conditions %in% control],
                    control,
                    sep = " - ")
}
if(type == "manual") {
    # Throw error if no test argument is present
    if(is.null(test)) {
        stop("run test_diff(type = 'manual') with a 'test' argument")
    }
    assertthat::assert_that(is.character(test))

    if(any(!unlist(strsplit(test, "_vs_")) %in% conditions)) {
        stop("run test_diff() with valid contrasts in 'test'",
             ".\nValid contrasts should contain combinations of: '",
             paste0(conditions, collapse = "', '"),
             "', for example '", paste0(conditions[1], "_vs_", conditions[2]),
             "'.", call. = FALSE)
    }

    cntrst <- gsub("_vs_", " - ", test)

}
# Print tested contrasts
message("Tested contrasts: ",
        paste(gsub(" - ", "_vs_", cntrst), collapse = ", "))

# Test for differential expression by empirical Bayes moderation
# of a linear model on the predefined contrasts
fit <- limma::lmFit(raw, design = design)
made_contrasts <- limma::makeContrasts(contrasts = cntrst, levels = design)
contrast_fit <- contrasts.fit(fit, made_contrasts)

if(any(is.na(raw))) {
    for(i in cntrst) {
        covariates <- strsplit(i, " - ") %>% unlist
        single_contrast <- makeContrasts(contrasts = i, levels = design[, covariates])
        single_contrast_fit <- contrasts.fit(fit[, covariates], single_contrast)
        contrast_fit$coefficients[, i] <- single_contrast_fit$coefficients[, 1]
        contrast_fit$stdev.unscaled[, i] <- single_contrast_fit$stdev.unscaled[, 1]
    }
}

eB_fit <- eBayes(contrast_fit)

# function to retrieve the results of
# the differential expression test using 'fdrtool'
retrieve_fun <- function(comp, fit = eB_fit){
    res <- topTable(fit, sort.by = "t", adjust.method="BH", coef = comp,
                    number = Inf, confint = TRUE)
    # res <- res[!is.na(res$t),]
    #fdr_res <- fdrtool::fdrtool(res$t, plot = FALSE, verbose = FALSE)
    # res$qval <- res$adj.P.Value
    #res$lfdr <- fdr_res$lfdr
    res$comparison <- rep(comp, dim(res)[1])
    res <- tibble::rownames_to_column(res)
    return(res)
}

#limma_res<- topTable(eB_fit, sort.by = 'B', adjust.method="BH", coef = cntrst, number = Inf, confint = T )
# limma_res$comparison <- rep(cntrst, dim(limma_res)[1])
#limma_res <- rownames_to_column(limma_res)
# Retrieve the differential expression test results
limma_res <- purrr::map_df(cntrst, retrieve_fun)

# Select the logFC, CI and qval variables
table <- limma_res %>%
    dplyr::select(rowname, logFC, CI.L, CI.R, P.Value, adj.P.Val, comparison) %>%
    dplyr::mutate(comparison = gsub(" - ", "_vs_", comparison)) %>%
    tidyr::gather(variable, value, -c(rowname,comparison)) %>%
    dplyr::mutate(variable = dplyr::recode(variable, logFC = "diff", P.Value = "p.val", adj.P.Val = "p.adj")) %>%
    tidyr::unite(temp, comparison, variable) %>%
    tidyr::spread(temp, value)
rowData(se) <- merge(rowData(se), table,
                     by.x = "name", by.y = "rowname", all.x = TRUE)
return(se)
#return(table)
}
