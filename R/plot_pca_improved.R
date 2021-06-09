#' Title
#'
#' @param dep
#' @param label_name
#' @param n
#'
#'
#' @importFrom SummarizedExperiment assay
#' @return
#' @export
#'
#' @examples
plot_pca_improved <- function(dep,
                              PC_x = 1,
                              PC_y = 2,
                       label_name = NULL, # 'name', 'replicate', 'label'
                       n = 500){

    # Get the variance per protein and take the top n variable proteins
    var <- apply(assay(dep), 1, sd)
    df <- assay(dep)[order(var, decreasing = TRUE)[seq_len(n)],]




    # Calculate PCA
    pca <- prcomp(t(df), scale = FALSE)
    pca_df <- pca$x %>%
        data.frame() %>%
        rownames_to_column() %>%
        left_join(., data.frame(colData(dep)), by = c("rowname" = "ID"))



    # Calculate the percentage of variance explained
    percent <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)


    # Make factors of indicate features
    for(feat in label_name) {
        pca_df[[feat]] <- as.factor(pca_df[[feat]])
    }


    # Plot the PCA plot
    p <- ggplot(pca_df, aes(PC1, PC2)) +
        labs(title = paste0("PCA plot - top ", n, " variable proteins"),
             x = paste0("PC", PC_x, ": ", percent[PC_x], "%"),
             y = paste0("PC", PC_y, ": ", percent[PC_y], "%")) +
        #coord_fixed()+
        theme_bw()+
        theme(#panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA))+
        geom_point(aes(fill = condition), shape = 21, color = 'black', size = 4)+
        geom_text_repel(aes_string(label = label_name))+
        theme(legend.title = element_blank())



    return(p)



}
