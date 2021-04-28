#' Profile Plot
#'
#' @param dep
#'
#' @return
#' @export
#'
#' @examples
plot_profilely <- function(dep,
                           intensity_type = 'LFQ',
                           color = '#56B4E9',
                           angle_labels = 45,
                           selected_genes = NULL,
                           color_selected = 'red',
                           plot = TRUE){

    row_data <- rowData(dep, use.names = FALSE)

    col_data <- colData(dep) %>%
        as.data.frame()

    # Filter for significant proteins only
    filtered <- dep[row_data$significant, ]



    # Get centered intensity values ('centered')
#
#     rowData(filtered)$mean <- rowMeans(assay(filtered), na.rm = TRUE)
#
#     df <- as.data.frame(assay(filtered) - rowData(filtered, use.names = FALSE)$mean)

    df <- as.data.frame(assay(filtered))
#
    df$name <- rownames(df)

    df_melt <- melt(df, id.vars = 'name')

    colnames(df_melt) <- c('Gene', 'Label', 'Intensity') #It is centered intenseity (removed)


    #selected_genes <- c(4,15,200,500)




    p <- ggplot(df_melt, aes(x=Label,
                             y= Intensity,
                             text = paste('\nGene:', Gene,
                                          '\nLabel:', Label,
                                          paste0('\nLog 2 ', intensity_type,': '), format(round(Intensity,1),nsmall =1)
                                          )))+
            theme_bw()+
            geom_line(aes(group=Gene), color= color)+
            ylab(paste0('Log2  ', intensity_type))+
            theme(axis.text.x = element_text(angle = angle_labels))


    if(!is.null(selected_genes)){

        df_selected <- df[selected_genes, ]
        #df_selected <- df_selected[df_melt$Gene == selected_genes,]

        df_selected <- melt(df_selected, id.vars = 'name')

        colnames(df_selected) <- c('Gene', 'Label', 'Intensity')


        p <-  p + geom_line(df_selected,
                            mapping = aes(x=Label,y= Intensity,group = Gene),
                                color = color_selected)
    }

    if (plot == FALSE) {

      rownames(df) <- NULL
      #put the name column the first one

      df <- df[c('name', setdiff(names(df), 'name'))]

      colnames(df[1]) <- 'Gene'

      colnames(df)[colnames(df) == "name"] <- "Genes"

      df


    } else{
        ggplotly(p,tooltip = c('text'))
    }




}
