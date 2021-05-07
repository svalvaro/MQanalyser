#' Profile Plot
#'
#' @param dep
#'
#' @return
#' @export
#'
#' @examples
plot_profilely <- function(dep,
                           centered = FALSE,
                           intensity_type = 'LFQ',
                           color = '#56B4E9',
                           angle_labels = 45,
                           selected_genes = NULL,
                           color_selected = 'red',
                           plot = TRUE,
                           clear_selection = FALSE,
                           prof_genes_de = FALSE,
                           user_genes_de = NULL,
                           color_genes_de = '#800080'){
  # clear_selection <- TRUE
   #selected_genes <- c(4,15,200,500)

    if(clear_selection == TRUE){
      selected_genes <- NULL
    }

    row_data <- rowData(dep, use.names = FALSE)

    col_data <- colData(dep) %>%
        as.data.frame()

    # Filter for significant proteins only
    filtered <- dep[row_data$significant, ]



# Get centered intensity values ('centered')

    if(centered == TRUE){

      rowData(filtered)$mean <- rowMeans(assay(filtered), na.rm = TRUE)

      df <- as.data.frame(assay(filtered) - rowData(filtered, use.names = FALSE)$mean)

    }else{
      # not centered

      df <- as.data.frame(assay(filtered))
    }


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


    if(!is.null(user_genes_de) & prof_genes_de == TRUE){
      p <- p +geom_line(data = df_melt[which(df_melt$Gene %in% user_genes_de),],
                        mapping = aes(group=Gene),
                        color = color_genes_de)
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
