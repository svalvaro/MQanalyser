#' Maxquant or spectronaut
#'
#' @param file
#' @return maxquant or spectrounaut
#' @export
#'
#' @examples
check_software_input <- function(file){


    # If no data frame was loaded, return NULL
    if (!is.data.frame(file)) {
        return(NULL)
    }

    # To check if the input is coming from MaxQuant:

    if ('Protein.IDs' %in% base::colnames(file) &&
        'Protein.names'%in% base::colnames(file) &&
        'Majority.protein.IDs' %in% base::colnames(file)) {



        software_used <- 'MaxQuant'

    } else if('PG.ProteinGroups' %in% base::colnames(file)&&
              'PG.ProteinDescriptions' %in% base::colnames(file)) {

        software_used <- 'Spectronaut'
    }


    message(paste0(software_used, ' software detected'))

    return(software_used)
}
