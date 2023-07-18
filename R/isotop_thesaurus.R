#' Read the thesaurus of authoritative labels for chemistry variables
#'
#' @name isotop_thesaurus
#'
#' @description Read the thesaurus of isotopes, elements, ... Add values and column names into a list that can be compared to input dataset.
#'
#' @param path.thes the path to the thesaurus file.
#' @param as.list if TRUE (Default: FALSE) return all values, and colnames, as a vector. If FALSE, return the thesaurus as a dataframe.
#' @param verbose if TRUE, print messages
#'
#' @return A vector of values
#'
#' @examples
#'
#' # Return the thesaurus as a vector of values
#' thes <- isotop_thesaurus(as.list = T)
#'
#' @export
isotop_thesaurus <- function(path.thes = "https://raw.githubusercontent.com/zoometh/itineRis/main/inst/extdata/isotops_thesaurus.csv",
                             as.list = FALSE,
                             verbose = TRUE){
  if(verbose){print("Read the thesaurus")}
  thes.isotop <- read.csv2(path.thes, sep = ",")
  if(as.list){
    all.values <- as.vector(as.matrix(thes.isotop))
    sel.values <- all.values[!all.values %in% '']
    val.values <- c(sel.values, colnames(thes.isotop))
    if(verbose){print("Thesaurus returned as list")}
    return(val.values)
  } else {
    if(verbose){print("Thesaurus returned")}
    return(thes.isotop)
  }
}
