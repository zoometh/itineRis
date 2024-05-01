#' Read XLSX dataset of objects described typologicaly
#'
#' @name read_objects_typo
#'
#' @description Read XLSX dataset of objects hosted on Google Drive
#'
#' @param ggdocument.url The filename, ex: "DATA_01_IT_VC_2.xlsx"
#' @param authentificate Authentificate when TRUE
#'
#' @return A dataframe of objects
#'
#' @examples
#'
#'
#' @export
read_objects_typo <- function(ggdocument.url = NA,
                         ggdocument.sheet = 1,
                         authentificate = TRUE,
                         verbose = TRUE){

  if(authentificate){
    if(verbose){
      print(paste0("Ask for Google Drive authentification"))
    }
    googledrive::drive_deauth()
    googledrive::drive_auth()
  }
  unlink(ggdocument.url)
  if(verbose){
    print(paste0("Download the document '", ggdocument.url,"'"))
  }
  data.border <- googledrive::drive_download(ggdocument.url)
  data <- openxlsx::read.xlsx(ggdocument.url, sheet = ggdocument.sheet)
  return(data)
}
