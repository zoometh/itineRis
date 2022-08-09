#' Create a simple dataframe with colors depending on a field
#' @name isotop_dataframe
#' @description Creates a new colum for the color based on another column. For example 'color.objects'
#' means the colors are based on the 'object' field
#'
#' @param df a dataframe. By defaut NA, so it will read a file using `df.path`
#' @param df.path if `df` is NA, will use this path and name to read the dataset from a file. Either a TSV or a CSV with ';' separators.
#' @param color.column the name on which the different colors will be setup. By default, the colors
#' are those of the objects
#'
#' @return a dataframe with an hexadecimal value for the colors
#'
#' @examples
#'
#' df.isotop <- isotop_dataframe()
#'
#' @export
isotop_dataframe<- function(df = NA,
                            df.path = paste0(system.file(package = "itineRis"),
                                             "/extdata/isotop_results.tsv"),
                            color.column = "object"){
  if(is.na(df)){
    if(DescTools::SplitPath(df.path)$extension == "tsv"){sep = "\t"}
    if(DescTools::SplitPath(df.path)$extension == "csv"){sep = ";"}
    df <- read.table(df.path, sep = sep, header = T)
  }
  print(message(paste0("column names of '",
                       DescTools::SplitPath(df.path)$fullfilename,
                       "' are: ", paste0(colnames(df), collapse = ", "))))
  u.colors <- unique(df[, color.column])
  df$type <- df[, color.column]
  df.colors <- data.frame(type = u.colors,
                          stringsAsFactors = F
  )
  color.var <- paste0("color.", color.column)
  df.colors[[color.var]] <- rainbow(length(u.colors))
  df.isotop <- merge(df, df.colors, by = "type")
  df.isotop$type <- NULL
  return(df.isotop)
}

