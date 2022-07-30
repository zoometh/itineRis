#' Create an HTML interactive table  with the values of 3 different isotops
#' @name isotop_datatable
#' @description get a simple dataframe and create an interactive datatable.
#'  This function will reuse the same colors as 'isotop_dataframe()' so it has
#'   to be run after this function
#'
#' @param df the dataframe. The color is displayed on the 'object' field
#' @param color.column the name on which the different colors will be setup. By default,
#' the 'object' column
#' @param out.plot the path and name of the created intearctive data table
#'
#' @return an DT table with a confirmation message
#'
#' @examples
#'
#' # color by default, on 'object' column
#' df.isotop <- isotop_dataframe()
#' isotop_datatable(df.isotop)
#'
#' # color on 'site' column
#' df.isotop <- isotop_dataframe(color.column = 'site')
#' isotop_datatable(df.isotop, color.column = 'site')
#'
#' @export
isotop_datatable <- function(df = NA,
                             color.column = "object",
                             out.plot = "isotop_datatable.html",
                             dirOut = paste0(system.file(package = "itineRis"), "/results/")){
  # type <- unlist(stringr::str_split(color.column, "\\."))[2]
  color <- paste0("color.", color.column)
  dt <- DT::datatable(
    df,
    rownames = FALSE,
    width = "90%",
    #escape = c("color", "type"),
    editable = FALSE) %>%
    DT::formatStyle(color.column,
                    backgroundColor = DT::styleEqual(df[, color.column],
                                                     toupper(df[, color]))
    )
  if (export.plot) {
    dir.create(dirOut, showWarnings = FALSE)
    htmlwidgets::saveWidget(dt, out.plot)
    print(paste("interactive data table '", out.plot, "' created in '", dirOut, "'"))
  } else {
    print(dt)
  }
}
