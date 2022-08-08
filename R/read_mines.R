#' Read the dataset of mines
#' @name read_mines
#' @description Read the dataset of mines. Will store every spreadsheet under its name within the hash object (e.g. "France")
#'
#' @param d a hash dictionnary-like object to store the data
#' @param df.path the path and name of the dataset. An XLSX file.
#' @param df.sheetnames the name or index of the spreadsheet(s) to be read. By default "all" will read all the spreadsheets
#' @param extract.mines if TRUE, will extract the coordinates of one to many spreadsheets and store them under the "mines.coords" key
#' @param pattern.mines.coords if `extract.mines` is TRUE, will search (regex) for these patterns. A vector of two strings: first longitude/X, second: Y/latitude. By default `c("longitude", "latitude")`
#'
#' @return A hash
#'
#' @examples
#'
#' d <- hash::hash()
#' d <- read_mines(d = d,
#'                 df.path = "C:/Rprojects/itineRis/results/Coordinates-mines_Thomas Huet.xlsx")
#'
#' @export
read_mines <- function(d = NA,
                       df.path = paste0(system.file(package = "itineRis"),
                                        "/extdata/Coordinates-mines_Thomas Huet.xlsx"),
                       df.sheetnames = c("all"),
                       extract.mines = TRUE,
                       pattern.mines.name = "locality",
                       pattern.mines.coords = c("longitude", "latitude")){
  l.keys <- c()
  if("all" %in% df.sheetnames){
    for(sheetname in openxlsx::getSheetNames(df.path)){
      l.keys <- c(l.keys, sheetname)
      data.sheetname <- openxlsx::read.xlsx(df.path,
                                            sheet = sheetname)
      d[[sheetname]] <- data.sheetname
    }
  } else {
    for(sheetname in df.sheetnames){
      data.sheetname <- openxlsx::read.xlsx(df.path,
                                            sheet = df.sheetnames)
      d[[sheetname]] <- df.sheetnames
    }
    l.keys <- df.sheetnames
  }
  if(extract.mines){
    df.coords <- data.frame(site = character(),
                            x = numeric(),
                            y = numeric())
    for(sheetname in l.keys){
      # sheetname <- "Iberian Peninsula"
      # sheetname <- "France"
      # sheetname <- "Switzerland"
      col.names <- colnames(d[[sheetname]])
      long.lat.idx <- c()
      # find the name of the mine
      name.idx <- grep(toupper(pattern.mines.name), toupper(col.names))
      # find the X,Y columns
      for(coords.columns in pattern.mines.coords){
        # coords.columns <- "longitude"
        long.lat.idx <- c(long.lat.idx, grep(toupper(coords.columns), toupper(col.names)))
      }
      df.coord.sheet <- data.frame(site = d[[sheetname]][name.idx],
                                   x = d[[sheetname]][long.lat.idx[1]],
                                   y = d[[sheetname]][long.lat.idx[2]])
      names(df.coord.sheet) <- c("site", "x", "y")
      # cleaning
      df.coord.sheet <- df.coord.sheet[!is.na(as.numeric(df.coord.sheet$x)), ]
      df.coord.sheet <- df.coord.sheet[!is.na(as.numeric(df.coord.sheet$y)), ]
      # df.coord.sheet <- df.coord.sheet[,as.numeric(as.character(df.coord.sheet$y))]
      df.coord.sheet <- na.omit(df.coord.sheet)
      df.coord.sheet <- df.coord.sheet[!duplicated(df.coord.sheet), ]
      df.coords <- rbind(df.coords, df.coord.sheet)
    }
    d[["mines.coords"]] <- df.coords
  }
  return(d)
}
