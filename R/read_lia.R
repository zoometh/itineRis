#' Read XLSX dataset of lead isotop analysis (LIA)
#' @name read_lia
#' @description Read the dataset of objects. Will store every spreadsheet under its name within the hash object (e.g. "golasecca")
#'
#' @param d a hash dictionnary-like object to store the data
#' @param d.tag the new field in `d`. By default, 'lia'
#' @param df.path the path and name of the dataset. An XLSX file.
#' @param df.sheetnames the name or index of the spreadsheet(s) to be read. By default "all" will read all the spreadsheets
#' @param header.line the row of the headers, by default the first line
#' @param pattern.objects.num Search (regex) for this pattern in the column names to find the laboratory number. It is important to remind that R doesn't accept spaces in header that must be replcaed by dots
#' @param pattern.objects.Pb206_Pb204,pattern.objects.Pb207_Pb204,pattern.objects.Pb208_Pb204 Search (regex) for these patterns in the column names to find the LIA measurements
#' @param export.plot if TRUE, export the dataframe
#' @param out.plot the name of the output dataframe
#' @param dirOut path of the output directory
#'
#' @return A hash with a new entrance. If export is TRUE, will export as TSV
#'
#' @examples
#'
#' d <- hash::hash()
#' d <- read_lia(d = d,
#'               d.tag = "lia.objects",
#'               df.path = "C:/Rprojects/itineRis/results/LIA data objects.xlsx",
#'               header.line = 2)
#' d <- read_lia(d = d,
#'               d.tag = "lia.mines",
#'               pattern.objects.num = "Locality/.Mine",
#'               pattern.objects.Pb206_Pb204 = "206Pb/204Pb",
#'               pattern.objects.Pb207_Pb204 = "207Pb/204Pb",
#'               pattern.objects.Pb208_Pb204 = "208Pb/204Pb",
#'               df.path = "C:/Rprojects/itineRis/results/Coordinates-mines_Thomas Huet.xlsx",
#'               header.line = 1)
#'
#' @export
read_lia <- function(d = NA,
                     d.tag = "lia",
                     df.path = paste0(system.file(package = "itineRis"),
                                      "/extdata/LIA data objects.xlsx"),
                     df.sheetnames = c("all"),
                     header.line = 1,
                     pattern.objects.num = "Labornr",
                     pattern.objects.Pb206_Pb204 = "Pb206/Pb204",
                     pattern.objects.Pb207_Pb204 = "Pb207/Pb204",
                     pattern.objects.Pb208_Pb204 = "Pb208/Pb204",
                     export.dataframe = FALSE,
                     out.df = "lia.tsv",
                     dirOut = paste0(system.file(package = "itineRis"),
                                     "/results/")){
  l.keys <- c()
  if("all" %in% df.sheetnames){
    for(sheetname in openxlsx::getSheetNames(df.path)){
      l.keys <- c(l.keys, sheetname)
      data.sheetname <- openxlsx::read.xlsx(df.path,
                                            sheet = sheetname)
      if(header.line > 1){
        header.is <- seq(header.line -1, 1)
        colnames(data.sheetname) <- data.sheetname[header.is, ]
        data.sheetname <- data.sheetname[-c(header.is), ]
        d[[sheetname]] <- data.sheetname
      } else {
        d[[sheetname]] <- data.sheetname
      }
    }
  } else {
    for(sheetname in df.sheetnames){
      data.sheetname <- openxlsx::read.xlsx(df.path,
                                            sheet = df.sheetnames)
      colnames(data.sheetname) <- data.sheetname[header.line - 1, ]
      data.sheetname <- data.sheetname[-c(header.line - 1), ]
      d[[sheetname]] <- df.sheetnames
    }
    l.keys <- df.sheetnames
  }
  df.lia <- data.frame(num = character(),
                       object = character(),
                       Pb206_Pb204 = numeric(),
                       Pb207_Pb204 = numeric(),
                       Pb208_Pb204 = numeric())
  for(sheetname in l.keys){
    # sheetname <- "golasecca"
    # sheetname <- "hochdorf"
    # sheetname <- "Switzerland"
    col.names <- colnames(d[[sheetname]])
    pb.idx <- c()
    # find the indices of columns
    num.idx <- grep(toupper(pattern.objects.num), toupper(col.names))
    Pb206_Pb204 <- grep(toupper(pattern.objects.Pb206_Pb204), toupper(col.names))
    Pb207_Pb204 <- grep(toupper(pattern.objects.Pb207_Pb204), toupper(col.names))
    Pb208_Pb204 <- grep(toupper(pattern.objects.Pb208_Pb204), toupper(col.names))
    # # find the X,Y columns
    # for(coords.columns in pattern.mines.coords){
    #   # coords.columns <- "longitude"
    #   pb.idx <- c(pb.idx, grep(toupper(coords.columns), toupper(col.names)))
    # }
    df.lia.sheet <- data.frame(num = d[[sheetname]][num.idx],
                               object = sheetname,
                               Pb206_Pb204 = d[[sheetname]][Pb206_Pb204],
                               Pb207_Pb204 = d[[sheetname]][Pb207_Pb204],
                               Pb208_Pb204 = d[[sheetname]][Pb208_Pb204])
    names(df.lia.sheet) <- c("num", "object", "Pb206_Pb204", "Pb207_Pb204", "Pb208_Pb204")
    # cleaning
    df.lia.sheet <- df.lia.sheet[!is.na(as.numeric(df.lia.sheet[, "Pb206_Pb204"])), ]
    df.lia.sheet <- df.lia.sheet[!is.na(as.numeric(df.lia.sheet[, "Pb207_Pb204"])), ]
    df.lia.sheet <- df.lia.sheet[!is.na(as.numeric(df.lia.sheet[, "Pb208_Pb204"])), ]
    # df.lia.sheet <- df.lia.sheet[,as.numeric(as.character(df.lia.sheet$y))]
    df.lia.sheet <- na.omit(df.lia.sheet)
    df.lia.sheet <- df.lia.sheet[!duplicated(df.lia.sheet), ]
    df.lia <- rbind(df.lia, df.lia.sheet)
  }
  df.lia[, 3:5] <- sapply(df.lia[, 3:5], as.numeric)
  if(export.dataframe){
    path.out <- paste0(dirOut, out.df)
    dir.create(dirOut, showWarnings = FALSE)
    write.table(df.lia, path.out, sep = "\t", row.names = FALSE)
    print(paste0("LIA dataframe '", out.df, "' created in '", dirOut, "'"))
  }
  d[[d.tag]] <- df.lia
  return(d)
}
