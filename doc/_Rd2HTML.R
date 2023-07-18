#' Convert .rd files to HTML files using pandoc
#'
pathMan <- "C:/Rprojects/itineRis/man/"
pathDoc <- "C:/Rprojects/itineRis/doc/"
for(a.file in list.files(pathMan)){
  # a.file <- "geojson_analysis.Rd"
  print(paste0("*Read: ", a.file))
  file.name <- DescTools::SplitPath(a.file)$filename
  file.name.path <- paste0(pathMan, file.name)
  rdIn <- paste0(paste0(pathMan, file.name), ".rd")
  htmlOut <-  paste0(paste0(pathDoc, file.name), ".html")
  # htmlRenderedOut <- paste0("https://zoometh.github.io/itineRis/doc/", file.name)
  tools::Rd2HTML(tools::parse_Rd(rdIn), out = htmlOut)
  cmd <- paste("pandoc -s -r html ", htmlOut, " -o ", rdIn, ".text", sep = "")
  system(cmd)
  # unlink(htmlOut)
  cat(paste0("      - HTML doc created!",
             "\n",
             "        + raw path: ", htmlOut,
             "\n\n"))
}
