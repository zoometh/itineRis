# for ...
source("R/read_objects_typo.R")
data <- read_objects_typo(ggdocument.url = "DATA_01_IT_VC_2.xlsx",
                          ggdocument.sheet = "Sites_NMI_MET",
                          authentificate = FALSE)

# for graphs
source("R/read_objects_typo.R")
data <- read_objects_typo(ggdocument.url = "DATA_01_IT_VC_2.xlsx",
                          ggdocument.sheet = "Ornament_IT_Archiv",
                          authentificate = FALSE)
selected.col.chrono <- c(#"GIIA.(600-550)",
  "GIIAB.(550-525)", "GIIAB_IIB.(550-490)", "GIIB.(525-490)",
  "GIIB_IIIA1.(525-450)",
  "GIIIA1.(490-450)", "GIIIA1_GIIIA2.(490-420)", "GIIIA2.(450-420)")
selected.col.groups <- c("gr1", "gr1", "gr1", "gr2", "gr3", "gr3", "gr3")
df.chrono <- data.frame(periods = selected.col.chrono,
                        groups = selected.col.groups,
                        stringsAsFactors = F)
data[,selected.col.chrono][is.na(data[,selected.col.chrono])] <- 0
ldegrees <- ldensities <- ldiameter <- lG <- list()
# subset obj by period
for(i in 1:length(unique(df.chrono$groups))){
  # i <- 3
  a.group <- unique(df.chrono$groups)[i]
  periods <- df.chrono[df.chrono[, "groups"] == a.group, "periods"]
  data.per <- data[, c(mandatory.fields, periods)]
  data.per.sum <- data.per %>%
    rowwise() %>%
    mutate(per = sum(c_across(any_of(periods))))
  # rename per
  names(data.per.sum)[names(data.per.sum) == 'per'] <- a.group
  data.per.sum <- data.per.sum[, c(mandatory.fields, a.group)]
  data.per <- data.per.sum[data.per.sum[ , a.group] > 0, ]
  # drop row with NA in mandatory columns
  a.graph <- paste0(as.character(i), "_", a.group)
  print(paste0("*read: ", a.graph))
  ext.title <- paste0(periods, collapse = " & ")
  aG <- f.graph(data.per, a.graph, ext.title)
  lG[[length(lG)+1]] <- aG
}
