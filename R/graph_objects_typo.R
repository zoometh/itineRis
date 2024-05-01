#' Create a Create graph and community detection of objects typo
#'
#' @name graph_objects_typo
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
graph_objects_typo <- function(data.per = NA,
                               a.graph = NA,
                               ext.title = NA,
                               LD_OB_TY_ST = TRUE,
                               only_Fibule = FALSE,
                               only_Others = FALSE, # selection on objects
                               algo.community = "cluster_edge_betweenness",
                               out.path.imgs = "C:/Rprojects/itineRis/talks/2024-spf/out/",
                               verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  # f.graph <- function(data.per, a.graph, ext.title){
  #######################################
  ## Create graph and community detection
  ########################################
  # a.graph <- "3_gr3"
  ### edges
  ## align Lieu-dit | Objet | type | style
  lg <- list()

  LD_OB_ST <- !LD_OB_TY_ST # selection on n-mode


  xt <- "allObj_"
  if(only_Fibule){
    xt <- "onlyFib_"
  }
  if(only_Others){
    xt <- "notFib_"
  }
  data.per <- as.data.frame(data.per)
  eds.sites_objets <- data.frame(a = data.per[,c("Lieu-dit")],
                                 b = data.per[,c("Objet")]
  ) # Lieu-dit | Objet
  eds.objets_types <- data.frame(a = data.per[,c("Objet")],
                                 b = data.per[,c("type")]
  )# Objet | type
  eds.objets_styles <- data.frame(a = data.per[,c("Objet")],
                                  b = data.per[,c("style")]
  )# Objet | style
  eds.types_styles <- data.frame(a = data.per[,c("type")],
                                 b = data.per[,c("style")]
  )# type | style
  # merge
  if(LD_OB_TY_ST){
    eds.per.graph <- rbind(eds.sites_objets, eds.objets_types, eds.types_styles)
    xt <- paste0(xt, "LD_OB_TY_ST")
  }
  if(LD_OB_ST){
    eds.per.graph <- rbind(eds.sites_objets, eds.objets_styles)
    xt <- paste0(xt, "LD_OB_ST")
  }
  # if(only_Fibule | only_Others){
  #   eds.per.graph <- eds.per.graph[eds.per.graph$a %notin% objets.to.suppress & eds.per.graph$b %notin% objets.to.suppress, ]
  # }
  eds.per.graph <- eds.per.graph %>%
    tidyr::drop_na("a","b")
  # count same edges (column 'n')
  eds.per.graph <- eds.per.graph %>%
    dplyr::count(a, b)
  ### nodes
  # class
  nds.sites <- data.frame(name = unique(data.per[,c("Lieu-dit")]),
                          shape = rep("circle", length(unique(data.per[,c("Lieu-dit")]))),
                          color = rep("grey", length(unique(data.per[,c("Lieu-dit")]))),
                          font = rep(2, length(unique(data.per[,c("Lieu-dit")])))
  )
  nds.objets <- data.frame(name = unique(data.per[,c("Objet")]),
                           shape = rep("square", length(unique(data.per[,c("Objet")]))),
                           color = rep("red", length(unique(data.per[,c("Objet")]))),
                           font = rep(1, length(unique(data.per[,c("Objet")])))
  )
  nds.types <- data.frame(name = unique(data.per[,c("type")]),
                          shape = rep("triangle", length(unique(data.per[,c("type")]))),
                          color = rep("orange", length(unique(data.per[,c("type")]))),
                          font = rep(1, length(unique(data.per[,c("type")])))
  )
  nds.styles <- data.frame(name = unique(data.per[,c("style")]),
                           shape = rep("star", length(unique(data.per[,c("style")]))),
                           color = rep("yellow", length(unique(data.per[,c("style")]))),
                           font = rep(3, length(unique(data.per[,c("style")])))
  )
  # merge
  if(LD_OB_TY_ST){
    nds.per.graph <- rbind(nds.sites, nds.objets, nds.types, nds.styles)
  }
  if(LD_OB_ST){
    nds.per.graph <- rbind(nds.sites, nds.objets, nds.styles)
  }
  # if(only_Fibule | only_Others){
  #   nds.per.graph <- nds.per.graph[nds.per.graph$name %notin% objets.to.suppress, ]
  # }
  nds.per.graph <- nds.per.graph %>%
    tidyr::drop_na("name")
  # TODO: find duplicated
  per.title <- paste0(a.graph, " ", ext.title)

  ### graph
  g <- igraph::graph_from_data_frame(eds.per.graph, vertices= nds.per.graph, directed=T)
  # rm isolated nodes
  Isolated <- which(igraph::degree(g)==0)
  g <- igraph::delete.vertices(g, Isolated)
  # plot(g)
  if(verbose){
    print("     - graph created")
  }
  ## Global index
  # degree distribution
  df.ldegrees <- data.frame(n = names(igraph::degree(g)),
                            degrees = as.numeric(igraph::degree(g)))
  g.ldegrees <- ggplot2::ggplot(df.ldegrees, ggplot2::aes(x = degrees))+
    ggplot2::ggtitle(a.graph) +
    ggplot2::geom_histogram(colour = "black", fill = "grey", binwidth = 1) +
    # scale_x_continuous(breaks = seq(1, max(df.ldegrees$x), by = 1)) +
    ggplot2::theme_bw()
  lg[['g.ldegrees']] <- g.ldegrees
  lg[['ldensities']] <- igraph::edge_density(g)
  lg[['diameter']] <- igraph::diameter(g)
  # ldegrees[[length(ldegrees)+1]] <- g.ldegrees
  # print(length(ldegrees))
  # density
  # ldensities[[length(ldensities)+1]] <- graph.density(g)
  # # diameter
  # ldiameter[[length(ldiameter)+1]] <- diameter(g)
  ##################
  set.seed(124)
  layout.fr <- igraph::layout_with_fr(g)
  ################################################################
  ## raw graph
  name.out <- paste0(out.path.imgs, a.graph,"_graph_", xt,".png")
  png(name.out,
      width = 25,
      height = 14,
      units = "cm",
      res = 300)
  par(mar=c(0, 0, 2, 0))
  g.out <- plot(g,
                layout = layout.fr,
                labels = igraph::V(g)$name,
                vertex.label.color = "black",
                vertex.label.family = "sans",
                vertex.label.font = igraph::V(g)$font,
                vertex.label.cex = .5,
                vertex.shape = igraph::V(g)$shape,
                vertex.color = igraph::V(g)$color,
                vertex.size = 10,
                # vertex.size = degree(g)+5,
                vertex.frame.color = NA,
                # edge.label = E(g)$n,
                # edge.label.cex = (E(g)$n)/3,
                # edge.label.color = "black",
                # edge.label.family = "sans",
                edge.arrow.size = .5,
                edge.color = "darkgrey",
                edge.width = igraph::E(g)$n
  )
  title(main = paste0(per.title, "\n", algo.community),
        font.main = 1.5)
  print(g.out)
  dev.off()
  print(paste("     - graph output exported", algo.community))
  # ## Other way ##
  # # library(ggraph)
  # gtypo <- ggraph::ggraph(g, layout = "fr") +
  #   ggraph::geom_edge_link(aes(width = n, color = "darkgrey"),
  #                          arrow = arrow(length = unit(0.5, "cm")),
  #                          end_cap = circle(3, 'mm')) +
  #   ggraph::geom_node_point(aes(color = color, size = 10, shape = shape),
  #                           show.legend = FALSE) +
  #   ggraph::geom_node_text(aes(label = name, color = "black"), # color
  #                          repel = TRUE,
  #                          fontface = "plain",
  #                          family = "sans",
  #                          size = 3.5) +
  #   ggraph::scale_edge_width(range = c(0.5, 2),
  #                            breaks = c(1, 2, 5, 10, 15)) +
  #   ggplot2::scale_shape_manual(values = c("circle" = 16,
  #                                          "square" = 15,
  #                                          "triangle" = 17,
  #                                          "star" = 8)) +
  #   ggplot2::labs(title = a.graph, caption = ext.title) +
  #   ggplot2::guides(color = "none",
  #                   edge_color = "none") +  # Remove edge color from legend
  #   ggplot2::scale_color_identity() +
  #   ggraph::theme_graph()
  # lg[['gtypo']] <- gtypo

  ## community detection
  if(verbose){
    print(paste0("community detection"))
  }
  # for (algo.community in algos.community){
  # algo.community <- c("edge.betweenness.community")
  g <- igraph::as.undirected(g) # convert to undir
  op <- paste0("igraph::", algo.community,"(g)")
  if(verbose){
    print(paste0("Evaluate: '", op, "'"))
  }
  g_community <- eval(parse(text=op))
  n.membership <- length(unique(g_community$membership))
  # g_community <- fastgreedy.community(g)
  name.out <- paste0(out.path.imgs, a.graph, "_zCD_", algo.community, "_graph_", xt,".png")
  png(name.out,
      width = 25,
      height = 14,
      units = "cm",
      res = 300)
  par(mar=c(0, 0, 2, 0))
  # rcolors <- c("red", "blue", "darkpink", "orange", "darkgreen", "purple", "brown")
  nodes.colors <- rainbow(n.membership)[igraph::membership(g_community)]
  comm.colors <- rainbow(n.membership, alpha=.3)

  # ## Other way ##
  # ggraph::ggraph(g, layout = "fr") +
  #   ggraph::geom_edge_link(ggplot2::aes(color = "darkgrey"), width = 1, alpha = 0.8) +
  #   ggraph::geom_node_point(ggplot2::aes(color = color, shape = shape, size = 10)) +
  #   ggraph::geom_node_text(ggplot2::aes(label = name, fontface = font), color = "black", size = 5) +
  #   ggplot2::scale_color_manual(values = igraph::V(g)$color) +
  #   ggplot2::ggtitle(paste0("Your Title", "\n", "Sub Title")) +
  #   ggplot2::theme_void()  # Clean theme

  g.out <- plot(g_community,
                layout = layout.fr,
                g,
                vertex.label.color = "black",
                vertex.label.family = "sans",
                vertex.label.font = igraph::V(g)$font,
                vertex.label.cex = .5,
                vertex.shape = igraph::V(g)$shape,
                vertex.color = igraph::V(g)$color,
                vertex.size = 10,
                # vertex.size = degree(g)+5,
                vertex.frame.color = NA,
                edge.color = "darkgrey",
                # edge.width = E(g)$n,
                col = nodes.colors,
                mark.col = comm.colors,
                mark.border = NA
  )
  title(main = paste0(per.title, "\n", algo.community),
        font.main = 1.5)
  print(g.out)
  dev.off()
  print(paste("     - graph output exported", algo.community))

  # # spatial
  # # get index of sites (= first position)
  # sites.idx <- match(nds.sites$name, igraph::V(g)$name)
  # # use index to get communities
  # sites.communities <- g_community$membership[sites.idx]
  # nodes.colors <- rainbow(n.membership)[igraph::membership(g_community)]
  # sites.colors <- nodes.colors[sites.idx] # sites colors
  # sites.membership <- data.frame(name = nds.sites$name,
  #                                community = sites.communities,
  #                                color = sites.colors)
  # sites.coordinates <- as.data.frame(unique(data[data$`Lieu-dit` %in% nds.sites$name,
  #                                                c("Lieu-dit", "x", "y")]))
  # names(sites.coordinates) <- c("name", "x", "y")
  # sites.coordinates$x <- as.numeric(sites.coordinates$x)
  # sites.coordinates$y <- as.numeric(sites.coordinates$y)
  # # merge
  # sites.xy.community <- merge(sites.coordinates, sites.membership, by = "name")
  # sp.local <- bck.stamen.sp %>%
  #   ggmap::ggmap() +
  #   ggtitle(per.title, subtitle = algo.community) +
  #   geom_point(data=sites.xy.community,
  #              aes(x = x, y = y, color = color),
  #              size = 2) +
  #   geom_text_repel(data=sites.xy.community,
  #                   aes(x = x, y = y, color = color, label = name), cex = 3.5) +
  #   scale_color_identity()
  # name.out <- paste0(out.path.imgs, a.graph, "_zCD_", algo.community, "_spat_", xt,".png")
  # ggsave(name.out, sp.local)
  # print(paste("     - map community output exported", algo.community))
  #
  #
  # lG <- list("ldegrees" = ldegrees,
  #            "ldensities" = ldensities,
  #            "ldiameter" = ldiameter)
  return(lG)
}
