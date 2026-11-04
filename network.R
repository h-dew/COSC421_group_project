#' COSC 421 Project - Network script.
#'
#' Input : dataset/clean/crashes_kel_kam.csv
#'
#' Output: dataset/clean/nodes.csv, dataset/clean/edges.csv, dataset/clean/graphs.rds
#'         figures/network_kelowna.png, figures/network_kamloops.png (+ _zoom versions)
#' 
#' What this does:
#' -Nodes  = intersections, with crash totals.
#' -Edges  = road segments linking neighbouring/connected intersections on the same street.
#' -Builds one undirected igraph network per city (Kelowna, Kamloops (may add others later)).
#' -Prints basic metrics such as size, density, degree and the top 5 by degree.
#' -Creates network maps and saves nodes, edges and graphs.
#' 

library(data.table)
library(igraph)

# Settings:
CLEAN_FILE  <- "dataset/clean/crashes_kel_kam.csv"
MAX_EDGE_KM <- 2   # edges longer than this are removed (I don't think they're real edges!)

# na.strings so empty values are read as NA and not as an intersection name
crashes <- fread(CLEAN_FILE, na.strings = c("", "NA"))

# Checking that it matches data_cleaning.R (Kelowna 20,113 / Kamloops 10,187)
crashes[!is.na(intersection), .(at_intersection = sum(n_crashes)), by = city]


# Nodes:

# Only intersections with coordinates can be placed on the network
with_coords <- crashes[!is.na(intersection) & !is.na(lat) & !is.na(lon)]

# How many intersection crashes have no coordinates?
crashes[!is.na(intersection),
        .(intersection_crashes = sum(n_crashes),
          no_coordinates       = sum(n_crashes[is.na(lat) | is.na(lon)])), by = city]

# One row per intersection name
names_table <- with_coords[, .(lat       = median(lat),
                               lon       = median(lon),
                               n_crashes = sum(n_crashes)),
                           by = .(city, intersection)]

# Some corners have 2+ names (ex. "DILWORTH DR & HARVEY AVE" and "DILWORTH DR & HWY 97 N").
# Names at the same coordinates (to ~10 m) are merged into one node.
names_table[, node_id := .GRP, by = .(city, round(lat, 4), round(lon, 4))]

# How many nodes had more than one name?
names_table[, .N, by = node_id][N > 1, .N]

# Adding the node_id to every crash
with_coords <- merge(with_coords, names_table[, .(city, intersection, node_id)],
                     by = c("city", "intersection"))

# Crash totals for each node
nodes <- with_coords[, .(n_crashes  = sum(n_crashes),
                         casualty   = sum(n_crashes[severity == "CASUALTY CRASH"]),
                         cyclist    = sum(n_crashes[cyclist == "Yes"]),
                         pedestrian = sum(n_crashes[pedestrian == "Yes"]),
                         lat        = median(lat),
                         lon        = median(lon)),
                     by = .(node_id, city)]

# Each node is labelled with its name that has the most crashes
node_labels <- names_table[order(-n_crashes), .(label = intersection[1]), by = node_id]
nodes <- merge(nodes, node_labels, by = "node_id")


# Edges:

# Every street that passes through each node
streets_at_node <- unique(rbind(with_coords[, .(node_id, city, street = street)],
                                with_coords[, .(node_id, city, street = cross_street)]))
streets_at_node <- streets_at_node[!is.na(street)]
streets_at_node <- merge(streets_at_node, nodes[, .(node_id, lat, lon)], by = "node_id")

# Distance in km between two points (online formula - "haversine")
km_between <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371 * asin(sqrt(a))
}

# For each street, its intersections are put in order along the street
# (north-south or east-west, whichever way it runs most),
# and each intersection is linked to the next one.
link_street <- function(s) {
  n <- nrow(s)
  if (n < 2) return(NULL)   # a street with 1 intersection has nothing to link

  # Does the street run more north-south or east-west?
  # AI suggested this one: longitude should be multiplied by cos(latitude) because degrees of longitude are shorter up north.
  north_south <- diff(range(s$lat)) >= diff(range(s$lon)) * cos(mean(s$lat) * pi / 180)
  if (north_south) s <- s[order(lat)] else s <- s[order(lon)]

  # Intersection 1 -> 2, 2 -> 3, 3 -> 4, etc.
  first <- 1:(n - 1)
  second <- 2:n
  data.table(city   = s$city[1],
             street = s$street[1],
             from   = s$node_id[first],
             to     = s$node_id[second],
             km     = km_between(s$lat[first], s$lon[first], s$lat[second], s$lon[second]))
}

# Running link_street on every street, one city at a time
streets_list <- split(streets_at_node, by = c("city", "street"))
edges <- rbindlist(lapply(streets_list, link_street))


# Graphs for Kelowna and Kamloops:

make_graph <- function(city_name) {
  v <- nodes[city == city_name, .(name = as.character(node_id), label, n_crashes,
                                  casualty, cyclist, pedestrian, lat, lon)]
  e <- edges[city == city_name, .(from = as.character(from), to = as.character(to),
                                  street, km)]
  g <- graph_from_data_frame(e, directed = FALSE, vertices = v)

  # Removing self-loops and duplicate pairs (keeping the shortest),
  # since the network is undirected
  g <- simplify(g, edge.attr.comb = list(km = "min", street = "first"))

  # Removing edges longer than MAX_EDGE_KM
  cat(city_name, "- edges removed for being longer than", MAX_EDGE_KM, "km:",
      sum(E(g)$km > MAX_EDGE_KM), "\n")
  delete_edges(g, E(g)[km > MAX_EDGE_KM])
}
g_kel <- make_graph("KELOWNA")
g_kam <- make_graph("KAMLOOPS")


# Nodes, edges, density, and degree:

basic_metrics <- function(g) {
  d <- degree(g)
  data.table(nodes         = vcount(g),
             edges         = ecount(g),
             density       = signif(edge_density(g), 3),
             mean_degree   = round(mean(d), 2),
             median_degree = median(d),
             max_degree    = max(d))
}
rbind(cbind(city = "KELOWNA",  basic_metrics(g_kel)),
      cbind(city = "KAMLOOPS", basic_metrics(g_kam)))

# Top 5 intersections by degree
top5_degree <- function(g) {
  top <- data.table(label = V(g)$label, n_crashes = V(g)$n_crashes, degree = degree(g))
  head(top[order(-degree)], 5)
}
top5_degree(g_kel)
top5_degree(g_kam)


# Network maps (node size = crashes):

plot_city <- function(g, title, file, zoom = FALSE) {

  # Longitude/latitude converted to km from the city centre,
  # so 1 km looks the same in both directions
  y <- (V(g)$lat - mean(V(g)$lat)) * 111
  x <- (V(g)$lon - mean(V(g)$lon)) * 111 * cos(mean(V(g)$lat) * pi / 180)

  # Whole city, or zoomed in on the 20 intersections with the most crashes
  if (zoom) {
    top20 <- order(-V(g)$n_crashes)[1:20]
    xlim  <- range(x[top20]) + c(-0.5, 0.5)   # 0.5 km of space around them
    ylim  <- range(y[top20]) + c(-0.5, 0.5)
  } else {
    xlim <- range(x)
    ylim <- range(y)
  }

  # Only the top 5 crash intersections w/ label
  top5 <- order(-V(g)$n_crashes)[1:5]

  # Bigger dot = more crashes
  dot_size <- 0.3 + 2.5 * sqrt(V(g)$n_crashes / max(V(g)$n_crashes))

  png(file, width = 1600, height = 1800, res = 200)
  par(mar = c(7, 1, 3, 1))   # extra space at the bottom for the legend and scale bar

  # Empty map. asp = 1 keeps 1 km the same length on both axes
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, axes = FALSE, xlab = "", ylab = "", main = title)

  # Edges = grey lines between the two intersections they link
  ends_idx <- ends(g, E(g), names = FALSE)
  segments(x[ends_idx[, 1]], y[ends_idx[, 1]], x[ends_idx[, 2]], y[ends_idx[, 2]],
           col = "grey60", lwd = 0.5)

  # Nodes = red dots
  points(x, y, pch = 16, col = adjustcolor("firebrick", 0.4), cex = dot_size)

  # darkslateblue outline around the top 5, so they stand out
  points(x[top5], y[top5], pch = 1, col = "darkslateblue", lwd = 2, cex = dot_size[top5])
  text(x[top5], y[top5], labels = 1:5, pos = 3, offset = 0.7, font = 2, cex = 0.9, col = "darkslateblue")

  # Legend and scale bar go below the map (xpd = NA lets them draw outside it)
  corner <- par("usr")   # edges of the map: left, right, bottom, top
  height <- corner[4] - corner[3]

  legend(x = corner[2], y = corner[3] - 0.02 * height, xjust = 1, yjust = 1,
         legend = paste0(1:5, ". ", V(g)$label[top5], " (", V(g)$n_crashes[top5], " crashes)"),
         title = "Most crashes", pch = 21, pt.bg = adjustcolor("firebrick", 0.4), col = "darkslateblue", pt.lwd = 2,
         cex = 0.7, bty = "n", xpd = NA)

  # Scale bar,to tell distances
  bar_km <- if (zoom) 0.5 else 2
  y0 <- corner[3] - 0.06 * height
  segments(corner[1], y0, corner[1] + bar_km, y0, lwd = 2, xpd = NA)
  text(corner[1] + bar_km / 2, y0, paste(bar_km, "km"), pos = 3, cex = 0.7, xpd = NA)

  invisible(dev.off())
}

dir.create("figures", showWarnings = FALSE)
plot_city(g_kel, "Kelowna intersection network (node size = crashes, 2021-2025)",
          "figures/network_kelowna.png")
plot_city(g_kel, "Kelowna - area with the most crashes",
          "figures/network_kelowna_zoom.png", zoom = TRUE)
plot_city(g_kam, "Kamloops intersection network (node size = crashes, 2021-2025)",
          "figures/network_kamloops.png")
plot_city(g_kam, "Kamloops - area with the most crashes",
          "figures/network_kamloops_zoom.png", zoom = TRUE)


# Saving networks:
# edges.csv now comes from the graphs, so it matches them exactly
edges <- rbind(cbind(city = "KELOWNA",  as_data_frame(g_kel, what = "edges")),
               cbind(city = "KAMLOOPS", as_data_frame(g_kam, what = "edges")))
fwrite(nodes, "dataset/clean/nodes.csv", na = "NA")
fwrite(edges, "dataset/clean/edges.csv", na = "NA")
saveRDS(list(kelowna = g_kel, kamloops = g_kam), "dataset/clean/graphs.rds")