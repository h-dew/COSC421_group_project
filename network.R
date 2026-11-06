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
crashes[!is.na(intersection), list(at_intersection = sum(n_crashes)), by = city]

# Nodes:

# Only intersections with coordinates can be placed on the network
with_coords <- crashes[!is.na(intersection) & !is.na(lat) & !is.na(lon)]

# How many intersection crashes have no coordinates?
crashes[!is.na(intersection),
        list(intersection_crashes = sum(n_crashes),
             no_coordinates       = sum(n_crashes[is.na(lat) | is.na(lon)])),
        by = city]

# One row per intersection name
# (median position is used in case the same name was recorded at slightly different spots)
names_table <- with_coords[, list(lat       = median(lat),
                                  lon       = median(lon),
                                  n_crashes = sum(n_crashes)),
                           by = list(city, intersection)]

# Some corners have 2+ names (ex. "DILWORTH DR & HARVEY AVE" and "DILWORTH DR & HWY 97 N").
# Names at the same coordinates (to approx. 10 m) are merged into one node.
# How this works: rows are grouped by city and rounded lat/lon, and each group is assigned 
# a node_id with .GRP. Names in the same group are given the same id.
names_table[, node_id := .GRP, by = list(city, round(lat, 4), round(lon, 4))]

# How many nodes had more than one name? (count names per node_id, keep nodes with more than 1)
names_per_node <- names_table[, list(n_names = .N), by = node_id]
names_per_node[n_names > 1, .N]

# Adding the node_id to every crash
# This also matches each crash to its row in names_table (uses city + intersection)
with_coords <- merge(with_coords, names_table[, list(city, intersection, node_id)],
                     by = c("city", "intersection"))
# add months to with coords table 
with_coords[, month_num := match(trimws(toupper(month)), toupper(month.name))]

month_cols <- sprintf("m%02d", 1:12)
nodes <- with_coords[, list(n_crashes  = sum(n_crashes),
                            casualty   = sum(n_crashes[severity == "CASUALTY CRASH"]),
                            cyclist    = sum(n_crashes[cyclist == "Yes"]),
                            pedestrian = sum(n_crashes[pedestrian == "Yes"]),
                            lat        = median(lat),
                            lon        = median(lon)),
                     by = list(node_id, city)]
#create counts of crashes by month, adding in combined crashes
month_counts <- with_coords[, lapply(1:12, function(k) sum(n_crashes[month_num == k])), by = node_id]
setnames(month_counts, c("node_id", month_cols))
# Sort by crashes, high to low, then take the first name in each node
node_labels <- names_table[order(-n_crashes), list(label = intersection[1]), by = node_id]
nodes <- merge(nodes, node_labels, by = "node_id")
nodes <- merge(nodes, month_counts, by = "node_id") 

# Every street that passes through each node
# A crash row lists a street and a cross_street. Stacking both columns (rbind)
# => one list of streets per node. No repeats bc of unique()
streets_at_node <- unique(rbind(with_coords[, list(node_id, city, street = street)],
                                with_coords[, list(node_id, city, street = cross_street)]))
streets_at_node <- streets_at_node[!is.na(street)]
streets_at_node <- merge(streets_at_node, nodes[, list(node_id, lat, lon)], by = "node_id")

# Distance in km between two points (online formula - "haversine")
# The Haversine formula provides straight-line distance over the Earth's curved surface.
# 6371 is the Earth's radius in km!
km_between <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371 * asin(sqrt(a))
}

# For each street, its intersections are put in order along the street
# (north-south or east-west, whichever way it runs most),
# and each intersection is linked to the next one.
# It takes in the rows of streets_at_node (one street in one city) and
# outputs a table of edges (from, to and km) for that street
link_street <- function(street_rows) {
  n_intersections <- nrow(street_rows)
  if (n_intersections < 2) return(NULL)   # a street with 1 intersection has nothing to link

  # Compares how far the street spreads north-south vs east-west.
  # AI suggested this one: longitude should be multiplied by cos(latitude)
  # because degrees of longitude are shorter up north.
  ns_spread <- diff(range(street_rows$lat))
  ew_spread <- diff(range(street_rows$lon)) * cos(mean(street_rows$lat) * pi / 180)
  if (ns_spread >= ew_spread) {
    street_rows <- street_rows[order(lat)]
  } else {
    street_rows <- street_rows[order(lon)]
  }

  # Intersection 1 -> 2, 2 -> 3, 3 -> 4, etc.
  first  <- street_rows[1:(n_intersections - 1)]
  second <- street_rows[2:n_intersections]
  data.table(city   = street_rows$city[1],
             street = street_rows$street[1],
             from   = first$node_id,
             to     = second$node_id,
             km     = km_between(first$lat, first$lon, second$lat, second$lon))
}

# Running link_street on every street, one city at a time
streets_list <- split(streets_at_node, by = c("city", "street"))
edges <- rbindlist(lapply(streets_list, link_street))

# Graphs for Kelowna and Kamloops:

make_graph <- function(city_name) {
  # filter to city
  v <- nodes[city == city_name]
  v[, name := as.character(node_id)]
  #should maybe be changed to include attributes as a default, instead of ahving to list them here?
  v <- v[, c("name", "label", "n_crashes", "casualty", "cyclist",
             "pedestrian", "lat", "lon", month_cols), with = FALSE]
  e <- edges[city == city_name, list(from = as.character(from), to = as.character(to),
                                  street, km)]
  graph <- graph_from_data_frame(e, directed = FALSE, vertices = v)

  # Removing self-loops and duplicate pairs (keeping the shortest),
  # since the network is undirected
  # It will keep the shortest distance ("min") and one street name ("first").
  graph <- simplify(graph, edge.attr.comb = list(km = "min", street = "first"))

  # Removing edges longer than MAX_EDGE_KM
  too_long <- E(graph)$km > MAX_EDGE_KM
  cat(city_name, "- edges removed for being longer than", MAX_EDGE_KM, "km:", sum(too_long), "\n")
  delete_edges(graph, E(graph)[too_long])
}
g_kel <- make_graph("KELOWNA")
g_kam <- make_graph("KAMLOOPS")


# Nodes, edges, density, and degree:

basic_metrics <- function(graph) {
  node_degrees <- degree(graph)
  data.table(nodes         = vcount(graph),
             edges         = ecount(graph),
             density       = signif(edge_density(graph), 3),
             mean_degree   = round(mean(node_degrees), 2),
             median_degree = median(node_degrees),
             max_degree    = max(node_degrees))
}
rbind(cbind(city = "KELOWNA",  basic_metrics(g_kel)),
      cbind(city = "KAMLOOPS", basic_metrics(g_kam)))

# Top 5 intersections by degree
top5_degree <- function(graph) {
  top <- data.table(label     = V(graph)$label,
                    n_crashes = V(graph)$n_crashes,
                    degree    = degree(graph))
  head(top[order(-degree)], 5)
}
top5_degree(g_kel)
top5_degree(g_kam)

month_analysis <- function(g) {
  m <- as.data.table(vertex_attr(g))[, ..month_cols]
  list(
    total_by_month = colSums(m),                       # city-wide crashes per month
    peak_month     = table(month_cols[max.col(m, ties.method = "first")])  # peak month per node
  )
}

month_analysis(g_kel)
month_analysis(g_kam)

plot_city <- function(graph, title, file, zoom = FALSE) {

  lat       <- V(graph)$lat
  lon       <- V(graph)$lon
  n_crashes <- V(graph)$n_crashes

  # Longitude/latitude converted to km from the city centre,
  # so 1 km looks the same in both directions
  y <- (lat - mean(lat)) * 111
  x <- (lon - mean(lon)) * 111 * cos(mean(lat) * pi / 180)

  # Whole city, or zoomed in on the 20 intersections with the most crashes
  if (zoom) {
    top20 <- order(-n_crashes)[1:20]
    xlim  <- range(x[top20]) + c(-0.5, 0.5)   # 0.5 km of space around them
    ylim  <- range(y[top20]) + c(-0.5, 0.5)
  } else {
    xlim <- range(x)
    ylim <- range(y)
  }

  # Only the top 5 crash intersections w/ label
  top5 <- order(-n_crashes)[1:5]

  # Bigger dot = more crashes
  # Sqrt so area, not width grows in size
  dot_size <- 0.3 + 2.5 * sqrt(n_crashes / max(n_crashes))

  png(file, width = 1600, height = 1800, res = 200)
  par(mar = c(7, 1, 3, 1))   # extra space at the bottom for the legend and scale bar

  # Empty map. asp = 1 keeps 1 km the same length on both axes
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, axes = FALSE, xlab = "", ylab = "", main = title)

  # Edges = grey lines between the two intersections they link
  edge_ends <- ends(graph, E(graph), names = FALSE)
  from_node <- edge_ends[, 1]
  to_node   <- edge_ends[, 2]
  segments(x[from_node], y[from_node], x[to_node], y[to_node],
           col = "grey60", lwd = 0.5)

  # Nodes = red dots
  points(x, y, pch = 16, col = adjustcolor("firebrick", 0.4), cex = dot_size)

  # darkslateblue outline around the top 5, so they stand out
  points(x[top5], y[top5], pch = 1, col = "darkslateblue", lwd = 2, cex = dot_size[top5])
  text(x[top5], y[top5], labels = 1:5, pos = 3, offset = 0.7, font = 2, cex = 0.9,
       col = "darkslateblue")

  # Legend and scale bar go below the map (xpd = NA lets them draw outside it)
  corner <- par("usr")   # edges of the map: left, right, bottom, top
  height <- corner[4] - corner[3]

  legend_text <- paste0(1:5, ". ", V(graph)$label[top5], " (", n_crashes[top5], " crashes)")
  legend(x = corner[2], y = corner[3] - 0.02 * height, xjust = 1, yjust = 1,
         legend = legend_text, title = "Most crashes",
         pch = 21, pt.bg = adjustcolor("firebrick", 0.4), col = "darkslateblue", pt.lwd = 2,
         cex = 0.7, bty = "n", xpd = NA) # had to set xpd = NA to draw outside the plot

  # Scale bar,to tell distances
  bar_km <- if (zoom) 0.5 else 2
  y0 <- corner[3] - 0.06 * height
  segments(corner[1], y0, corner[1] + bar_km, y0, lwd = 2, xpd = NA)
  text(corner[1] + bar_km / 2, y0, paste(bar_km, "km"), pos = 3, cex = 0.7, xpd = NA)

  # Close and save the PNG (invisible() just hides the "null device" message)
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
# This one is for RStudio (so we don't need to rebuild from scratch with the csv files every time)
saveRDS(list(kelowna = g_kel, kamloops = g_kam), "dataset/clean/graphs.rds")
