#' COSC 421 Project - Data cleaning script.
#'
#' Input : dataset/SouthernInterior_FullData_data.csv
#'
#' Output: dataset/clean/crashes_kel_kam.csv

library(data.table)

# Settings:
RAW_FILE   <- "dataset/SouthernInterior_FullData_data.csv"
CLEAN_FILE <- "dataset/clean/crashes_kel_kam.csv"
CITIES     <- c("KELOWNA", "KAMLOOPS")
YEARS      <- 2021:2025
EXPECTED_TOTALS <- c(KELOWNA = 47108, KAMLOOPS = 29552)  # from the Tableau dashboard

raw <- fread(RAW_FILE)
dim(raw)

# Columns we'll be needing:
keep <- c(
  "Municipality Name"           = "city",
  "Date Of Loss Year"           = "year",
  "Month Of Year"               = "month",
  "Day Of Week"                 = "day",
  "Time Category"               = "time",
  "Crash Severity"              = "severity",
  "Derived Crash Configuration" = "configuration",
  "Intersection Crash"          = "intersection_crash",
  "Mid Block Crash"             = "mid_block",
  "Cyclist Flag"                = "cyclist",
  "Pedestrian Flag"             = "pedestrian",
  "Motorcycle Flag"             = "motorcycle",
  "Heavy Veh Flag"              = "heavy_vehicle",
  "Street Full Name"            = "street",
  "Cross Street Full Name"      = "cross_street",
  "Road Location Description"   = "location",
  "Latitude"                    = "lat",
  "Longitude"                   = "lon",
  "Total Crashes"               = "n_crashes",
  "Total Victims"               = "n_victims"
)
crashes <- raw[, names(keep), with = FALSE]
setnames(crashes, names(keep), keep)

crashes[, city := toupper(trimws(city))]
crashes <- crashes[city %in% CITIES & year %in% YEARS]

# Checking that they match the Tableau dashboard
city_totals <- crashes[, .(rows = .N, total_crashes = sum(n_crashes)), by = city]
city_totals
stopifnot(all(city_totals[, total_crashes == EXPECTED_TOTALS[city]]))  # stops if they don't match

# Fixing street names:
clean_street_name <- function(x) {
  x <- toupper(trimws(gsub("\\s+", " ", x)))
  x[x %in% c("", "UNKNOWN", "NA")] <- NA
  x
}
crashes[, `:=`(street       = clean_street_name(street),
               cross_street = clean_street_name(cross_street),
               location     = clean_street_name(location))]

# Street names are sorted so "HARVEY AVE & GORDON DR" and "GORDON DR & HARVEY AVE"
# become the same intersection.
# Rows with no cross street get NA (since only intersections will be nodes).
has_two_streets <- crashes[, !is.na(street) & !is.na(cross_street) & street != cross_street]
crashes[, intersection := fifelse(
  has_two_streets,
  paste(pmin(street, cross_street), "&", pmax(street, cross_street)),
  NA_character_)]

# How many crashes are or aren't at an intersection?
crashes[, .(total_crashes       = sum(n_crashes),
            at_intersection     = sum(n_crashes[!is.na(intersection)]),
            not_at_intersection = sum(n_crashes[is.na(intersection)])), by = city]

crashes[, .(n_crashes = sum(n_crashes)), by = severity][order(-n_crashes)]

# Top 10 intersections per city
top10 <- crashes[!is.na(intersection),
                 .(n_crashes = sum(n_crashes)), by = .(city, intersection)]
top10[order(city, -n_crashes)][, head(.SD, 10), by = city]

# Cleaned data:
dir.create(dirname(CLEAN_FILE), recursive = TRUE, showWarnings = FALSE)
fwrite(crashes, CLEAN_FILE, na = "NA")  # na = "NA" so missing values don't save as blanks