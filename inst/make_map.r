library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(ggrepel)

# Get county data for a specific state, e.g., "michigan"
wc_counties <- map_data("county", c("washington", "oregon", "california")) %>%
  select(lon = long, lat, group, id = subregion, state = region) %>%
  mutate(State = case_when(
    str_to_lower(state) == "washington" ~ "WA",
    str_to_lower(state) == "california" ~ "CA",
    str_to_lower(state) == "oregon" ~ "OR",
    TRUE ~ NA_character_
  )) # rename long to lon, include state, and add State abbrev

path <- system.file("extdata", "IOPAC_Port_Groups_Counties.csv", package = "IOPAC")
if (nzchar(path) && file.exists(path)) {
  iopac_ports <- data.frame(readr::read_csv(path, skip = 0, col_names = TRUE))
} else stop("CSV not found in package extdata: ", path)

# create a duplicate column `IOPAC port group` matching `IO.PAC.port.group`
# and normalize a known inconsistent value
if ("IO.PAC.port.group" %in% names(iopac_ports)) {
  iopac_ports <- iopac_ports %>%
    mutate(
      `IOPAC port group` = `IO.PAC.port.group`,
      `IO.PAC.port.group` = case_when(
        as.character(`IO.PAC.port.group`) == "South and central WA coast" ~ "South and Central WA Coast",
        as.character(`IO.PAC.port.group`) == "North WA coast" ~ "North WA Coast",
        TRUE ~ as.character(`IO.PAC.port.group`)
      )
    )
}

# 3) create normalized join key (choose correct column name from each df)
# For wc_counties we expect `id` from your code; adjust if different.
wc_counties <- wc_counties %>%
  mutate(.join_key = str_to_lower(str_trim(as.character(id))))

# For iopac_ports, try common name choices
if ("id" %in% names(iopac_ports)) {
  iopac_ports <- iopac_ports %>%
    mutate(.join_key = str_to_lower(str_trim(as.character(id))))
} else if ("county" %in% names(iopac_ports)) {
  iopac_ports <- iopac_ports %>%
    mutate(.join_key = str_to_lower(str_trim(as.character(county))))
} else if ("County" %in% names(iopac_ports)) {
  iopac_ports <- iopac_ports %>%
    mutate(.join_key = str_to_lower(str_trim(as.character(County))))
} else stop("No suitable id/county column found in iopac_ports")

# create State abbreviation in iopac_ports if a state column exists
state_col_ip <- intersect(c("State", "state", "STATE"), names(iopac_ports))
if (length(state_col_ip) >= 1) {
  sc <- state_col_ip[1]
  v <- tolower(as.character(iopac_ports[[sc]]))
  iopac_ports$State <- ifelse(v == "washington", "WA",
                              ifelse(v == "california", "CA",
                                     ifelse(v == "oregon", "OR",
                                            ifelse(v %in% c("wa", "ca", "or"), toupper(v), NA_character_))))
} else {
  # no state info in iopac_ports — create NA so join will only match on key when possible
  iopac_ports$State <- NA_character_
}

# 4) keep only wc_counties rows that exist in iopac_ports (drop non-matches)
# keep only wc_counties rows that exist in iopac_ports (match on key + State)
wc_matched <- wc_counties %>%
  semi_join(iopac_ports %>% select(.join_key, State), by = c(".join_key", "State"))
# 5) optionally attach iopac attributes to the matched map rows
# avoid suffixing map-side columns so the `group` column is preserved
merged <- wc_matched %>%
  left_join(iopac_ports, by = c(".join_key", "State"), suffix = c("", ".iopac"))

# if `group` was suffixed for some reason, restore it to `group`
if (!"group" %in% names(merged) && "group.map" %in% names(merged)) {
  merged <- merged %>% rename(group = group.map)
}

# inspect mismatches
unmatched_map <- anti_join(wc_counties, iopac_ports, by = ".join_key")
unmatched_iopac <- anti_join(iopac_ports, wc_counties, by = ".join_key")

# ensure the group column exists; replace name if needed
stopifnot("IO.PAC.port.group" %in% names(merged))

# ensure group column
merged <- merged %>% mutate(io_group = as.character(`IO.PAC.port.group`))

# create recycled 4-color mapping
cols <- c("#1b9e77", "#d95f02", "#7570b3", "#999999")
groups <- merged %>% distinct(io_group) %>% pull(io_group)
vals <- setNames(rep(cols, length.out = length(groups)), groups)

# force Puget Sound to use the specific purple color
if ("Puget Sound" %in% names(vals)) {
  vals["Puget Sound"] <- "#d95f02"
}
if ("San Diego" %in% names(vals)) {
  vals["San Diego"] <- "#7570b3"
}
if ("Bodega Bay" %in% names(vals)) {
  vals["Bodega Bay"] <- "#999999"
}
if ("Tillamook" %in% names(vals)) {
  vals["Tillamook"] <- "#999999"
}

# centroids for labels
label_df <- merged %>%
  group_by(io_group) %>%
  summarize(mean_lon = mean(lon, na.rm = TRUE),
    mean_lat = mean(lat, na.rm = TRUE),
    .groups = "drop")

label_df$mean_lon <- label_df$mean_lon - 1.75
label_df$mean_lat <- label_df$mean_lat - 0.25

# # nudge label position for North WA coast
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "North WA Coast",
    mean_lon - 0.9, mean_lon),
  mean_lat = if_else(io_group == "North WA Coast", mean_lat + 0, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Puget Sound",
    mean_lon + 4.3, mean_lon),
  mean_lat = if_else(io_group == "Puget Sound", mean_lat + 0, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "South and Central WA Coast",
    mean_lon + 5.2, mean_lon),
  mean_lat = if_else(io_group == "South and Central WA Coast",
    mean_lat + 0.1, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Astoria",
    mean_lon + 0.4, mean_lon),
  mean_lat = if_else(io_group == "Astoria", mean_lat + 0.2, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Newport",
    mean_lon + 0.4, mean_lon),
  mean_lat = if_else(io_group == "Newport", mean_lat + 0.3, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Bodega Bay",
    mean_lon + 0, mean_lon),
  mean_lat = if_else(io_group == "Bodega Bay", mean_lat + 0.5, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Coos Bay",
    mean_lon - 0.5, mean_lon),
  mean_lat = if_else(io_group == "Coos Bay", mean_lat + 0.5, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Crescent City",
    mean_lon - 0, mean_lon),
  mean_lat = if_else(io_group == "Crescent City", mean_lat + 0.25, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Tillamook",
    mean_lon + 0.2, mean_lon),
  mean_lat = if_else(io_group == "Tillamook", mean_lat + 0.2, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Los Angeles",
    mean_lon + 0.2, mean_lon),
  mean_lat = if_else(io_group == "Los Angeles", mean_lat - 0.1, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Brookings",
    mean_lon + 0.2, mean_lon),
  mean_lat = if_else(io_group == "Brookings", mean_lat + 0.1, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "San Francisco",
    mean_lon - 0.2, mean_lon),
  mean_lat = if_else(io_group == "San Francisco", mean_lat + 0, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Morro Bay",
    mean_lon - 0, mean_lon),
  mean_lat = if_else(io_group == "Morro Bay", mean_lat + 0.1, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Santa Barbara",
    mean_lon - 0, mean_lon),
  mean_lat = if_else(io_group == "Santa Barbara", mean_lat - 0.1, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Fort Bragg",
    mean_lon - 0, mean_lon),
  mean_lat = if_else(io_group == "Fort Bragg", mean_lat + 0.2, mean_lat))
label_df <- label_df %>%
  mutate(mean_lon = if_else(io_group == "Eureka",
    mean_lon + 0.2, mean_lon),
  mean_lat = if_else(io_group == "Eureka", mean_lat + 0, mean_lat))
  
# compute x limits to extend plot slightly to the left
map_lon_range <- range(c(unmatched_map$lon, merged$lon), na.rm = TRUE)
map_xlim <- c(map_lon_range[1] - 2.7, map_lon_range[2])

# base plot: draw unmatched map counties as unfilled outlines behind the colored map
p <- ggplot() +
  geom_polygon(data = unmatched_map,
               aes(x = lon, y = lat, group = group),
               fill = NA, color = "grey80", size = 0.2) +
  geom_polygon(data = merged,
               aes(x = lon, y = lat, group = group, fill = io_group),
               color = "black", size = 0.2) +
  scale_fill_manual(values = vals, name = "IOPAC port group") +
  coord_map(xlim = map_xlim) +
  theme_void()

# add labels: prefer ggrepel if installed
p <- p + geom_text(
  data = label_df,
  mapping = aes(x = mean_lon, y = mean_lat, label = io_group),
  size = 3, color = "black",
  inherit.aes = FALSE
)

out_file <- here::here("inst", "IOPAC_map.png")

ggplot2::ggsave(filename = out_file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
message("Saved map: ", out_file)
