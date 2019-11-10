# header ----
library(data.table)
library(lubridate)
library(stringi)
library(ggplot2)
library(osmdata)
library(ggsci)
# library(nominatim)
library(ggmap)
library(gganimate)
library(magick)
library(sf)

# read data ----
# you can download the data at https://opendata.bratislava.sk
data_violation_18 <- fread("..//..//opendata/priestupky_2018.csv", encoding = "Latin-1")
data_violation_19 <- fread("..//..//opendata/priestupky_2019.csv", encoding = "Latin-1")
data_violation_17 <- fread("..//..//opendata/priestupky_2017.csv", encoding = "Latin-1")

# colnames to English
setnames(data_violation_17, colnames(data_violation_17), c("Date_Time", "Group_vio", "Type_vio", "Street", "Place"))
setnames(data_violation_18, colnames(data_violation_18), c("Date_Time", "Group_vio", "Type_vio", "Street", "Place"))
setnames(data_violation_19, colnames(data_violation_19), c("Date_Time", "Group_vio", "Type_vio", "Street", "Place"))

# bind all the data
data_violation <- rbindlist(list(data_violation_17, data_violation_18, data_violation_19), use.names = T)

# what we got
str(data_violation)
data_violation

# Create time columns for aggregation
data_violation[, Date_Time := dmy_hm(Date_Time)]
data_violation[, Date := date(Date_Time)]
# data_violation[, Yweek := week(Date_Time)]
# data_violation[, Month_name := month(Date_Time, label = T)]
data_violation[, Month := month(Date_Time)]
data_violation[, Year := year(Date_Time)]
data_violation[, Year_M := Year + Month/100]

str(data_violation)
data_violation[, unique(Place)]
data_violation[, .N, by = .(Place)]

# Handle slavic (slovak) symbols
data_violation[, stri_trans_general(unique(Place), "Latin-ASCII")]
data_violation[, Place := stri_trans_general(Place, "Latin-ASCII")]
data_violation[, unique(Place)]
data_violation[.("Eunovo"), on = .(Place), Place := "Cunovo"]
data_violation[.("Raea"), on = .(Place), Place := "Raca"]
data_violation[.("Vrakuoa"), on = .(Place), Place := "Vrakuna"]
data_violation[.("Lamae"), on = .(Place), Place := "Lamac"]
data_violation[, unique(Place)]

data_violation[, unique(Group_vio)]
data_violation[, .N, by = .(Group_vio)]

# Handle slavic (slovak) symbols
data_violation[, Group_vio := gsub(pattern = "È", replacement = "C", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "ò", replacement = "n", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "è", replacement = "c", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "\u009d", replacement = "t", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "¾", replacement = "l", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "¼", replacement = "L", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "ò", replacement = "n", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "_", replacement = "", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "ï", replacement = "d", x = Group_vio)]
data_violation[, Group_vio := gsub(pattern = "à", replacement = "r", x = Group_vio)]

data_violation[, unique(Group_vio)]
data_violation[, .N, by = .(Group_vio)]

# Exctract code of violations
data_violation[, Group_vio_ID := as.numeric(substr(Group_vio, 1, 4))]
# 4836, 5000, 4834, 4700, 4838, 4828, 4839, 4813, 9803, 4840, 4841, 9809, 3000, 9806, 4900

data_violation[, unique(Type_vio)]
data_violation[, .N, by = .(Group_vio)]

data_violation[, unique(Street)]
data_violation[, .N, by = .(Street)]

# Get polygons of city districts
poly_sm <- getbb(place_name = "Stare Mesto Bratislava", format_out = "polygon")
poly_nm <- getbb(place_name = "Nove Mesto Bratislava", format_out = "polygon")
poly_ruz <- getbb(place_name = "Ruzinov Bratislava", format_out = "polygon")
poly_raca <- getbb(place_name = "Raca Bratislava", format_out = "polygon")
poly_petr <- getbb(place_name = "Petrzalka Bratislava", format_out = "polygon")
poly_kv <- getbb(place_name = "Karlova Ves Bratislava", format_out = "polygon")
poly_dubr <- getbb(place_name = "Dubravka Bratislava", format_out = "polygon")
poly_vajn <- getbb(place_name = "Vajnory Bratislava", format_out = "polygon")
poly_lamac <- getbb(place_name = "Lamac Bratislava", format_out = "polygon")
poly_vrak <- getbb(place_name = "Vrakuna Bratislava", format_out = "polygon")
poly_pd <- getbb(place_name = "Podunajske Biskupice Bratislava", format_out = "polygon")
poly_jar <- getbb(place_name = "Jarovce Bratislava", format_out = "polygon")
poly_dnv <- getbb(place_name = "Devinska Nova Ves Bratislava", format_out = "polygon")
poly_rus <- getbb(place_name = "Rusovce Bratislava", format_out = "polygon")
poly_zb <- getbb(place_name = "Zahorska Bystrica Bratislava", format_out = "polygon")
poly_devin <- getbb(place_name = "Devin Bratislava", format_out = "polygon")
poly_cun <- getbb(place_name = "Cunovo Bratislava", format_out = "polygon")

# bind all the districts
data_poly_ba <- rbindlist(list(
  as.data.table(poly_sm)[, Place := "Stare Mesto"],
  as.data.table(poly_nm[[1]])[, Place := "Nove Mesto"],
  as.data.table(poly_ruz[[1]])[, Place := "Ruzinov"],
  as.data.table(poly_raca)[, Place := "Raca"],
  as.data.table(poly_petr)[, Place := "Petrzalka"],
  as.data.table(poly_kv[[1]])[, Place := "Karlova Ves"],
  as.data.table(poly_dubr[[1]])[, Place := "Dubravka"],
  as.data.table(poly_vajn[[1]])[, Place := "Vajnory"],
  as.data.table(poly_lamac[[1]])[, Place := "Lamac"],
  as.data.table(poly_vrak[[1]])[, Place := "Vrakuna"],
  as.data.table(poly_pd[[1]])[, Place := "Pod. Biskupice"],
  as.data.table(poly_jar[[1]])[, Place := "Jarovce"],
  as.data.table(poly_dnv[[1]])[, Place := "Devinska Nova Ves"],
  as.data.table(poly_rus[[1]])[, Place := "Rusovce"],
  as.data.table(poly_zb[[1]])[, Place := "Zah. Bystrica"],
  as.data.table(poly_devin[[1]])[, Place := "Devin"],
  as.data.table(poly_cun[[1]])[, Place := "Cunovo"]
))

setnames(data_poly_ba, c("V1", "V2"), c("lon", "lat"))
data_poly_ba[, Place := factor(Place)]

# Visualise simply the districts
ggplot(data_poly_ba) +
  geom_polygon(aes(x = lon, y = lat,
                   fill = Place, color = Place)) +
  
  theme_void()

# aggregate violations by districts (Place) and Year_M
data_violation_agg_place <- copy(data_violation[.(c(4836, 5000, 4834, 4700, 4838, 4813, 9803, 4840)),
                                                on = .(Group_vio_ID),
                                                .(N = .N),
                                                by = .(Place, Year_M)])

# merge
data_places <- merge(data_poly_ba[, .(Place, lon, lat)],
                     data_violation_agg_place[, .(Place, N_violation = N, Year_M)],
                     by = "Place",
                     all.y = T, allow.cartesian = T)

# compute mean lon lat for labels
data_poly_ba_mean_place <- copy(data_poly_ba[, .(lon = mean(lon),
                                                 lat = mean(lat)),
                                             by = .(Place)])

# plot
ggplot() +
  geom_polygon(data = data_places[.(2019.08), on = .(Year_M)],
               aes(x = lon, y = lat,
                   fill = N_violation,
                   group = Place),
               color = "grey40") +
  scale_fill_material("grey") +
  geom_label(data = data_poly_ba_mean_place,
             aes(x = lon, y = lat, label = Place),
             color = "black", fill = "dodgerblue1", alpha = 0.65) +
  theme_void()

# search street -----
# Handle slavic (slovak) symbols in street names
dt_uni_streets <- copy(data_violation[, .(Place = data.table::first(Place)), by = .(Street)])
dt_uni_streets[, Street_edit := gsub(pattern = "È", replacement = "C", x = Street)]
dt_uni_streets[, Street_edit := gsub(pattern = "ò", replacement = "n", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "è", replacement = "c", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "\u009d", replacement = "t", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "¾", replacement = "l", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "¼", replacement = "L", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "ò", replacement = "n", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "_", replacement = "", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "ï", replacement = "d", x = Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "à", replacement = "r", x = Street_edit)]
dt_uni_streets[, Street_edit := stri_trans_general(Street_edit, "Latin-ASCII")]
dt_uni_streets[, Street_edit := sub(" - MC.*", "", Street_edit)]
dt_uni_streets[, Street_edit := gsub(pattern = "Nam.", replacement = "Namestie ", x = Street_edit)]

dt_uni_streets[, Street_query := gsub(pattern = " ", replacement = "+", x = Street_edit)]
dt_uni_streets[, Street_query := paste0(Street_query, "+Bratislava")]

# street lines of BA -----
ba_bb <- getbb(place_name = "Bratislava")

bratislava <- opq(bbox = ba_bb) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>%
  osm_poly2line()

# Plot it
ggplot(data = bratislava$osm_lines) + geom_sf()

# Street names
street_names <- data.table(Street = unique(bratislava$osm_lines$name)) # bleh

street_names[, Street_edit := gsub(pattern = "Ã", replacement = "i", x = Street)]
street_names[, Street_edit := gsub(pattern = "i©", replacement = "e", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "i¡", replacement = "a", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "iº", replacement = "u", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å¾", replacement = "z", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Ä¾", replacement = "l", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Ä\u008d", replacement = "c", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å¡", replacement = "s", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Ä½", replacement = "L", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Åˆ", replacement = "n", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "i½", replacement = "y", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å•", replacement = "r", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "i³", replacement = "o", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "i¤", replacement = "a", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å½", replacement = "Z", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å ", replacement = "S", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å¥", replacement = "t", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "ÄŒ", replacement = "C", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Ä\u008f", replacement = "d", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "i¶", replacement = "o", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Å‘", replacement = "o", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Ä›", replacement = "e", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "Ä›", replacement = "e", x = Street_edit)]

street_names[, Street_edit := stri_trans_general(Street_edit, "Latin-ASCII")]
street_names[, Street_edit := gsub(pattern = "i-", replacement = "i", x = Street_edit)]
street_names[, Street_edit := gsub(pattern = "A ", replacement = "S", x = Street_edit)]

street_names[dt_uni_streets, on = .(Street_edit)]
street_names_vio <- copy(street_names[dt_uni_streets, on = .(Street_edit), Street_orig := i.Street])[!is.na(Street_orig)]
# we lost 195 streets from 1464 - not good not bad

# bratislava

# work with street data
ba_streets_vio <- bratislava$osm_lines
# only available streets
ba_streets_vio <- ba_streets_vio[ba_streets_vio$name %in% street_names_vio$Street,]
ba_streets_vio <- ba_streets_vio[!is.na(ba_streets_vio$name),]
str(ba_streets_vio)

# check
ggplot(data = ba_streets_vio) +
  geom_sf()

# transform to standard lon/lat matrix format instead of sf object
data_streets_st <- data.table::data.table(sf::st_coordinates(ba_streets_vio$geometry))

ba_streets_vio$L1 <- 1:nrow(ba_streets_vio)
data_streets_st <- merge(data_streets_st, as.data.table(ba_streets_vio)[, .(L1, name)], by = "L1")
data_streets_st[street_names_vio[, .(name, Street_edit)], on = .(name), Street_edit := i.Street_edit]
setnames(data_streets_st, c("X", "Y"), c("lon", "lat"))
data_streets_st <- data_streets_st[!lon > data_poly_ba[, max(lon)]]
data_streets_st <- data_streets_st[!lon < data_poly_ba[, min(lon)]]
data_streets_st <- data_streets_st[!lat > data_poly_ba[, max(lat)]]

# aggregate data by streets and Year_M
data_violation_agg_street <- copy(data_violation[.(c(4836, 5000, 4834, 4700, 4838, 4813, 9803, 4840)), on = .(Group_vio_ID),
                                                 .(N = .N), by = .(Street, Year_M)])

setorder(data_violation_agg_street, Year_M, -N)

# lookup
data_violation_agg_street[dt_uni_streets, on = .(Street), Street_edit := i.Street_edit]

data_streets_st <- merge(data_streets_st,
                         data_violation_agg_street[, .(Street_edit, N_violations = N, Year_M)],
                         by = "Street_edit",
                         all.y = T,
                         allow.cartesian = TRUE)

# transform integers to reasonable factors segments
table(data_streets_st$N_violations)
data_streets_st[N_violations >= 11, N_violation_type := "> 11"]
data_streets_st[N_violations >= 6 & N_violations < 11, N_violation_type := "(6, 10)"]
data_streets_st[N_violations >= 3 & N_violations < 6, N_violation_type := "(3, 5)"]
data_streets_st[N_violations <= 2, N_violation_type := "< 2"]
data_streets_st[, N_violations_per_street := factor(N_violation_type, levels = c("< 2",  "(3, 5)", "(6, 10)", "> 11"))]
levels(data_streets_st$N_violations_per_street)

ggplot(data_streets_st[!is.na(L1)][.(2019.08), on = .(Year_M)]) +
  geom_line(aes(lon, lat, group = L1, color = N_violations_per_street)) +
  scale_color_brewer(palette = "Reds") +
  theme_bw()

# for ggmap
bbox <- make_bbox(lon, lat, data = data_poly_ba, f = .01)
map_ba <- get_map(location = bbox, source = 'stamen', maptype = "watercolor") # toner, terrain

# althogether
ggmap(map_ba, extent = "device") +
  geom_polygon(data = data_places[.(2019.08), on = .(Year_M)],
               aes(x = lon, y = lat,
                   fill = N_violation,
                   group = Place),
               color = "grey40") +
  scale_fill_material("grey", alpha = 0.7) +
  geom_line(data = data_streets_st[!is.na(L1)][.(2019.08), on = .(Year_M)],
            aes(lon, lat, group = L1, color = N_violations_per_street)) +
  scale_color_brewer(palette = "Reds") +
  geom_label(data = data_poly_ba_mean_place,
             aes(x = lon, y = lat, label = Place,
                 fill = NA_real_),
             color = "black", fill = "dodgerblue1", alpha = 0.65) +
  labs(fill = "N_vio_district", color = "N_vio_street") +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 2), title.position = "top")) +
  theme_void()

# zoom to Oldtown

# extract most violated streets by Year_M
data_streets_st_max <- copy(data_streets_st[!is.na(L1), .SD[N_violations == max(N_violations, na.rm = T)], by = .(Year_M)])
data_streets_st_max[, unique(Street_edit)]
data_streets_st_max <- copy(data_streets_st_max[, .(lon = mean(lon),
                                                    lat = mean(lat),
                                                    Street_edit = first(Street_edit),
                                                    N_violations = first(N_violations),
                                                    N_violations_per_street = first(N_violations_per_street)),
                                                by = .(Year_M)])

ggmap(map_ba) +
  geom_polygon(data = data_places[.(2019.08), on = .(Year_M)],
               aes(x = lon, y = lat,
                   fill = N_violation,
                   # color = Violations_per_capita,
                   group = Place
               ),
               color = "grey40") +
  scale_fill_material("grey", alpha = 0.7) +
  geom_line(data = data_streets_st[!is.na(L1)][.(2019.08), on = .(Year_M)],
            aes(lon, lat, group = L1, color = N_violations_per_street), size = 1.4) +
  geom_label(data = data_streets_st_max[.(2019.08), on = .(Year_M)],
             aes(x = lon, y = lat,
                 label = paste0(Street_edit, " - ", N_violations)
             ),
             color = "black",
             fill = "firebrick2",
             size = 4.5,
             alpha = 0.75
  ) +
  scale_color_brewer(palette = "Reds") +
  geom_label(data = data_poly_ba_mean_place,
             aes(x = lon, y = lat, label = Place,
                 fill = NA_real_),
             color = "black", fill = "dodgerblue1", alpha = 0.75, size = 4.5) +
  coord_map(
    xlim=c(data_poly_ba[.("Stare Mesto"), on = .(Place), min(lon)],
           data_poly_ba[.("Stare Mesto"), on = .(Place), max(lon)]),
    ylim= c(data_poly_ba[.("Stare Mesto"), on = .(Place), min(lat)],
            data_poly_ba[.("Stare Mesto"), on = .(Place), max(lat)])
  ) +
  labs(fill = "N_vio_district", color = "N_vio_street") +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 2), title.position = "top"),
         fill = guide_legend(nrow = 2, title.position = "top")) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom")

# gganimate with street lines ----

gg_ba_anim_lines <- ggmap(map_ba, extent = "device") +
  geom_polygon(data = data_places,
               aes(x = lon, y = lat,
                   fill = N_violation,
                   group = Place),
               color = "grey40") +
  scale_fill_material("grey", alpha = 0.7) +
  geom_line(data = data_streets_st[!is.na(L1)],
            aes(lon, lat,
                group = L1,
                color = N_violations_per_street
            ),
            size = 1.4
  ) +
  scale_color_brewer(palette = "Reds") +
  labs(fill = "N_vio_district", color = "N_vio_street") +
  geom_label(data = data_poly_ba_mean_place,
             aes(x = lon, y = lat, label = Place,
                 fill = NA_real_),
             color = "black", fill = "dodgerblue1", alpha = 0.75, size = 4.5) +
  transition_states(reorder(Year_M, Year_M), transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out") +
  labs(title = "Year.Month - {closest_state}") +
  guides(color = FALSE, fill = FALSE) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold")
  )

anim_whole <- animate(gg_ba_anim_lines, duration = 46, width = 700, height = 750)

# zoomed animated map ----
gg_ba_anim_lines_zoom <- ggmap(map_ba, extent = "device") +
  geom_polygon(data = data_places,
               aes(x = lon, y = lat,
                   fill = N_violation,
                   group = Place),
               color = "grey40") +
  scale_fill_material("grey", alpha = 0.7) +
  geom_line(data = data_streets_st[!is.na(L1)],
            aes(lon, lat,
                group = L1,
                color = N_violations_per_street
            ),
            size = 1.4
  ) +
  scale_color_brewer(palette = "Reds") +
  geom_label(data = data_streets_st_max,
             aes(x = lon, y = lat,
                 label = paste0(Street_edit, " - ", N_violations)
             ),
             color = "black", fill = "firebrick2", alpha = 0.75, size = 4.5
  ) +
  geom_label(data = data_poly_ba_mean_place,
             aes(x = lon, y = lat, label = Place,
                 fill = NA_real_),
             color = "black", fill = "dodgerblue1", alpha = 0.75, size = 4.5
  ) +
  coord_map(
    xlim=c(data_poly_ba[.("Stare Mesto"), on = .(Place), min(lon)],
           data_poly_ba[.("Stare Mesto"), on = .(Place), max(lon)]),
    ylim= c(data_poly_ba[.("Stare Mesto"), on = .(Place), min(lat)],
            data_poly_ba[.("Stare Mesto"), on = .(Place), max(lat)])
  ) +
  transition_states(reorder(Year_M, Year_M), transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out") +
  labs(fill = "N_vio_district", color = "N_vio_street") +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 2), title.position = "top"),
         fill = guide_legend(nrow = 2, title.position = "top")) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom")

anim_zoom <- animate(gg_ba_anim_lines_zoom, duration = 46, width = 700, height = 750)

# GIFS binding ----
# bind two gifs
a_mgif <- image_read(anim_whole)
b_mgif <- image_read(anim_zoom)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))

for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

# final!
new_gif

# some most viol. streets time series ----
data_streets_st_max[, .N, by = .(Street_edit)]
data_streets_st_max[, sum(N_violations), by = .(Street_edit)]

times <- data.table(Year_M = data_violation_agg_street[, sort(unique(Year_M))])
times[, Date := as.Date("2017-01-01")]
times[, row_id := 1:.N]
times[, Date := Date + row_id*30.4]

data_vio_most_street <- copy(data_violation_agg_street[
  .(c("Michalska", "Postova",
      "Rybarska brana", "Namestie slobody")),
  on = .(Street_edit)])

data_vio_most_street[times, on = .(Year_M), Date := i.Date]
setnames(data_vio_most_street, "N", "N_violations")

ggplot(data_vio_most_street) +
  # facet_wrap(~Street_edit, nrow = 2) +
  geom_line(aes(Date, N_violations, color = Street_edit), size = 0.8) +
  theme_bw()

# last three months is Postova the "winner of most violated street"...maybe beacuse of the new city-police station nearby.
