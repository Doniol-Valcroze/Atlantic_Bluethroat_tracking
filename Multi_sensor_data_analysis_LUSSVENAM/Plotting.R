rm(list=ls())
gc()

library(readr)
library(GeoPressureR)


id <- "CS759"

rdata_path <- paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/data/interim/", id, ".RData")

if (file.exists(rdata_path)) {load(rdata_path)}



library(tidyverse)
library(lubridate)
library(scales)

# Conversion de la colonne date
pressurepath_most_likely$date <- as.POSIXct(pressurepath_most_likely$date)

Sys.setlocale("LC_TIME", "C")

total <- pressurepath_most_likely |>
  filter(label != "discard") |>
  mutate(
    altitude = ifelse(altitude < 0, 0, altitude),
    date = as.Date(date)  # convert datetime to Date
  ) |>
  ggplot(aes(x = date, y = altitude)) +
  geom_line(color = "black", linewidth = 0.8) +
  labs(x = "Date",
       y = "Altitude (m. a.s.l)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_bw() +   theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"))

ggsave(filename = paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/", id, "_total_altitude.tiff"),
       plot = total,   # replace with the name of your ggplot
       device = "tiff",
       height = 5,
       width = 10,
       units = "in",
       dpi = 300)


# Autumn #1
start_date <- as.POSIXct("2024-08-29 01:00:00")
end_date   <- as.POSIXct("2024-09-03 23:00:00")

# 1. Create night and day periods from sunset/sunrise
day_night_periods <- pressurepath_most_likely |>
  select(date, sunset, sunrise) |>
  distinct() |>
  arrange(date) |>
  mutate(
    night_start = sunset,
    night_end = lead(sunrise),
    day_start = lead(sunrise),
    day_end = lead(sunset)
  ) |>
  filter(!is.na(night_end), !is.na(day_start), !is.na(day_end)) |>
  filter(night_start >= start_date, night_end <= end_date)

# 2. Create a long-format dataframe of both types
background_rects <- bind_rows(
  day_night_periods |> transmute(start = night_start, end = night_end, period = "night"),
  day_night_periods |> transmute(start = day_start, end = day_end, period = "day")
)

# 3. Altitude data
altitude_data <- pressurepath_most_likely |>
  filter(label != "discard",
         date >= start_date,
         date <= end_date) |>
  mutate(altitude = ifelse(altitude < 0, 0, altitude))

# 4. Plot with background rects
aut_1 <- ggplot() +
  # Plot white and grey rectangles
  geom_rect(data = background_rects,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = period),
            inherit.aes = FALSE, alpha = 1) +  # full opacity
  # Altitude line
  geom_line(data = altitude_data, aes(x = date, y = altitude),color = "black", linewidth = 1.2) +
  # Labels
  labs(x = "Date", y = "Altitude (m. a.s.l)") +
  # White/grey coloring
  scale_fill_manual(values = c(night = "#CCCCCC80", day = "#ffffff80"), guide = "none") +
  theme_bw() +   theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"))


ggsave(filename = paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/", id, "_aut_1_altitude.tiff"),
       plot = aut_1,   # replace with the name of your ggplot
       device = "tiff",
       height = 5,
       width = 10,
       units = "in",
       dpi = 300)



# Autumn #2
start_date <- as.POSIXct("2024-10-04 01:00:00")
end_date   <- as.POSIXct("2024-10-07 23:00:00")


# 1. Create night and day periods from sunset/sunrise
day_night_periods <- pressurepath_most_likely |>
  select(date, sunset, sunrise) |>
  distinct() |>
  arrange(date) |>
  mutate(
    night_start = sunset,
    night_end = lead(sunrise),
    day_start = lead(sunrise),
    day_end = lead(sunset)
  ) |>
  filter(!is.na(night_end), !is.na(day_start), !is.na(day_end)) |>
  filter(night_start >= start_date, night_end <= end_date)

# 2. Create a long-format dataframe of both types
background_rects <- bind_rows(
  day_night_periods |> transmute(start = night_start, end = night_end, period = "night"),
  day_night_periods |> transmute(start = day_start, end = day_end, period = "day")
)

# 3. Altitude data
altitude_data <- pressurepath_most_likely |>
  filter(label != "discard",
         date >= start_date,
         date <= end_date) |>
  mutate(altitude = ifelse(altitude < 0, 0, altitude))

# 4. Plot with background rects
aut_2 <- ggplot() +
  # Plot white and grey rectangles
  geom_rect(data = background_rects,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = period),
            inherit.aes = FALSE, alpha = 1) +  # full opacity
  # Altitude line
  geom_line(data = altitude_data, aes(x = date, y = altitude),color = "black", linewidth = 1.2) +
  # Labels
  labs(x = "Date", y = "Altitude (m. a.s.l)") +
  # White/grey coloring
  scale_fill_manual(values = c(night = "#CCCCCC80", day = "#ffffff80"), guide = "none") +
  theme_bw() +   theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"))

ggsave(filename = paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/", id, "_aut_2_altitude.tiff"),
       plot = aut_2,   # replace with the name of your ggplot
       device = "tiff",
       height = 5,
       width = 10,
       units = "in",
       dpi = 300)



# Spring
start_date <- as.POSIXct("2025-03-05 23:00:00")
end_date   <- as.POSIXct("2025-03-10 06:00:00")

# 1. Create night and day periods from sunset/sunrise
day_night_periods <- pressurepath_most_likely |>
  select(date, sunset, sunrise) |>
  distinct() |>
  arrange(date) |>
  mutate(
    night_start = sunset,
    night_end = lead(sunrise),
    day_start = lead(sunrise),
    day_end = lead(sunset)
  ) |>
  filter(!is.na(night_end), !is.na(day_start), !is.na(day_end)) |>
  filter(night_start >= start_date, night_end <= end_date)

# 2. Create a long-format dataframe of both types
background_rects <- bind_rows(
  day_night_periods |> transmute(start = night_start, end = night_end, period = "night"),
  day_night_periods |> transmute(start = day_start, end = day_end, period = "day")
)

# 3. Altitude data
altitude_data <- pressurepath_most_likely |>
  filter(label != "discard",
         date >= start_date,
         date <= end_date) |>
  mutate(altitude = ifelse(altitude < 0, 0, altitude))

# 4. Plot with background rects
spring <- ggplot() +
  # Plot white and grey rectangles
  geom_rect(data = background_rects,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = period),
            inherit.aes = FALSE, alpha = 1) +  # full opacity
  # Altitude line
  geom_line(data = altitude_data, aes(x = date, y = altitude),color = "black", linewidth = 1.2) +
  # Labels
  labs(x = "Date", y = "Altitude (m. a.s.l)") +
  # White/grey coloring
  scale_fill_manual(values = c(night = "#CCCCCC80", day = "#ffffff80"), guide = "none") +
  theme_bw() +   theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"))


ggsave(filename = paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/", id, "_spring_altitude.tiff"),
       plot = spring,   # replace with the name of your ggplot
       device = "tiff",
       height = 5,
       width = 10,
       units = "in",
       dpi = 300)


rm(list=ls())
library(GeoPressureR)
library(ggplot2)
library(scales)
library(mapdata)
library(gridExtra)
library(terra)

path_most_likely <- path_geopressureviz

tiff(file = paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/", id, ".tiff"), height = 5, width = 4, units = "in", res = 300)


  par(mar = c(0, 0, 0, 0))

  marginal_rast <- rast.map(marginal)
  path_most_likely$start <- as.POSIXct(path_most_likely$start)
  path_most_likely$end <- as.POSIXct(path_most_likely$end)
  path_most_likely$diff <- difftime(path_most_likely$end, path_most_likely$start, units = "days")

  t1 <- which(sapply(1:nrow(path_most_likely), function(i) {
    any(as.integer(format(seq(as.Date(path_most_likely$start[i]),
                              as.Date(path_most_likely$end[i]),
                              by = "day"), "%m")) == 12)
  }))[1]
  t2 <- nrow(path_most_likely)

  # Identify winter stops
  is_winter_stop <- sapply(1:nrow(path_most_likely), function(i) {
    date_seq <- seq(as.Date(path_most_likely$start[i]), as.Date(path_most_likely$end[i]), by = "day")
    any(as.integer(format(date_seq, "%m")) %in% c(12, 1))
  })

  # Assign point colors
  colors_vec_first  <- rep(alpha("#F0833A", 0.5), t2)  # orange
  colors_vec_second <- rep(alpha("#0F9D58", 0.5), t2)  # green
  colors_vec_first[1:t1]    <- ifelse(is_winter_stop[1:t1], alpha("blue", 0.4), colors_vec_first[1:t1])
  colors_vec_second[t1:t2]  <- ifelse(is_winter_stop[t1:t2], alpha("blue", 0.4), colors_vec_second[t1:t2])

  # Base map

  map('worldHires', xlim=c(-15,5), ylim= c(35.5,48.5), col="darkgray")#spain
  plot(marginal_rast[[1]], add=T, col = c("transparent",alpha("#EDEDED", 0.2)))

  map('worldHires', xlim=c(-15,5), ylim= c(35.5,48.5), col="darkgray")#spain


  # Raster overlays
  for (i in 1:length(marginal)) {
    rasteri <- marginal_rast[[i]]
    rasteri[rasteri < 0.001] <- NA
    plot(rasteri, add = TRUE, col = alpha("#B5B5B5", 0.2))
  }

  # First segment (before T1)
  lines(path_most_likely$lon[1:t1], path_most_likely$lat[1:t1], lwd = 1.2, col = "#F0833A")
  points(path_most_likely$lon[2:t1], path_most_likely$lat[2:t1],
         pch = 21, bg = colors_vec_first[2:t1], col = "black",
         cex = log((as.numeric(path_most_likely$diff[2:t1]) / 4) + 2))

  # Highlight T1 step
  #  points(path_most_likely$lon[t1], path_most_likely$lat[t1],
  #         pch = 21, bg = alpha("blue", 0.4), col = "black",
  #         cex = log((as.numeric(path_most_likely$diff[t1]) / 4) + 2))

  # Second segment (from T1 to end)
  lines(path_most_likely$lon[t1:t2], path_most_likely$lat[t1:t2], lwd = 1.2, col = "#0F9D58")
  points(path_most_likely$lon[t1:(t2-1)], path_most_likely$lat[t1:(t2-1)],
         pch = 21, bg = colors_vec_second[t1:(t2-1)], col = "black",
         cex = log((as.numeric(path_most_likely$diff[t1:(t2-1)]) / 4) + 2))

  # Breeding dot
  points(path_most_likely$lon[1], path_most_likely$lat[t2],
         pch = 17, col = "black", cex = 1)

  # Legend
  legend_lon <- 2
  legend_lat <- 41.5
  legend_sizes <- c(0.5, 5, 10, 50, 100)
  scaled_sizes <- log(legend_sizes / 4 + 2)

  for (i in seq_along(legend_sizes)) {
    points(legend_lon + 3, legend_lat - i * 1.3,
           pch = 21, bg = alpha("gray", 0.5), col = "black",
           cex = scaled_sizes[i])
    text(legend_lon + 4, legend_lat - i * 1.3,
         labels = paste0(legend_sizes[i], ""), adj = 0, cex = 0.7)
  }

  dev.off()
