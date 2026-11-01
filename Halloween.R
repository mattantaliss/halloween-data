library(ggplot2)
library(dplyr)
library(scales)
library(ggthemes)
library(ggrepel)

input.path <- "./input/"
output.path <- "./output/"

data.files <- list.files(path = input.path, pattern = "*.csv$")
num.files <- length(data.files)

create_density_plot <- function(data, output_file_name) {
  p <-
    ggplot(
      data %>% select(Time.of.Day, Year, N) %>% filter(N != 0),
      aes(x = Time.of.Day, y = after_stat(scaled))
    ) +
    geom_density(aes(fill = Year, colour = Year), alpha = 0.1) +
    geom_density(linewidth = 1, show.legend = FALSE) + 
    xlab("Time") +
    ylab("Scaled Density of Trick-or-Treaters") +
    coord_cartesian(
      xlim =
        as.POSIXct(
          paste0("2000-10-31", c("18:00", "21:00")),
          format = "%Y-%m-%d %H:%M"
        )
    ) +
    scale_x_datetime(
      date_breaks = "30 min", labels = date_format("%H:%M", "US/Mountain")
    ) +
    scale_y_continuous(breaks = pretty_breaks()) +
    theme_tufte()
  
  ggsave(
    paste0(output.path, output_file_name),
    plot = p, width = 10, height = 6.18, units = "in", dpi = 600, bg = "white"
  )
}
  
if (num.files < 1) {
  stop("No data", call. = FALSE)
}

all.halloween.data <- NULL

# Create separate histograms for each data set
for (i in 1:num.files) {
  halloween.data <-
    read.csv(paste0(input.path, data.files[i]), stringsAsFactors = FALSE) %>%
    mutate(
      Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M"),
      Cum.N = cumsum(N),
      Year = as.character(as.POSIXlt(Time)$year + 1900),
      Time.of.Day =
        as.POSIXct(
          paste0("2000-10-31", strftime(Time, format = "%H:%M")),
          format = "%Y-%m-%d %H:%M"
        )
    )

  all.halloween.data <- rbind(all.halloween.data, halloween.data)

  p <- ggplot(halloween.data, aes(x = Time, y = N)) +
    geom_bar(stat = "identity", colour = "black", fill = "black") +
    annotate(
      "text",
      x = tail(halloween.data$Time, 1),
      y = max(halloween.data$N),
      label = paste0("Total: ", sum(halloween.data$N)),
      hjust = 1,
      vjust = 1
    ) +
    xlab("Time") +
    ylab("Number of Trick-or-Treaters") +
    scale_x_datetime(
      date_breaks = "30 min", labels = date_format("%H:%M", "US/Mountain")
    ) +
    scale_y_continuous(breaks = pretty_breaks()) +
    theme_tufte()

  y_major_breaks <- ggplot_build(p)$layout$panel_params[[1]]$y$get_breaks()

  p <- p +
    geom_hline(
      yintercept = y_major_breaks[which(y_major_breaks > 0)],
      colour = "white",
      lwd = 0.5
    )

  ggsave(
    paste0(output.path, gsub(".csv", ".png", data.files[i])),
    plot = p, width = 10, height = 6.18, units = "in", dpi = 600, bg = "white"
  )
}

all.halloween.data <- all.halloween.data %>%
  filter(Year != 2020) %>% # Exclude 2020 since COVID made for all 0s that year
  mutate(Current_Year = ifelse(Year == max(Year), "yes", "no"))

# Create density plots to compare all of the data sets and the last few years
# of data
create_density_plot(all.halloween.data, "Density.png")
create_density_plot(
  all.halloween.data %>% filter(Year > max(as.integer(Year)) - 3),
  "Density_Recent.png"
)

# Cumulative number of trick-or-treaters versus time
final_points <- all.halloween.data %>%
  group_by(Year) %>%
  filter(Time.of.Day == max(Time.of.Day)) %>%
  bind_rows(
    tibble(
      Time =
        as.POSIXct(
          paste0("2000-10-31", strftime(max(.$Time), format = "%H:%M")),
          format = "%Y-%m-%d %H:%M"
        ),
      N = 0,
      Cum.N = round(mean(.$Cum.N), 0),
      Year = paste0("Avg (", Cum.N, ")"),
      Time.of.Day =
        as.POSIXct(
          paste0("2000-10-31", strftime(Time, format = "%H:%M")),
          format = "%Y-%m-%d %H:%M"
        ),
      Current_Year = "n/a"
    )
  ) %>%
  # Shift the last time of day over a bit (7 mins seems to work well) for
  # positioning labels in geom_text_repel well (below)
  mutate(Time.of.Day = Time.of.Day + 7 * 60)

p <-
  ggplot(
    all.halloween.data,
    aes(x = Time.of.Day, y = Cum.N, colour = Current_Year, label = Year)
  ) +
  # geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_line(
    data = . %>% filter(Current_Year == "no"),
    aes(group = Year),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_line(
    data = . %>% filter(Current_Year == "yes"),
    aes(group = Year),
    # linewidth = 0.5,
    show.legend = FALSE
  ) +
  stat_summary(fun = mean, geom = "line", color = "black", linewidth = 0.75) +
  geom_text_repel(
    data = final_points,
    aes(label = Year, colour = Current_Year),
    max.overlaps = Inf,      # Always show all labels
    hjust = 0,
    nudge_x = 0, # Adjust horizontal position of labels
    direction = "y", # Allow labels to move vertically
    box.padding = unit(0.5, "lines"), # Padding around labels
    point.padding = unit(0.5, "lines"), # Padding around points
    show.legend = FALSE
  ) +
  labs(x = "Time", y = "Cumulative Number of Trick-or-Treaters") +
  coord_cartesian(
    xlim =
      as.POSIXct(
        paste0("2000-10-31", c("18:00", "21:15")), # 21:15 gives space for year label
        format = "%Y-%m-%d %H:%M"
      )
  ) +
  scale_x_datetime(
    date_breaks = "30 min", labels = date_format("%H:%M", "US/Mountain")
  ) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_color_manual(values = c("yes" = "#e66c2c", "no" = "gray", "n/a" = "black")) +
  theme_tufte()

ggsave(
  paste0(output.path, "Cumulative_Count.png"),
  plot = p, width = 10, height = 6.18, units = "in", dpi = 600, bg = "white"
)