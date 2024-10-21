library(ggplot2)
library(stringr)
library(dplyr)

df <- read.csv("trend_vids.csv")

df$publishTime <- as.POSIXct(df$publishedAt, format = "%Y-%m-%d")


grouped_date <- group_by(df, publishTime)
view_change_df <- summarize(grouped_date,
                           sum_views = sum(view_count, na.rm = TRUE))


# find max values
max_values <- view_change_df[view_change_df$sum_views == max(view_change_df$sum_views), ]

# average views
avg_views <- mean(view_change_df$sum_views, na.rm = TRUE)

spike_threshold <- 2 * avg_views
spike_points <- view_change_df$sum_views >= spike_threshold

views_per_time <- ggplot(data = view_change_df) +
  geom_line(mapping = aes(x = publishTime, y = sum_views)) +
  geom_hline(yintercept = avg_views, color = "blue", linetype = "dashed") +
  geom_point(data = max_values, mapping = aes(x = publishTime, y = sum_views),
             color = "red", size = 2) +
  geom_text(data = max_values, mapping = aes(x = publishTime,
                                             y = sum_views, label = paste(sum_views, "views\n  on", publishTime)),
            vjust = 0.8, hjust = -0.05) +
  labs(caption = "Horizontal blue line: average total number of views",
        x = "Time", y = "Total Number of Views") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) 

plot(views_per_time)

max_videos <- filter(df, publishTime == "2021-06-25")

