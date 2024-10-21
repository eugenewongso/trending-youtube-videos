library("tidyverse")
library("stringr")

# import datasets
df1 <- read.csv("US_youtube_trending_data.csv")
df2 <- read.csv("topSubscribed.csv")
df3 <- read.csv("youtube_category_ids.csv")

# make copy of datasets
US_trend_vids <- df1
top_subs <- df2
cat_ids <- df3

# remove duplicate rows
US_trend_vids <- distinct(US_trend_vids, title, .keep_all = TRUE)

# remove rows that have no like or no dislikes
US_trend_vids <- filter(US_trend_vids, likes > 0 & dislikes > 0)

# remove thumbnail link column
US_trend_vids <- select(US_trend_vids, !thumbnail_link)

# add new column "Category" to US_trending_videos.csv dataset (categorical variable)
US_trend_vids <- left_join(US_trend_vids, cat_ids, by = c("categoryId" = "ID"))

# add new number_of_tags column to US_trend_vids dataset (continuous/numerical variable)
US_trend_vids <- mutate(US_trend_vids, number_of_tags = str_count(tags, fixed("|")) + 1)

# remove redundant Category in top_subs dataset
top_subs <- select(top_subs, -Category)

# left join by the channel name
trend_vids <- left_join(US_trend_vids, top_subs, by = c("channelTitle" = "Youtube.Channel"))

# group by category and summarize total of view_count, likes, dislikes, and comment_count
category_grouped <- group_by(trend_vids, Category)
category_summary <- summarise(category_grouped,
                              total_views = sum(view_count),
                              total_likes = sum(likes),
                              total_dislikes = sum(dislikes),
                              total_comments = sum(comment_count))

write.csv(trend_vids, file = "trend_vids.csv", row.names = FALSE)