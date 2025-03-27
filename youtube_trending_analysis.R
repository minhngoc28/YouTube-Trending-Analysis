library(ggplot2)
library(dplyr)
library(gridExtra)

df <- read.csv("/Users/macbook/Downloads/YouTube-Trending-Analysis/youtube.csv",header = TRUE)
str(df)
View(df)
anyNA(df)
youtube <- df[match(unique(df$title), df$title),]

#Change the category which still in numbered forma into character to make it more understadable.
youtube <- youtube %>%
  mutate(category_id = case_when(
    category_id == 1  ~ "Film and Animation",
    category_id == 2  ~ "Autos and Vehicles",
    category_id == 10 ~ "Music",
    category_id == 15 ~ "Pets and Animals",
    category_id == 17 ~ "Sports",
    category_id == 18 ~ "Short Movies",
    category_id == 19 ~ "Travel and Events",
    category_id == 20 ~ "Gaming",
    category_id == 22 ~ "People and Blogs",
    category_id == 23 ~ "Comedy",
    category_id == 24 ~ "Entertainment",
    category_id == 25 ~ "News and Politics",
    category_id == 26 ~ "Howto and Style",
    category_id == 27 ~ "Education",
    category_id == 28 ~ "Science and Technology",
    category_id == 29 ~ "Nonprofit and Activism",
    category_id == 30 ~ "Movies",
    category_id == 43 ~ "Shows",
    category_id == 44 ~ "Trailers",
    TRUE ~ as.character(category_id)
  ))

# Period of the day
youtube <- youtube %>%
  mutate(periodtotrend = case_when(
    time_frame %in% c(
      "0:00 to 0:59", "1:00 to 1:59", "2:00 to 2:59", "3:00 to 3:59",
      "4:00 to 4:59", "5:00 to 5:59", "6:00 to 6:59", "7:00 to 7:59"
    ) ~ "0:00 to 7:59",
    
    time_frame %in% c(
      "8:00 to 8:59", "9:00 to 9:59", "10:00 to 10:59", "11:00 to 11:59",
      "12:00 to 12:59", "13:00 to 13:59", "14:00 to 14:59", "15:00 to 15:59"
    ) ~ "8:00 to 15:59",
    
    time_frame %in% c(
      "16:00 to 16:59", "17:00 to 17:59", "18:00 to 18:59", "19:00 to 19:59",
      "20:00 to 20:59", "21:00 to 21:59", "22:00 to 22:59", "23:00 to 23:59"
    ) ~ "16:00 to 23:59",
    
    TRUE ~ NA_character_
  ))

# Time needed for a video to become trending (how many days needed for trending videos)
youtube$timetotrend <- as.Date(youtube$trending_date, format = "%y.%d.%m") -
  as.Date(youtube$publish_date, format = "%d/%m/%Y")


youtube$timetotrend <- as.factor(ifelse(youtube$timetotrend <= 7, 
                                        youtube$timetotrend, "8+"))

#1 Distribution of trending videos by Categories
yt1 <- data.frame(table(youtube$category_id))
ggplot(yt1, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_segment(
    aes(x = reorder(Var1, Freq), xend = reorder(Var1, Freq), y = 0, yend = Freq),
    color = "blue"
  ) +
  geom_point(color = "blue", size = 4, alpha = 0.6) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Figure 1: Distribution of Trending Videos by Categories",
    caption = "Source: YouTube Trending Videos Dataset",
    x = "Category",
    y = "Number of Videos"
  )
ggsave("plots/fig1_category_distribution.png", width = 8, height = 6)

#2 Distribution of trending videos by Country
# Create frequency table by country
yt2 <- data.frame(table(youtube$publish_country))

# Create bar chart
ggplot(yt2, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue2", color = "grey") +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Figure 2: Distribution of Trending Videos by Country",
    caption = "Source: YouTube Trending Videos Dataset",
    x = "Country",
    y = "Number of Videos"
  )
ggsave("plots/fig2_country_distribution.png", width = 8, height = 6)


#3 Frequency of Trending Videos based on time of publish
# Create a frequency table by time frame (ordered levels)
yt3 <- data.frame(table(factor(
  youtube$time_frame,
  levels = c(
    "0:00 to 0:59", "1:00 to 1:59", "2:00 to 2:59", "3:00 to 3:59",
    "4:00 to 4:59", "5:00 to 5:59", "6:00 to 6:59", "7:00 to 7:59",
    "8:00 to 8:59", "9:00 to 9:59", "10:00 to 10:59", "11:00 to 11:59",
    "12:00 to 12:59", "13:00 to 13:59", "14:00 to 14:59", "15:00 to 15:59",
    "16:00 to 16:59", "17:00 to 17:59", "18:00 to 18:59", "19:00 to 19:59",
    "20:00 to 20:59", "21:00 to 21:59", "22:00 to 22:59", "23:00 to 23:59"
  )
)))

# Create the line chart
ggplot(yt3, aes(x = Var1, y = Freq, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3.5, color = "black") +
  labs(
    title = "Figure 3: Frequency of Trending Videos Based on Time of Publish",
    x = "Time Frame",
    y = "Frequency",
    caption = "Source: YouTube Trending Videos Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)
  )
ggsave("plots/fig3_publish_time_frequency.png", width = 10, height = 6)

#4 Frequency of Videos in each period
# Make sure periodtotrend is a factor in the correct order
youtube$periodtotrend <- factor(
  youtube$periodtotrend,
  levels = c("0:00 to 7:59", "8:00 to 15:59", "16:00 to 23:59")
)

# Create bar chart
ggplot(youtube, aes(x = periodtotrend)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Figure 4: Frequency of Videos in Each Period",
    x = "Time Period",
    y = "Frequency",
    caption = "Source: YouTube Trending Videos Dataset"
  ) +
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    axis.title.x = element_blank()
  )
ggsave("plots/fig4_frequency_by_period.png", width = 8, height = 6)

#5 Frequency of Trending Videos base on publised day of the week
# Create frequency table for published day of the week
yt5 <- data.frame(table(youtube$published_day_of_week))

# Ensure correct day order
yt5$Var1 <- factor(yt5$Var1, levels = c(
  "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday", "Sunday"
))

# Create bar chart
ggplot(yt5, aes(x = Var1, y = Freq)) +
  geom_col(fill = "skyblue", alpha = 0.9) +
  geom_text(aes(label = Freq), size = 4, hjust = 0.5, vjust = 3) +
  labs(
    title = "Figure 5: Frequency of Trending Videos Based on Published Day of the Week",
    x = NULL,
    y = "Frequency",
    caption = "Source: YouTube Trending Videos Dataset"
  )

ggsave("plots/fig5_frequency_by_weekday.png", width = 8, height = 6)


#6: Total Views, Likes, Dislikes, Comments of Categories
# Function to generate bar plot by category
create_plot <- function(data, y_var, y_lab, fill_color) {
  yt6 <- data %>%
    select(category_id, {{ y_var }}) %>%
    group_by(category_id) %>%
    summarise({{ y_var }} := sum({{ y_var }}) / 1e6) %>%  # convert to millions
    arrange(desc({{ y_var }}))
  
  plot <- ggplot(yt6, aes(x = reorder(category_id, -{{ y_var }}), y = {{ y_var }})) +
    geom_bar(stat = "identity", fill = fill_color) +
    coord_flip() +
    theme_minimal() +
    labs(x = "Categories", y = y_lab)
  
  return(plot)
}

# Generate each plot
plot1 <- create_plot(youtube, views, "Number of Million Views", "skyblue3")
plot2 <- create_plot(youtube, likes, "Number of Million Likes", "green3")
plot3 <- create_plot(youtube, dislikes, "Number of Million Dislikes", "purple3")
plot4 <- create_plot(youtube, comment_count, "Number of Million Comments", "orange3")

# Combine all plots into a 2x2 grid
combined_plots <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)
ggsave("plots/fig6_total_engagement_by_category.png", plot = combined_plots, width = 12, height = 8)

#7 Number of Likes for Entertainment

yt7 <- youtube %>%
  filter(category_id == "Entertainment") %>%
  select(periodtotrend, likes) %>%
  group_by(periodtotrend)

ggplot(data = yt7, aes(x = periodtotrend, y = log(likes), fill = periodtotrend)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = log(mean(likes))), color = "red", linetype = "dashed") +
  labs(
    title = "Figure 7: Number of Likes for Entertainment",
    subtitle = "Based on Time of Publish",
    caption = "Source: YouTube Trending Videos Dataset",
    x = NULL,
    y = "Log(Likes)"
  )
ggsave("plots/fig7_likes_entertainment_period.png", width = 8, height = 6)

#8: Top 10 Trending YouTube Channels by Total Views
yt8 <- youtube %>%
  select(channel_title, views) %>%
  group_by(channel_title) %>%
  summarise(views = sum(views) / 1e6) %>%
  arrange(desc(views))

yt8_top10 <- head(yt8, 10)

ggplot(yt8_top10, aes(x = reorder(channel_title, -views), y = views, fill = channel_title)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Figure 8: Top 10 Trending YouTube Channels by Views",
    caption = "Source: YouTube Trending Videos Dataset",
    x = "Channel",
    y = "Views (Millions)"
  )
ggsave("plots/fig8_top10_channels_views.png", width = 10, height = 6)

#9: Time to Trend vs. Period of Publish
yt9 <- youtube %>%
  select(timetotrend, periodtotrend)

ggplot(data = yt9, aes(x = timetotrend)) +
  geom_bar(aes(fill = periodtotrend), position = "stack") +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  labs(
    title = "Figure 9: Time to Trend vs. Time of Publish",
    caption = "Source: YouTube Trending Videos Dataset",
    x = "Days Needed to Trend",
    y = "Frequency"
  )
ggsave("plots/fig9_time_to_trend.png", width = 10, height = 6)

#10: Correlation Matrix (Views, Likes, Dislikes, Comments)
library(corrplot)

columns_of_interest <- c("views", "likes", "dislikes", "comment_count")
correlation_matrix <- cor(youtube[columns_of_interest])

corrplot(correlation_matrix, method = "pie")
png("plots/fig10_correlation_matrix.png", width = 600, height = 600)
corrplot(correlation_matrix, method = "pie")
dev.off()

#11: Correlation Between Likes & Views
ggplot(data = youtube, aes(x = log(views), y = log(likes))) +
  geom_point(color = "black", fill = "#69b3a2", shape = 5, alpha = 0.5, size = 1, stroke = 1) +
  geom_smooth() +
  labs(
    title = "Figure 11: Correlation Between Likes and Views",
    caption = "Source: YouTube Trending Videos Dataset",
    x = "Log(Views)",
    y = "Log(Likes)"
  )
ggsave("plots/fig11_likes_vs_views.png", width = 8, height = 6)

#12: Line Chart of Likes vs. Dislikes
plot(youtube$likes, type = 'l', col = '#93089A',
     xlab = 'Number of Videos',
     ylab = 'Likes and Dislikes',
     main = 'Figure 12: Correlation of Likes vs. Dislikes',
     lty = 2)

lines(youtube$dislikes, col = 'black')

legend('topright',
       legend = c('Likes', 'Dislikes'),
       col = c('#93089A', 'black'),
       lty = c(2, 1))

png("plots/fig12_likes_vs_dislikes.png", width = 800, height = 400)
plot(youtube$likes, type = 'l', col = '#93089A',
     xlab = 'Number of Videos', ylab = 'Likes and Dislikes',
     main = 'Figure 12: Correlation of Likes vs. Dislikes', lty = 2)
lines(youtube$dislikes, col = 'black')
legend('topright', legend = c('Likes', 'Dislikes'),
       col = c('#93089A', 'black'), lty = c(2, 1))
dev.off()

#13 Calculating ratio likes-dislike for each category
# Calculate total likes and dislikes by category
likesdf <- youtube %>%
  group_by(category_id) %>%
  summarise(total_likes = sum(likes))

dislikesdf <- youtube %>%
  group_by(category_id) %>%
  summarise(total_dislikes = sum(dislikes))

# Calculate ratio and merge into one dataframe
ratiodf <- likesdf
ratiodf$ratio <- likesdf$total_likes / dislikesdf$total_dislikes

# Order by highest ratio
ratiodf <- ratiodf[order(ratiodf$ratio, decreasing = TRUE), ]

# Plot
ggplot(ratiodf, aes(x = reorder(category_id, -ratio), y = ratio)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  labs(
    y = "Likes-Dislikes Ratio",
    x = "Category",
    title = "Figure 13: Ratio of Likes to Dislikes by Category"
  ) +
  theme_minimal() +
  coord_flip()
ggsave("plots/fig13_likes_dislikes_ratio.png", width = 10, height = 6)

#14: Correlation between Dislikes & Comment_count
ggplot(data = youtube, aes(x = log(dislikes), y = log(comment_count))) +
  geom_point(color = "black", shape = 5, alpha = 0.5, size = 1, stroke = 1) +
  geom_smooth() +
  labs(
    title = "Figure 14: Correlation between Dislikes & Comment Count",
    caption = "Source: YouTube Trending Videos Dataset",
    x = "Log(Dislikes)",
    y = "Log(Comment Count)"
  )
ggsave("plots/fig14_dislikes_vs_comments.png", width = 8, height = 6)

#15: Views over time
ggplot(youtube, aes(x = trending_date, y = views / 1000)) +
  geom_line(color = "orange4") +
  labs(
    title = "Figure 15: Views Over Time",
    x = "Trending Date",
    y = "View Counts (Thousands)"
  ) +
  theme_minimal()

ggsave("plots/fig15_views_over_time.png", width = 10, height = 6)

#16: The relationship between view count and comments disabled.
ggplot(youtube, aes(x = comments_disabled, y = views, fill = comments_disabled)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Figure 16: Views vs. Comments Disabled",
       x = "Comments Disabled", y = "Views")
ggsave("plots/fig16_views_vs_comments_disabled.png", width = 8, height = 6)

#17 Relationship ratings disabled with views per each category
ggplot(youtube, aes(x = views, y = category_id, fill = ratings_disabled)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Figure 17: Views vs. Ratings Disabled by Category",
       x = "Views", y = "Category ID")
ggsave("plots/fig17_views_vs_ratings_disabled.png", width = 10, height = 6)

#18 Relationship among Views, Country with Dislikes
ggplot(youtube, aes(x = factor(publish_country), y = views / 1000)) +
  geom_point(aes(size = dislikes / 1000, color = dislikes / 1000)) +
  facet_wrap(~cut(dislikes / 1000, breaks = 2), scales = "free") +
  scale_size_continuous(range = c(1, 8)) +
  labs(
    title = "Figure 18: Relationship Among Views, Country with Dislikes",
    x = "Country", y = "Views (Thousands)",
    size = "Dislikes (Thousands)", color = "Dislikes (Thousands)"
  )
ggsave("plots/fig18_views_dislikes_country.png", width = 10, height = 6)

#19 Views in each country with categories
ggplot(youtube, aes(x = factor(publish_country), y = category_id)) +
  geom_point(aes(size = views / 1e6, color = views / 1e6)) +
  facet_wrap(~cut(views / 1e6, breaks = 3), scales = "free") +
  scale_size_continuous(range = c(1, 8)) +
  labs(
    title = "Figure 19: Views in Each Country by Category",
    x = "Country", y = "Category",
    size = "Views (Millions)", color = "Views (Millions)"
  )
ggsave("plots/fig19_views_by_country_category.png", width = 10, height = 6)


