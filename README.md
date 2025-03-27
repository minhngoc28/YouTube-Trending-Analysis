# 📊 YouTube Trending Videos Analysis

This project explores the trending videos on YouTube using R. The dataset is sourced from [Kaggle](https://www.kaggle.com/datasets/thedevastator/youtube-trending-videos-dataset). Key goals:

- Clean and transform the dataset
- Add derived features like time-to-trend, publish period
- Explore trends by category, country, time, etc.
- Visualize correlations between views, likes, dislikes, comments

## 📁 Structure

- `youtube_trending_analysis.R` - code
- `plots/` - exported visualizations
- `youtube.csv` - raw dataset
- `YouTube Trending Videos Dataset Analysis.pdf` - full analysis script
## 📌 Insights

- Entertainment has the most trending videos, but Music gets more likes per video.
- Videos posted from 16:00 to 23:59 trend the most.
- Friday is the most popular day to upload trending videos.
- There is a strong positive correlation between views and likes.

## 🔧 Tools

- R
- ggplot2, dplyr, corrplot, etc.
