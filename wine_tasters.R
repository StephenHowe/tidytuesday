### Tidy Tuesday: Wine Exploration
### Stephen Howe
### May 31, 2019

# libraries ----
library(tidyverse)

# data ----
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

# group by wine taster
tasters <- wine_ratings %>%
  group_by(taster_name) %>%
  dplyr::summarise(count=n())

#drop row without a taster_name
taster <- tasters %>% drop_na()

# scatterplot of points to price, subsetted by wine taster
ggplot(data=wine_ratings, 
       mapping = aes(x=log(price), y=points, col=taster_name)
       ) +
  geom_point() + 
  geom_smooth(method='lm') +
  labs(title="Pricier wines earn higher ratings")

# group tasters by number of reviews
breaks <- c(0,500,1000,5000,10000,25000,30000)
labels <- c("0-500", 
            "500-1000",
            "1000-5000",
            "5000-10000",
            "10000-25000",
            "25000+")
tasters$review_group <- cut(tasters$count, breaks = breaks, labels = labels)


# label original data set
wine_ratings2 <- merge(wine_ratings,
                       tasters,
                       by="taster_name")

# new plot
custom.col <- c("green", "orange", 
                "yellow", "red", "dark blue", "light blue")


# plot with reviewers grouped by numnber of review
ggplot(data=wine_ratings2, 
       mapping = aes(x=log(price), y=points, col=review_group)
) +
  geom_point(position = "jitter") + 
  geom_smooth(method='lm') +
  scale_colour_manual(values=custom.col) +
  labs(title="Seasoned reviewers reinforce the correlation")
