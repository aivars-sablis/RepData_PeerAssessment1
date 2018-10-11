zip <- "activity.zip"
f <- "activity.csv"
activityDt <- read.table(unz(file.path(path, zip), f), sep = ",", header = TRUE)

activityDt <- transform(activityDt , date=as.Date(date, format = "%Y-%m-%d"))

## What is mean total number of steps taken per day?

daily_total <- activityDt %>% 
    group_by(date) %>% 
    summarise(count = sum(steps, na.rm = F))


ggplot(data = daily_total, aes(daily_total$count)) + 
    geom_histogram(bins = 20) +
    geom_vline(aes(xintercept = mean(daily_total$count), colour = "myline1"), color = "blue", show.legend = TRUE) + 
    geom_vline(xintercept = median(daily_total$count), aes(colour = "myline2"), color = "lightblue") + 
    scale_y_continuous(breaks = seq(0, 10, by = 2)) + 
    xlab("Number of steps taken per day") +
    ylab("Frequency") + 
    geom_text(aes(x=mean(daily_total$count)-1100, 
                  label=paste("mean: ", round(mean(daily_total$count), digits = 1), sep = ""), 
                  y = 11), size = 3) +
    geom_text(aes(x=median(daily_total$count)+1000, 
                  label=paste("median: ", round(median(daily_total$count), digits = 1), sep = ""), 
                  y = 10.5), size = 3) +
    theme_minimal(base_family = "Times") 




interval_total <- activityDt %>% 
    group_by(interval) %>% 
    summarise(avg_steps = mean(steps, na.rm = T))

#Find Interval That Has The Maximum Avg Steps
max <- interval_total[which.max(interval_total$avg_steps),]

#Generate Label for max point 
max_label <- paste("steps:", round(max$avg_steps), " | interval: ", max$interval, sep = "")

#plot the time series
ggplot(data = interval_total, aes(x = interval_total$interval, y = interval_total$avg_steps)) + 
    geom_line() + 
    geom_point(data = max, aes(x = max$interval, y = max$avg_steps), color = "red") + 
    geom_text(aes(label=ifelse(interval_total$avg_steps >= max$avg_steps, as.character(max_label),'')), 
              hjust = -.1, vjust = 0, color = "red") +
    xlab("5 Minute Time Interval") + 
    ylab("Average Number of Steps") + 
    labs(title="Average Steps by Time Interval") + 
    theme_minimal(base_family = "Times")

sum(is.na(activityDt$steps))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.

na_replace <- function(x, y) {
    if(is.na(x)) {
        return (y)
    }
    return (x)
}

# Create new data set by mergin original data set and avg_steps per day data set
activityDf2 <- merge(x = activityDt, y = interval_total, by = "interval", all = TRUE)

# Replace missing values with average in inverval
activityDf2$new_steps <- mapply(na_replace, activityDf2$steps, activityDf2$avg_steps)

# Clean up data set and order columns by date and interval
activityDf2 <- activityDf2 %>% select(interval = interval, date = date, steps = new_steps)
activityDf2 <- activityDf2[with(activityDf2, order(date, interval)), ]


# Function determinate if day is weekend or weekday 
is.weekend <- function(x) {
    if (weekdays(x) %in% c("Saturday", "Sunday")) {
        return ("Weekend")
    }
    return ("Weekday")
}

activityDf2$daytype <- as.factor(mapply(is.weekend, activityDf2$date))

interval_total <- activityDf2 %>% 
    group_by(daytype, interval) %>% 
    summarise(avg_steps = mean(steps, na.rm = T))


ggplot(data = interval_total, aes(x = interval_total$interval, y = interval_total$avg_steps)) + 
    geom_line() + 
    facet_grid(daytype ~ .) +
    xlab("5 Minute Time Interval") + 
    ylab("Average Number of Steps") + 
    labs(title="Average Steps by Time Interval") + 
    theme_minimal(base_family = "Times")
    
    
    geom_point(data = max_wd, aes(x = max$interval, y = max$avg_steps), color = "red") + 
    geom_text(aes(label=ifelse(interval_total$avg_steps >= max$avg_steps, as.character(max_label),'')), 
              hjust = -.1, vjust = 0, color = "red") +
    xlab("5 Minute Time Interval") + 
    ylab("Average Number of Steps") + 
    labs(title="Average Steps by Time Interval") + 
    theme_minimal(base_family = "Times")

max <- interval_total[which.max(interval_total$avg_steps),]

max <- aggregate(interval_total$avg_steps, by = list(interval_total$daytype), which.max)

tmp <- aggregate(interval_total$avg_steps, by = list(interval_total$daytype), length)

max_wd <- interval_total[max[1,]$x, ]
max_wn <- interval_total[(max[2,]$x + tmp[1, ]$x), ]
    
    
