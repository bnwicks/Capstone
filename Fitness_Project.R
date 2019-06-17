## Fitness Project
#  Byron Wicks
#  bnwicks@gmail.com
#  https://github.com/bnwicks

# Description
# This project explores fitness data collected by Strava from the previous 3 years of exercise, both running and riding. It will review, and attempt to create a metric and to understand what factors influence performance.

## Ver 1.0 - Byron Wicks (bnwicks)

# Libraries
#####################################################

devtools::install_github("marcusvolz/strava") ## Marcus Volz strava functions
library(tidyverse)
library(gridExtra)
library(strava)
library(lubridate)
library(gtools)
library(sugrrants)
library(DescTools)

# Investigate Data
#####################################################

# Import and Process data
#####################################################

# if(FALSE) will stop R searching for personal data downloaded from Strava.
# "StravaData.Rdata" uploaded to github
# Excluded from Rmd file

if(FALSE) 
{
data_1 <- process_data("./data/activities/")
data_2 <- read.csv(file = "data/activities.csv")

# Data 1 Type is wrong
data_1$type <- NULL

# Correct slow, short runs to walk
data_2 <- data_2 %>% 
  filter(distance > 1000) %>%
  mutate(type = ifelse(distance < 2500, "Walk", paste(type))) %>%
  mutate(type = ifelse((elapsed_time/60) / (distance / 1000) > 10, "Walk", paste(type))) %>%
  mutate(id = row_number())

# Merge data_1 and data_2
data <- merge(data_1, data_2, by="id")

# Save data
save(data, file = "StravaData.Rdata")
}

#Load Data
load(file = "StravaData.Rdata")

# Summarise data
data_summary <- data %>%
  mutate(time = lubridate::date(data$time),
         year = strftime(data$time, format = "%Y"),
         date_without_month = strftime(data$time, format = "%j"),
         month = strftime(data$time, format = "%m"),
         day_of_month = strftime(data$time, format = "%d"),
         year_month = strftime(data$time, format = "%Y-%m"),
         type = type) %>%
  group_by(time, year, date_without_month, month, day_of_month, year_month) %>%
  summarise(total_dist = sum(dist_to_prev), total_time = sum(time_diff_to_prev), type = type[1]) %>%
  mutate(speed = (total_dist) / (total_time /60^2)) %>%
  mutate(pace = (total_time / 60) / (total_dist)) %>%
  ungroup %>%
  mutate(id = as.numeric(row.names(.)))

# Plot Calendar
#####################################################

# Generate plot data
time_max <- today()

daily_data <- data_summary %>%
  group_by(time) %>%
  mutate(dist = sum(total_dist), type = type[1]) %>%
  ungroup() %>%
  mutate(time = lubridate::date(time))

daily_data <- daily_data %>%
  group_by(type) %>%
  mutate(max_distance = max(dist))

daily_data <- daily_data %>%
  group_by(time) %>%
  mutate(scaled_distance = dist / max_distance)

daily_data_cal <- daily_data %>%  
  frame_calendar(x = time, y = 1, date = time, calendar = "monthly") %>%
  transform(week = as.POSIXlt(time)$yday %/% 7 + 1,
            wday = weekdays(as.POSIXlt(time)),
            year = as.POSIXlt(time)$year + 1900)

# Reverse days for graphing
daily_data_cal$wday <- factor(daily_data_cal$wday, day.name[7:1])

# Graph data
p <- daily_data_cal %>% 
  ggplot(aes(x = week, y = wday, fill = type, alpha=scaled_distance)) + 
  geom_tile(size=1) +
  scale_fill_manual(values=c(Ride="red", Run="blue", Walk="green", Hike="yellow")) +
  ylab("Weekday") +
  xlab("Month") +
  theme(legend.position="right") +
  scale_x_continuous(breaks = (c(0:11)*52/12), labels = month.abb) +
  facet_wrap(~year, ncol = 1)
p

# Save plot
ggsave(paste("calendar",Sys.Date(),".png"), p, width = 30, height = 30, units = "cm", dpi = 300)

# Plot Facets
#####################################################

data_facets <- data %>%
  group_by(id) %>%
  summarise(lon_avg = mean(lon),
            lat_avg = mean(lat),
            time = max(elapsed_time),
            distanceKm = max(distance/1000),
            timePerKm = round((time) / (distanceKm)),
            description = paste(type[1],"\n",as.Date(date[1]),"\n",round(distanceKm),"km\n", tolower(seconds_to_period(timePerKm)), "/ km"))

p <- data %>% ggplot(aes(x = lon, y = lat, group = id, col = type)) +
  geom_point() +
  facet_wrap(~id, scales = "free") +
  theme_void() +
  ggtitle("Facets") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), 
        strip.text = element_blank(), 
        plot.margin = unit(rep(1, 4), "cm"),
        legend.position="right") +
  geom_text(aes(lon_avg, lat_avg, label = description), data = data_facets, alpha = 0.25, size = 3, color = "black")
p

# Save plot
ggsave(paste("facets",Sys.Date(),".png"), p, width = 50, height = 50, units = "cm")


# Create Fitness Metrics 
#####################################################

# Fitness, Freshness and Form Graphs
# Analyse and plot graph of fitness and freshness using Strava running and cycling data

# Assumptions
impulse_runningPerKm <- 13 # between ~10 - 15
impulse_ridingPerKm <-  3.3 # between ~3 - 5

# Fitness constants 
#fitness = 1*impulse, -20% per week, ~2% per day, use ~3 months data
fitness_constant <- 0.98
impulse_to_fitness <- 0.02

# Fatigue constants
# fatigue = 5*impulse, -80% per week, ~15% per day, use ~3 months data
fatigue_constant <- 0.85
impulse_to_fatigue <- 0.125

# Form
# form = difference between fitness and fatigue

# Calculate form, fitness and fatigue for each day
df_raw <- read.csv(file = "./data/activities.csv") # read data

#Specify Start and End dates
start_date <- min(as.numeric(as.Date(df_raw$date)))
end_date <- max(as.numeric(as.Date(df_raw$date)))

df_raw <- df_raw %>%
  filter(as.numeric(as.Date(date)) >= start_date)

# Impulse
df <- df_raw %>%
  mutate(impulse = ifelse(type=="Ride", impulse_ridingPerKm * distance / 1000, impulse_runningPerKm * distance / 1000))

plot <- df %>% 
  ggplot(aes(x = as.Date(date), y = impulse, color = type)) +
  geom_point()

plot
# Save plot
ggsave(paste("Impulse_Date",Sys.Date(),".png"), plot, width = 50, height = 50, units = "cm")

#Function - Fitness
f_fitness <- function(i_date) {
  df_temp <- df %>%
    mutate(date = as.Date(date)) %>%
    filter(as.numeric(date) - as.numeric(as.Date(i_date, origin = "1970-01-01")) <= 0) %>%
    mutate(relative_impulse = 0) %>%
    mutate(relative_impulse = impulse * impulse_to_fitness * fitness_constant^(as.numeric(as.Date(i_date, origin = "1970-01-01") - as.numeric(date))))
  return(sum(df_temp$relative_impulse))
}

# Function - Fatigue
f_fatigue <- function(i_date) {
  df_temp <- df %>%
    mutate(date = as.Date(date)) %>%
    filter(as.numeric(date) - as.numeric(as.Date(i_date, origin = "1970-01-01")) <= 0) %>%
    mutate(relative_impulse = 0) %>%
    mutate(relative_impulse = impulse * impulse_to_fatigue * fatigue_constant^(as.numeric(as.Date(i_date, origin = "1970-01-01") - as.numeric(date))))
  return(sum(df_temp$relative_impulse))
}

# Update df with Fitness, Fatigue, Form Metrics
df <- df %>%
  mutate(fitness = sapply(date, f_fitness)) %>%
  mutate(fatigue = sapply(date, f_fatigue)) %>%
  mutate(form = fitness - fatigue)

# Function - Form, Fitness and Fatigue
#####################################################

df_fff <- data_frame("date" = start_date:end_date)

df_fff <- df_fff %>%
  mutate(fitness = sapply(date, f_fitness)) %>%
  mutate(fatigue = sapply(date, f_fatigue)) %>%
  mutate(form = fitness - fatigue)

# Plot for fitness and fatigue data
df_tidy <- df_fff %>% gather(type = fitness:form)

plot_1 <- df_tidy %>% ggplot(aes(x = as.Date(date, origin = "1970-01-01"), y = value, col = key)) +
  geom_line(size=1)

plot_2 <- df %>% ggplot(aes(x = as.Date(date, origin = "1970-01-01"), y = impulse, fill=type)) +
  geom_bar(stat = "identity")

grid.arrange(plot_1, plot_2, ncol = 1)

# Plot Type, Fitness Impulse, Linear approximation
plot_2 <- df %>% ggplot(aes(x = fitness, y = impulse, color = type)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ x, fullrange=TRUE)
plot_2

# Plot Attributes against type
plot_3 <- df %>% ggplot(aes(x = distance, y = type)) +
  geom_point()

plot_4 <- df %>% ggplot(aes(x = fitness, y = type)) +
  geom_point()

plot_5 <- df %>% ggplot(aes(x = impulse, y = type)) +
  geom_point()

grid.arrange(plot_3, plot_4, plot_5, ncol = 1)

# Create Fitness Machine Learning Models 
#####################################################
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))
smp_size

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

df_train <- df[train_ind, ]
df_test <- df[-train_ind, ]

# KNN 
#####################################################


train_knn <- train(type ~ distance, method = "knn", 
                   data = df_train,
                   tuneGrid = data.frame(k = c(1:100)))
y_hat_knn <- predict(train_knn, df_test)
y_hat_knn
cm_knn <- confusionMatrix(data = y_hat_knn, reference = df_test$type)
cm_knn$overall
train_knn
plot(train_knn)

df_cm <- as.data.frame(cm_knn$table)

df_cm %>% ggplot(aes(Prediction, Reference)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq))

# Plot descision boundary
lgrid <- expand.grid(distance=seq(0, 100000, by=100))
knnPredGrid <- predict(train_knn, newdata=lgrid)

knnPred <- cbind(lgrid, knnPredGrid)

knnPred %>% ggplot(aes(x = distance, y = knnPredGrid)) +
  geom_point()

######################################################

train_knn <- train(type ~ fitness + impulse, method = "knn", 
                   data = df_train,
                   tuneGrid = data.frame(k = c(1:20)*5))
y_hat_knn <- predict(train_knn, df_test)
y_hat_knn
cm_knn <- confusionMatrix(data = y_hat_knn, reference = df_test$type)
cm_knn$overall

train_knn
plot(train_knn)

df_cm <- as.data.frame(cm_knn$table)

df_cm %>% ggplot(aes(Prediction, Reference)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq))

# Plot descision boundary
lgrid <- expand.grid(fitness=seq(0, 50, by=1),
                     impulse=seq(0, 250,by=1))
knnPredGrid <- predict(train_knn, newdata=lgrid)

knnPred <- cbind(lgrid, knnPredGrid)

plot <- knnPred %>% ggplot(aes(x = fitness, y = impulse, color=knnPredGrid)) +
  geom_point(size=5)
plot

# QDA 
#####################################################

train_qda <- train(type ~ distance, data = df_train)
train_qda
y_hat <- predict(train_qda, df_test)
y_hat

cm <- confusionMatrix(data = y_hat, reference = df_test$type)
cm$overall

df_cm <- as.data.frame(cm$table)

df_cm %>% ggplot(aes(Prediction, Reference)) + 
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq))

# Plot descision boundary
lgrid <- expand.grid(distance=seq(0, 100000, by=10))
qdaPredGrid <- predict(train_qda, newdata=lgrid)

qdaPred <- cbind(lgrid, qdaPredGrid)

p <- qdaPred %>% ggplot(aes(x = distance, y = qdaPredGrid)) +
  geom_point()
p

# Conclusion
#####################################################
# My Strava data was investigated, metrics for fitness and freshness implemented and models created to predict my performance and type of activity. Overall a lot of fun was had.


