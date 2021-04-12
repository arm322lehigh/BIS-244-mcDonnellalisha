#Alisha McDonnell Midterm R.


# Clear console
rm(list=ls(all=TRUE))
cat("\014")

# Load packages
library(gapminder)
library(here)
library(tidyverse)

# Reading in file

midterm.r <- read.csv("AAPL.csv")

View(midterm.r)

n <- length(midterm.r$Adj.Close)


# Initialize new variable in data frame
midterm.r$incr_cases <- 1

View(midterm.r)

# Calculate values for other than first row using FOR loop
for (i in 2:n) {
  midterm.r$incr_cases[i] <- (midterm.r$Adj.Close[i]-midterm.r$Adj.Close[i-1]) 
}

# Plotting
p <- ggplot(data = midterm.r,
            mapping = aes(x = Date,
                          y = incr_cases))
p + geom_point() +
  labs(x = "03/19/2020-03/18/2021", y = "Change in Adjusted Closing Price",
       title = "Changes in AAPL Daily Prices over Last Year",
       subtitle = "Alisha McDonnell",
       caption = "Exam 1")

#finding the mean
mean(midterm.r$incr_cases)

mean_adj <-mean(midterm.r$incr_cases)

for (i in 1:n) {
  if(midterm.r$incr_cases[i]>=mean_adj) {
    midterm.r$above_cases[i] <- midterm.r$incr_cases[i]
  } else {
    midterm.r$below_cases[i] <- midterm.r$incr_cases[i]
  }
}
View(midterm.r)

#replacing all the values of 1 and -3.855114 to get rid of the ugly lines on the graph
for (i in 1:n) {
  if(SNOHOMISH$above_cases[i]==1) {
    SNOHOMISH$above_cases[i] <- NA
  } else {}
  if(SNOHOMISH$below_cases[i]==-3.855114) {
    SNOHOMISH$below_cases[i] <- NA
  } else {}
}



# Plotting

p <- ggplot(data = midterm.r,
            mapping = aes(x = Date,
                          y = incr_cases))
p = ggplot() + 
  geom_point(data = midterm.r, aes(x = Date, y = above_cases), color = "blue") +
  geom_point(data = midterm.r, aes(x = Date, y = below_cases), color = "red") +
  labs(x = "03/19/2020-03/18/2021", y = "Change in Adjusted Closing Price",
       title = "Changes in AAPL Daily Prices over Last Year",
       subtitle = "Blue = Up, Red = Down, Alisha McDonnell",
       caption = "Exam 1")

p



##############################



