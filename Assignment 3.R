rm(list=ls(all=TRUE))
library(tidyverse)  
RESULTS <- read_csv("PRESIDENT_precinct_primary.csv")
if (!require("validate")) install.packages("validate")
library("validate")

RESULTS_replaced <- mutate(RESULTS,candidate=replace(candidate,candidate=="BIDEN / HARRIS","JOSEPH R BIDEN"),
candidate=replace(candidate, candidate =="BIDEN AND HARRIS","JOSEPH R BIDEN"),
candidate=replace(candidate,candidate=="JOE BIDEN","JOSEPH R BIDEN"),
candidate=replace(candidate,candidate=="JOSEPH R BIDEN JR","JOSEPH R BIDEN"),
candidate=replace(candidate,candidate=="JOSEPH R BIDEN / KAMALA HARRIS","JOSEPH R BIDEN"),
candidate=replace(candidate,candidate=="JOSEPH R BIDEN/KAMALA HARRIS","JOSEPH R BIDEN"),
candidate=replace(candidate, candidate=="JOSEPH ROBINETTE BIDEN", "JOSEPH R BIDEN"),
candidate=replace(candidate,candidate=="TRUMP / PENCE","DONALD J TRUMP"),
candidate=replace(candidate,candidate=="TRUMP AND PENCE","DONALD J TRUMP"),
candidate=replace(candidate,candidate=="DONALD TRUMP","DONALD J TRUMP"),
candidate=replace(candidate,candidate=="DONALD J TRUMP / MICHAEL R PENCE","DONALD J TRUMP"),
candidate=replace(candidate,candidate=="DONALD J TRUMP/MICHAEL R PENCE","DONALD J TRUMP"))


View(RESULTS_replaced)

#making candidate and state factor variables
str(RESULTS_replaced$candidate)
str(RESULTS_replaced$state)


RESULTS_replaced$candidate <- as.factor(RESULTS_replaced$candidate)
RESULTS_replaced$state <- as.factor(RESULTS_replaced$state)

#checking to see if there are duplicates
STATES <- levels(RESULTS_replaced$state)
CANDIDATES <- levels(RESULTS_replaced$candidate)


#finding votes per candidate per state
RESULTS_replaced<-group_by(RESULTS_replaced,state,candidate)
COUNTS1 <- summarise(RESULTS_replaced,votes = sum(votes))
View(COUNTS1)

RESULTS_replaced<-group_by(RESULTS_replaced,state)
COUNTS2 <- summarise(RESULTS_replaced,votes = sum(votes))
View(COUNTS2)

library(ggplot2)
ggplot(COUNTS2, aes(x =state, y =votes)) +
  geom_point()+
  labs(x = "State", y = "Votes",
       title = "Votes in each state",
       subtitle = "Colored by winner") +
  geom_point(color = "blue")


#END OF SCRIPT
#below is all my trials trying to get the right graph with the variables I was using.
#I had a very difficult time and wanted to show how much effort I put into making this graph
#Before professor posted the start scrip I tried really hard so it was to late







n_states <- length(levels(RESULTS$state))

CAND_CONS <- levels(as.factor(RESULTS_replaced$cand_cons))
STATES <- levels(RESULTS$state)
CANDIDATES <- levels(RESULTS_replaced$candidate)

n_COUNTS2 <- length(RESULTS_replaced$state)
COUNTS3 <- data.frame(STATES)
COUNTS3$votes <- 0
COUNTS3$winner <- NA
for (i in 1:n_states) {
  BIDEN <- 0
  TRUMP <- 0
  VOTES <- 0
  for (j in 1:n_COUNTS2){
    if (RESULTS_replaced$STATES[j]==COUNTS3$STATES[i]){
      VOTES <- VOTES + RESULTS_replaced$votes[j]
      if (RESULTS_replaced$candidate[j]=="BIDEN") {
        BIDEN <- BIDEN + RESULTS_replaced$votes[j]
      }
      else if (RESULTS_replaced$candidate[j]=="TRUMP") {
        TRUMP <- TRUMP + RESULTS_replaced$votes[j]
      }
      else {}
    }
  }
  COUNTS3$votes[i] <- VOTES
  if (BIDEN > TRUMP) {
    COUNTS3$winner[i] <- "BIDEN"
  }
  else COUNTS3$winner[i] <- "TRUMP"
}

library(ggplot2)
P <-ggplot(COUNTS2, aes(x =state, y =votes)) +
  geom_point()
p + geom_point()+
  labs(x = "State", y = "Votes",
       title = "Votes in each state",
       subtitle = "Colored by winner")


# We'll use package readr, which is part of the tidyverse
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Reading the PRESIDENT_precinct_primary.csv file in as a data frame
# RESULTS <- read_csv("PRESIDENT_precinct_primary.csv")

# Or, if you'd prefer to keep the data file zipped to keep it smaller....
RESULTS <- read_csv(unz("PRESIDENT_precinct_primary.csv.zip", "PRESIDENT_precinct_primary.csv")) 

# Making candidate and state factor variables
RESULTS$candidate <- as.factor(RESULTS$candidate)
RESULTS$state <- as.factor(RESULTS$state)
n_candidates <- length(levels(RESULTS$candidate))
n_states <- length(levels(RESULTS$state))

# dplyr approach
RESULTS <- group_by(RESULTS, state, candidate)
COUNTS1 <- summarise(RESULTS, votes = sum(votes))

# Determining unique values for states and candidates factors
STATES <- levels(RESULTS$state)
CANDIDATES <- levels(COUNTS1$candidate)

# Replacing multiple variables/instances 
COUNTS1 <- mutate(COUNTS1,cand_cons=case_when(candidate =="JOSEPH R BIDEN" ~ "BIDEN",
                                              candidate == "DONALD J TRUMP" ~ "TRUMP",
                                              candidate == "JOSEPH R BIDEN/KAMALA HARRIS" ~ "BIDEN",
                                              candidate == "JOSEPH R BIDEN JR" ~ "BIDEN",
                                              candidate == "BIDEN / HARRIS" ~ "BIDEN",
                                              candidate == "BIDEN AND HARRIS" ~ "BIDEN",
                                              candidate == "JOE BIDEN" ~ "BIDEN",
                                              candidate == "JOSEPH ROBINETTE BIDEN" ~ "BIDEN",
                                              candidate == "DONALD J TRUMP/MICHAEL R PENCE" ~ "TRUMP",
                                              candidate == "TRUMP / PENCE" ~ "TRUMP",
                                              candidate == "TRUMP AND PENCE" ~ "TRUMP",
                                              TRUE ~ "OTHER"))


CAND_CONS <- levels(as.factor(RESULTS_replaced$cand_cons))
CAND_CONS

RESULTS <- group_by(COUNTS1, state, cand_cons)
COUNTS2 <- summarise(RESULTS, votes = sum(votes))

n_COUNTS2 <- length(CANDIDATES$state)
COUNTS3 <- data.frame(STATES)
COUNTS3$votes <- 0
COUNTS3$winner <- NA
for (i in 1:n_states) {
  BIDEN <- 0
  TRUMP <- 0
  VOTES <- 0
  for (j in 1:n_COUNTS2){
    if (COUNTS2$state[j]==COUNTS3$STATES[i]){
      VOTES <- VOTES + COUNTS2$votes[j]
      if (COUNTS2$cand_cons[j]=="BIDEN") {
        BIDEN <- BIDEN + COUNTS2$votes[j]
      }
      else if (COUNTS2$cand_cons[j]=="TRUMP") {
        TRUMP <- TRUMP + COUNTS2$votes[j]
      }
      else {}
    }
  }
  COUNTS3$votes[i] <- VOTES
  if (BIDEN > TRUMP) {
    COUNTS3$winner[i] <- "BIDEN"
  }
  else COUNTS3$winner[i] <- "TRUMP"
}

library(ggplot2)
ggplot(COUNTS3, aes(x =n_states, y =COUNTS3)) +
  geom_point()

