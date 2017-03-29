library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# LOADING

# Original data
jobs <- read_excel("uber-job-listings-thinknum-all.xlsx")

# Remove unneeded columns and clean up column names
jobs.clean <- select(jobs,
                     unique.id = `Unique ID`,
                     listing.id = `Listing ID`,
                     as.of.date = `As Of Date`,
                     title = Title,
                     category = Category,
                     city = City,
                     state = State,
                     country = Country,
                     posted.date = `Posted Date`)

# Convert discrete labels to factors
jobs.clean$category <- as.factor(jobs.clean$category)
jobs.clean$city <- as.factor(jobs.clean$city)
jobs.clean$state <- as.factor(jobs.clean$state)
jobs.clean$country <- as.factor(jobs.clean$country)

# VALIDATION

# There are jobs that change category...
jobs.clean %>%
  group_by(listing.id) %>%
  distinct(category) %>%
  summarise(count=n()) %>%
  filter(count > 1)

# And many jobs that change post date...
# Example of a job that is re-opened: https://careers-uber.icims.com/jobs/28368/job/login
# Starts 2/22/17, re-opened 3/8/17, last seen 3/24/17
jobs.clean %>%
  group_by(listing.id) %>%
  distinct(posted.date) %>%
  summarise(count=n()) %>%
  filter(count > 1)

# SIMPLIFICATION

# Filter to only the most recent observation of each job opening
jobs.reduced <- jobs.clean %>%
  filter(!is.na(posted.date), !is.na(as.of.date)) %>%
  group_by(listing.id) %>%
  summarise(
    title = first(title),
    category = first(category),
    city = first(city),
    country = first(country),
    first.posted = min(posted.date),
    last.seen = max(as.of.date)
  )

# EXPORT USEFUL SUBSETS

# Country
jobs.by.country <- jobs.reduced %>%
  group_by(country) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

write.csv(jobs.by.country, "jobs.by.country.csv")

# City
jobs.by.city <- jobs.reduced %>%
  group_by(country, city) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

write.csv(jobs.by.city, "jobs.by.city.csv")

# Category
jobs.by.category <- jobs.reduced %>%
  group_by(category) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

write.csv(jobs.by.category, "jobs.by.category.csv")

# Title
jobs.by.title <- jobs.reduced %>%
  group_by(title) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

write.csv(jobs.by.title, "jobs.by.title.csv")

# ANALYZE JOB TURNOVER

MakeJobCounter <- function(jobs.data) {
  CountJobs <- function(d) {
    dJobs <- jobs.data %>%
      filter(d >= first.posted, d <= last.seen)
    
    return(nrow(dJobs))
  }
  
  return(CountJobs)
}

dates = seq(
  min(jobs.reduced$last.seen),
  max(jobs.reduced$last.seen),
  by="day"
)

# Open jobs
jobs.open <- tibble(
  date=dates,
  open=sapply(dates, MakeJobCounter(jobs.reduced))
)

# Opened (new) jobs
jobs.opened <- jobs.reduced %>%
  group_by(first.posted) %>%
  summarise(opened=n()) %>%
  filter(first.posted > dates[1])

# Closed jobs
jobs.closed <- jobs.reduced %>%
  group_by(last.seen) %>%
  summarise(closed=n()) %>%
  filter(last.seen < dates[length(dates)])

jobs.counts <- merge(x=jobs.open, y=jobs.opened, by.x="date", by.y="first.posted", all=TRUE)
jobs.counts <- merge(x=jobs.counts, y=jobs.closed, by.x="date", by.y="last.seen", all=TRUE)
jobs.turnover <- gather(jobs.counts, jobs, count, opened:filled)

ggplot(jobs.open, aes(x=date, y=open)) +
  geom_line() +
  ggtitle("Open Uber jobs by date") +
  labs(x="Date", y="Open jobs")

# ANALYZE JOB CATEGORIES

# CountOpenJobsByCategory <- function(d) {
#   dJobs <- jobs.reduced %>%
#     filter(d >= first.posted, d <= last.seen) %>%
#     group_by(category) %>%
#     summarise(open=n()) %>%
#     mutate(date=d)
#   
#   return(dJobs)c("#FFFFFF")
# }

# jobs.open.by.category <- do.call(rbind, sapply(dates, CountOpenJobsByCategory, simplify=FALSE))
# 
# ggplot(jobs.open.by.category, aes(x=date, y=open, color=category)) +
#   geom_line() +
#   ggtitle("Uber jobs") +
#   labs(x="Date", y="Jobs", color="Series")

# ANALYZE PITTSBURGH (SELF-DRIVING) JOBS

jobs.pittsburgh <- jobs.reduced %>%
  filter(city == "Pittsburgh")

jobs.pittsburgh.open <- tibble(
  date=dates,
  open=sapply(dates, MakeJobCounter(jobs.pittsburgh))
)

ggplot(jobs.pittsburgh.open, aes(x=date, y=open)) +
  geom_line() +
  ggtitle("Open Uber jobs in Pittsburgh by date") +
  labs(x="Date", y="Open jobs")

# ANALYZE LEASING / XCHANGE JOBS

jobs.leasing <- jobs.reduced %>%
  filter(grepl("xchange", title, ignore.case=TRUE) | grepl("leasing", title, ignore.case=TRUE))

jobs.leasing.open <- tibble(
  date=dates,
  open=sapply(dates, MakeJobCounter(jobs.leasing))
)

ggplot(jobs.leasing.open, aes(x=date, y=open)) +
  geom_line() +
  ggtitle("Open Uber jobs with either 'xchange' or 'leasing' in title") +
  labs(x="Date", y="Open jobs")
