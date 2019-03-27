data <- read.csv(choose.files(), stringsAsFactors = FALSE)
sapply(data, class)
for (i in 4:7){data[, i] = as.Date(data[, i], origin = as.Date('1899-12-30'), na.rm = T)}
for (i in 8:10){data[, i] = as.numeric(data[, i], na.rm = T)}
sapply(data, class)
set.seed(21521)
questions <- sort(sample(c(1:10), 3, replace = F))
questions
included_data <- data[which(data$approval.date < as.Date("2017-01-01") | data$approval.date > as.Date("1995-01-01")), ]
# Question 3
sum(is.na(included_data)) # find the number of NA data
# Question 8
d <- density(included_data$project.budget)
polygon(d, main="Project budget", xlab="budget", col="red", border="black")
# Question 9
assessed_projects <- sum(included_data$success.rating == 0 | included_data$success.rating == 1, na.rm = T)
completed_projects <- length(included_data$revised.completion.date) - sum(is.na(included_data$revised.completion.date))
fraction <- assessed_projects / completed_projects
fraction