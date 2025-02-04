library(dplyr)

setwd("/Users/kevin2/Dropbox/Documents/Teaching Materials/Health Policy/Course Website/hpam7660/assignments/Case")
students <- read.csv("students.csv", stringsAsFactors = FALSE)

if (!"Name" %in% colnames(students)) {
  stop("The CSV file must have a column named 'Name'")
}

set.seed(8675309)
students <- students[sample(nrow(students)), , drop = FALSE]

students$Team <- rep(1:5, each = 4) 

print(students)