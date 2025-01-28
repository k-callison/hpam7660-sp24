#Open intro_data.csv file in R
intro <- read.csv("data/intro_data.csv")

#View the data frame to make sure it loaded properly
View(intro)

#Construct a bar chart that displays the average response score for each question
#First calculate averages and standard deviations for each question
avg_values <- colMeans(intro)
std_dev <- apply(intro, 2, sd)

#Then set the bar width and create the bar chart
bar_width <- 0.8
bar_centers <- barplot(avg_values, names.arg = c("Collection", "Import", "Cleaning", "Visualization", "Memo" ), main = "Intro Scenario Scores", 
        ylab = "Average Score", ylim = c(0,5), cex.name = 0.8)
#The central command is `barplot(avg_values)`
#The `names.arg` option allows me to rename the columns
#The `main` option provides the title for the figure
#The `ylab` option provides the label for the y-axis
#The `ylim` option sets the minimum and maximum values for the y-axis
#The `cex.name` option adjusts the size of the y-axis lables

#This code block adds the standard deviation above each column 
for (i in seq_along(avg_values)) {
  text(bar_centers[i], avg_values[i] + 0.1, sprintf("SD=%.2f", std_dev[i]), pos = 3, col = "red", cex = 0.8)
}
#The `for (i)` command is a loop that loops over the indices of avg_values
#The `bar_centers` command tells R to place the label above the center of each bar
#I wanted the labels to be slightly above each bar, so I placed them at the level of `avg_values[i]` and then added 0.1
#The `sprintf()` command creates the string value for the label, which is SD=, and the std_dev[i] fills in the standard deviation value
#The `pos`, `col`, and `cex` commands adjust the position, color, and size of the labels

