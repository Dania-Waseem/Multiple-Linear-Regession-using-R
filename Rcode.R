data <- read.csv("C:/Users/DELL/Downloads/probProject spreadsheet - Sheet1.csv", header = TRUE)


cat("\n------------------------------------First Few Rows of Data-----------------------------------\n")
print(head(data))

cat("\n------------------------------------Last Few Rows--------------------------------------------\n")
print(tail(data))

cat("\n----------------------------------Structure of the Dataset-----------------------------------\n")
print(str(data))

cat("\n-----------------------------------Summary Statistics----------------------------------------\n")
print(summary(data))

cat("\n------------------------------------Number of Rows-------------------------------------------\n")
print(nrow(data))

cat("\n------------------------------------Number of Columns----------------------------------------\n")
print(ncol(data))

cat("\n----------------------------------Column Names-----------------------------------------------\n")
print(names(data))

cat("\n---------------------------------------------------------------------------------\n")

gender_counts <- table(data$Gender)

# Bar chart for gender 
barplot(gender_counts,
        names.arg = c("Male", "Female"),
        xlab = "Gender",
        ylab = "Count",
        col = c("blue", "pink"), 
        main = "Bar Chart for Gender",
        border = "black"
)  

#BOX PLot
boxplot_data <- data[, c("Weight..kg.", "Height.cm.", "Age", "Exercise.Week", 
                         "Sugar.taken.Week", "Steps.Day", "Screen.Time.hours.", "Meals.Day")]

# plotting area to arrange boxplots with 4 rows and 2 columns
par(mfrow = c(4, 2), mar = c(5, 4, 2, 1))  # margins to fit multiple plots


for (i in 1:ncol(boxplot_data)) {
  boxplot(boxplot_data[, i], 
          horizontal = TRUE, 
          col = rainbow(ncol(boxplot_data))[i],  
          main = names(boxplot_data)[i],  # the title to the column name
          xlab = "Values",  
          border = "black",  
          outline = TRUE,  
          notch = TRUE,  # notch for confidence interval
          cex.axis = 0.8  # axis label size
  )
}


#SCATTER PLOTS
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1)) 

independent_vars <- c("Height.cm.", "Age", "Exercise.Week", "Sugar.taken.Week", 
                      "Steps.Day","Meals.Day","Screen.Time.hours.")

for (var in independent_vars) {
  plot(data[[var]], data$Weight..kg., 
       main = paste("Weight vs", var),  # Title for each plot
       xlab = var,                     
       ylab = "Weight (kg)",           
       col = "blue",                   # Color of the points
       pch = 19,                       # Type of point
       cex = 1.1)                      # Size of the points
}


height <- data$Height.cm.
sugary_foods <- data$Sugar.taken.Week
meals <- data$Meals.Day
weight <- data$Weight..kg.

# DataFrame
LDF <- data.frame(Height = height, Sugary_Foods = sugary_foods,
                  Meals = meals, Weight = weight)

# Scatter Plots
pairs(LDF)

# Correlation Matrix
cat("Correlation Matrix:\n")
print(cor(LDF))

# Multiple Linear Regression Model
MLRM <- lm(Weight ~ Height + Sugary_Foods + Meals, data = LDF)
cat("Multiple Linear Regression Model Summary:\n")
print(summary(MLRM))

cat("\n----------------------------Summary stats separately for MLRM variables----------------------------------\n")

height_mean <- mean(data$Height.cm.)
height_median <- median(data$Height.cm.)
height_q1 <- quantile(data$Height.cm., 0.25)  
height_q3 <- quantile(data$Height.cm., 0.75)  
height_iqr <- IQR(data$Height.cm.)

sugar_mean <- mean(data$Sugar.taken.Week)
sugar_median <- median(data$Sugar.taken.Week)
sugar_q1 <- quantile(data$Sugar.taken.Week, 0.25)
sugar_q3 <- quantile(data$Sugar.taken.Week, 0.75)
sugar_iqr <- IQR(data$Sugar.taken.Week)

meals_mean <- mean(data$Meals.Day)
meals_median <- median(data$Meals.Day)
meals_q1 <- quantile(data$Meals.Day, 0.25)
meals_q3 <- quantile(data$Meals.Day, 0.75)
meals_iqr <- IQR(data$Meals.Day)

weight_mean <- mean(data$Weight..kg.)
weight_median <- median(data$Weight..kg.)
weight_q1 <- quantile(data$Weight..kg., 0.25)
weight_q3 <- quantile(data$Weight..kg., 0.75)
weight_iqr <- IQR(data$Weight..kg.)

# Display the results
cat("Height Summary Statistics:\n")
cat("Mean:", height_mean, "Median:", height_median, 
    "Q1:", height_q1, "Q3:", height_q3, "IQR:", height_iqr, "\n\n")

cat("Sugar Intake Summary Statistics:\n")
cat("Mean:", sugar_mean, "Median:", sugar_median,  
    "Q1:", sugar_q1, "Q3:", sugar_q3, "IQR:", sugar_iqr, "\n\n")

cat("Meals Summary Statistics:\n")
cat("Mean:", meals_mean, "Median:", meals_median, 
    "Q1:", meals_q1, "Q3:", meals_q3, "IQR:", meals_iqr, "\n\n")

cat("Weight Summary Statistics:\n")
cat("Mean:", weight_mean, "Median:", weight_median,  
    "Q1:", weight_q1, "Q3:", weight_q3, "IQR:", weight_iqr, "\n\n")

cat("\n----------------------------Predictions---------------------------------------\n")


predicted_weight <- predict(MLRM, list(Height = 190, Sugary_Foods = 20, Meals = 6))

print(predicted_weight)