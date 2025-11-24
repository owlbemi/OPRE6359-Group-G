
# Vinisha's Decision Tree Section

# Load libraries
library(rpart)
library(rpart.plot)

# Keep only numeric predictors + target
df_numeric <- df[, c(
  "Job.Status",
  "Median.Salary..USD.",
  "Experience.Required..Years.",
  "Job.Openings..2024.",
  "Projected.Openings..2030.",
  "Remote.Work.Ratio....",
  "Automation.Risk....",
  "Gender.Diversity...."
)]

# Check structure
str(df_numeric)

# Split into training and test sets (2/3 training, 1/3 test)
set.seed(345)
train_index <- sample(1:nrow(df_numeric), nrow(df_numeric) * (2/3))
train_data <- df_numeric[train_index, ]
test_data  <- df_numeric[-train_index, ]

# Fit the decision tree
fit_numeric <- rpart(
  Job.Status ~ ., 
  data = train_data,
  method = "class",
  control = rpart.control(minsplit = 200, cp = 0.001, maxdepth = 4),
  parms = list(split = "gini")
)

# Check the fit
print(fit)

# Plot the tree
rpart.plot(fit_numeric, type = 1, extra = 2, cex = 0.6)

# Predict on test data and create confusion matrix
jobstatus.pred <- predict(fit_numeric, newdata = test_data, type = "class")
jobstatus.actual <- test_data$Job.Status
confusion.matrix <- table(jobstatus.pred, jobstatus.actual)
confusion.matrix

# Accuracy on Training Data
jobstatus.pred <- predict(fit_numeric, train_data, type = "class")
jobstatus.actual <- train_data$Job.Status
train_accuracy <- sum(jobstatus.pred == jobstatus.actual) / nrow(train_data)
print(paste("Training Accuracy:", round(train_accuracy, 3)))


# Accuracy on Testing Data
jobstatus.pred <- predict(fit_numeric, test_data, type = "class")
jobstatus.actual <- test_data$Job.Status
test_accuracy <- sum(jobstatus.pred == jobstatus.actual) / nrow(test_data)
print(paste("Testing Accuracy:", round(test_accuracy, 3)))

