############################################################
# 📌 Nivas – Decision Tree: Plot + Interpretation + Accuracy
############################################################

# ========= 0. PACKAGES & DATA LOADING =========

# Install if needed:
# remotes::install_github("benyamindsmith/RKaggle")
# install.packages(c("dplyr","tidyr","caret","rpart","rpart.plot"))

library(RKaggle)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)

# Download dataset directly from Kaggle
raw_df <- get_dataset("sahilislam007/ai-impact-on-job-market-20242030")

# Use a clean copy
df <- raw_df

# ========= 1. DATA CLEANING (Factors) ==========

factor_cols <- c("Job Status", "Required Education", "AI Impact Level", "Industry")
df[factor_cols] <- lapply(df[factor_cols], as.factor)

# Order factor levels
df$`AI Impact Level` <- factor(
  df$`AI Impact Level`,
  levels = c("Low", "Moderate", "High"),
  ordered = TRUE
)

df$`Required Education` <- factor(
  df$`Required Education`,
  levels = c("High School","Associate Degree","Bachelor’s Degree","Master’s Degree","PhD"),
  ordered = TRUE
)

# ========= 2. SELECT NUMERIC FEATURES =========

df_numeric <- df[, c(
  "Job Status",
  "Median Salary (USD)",
  "Experience Required (Years)",
  "Job Openings (2024)",
  "Projected Openings (2030)",
  "Remote Work Ratio (%)",
  "Automation Risk (%)",
  "Gender Diversity (%)"
)]

# ========= 3. TRAIN/TEST SPLIT =========

set.seed(345)
train_index <- sample(1:nrow(df_numeric), nrow(df_numeric) * 2/3)
train_data  <- df_numeric[train_index, ]
test_data   <- df_numeric[-train_index, ]

# ========= 4. FIT DECISION TREE =========

fit_numeric <- rpart(
  `Job Status` ~ .,
  data   = train_data,
  method = "class",
  control = rpart.control(minsplit = 200, cp = 0.001, maxdepth = 4),
  parms = list(split = "gini")
)

# ========= 5. 🚩 (3) PLOT TREE + INTERPRETATION =========

# Print rules for interpretation (read these for slide explanation)
print(fit_numeric)

# Plot the tree (use screenshot in Slide 13)
rpart.plot(
  fit_numeric,
  type  = 1,       # show labels
  extra = 2,       # class + (correct/total)
  cex   = 0.6      # shrink text for slides
)

# ========= 6. 🚩 (4) ACCURACY – TRAIN & TEST =========

## ---- TRAIN ----
pred_train   <- predict(fit_numeric, train_data, type = "class")
actual_train <- train_data$`Job Status`
cm_train     <- table(pred_train, actual_train)
pt_train     <- prop.table(cm_train)
train_accuracy <- pt_train[1, 1] + pt_train[2, 2]

## ---- TEST ----
pred_test   <- predict(fit_numeric, test_data, type = "class")
actual_test <- test_data$`Job Status`
cm_test     <- table(pred_test, actual_test)
pt_test     <- prop.table(cm_test)
test_accuracy <- pt_test[1, 1] + pt_test[2, 2]

# ========= 7. PRINT EXACT OUTPUT FORMAT =========

cat("train_accuracy =", round(train_accuracy, 3), "\n")
cat("test_accuracy  =", round(test_accuracy, 3), "\n")

# (Optional) Print confusion matrices too
cm_train
cm_test
