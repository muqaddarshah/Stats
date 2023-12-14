--------------------------------------------------
#  Importing the Packages
--------------------------------------------------
library("ggplot2")
install.packages("dplyr")
library("dplyr")
#install.packages("tm")
library("tm") # for text mining, similar to wordcloud
library("stats") # for statistical models
options(warn=-1) # to suppress warnings


--------------------------------------------------
#  Setting Work Directory and Loading the Dataset
--------------------------------------------------
setwd("C:/Users/muqad./Documents/Documents/R-Statistics/report/world happiness")
data <- read.csv("2019.csv", header = TRUE)
df <- data
head(df)


--------------------------------------------------
#  Data Analysis
--------------------------------------------------
  # print shape
  print(dim(df))

# check for nulls
print(colSums(is.na(df)))

# drop the duplicates if any
df <- unique(df)

# see the structure of the dataframe
print(str(df))

# drop the column
df <- df[, !(names(df) %in% "Overall rank")]

# df.head()
print(head(df))

# desc = pd.DataFrame(round((df.describe().T), 2))
# desc
# Get summary for numeric columns only
numeric_columns <- sapply(df, is.numeric)
#desc <- summary(df[, numeric_columns])
desc <- summarise_all(df, list(mean = mean, sd = sd, min = min, max = max))
summary1 <- summary(df)

print(summarise_all)

# Identify numeric columns
numeric_columns <- sapply(df, is.numeric)

#Exclude problematic columns
#desc <- round(summary(df[, numeric_columns, drop = FALSE]), 2)
print(summary)
print(desc)

write.csv(summary1, "C:/Users/muqad./Documents/Documents/R-Statistics/report/world happiness/summary1.csv", row.names = FALSE)
--------------------------------------------------
#  Visualization
--------------------------------------------------
#*************PAIR PLOTS*****************
pairs(df[, numeric_columns], col=df$Score)


#*************WORDCLOUD*****************

# Load required libraries
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

# Create a word cloud based on the "Country or region" and "Score" columns
wordcloud_data <- aggregate(df$Score, by=list(Category=df$Country.or.region), FUN=mean)

# Sort the wordcloud_data DataFrame by descending scores
wordcloud_data <- wordcloud_data[order(-wordcloud_data$x), ]

# Generate a word cloud
wordcloud(words = wordcloud_data$Category, freq = wordcloud_data$x, min.freq = 1,
          max.words=80, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#*************CORRELATION*****************

  # Load required library
  install.packages("corrplot")
  library(corrplot)

# Calculate correlation matrix
# Select only numeric columns
numeric_df <- df[sapply(df, is.numeric)]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_df)

# Print the correlation matrix
print(correlation_matrix)
kable(correlation_matrix)

write.csv(correlation_matrix, "C:/Users/muqad./Documents/Documents/R-Statistics/report/world happiness/correlation_matrix.csv", row.names = FALSE)

# Plot
corrplot(correlation_matrix, method="color", col=colorRampPalette(c("white", "seagreen"))(200), 
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45) # Text label color and rotation

--------------------------------------------------
#  MLR -- Linear Regression
--------------------------------------------------
  
  #Define the independent variables (X) and the dependent variable (y)
  str(df)
X <- df[, c("GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")]
y <- df[["Score"]]

# Load required library
library(lmtest)

# Add a constant column to X
X$const <- 1

# Fit the model

# Fit the model
model <- lm(y ~ ., data = X)

# Print the summary
summary <- summary(model)
print(summary)


# Extract coefficients and related information from the model summary
coef_table <- as.data.frame(summary(model)$coefficients)

# Print the table
print(coef_table)

write.csv(coef_table,"C:/Users/muqad./Documents/Documents/R-Statistics/report/world happiness/coeftable", row.names = FALSE)

kable(coef_table)

# Get the confidence intervals
confidence_intervals <- confint(model)
print(confidence_intervals)

write.csv(confidence_intervals,"C:/Users/muqad./Documents/Documents/R-Statistics/report/world happiness/confidence_intervals.csv", row.names = FALSE)


# Get the standardized residuals
standardized_residuals <- rstandard(model)
print(standardized_residuals)


#**********Scatter plot of the standardized residuals with std limits*************

# Load required library
library(ggplot2)

# Create a data frame for plotting
plot_data <- data.frame(Fitted_Values = model$fitted.values, Standardized_Residuals = standardized_residuals)

# Create the plot
install.packages("ggrepel")
library(ggrepel)
ggplot(plot_data, aes(x = Fitted_Values, y = Standardized_Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 2, color = 'red', linetype = 'dashed', size = 0.5) +
  geom_hline(yintercept = -2, color = 'red', linetype = 'dashed', size = 0.5) +
  geom_hline(yintercept = 3, color = 'green', linetype = 'dashed', size = 0.5) +
  geom_hline(yintercept = -3, color = 'green', linetype = 'dashed', size = 0.5) +
  geom_hline(yintercept = 0, color = 'blue', linetype = 'dashed', size = 0.5) +
  geom_text_repel(data = subset(plot_data, abs(Standardized_Residuals) >= 2), 
                  aes(label = rownames(subset(plot_data, abs(Standardized_Residuals) >= 2))), 
                  size = 3) +
  labs(x = 'Fitted Values', y = 'Standardized Residuals', title = 'Standardized Residuals vs. Fitted Values') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#************Breusch-Pagan test for heteroskedasticity**********
  
# Load required library
library(lmtest)

# Perform the Breusch-Pagan test
bp_test <- bptest(model)

# Round the results to 4 decimal places
bp_lm_statistic <- round(bp_test$statistic, 4)
bp_p_value <- round(bp_test$p.value, 4)

# Print the results
print("Breusch-Pagan Test:")
print(paste("LM Statistic:", bp_lm_statistic))
print(paste("p-value:", bp_p_value))

# Interpret the results
alpha <- 0.05  # Significance level
if (bp_p_value > alpha) {
  print("There is no evidence of heteroskedasticity (fail to reject H0)")
} else {
  print("There is evidence of heteroskedasticity (reject H0)")
}
  

#***********************Histogram of Standardized Residuals*********************

# Create a data frame for plotting
plot_data <- data.frame(Standardized_Residuals = standardized_residuals)

# Create the plot
ggplot(plot_data, aes(x = Standardized_Residuals)) +
  geom_histogram(aes(y = ..density..), fill = "darkgrey", color = "black", bins = 30) +
  geom_density(color = "red", size = 1) +
  labs(x = 'Standardized Residuals', y = 'Density', title = 'Histogram of Standardized Residuals') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Load the required package
install.packages("knitr")
library(knitr)

# Use kable() function to print data in a tabular format
kable(head(mtcars))

kable(desc)


-------------------------------------------------------------------------------------------------------------
#Q-Q Plot
-------------------------------------------------------------------------------------------------------------
  str(df)
library(ggplot2)
library(ggrepel)

# Create a data frame for plotting
plot_data <- data.frame(Fitted_Values = model$fitted.values, Standardized_Residuals = standardized_residuals)

# Create the QQ plot
ggplot(plot_data, aes(sample = Fitted_Values)) +
  stat_qq(distribution = qnorm, color = 'blue', size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = 'red', size = 0.5) +
  geom_text_repel(data = subset(plot_data, abs(Standardized_Residuals) >= 2), 
                  aes(label = rownames(subset(plot_data, abs(Standardized_Residuals) >= 2)),
                      x = Fitted_Values, y = Standardized_Residuals), 
                  size = 3) +
  labs(title = 'QQ Plot of Standardized Residuals') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

