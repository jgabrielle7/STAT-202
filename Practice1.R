# Load the ToothGrowth dataset
data(ToothGrowth)

# Display the structure of the dataset
str(ToothGrowth)

# Use the summary function to get the five-number summary for len, grouped by supp
summary(ToothGrowth$len[ToothGrowth$supp == "VC"])
# Load the ToothGrowth dataset
data(ToothGrowth)

# Calculate the standard deviation for the len variable
standard_deviation <- sd(ToothGrowth$len)

# Print the result
print(standard_deviation)
# Calculate the standard deviation for the len variable, grouped by supp
standard_deviation_vc <- sd(ToothGrowth$len[ToothGrowth$supp == "VC"])

# Print the result
print(standard_deviation_vc)
# Load the ToothGrowth dataset
data(ToothGrowth)

# Create a boxplot for tooth length, grouped by supp (OJ and VC)
boxplot(len ~ supp, data = ToothGrowth, col = c("orange", "green"),
        main = "Tooth Length for OJ and VC Supplements",
        xlab = "Supplement", ylab = "Tooth Length")
# Load the ToothGrowth dataset
data(ToothGrowth)

# Create a boxplot for tooth length, grouped by dose
boxplot(len ~ dose, data = ToothGrowth, col = c("lightblue", "lightgreen", "lightcoral"),
        main = "Tooth Length for Different Doses",
        xlab = "Dose", ylab = "Tooth Length")
# Load the ToothGrowth dataset
data(ToothGrowth)

# Subset the data for the VC supplement
vc_data <- ToothGrowth[ToothGrowth$supp == "VC", ]

# Calculate covariance
covariance_vc <- cov(vc_data$dose, vc_data$len)
cat("Covariance:", covariance_vc, "\n")

# Calculate correlation
correlation_vc <- cor(vc_data$dose, vc_data$len)
cat("Correlation:", correlation_vc, "\n")
# Load the ToothGrowth dataset
data(ToothGrowth)

# Subset the data for the OJ supplement
oj_data <- ToothGrowth[ToothGrowth$supp == "OJ", ]

# Calculate covariance
covariance_oj <- cov(oj_data$dose, oj_data$len)
cat("Covariance:", covariance_oj, "\n")

# Calculate correlation
correlation_oj <- cor(oj_data$dose, oj_data$len)
cat("Correlation:", correlation_oj, "\n")
# Load the ToothGrowth dataset
data(ToothGrowth)

# Fit linear regression models for VC and OJ supplements
model_vc <- lm(len ~ dose, data = subset(ToothGrowth, supp == "VC"))
model_oj <- lm(len ~ dose, data = subset(ToothGrowth, supp == "OJ"))

# Create a scatterplot with regression lines for both supplements
plot(len ~ dose, col = supp, data = ToothGrowth, pch = 16)
abline(model_vc, col = "blue", lty = 2)  # Add regression line for VC supplement
abline(model_oj, col = "red", lty = 2)   # Add regression line for OJ supplement

# Add legend
legend("topright", legend = c("VC", "OJ"), col = c("blue", "red"), pch = 16, title = "Supplement")
