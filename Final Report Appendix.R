# Load package
library(leaps)

# Read the data (Please adjust the file path when reproducing this process)
insurance <-
  read.csv("insurance.csv", stringsAsFactors = TRUE)

# Explore the response variable
hist(insurance$charges, xlab = "Charges in US Dollars", 
     main = "Histogram of Charges")

# Build the full model
reg_full <-
  lm(charges ~ region + smoker + children + bmi + sex + age, data = insurance)
summary(reg_full)

# Model Selection
selection <-
  regsubsets(charges ~ ., data = insurance, method = "exhaustive")
selection_summary <- summary(selection)
selection_summary
plot(selection_summary$cp, xlab = "Number of Parameters", 
     ylab = "Mallow's Cp Values", main = "Mallow's Cp vs. P Plot")
abline(c(1:8), c(1:8), col = "red")

# Reduced model without sex and region
reg_NoRegion_NoSex <-
  lm(charges ~ smoker + children + bmi + age, data = insurance)
summary(reg_NoRegion_NoSex)

# Plot residuals vs. fitted values
par(mar = c(4, 4, 4, 6), xpd = TRUE)
plot(
  reg_NoRegion_NoSex$residuals ~ reg_NoRegion_NoSex$fitted.values,
  xlab = "Fitted Values in US Dollars",
  ylab = "Risiduals in US Dollars",
  col = as.factor(insurance$smoker)
)
title("Residual vs. Fitted Values")
legend(
  x = "topright",
  inset = c(-0.2, 0),
  legend = c("Yes", "No"),
  fill = as.factor(insurance$smoker),
  title = "Smoker"
)

# Plot residuals vs. Age and BMI respectively
par(mar = c(4, 4, 4, 6), xpd = TRUE)
plot(
  reg_NoRegion_NoSex$residuals ~ insurance$age,
  xlab = "Age",
  ylab = "Risiduals in US Dollars",
  col = as.factor(insurance$smoker)
)
title("Residual vs. Age")
legend(
  x = "topright",
  inset = c(-0.2, 0),
  legend = c("Yes", "No"),
  fill = as.factor(insurance$smoker),
  title = "Smoker"
)
par(mar = c(4, 4, 4, 6), xpd = TRUE)
plot(
  reg_NoRegion_NoSex$residuals ~ insurance$bmi,
  xlab = "BMI in kg/m^2",
  ylab = "Risiduals in US Dollars",
  col = as.factor(insurance$smoker)
)
title("Residual vs. BMI")
legend(
  x = "topright",
  inset = c(-0.2, 0),
  legend = c("Yes", "No"),
  fill = as.factor(insurance$smoker),
  title = "Smoker"
)

# Categorize BMI into "Obese" and "not Obese"
insurance$obesity <- ifelse(insurance$bmi >= 30, 1, 0)

# New model with Obesity and interaction
reg_Obesity_Smoker_Int <-
  lm (charges ~ obesity + children + smoker + age + obesity * smoker,
      insurance)
summary(reg_Obesity_Smoker_Int)

# Residual Plot
par(mar = c(4, 4, 4, 6), xpd = TRUE)
plot(
  reg_Obesity_Smoker_Int$residuals ~ reg_Obesity_Smoker_Int$fitted.values,
  xlab = "Fitted Values in US Dollars",
  ylab = "Risiduals in US Dollars",
  col = as.factor(insurance$smoker)
)
title("Residual vs. Fitted Values")
legend(
  x = "topright",
  inset = c(-0.2, 0),
  legend = c("Yes", "No"),
  fill = as.factor(insurance$smoker),
  title = "Smoker"
)

# Squared predictor added
reg_Obesity_Smoker_Int_Age2 <-
  lm (charges ~ obesity + children + smoker + age +
        I((age - mean(insurance$age)) ^ 2) + obesity * smoker,
      insurance)
summary(reg_Obesity_Smoker_Int_Age2)

plot(
  reg_Obesity_Smoker_Int_Age2$residuals ~ reg_Obesity_Smoker_Int_Age2$fitted.values,
  xlab = "Fitted Values in US Dollars",
  ylab = "Risiduals in US Dollars",
  col = as.factor(insurance$smoker)
)
title("Residual vs. Fitted Values")
legend(
  x = "topright",
  inset = c(-0.2, 0),
  legend = c("Yes", "No"),
  fill = as.factor(insurance$smoker),
  title = "Smoker"
)

# Q–Q plot of the final model
par(mar = c(5.1, 4.1, 4.1, 2.1), xpd = FALSE)
qqnorm(reg_Obesity_Smoker_Int_Age2$residuals,
       pch = 1,
       frame = FALSE)
qqline(reg_Obesity_Smoker_Int_Age2$residuals,
       col = "steelblue",
       lwd = 2)

