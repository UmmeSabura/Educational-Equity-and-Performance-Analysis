# ==============================
# Educational Dataset Analysis in R
# ==============================

# 1. Load libraries
library(readxl)
library(tidyverse)    # data manipulation + ggplot2
library(Hmisc)        # descriptive stats
library(car)          # ANOVA
library(reshape2)     # data reshaping
library(psych)        # descriptive statistics
library(broom)        # tidy regression output
library(ggplot2)
library(dplyr)




# 2. Load dataset
df<- read_excel("student data.xlsx")  # sheet can be name or index


# 3. Inspect data
head(df)
str(df)
summary(df)

# Select only the important variables
vars_of_interest <- c("G1", "G2", "G3", "failures", "absences", "studytime")

# Summary statistics function
summary_stats <- data.frame(
  Variable = vars_of_interest,
  Mean = sapply(df[vars_of_interest], function(x) mean(x, na.rm = TRUE)),
  SD = sapply(df[vars_of_interest], function(x) sd(x, na.rm = TRUE)),
  Min = sapply(df[vars_of_interest], function(x) min(x, na.rm = TRUE)),
  Median = sapply(df[vars_of_interest], function(x) median(x, na.rm = TRUE)),
  Max = sapply(df[vars_of_interest], function(x) max(x, na.rm = TRUE))
)

print(summary_stats)

# 4. Create derived variables
# Parental Education Average
df$Parental_Edu_Avg <- rowMeans(df[,c("Medu","Fedu")], na.rm=TRUE)
df$Parental_Edu_Avg


# Parental Education Category
df$Parental_Edu_Cat <- cut(df$Parental_Edu_Avg,
                           breaks=c(-1,1,2,3,4),
                           labels=c("Low","Medium","High","Very High"))


# Age Group
df$Age_Group <- cut(df$age, breaks=c(0,14,17,100), labels=c("Under 15","15-17","18+"))

# Convert binary Yes/No to 1/0
binary_vars <- c("schoolsup","famsup","paid","activities","nursery","higher","internet","romantic")
df[binary_vars] <- lapply(df[binary_vars], function(x) ifelse(x=="yes",1,0))

# 5. Descriptive statistics
numeric_vars <- c("age","traveltime","studytime","failures","absences","Dalc","Walc","health","G1","G2","G3")
psych::describe(df[numeric_vars])

# 6. Exploratory Data Analysis (EDA)


# Histogram of G3 with value labels
ggplot(df, aes(x = G3)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(stat = "bin", binwidth = 1, aes(label = ..count..), 
            vjust = -0.5, size = 3) +
  ggtitle("Distribution of Final Grades (G3)") +
  xlab("Final Grade (G3)") +
  ylab("Number of Students")



# Boxplot by Gender with mean labels
ggplot(df, aes(x = sex, y = G3)) +
  geom_boxplot(fill = "lightgreen") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y..,1)), 
               vjust = -0.7, color = "red") +
  ggtitle("Final Grades by Gender") +
  xlab("Gender") +
  ylab("Final Grade (G3)")

# Install effsize package if not already installed
install.packages("effsize")

# Load library
library(effsize)

# Cohen's d for final grades (G3) by gender
cohen_d_result <- cohen.d(df$G3, df$sex, hedges = TRUE)
print(cohen_d_result)


# Boxplot by Parental Education with mean labels
ggplot(df, aes(x = Parental_Edu_Cat, y = G3)) +
  geom_boxplot(fill = "salmon") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "blue") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y..,1)), 
               vjust = -0.7, color = "blue") +
  ggtitle("Final Grades by Parental Education") +
  xlab("Parental Education Category") +
  ylab("Final Grade (G3)")



# 7. Correlation Analysis
cor_matrix <- cor(df[,numeric_vars], use="complete.obs")
print(cor_matrix)
corrplot::corrplot(cor_matrix, method="color", addCoef.col="black")



# 8. T-Test (Gender Gap)
t_test_gender <- t.test(G3 ~ sex, data=df)
print(t_test_gender)

# 9. ANOVA (Parental Education Impact)
anova_model <- aov(G3 ~ Parental_Edu_Cat, data=df)
summary(anova_model)

# Optional: Tukey post-hoc test
TukeyHSD(anova_model)

# 10. Regression Analysis
reg_model <- lm(G3 ~ age + studytime + failures + absences + Dalc + Walc + health +
                  schoolsup + famsup + paid + internet, data=df)
summary(reg_model)
broom::tidy(reg_model)  # tidy table of coefficients

# 11. Equity-Focused Analysis
# Average grades by gender
df %>% group_by(sex) %>% summarise(mean_G3 = mean(G3, na.rm=TRUE))

# Average grades by parental education
df %>% group_by(Parental_Edu_Cat) %>% summarise(mean_G3 = mean(G3, na.rm=TRUE))

# Identify at-risk students
at_risk <- df %>% filter(G3 < 10, Parental_Edu_Cat == "Low", absences > mean(absences, na.rm=TRUE))
nrow(at_risk)
at_risk %>% select(G3, Parental_Edu_Cat, absences, sex)



# At-Risk Students Correlation Heatmap (R)


# Subset numeric variables for at-risk students
at_risk_num <- at_risk[, numeric_vars]

# Remove columns with zero variance
at_risk_num_var <- at_risk_num[, sapply(at_risk_num, function(x) sd(x, na.rm = TRUE) > 0)]

# Compute correlation matrix
at_risk_cor <- cor(at_risk_num_var, use = "complete.obs")

# Inspect correlation matrix
print(at_risk_cor)

# Melt correlation matrix for heatmap
library(reshape2)
melted_cor <- melt(at_risk_cor)

# Create heatmap
library(ggplot2)
ggplot(data = melted_cor, aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  ggtitle("Correlation Heatmap for At-Risk Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(corrplot)
corrplot::corrplot(at_risk_cor, method="color", addCoef.col="black")



