#DATA PREPARATION
set.seed(123)

n <- (300)

health <- data.frame(
  age = sample(18:65, n, replace = TRUE),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  visits = sample(0:10, n, replace = TRUE),
  income = sample(50000:300000, n, replace = TRUE)
)
health$outcome <- ifelse(health$visits > 4, 1, 0)
health$sex <- factor(health$sex)
health$outcome <- factor(health$outcome)

#Descriptive Statistics
summary(health)
table(health$outcome)
table(health$sex, health$outcome)

#Visualization
ggplot(health, aes(x=age)) + geom_histogram()
ggplot(health, aes(x=sex, fill=outcome)) + geom_bar(position = "fill")

#Logistic Regression
model <- glm(outcome ~ age + sex + income, data=health, family="binomial")
summary(model)
exp(coef(model))
