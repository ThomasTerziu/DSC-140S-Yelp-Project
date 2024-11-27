#Imports
library(ggplot2)
library(readr)
library(caret)
library(class)
library(corrplot)

user <- read_csv("user.json.csv")

print(user)

numeric_only <- user[,sapply(user, is.numeric)]
correlation_matrix <- cor(numeric_only)

View(correlation_matrix)


linear_model <- lm(user$cool_votes~user$funny_votes)
print(linear_model)
coefs <- coef(linear_model)
print(coefs)
lm_slope <- coefs[2]
lm_intercept <- coefs[1]
cat("Slope:", lm_slope, "Intercept:", lm_intercept)


ggplot(user, aes(x=cool_votes, y= funny_votes))+
  geom_point() + labs(x="Cool Votes", y="Funny Votes", title = "Cool vs. Funny Votes")+
  geom_smooth(aes(cool_votes,funny_votes), method="lm", se=F)

linear_model2 <- lm(user$review_count~user$fans)
print(linear_model)
coefs <- coef(linear_model)
print(coefs)
lm_slope <- coefs[2]
lm_intercept <- coefs[1]
cat("Slope:", lm_slope, "Intercept:", lm_intercept)

ggplot(user, aes(x=review_count, y=fans))+
  geom_point() + labs(x="Review Count", y="Fans", title = "Review Count vs Fans")+
  geom_smooth(aes(review_count,fans), method="lm", se=F)

#More reviews do bring in more fans since the linear regression model have a positive slop that increases

linear_model3 <- lm(user$cool_votes~user$fans)
print(linear_model)
coefs <- coef(linear_model)
print(coefs)
lm_slope <- coefs[2]
lm_intercept <- coefs[1]
cat("Slope:", lm_slope, "Intercept:", lm_intercept)

ggplot(user, aes(x=cool_votes, y=fans))+
  geom_point() + labs(x="Cool Votes", y="Fans", title = "Cool Votes vs Fans")+
  geom_smooth(aes(cool_votes,fans), method="lm", se=F)

#Cool Votes and Fans are correlated since the linear regression line has a positive increasing slope

#ML 1
View(user)
cluster1 <- kmeans(user[,6:7], 2)
table(cluster1$cluster, user$cool_votes)
user$cluster <- as.factor(cluster1$cluster)
wcss <- function(k) {
  kmeans(user, centers = k, nstart = 10)$tot.withinss
}
k_values <- 1:10
wcss_values <- sapply(k_values, wcss)
elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
ggplot(elbow_plot, aes(x = k, y = wcss)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Plot for User Data",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares") +
  theme_minimal()

#We chose 2 clusters since thats how many columns of data we  are analyzing

#ML 2

cluster2 <- kmeans(user[,7:11], 2)
table(cluster2$cluster, user$cool_votes)
user$cluster <- as.factor(cluster2$cluster)
wcss <- function(k) {
  kmeans(user, centers = k, nstart = 10)$tot.withinss
}
k_values <- 1:10
wcss_values <- sapply(k_values, wcss)
elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
ggplot(elbow_plot, aes(x = k, y = wcss)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Plot for User Data",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares") +
  theme_minimal()