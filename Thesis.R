library(data.table)
library(mice)
library(VIM)
library(dplyr)
library(tidyverse)
library(onewaytests)
library(pgirmess)
library(psych)
library(corrplot)
library(lmtest)
library(ggplot2)
library(car)
library(caTools)
library(mgcv)
library(caret)
library(e1071)
library(MASS)
library(stats)
library(factoextra)
library(cluster)

apple = read.csv("C:/Users/Ria/Downloads/appleAppData.csv", sep = ",")
sum(is.na(apple))

apple= data.frame(apple$App_Name, apple$Primary_Genre, apple$Content_Rating,
                  apple$Size_Bytes, apple$Released,apple$Price, apple$Free, 
                  apple$Average_User_Rating, apple$Reviews, 
                  apple$Current_Version_Score, apple$Current_Version_Score)

sum(is.na(apple))
tbl = md.pattern(apple, rotate.names = T)
print(tbl)
mice_plot = aggr(apple, col=c('navyblue', 'yellow'),numbers=T, sortVars=T,
                 labels=names(apple), cex.axis=.7, gap=3, 
                 ylab=c("Missing Data", "Pattern"), plot=F)
plot(mice_plot)

numericnas = select_if(apple, is.numeric)


tempData = mice(numericnas,m=5,maxit=50,meth='rf',seed=500)
summary(tempData)
tempData$imp$apple.apple.Size_Bytes
completedData = complete(tempData,1)
sum(is.na(completedData))

compApple = data.frame(apple[c(1,2,3,5,7,8,9,10,11)], completedData)
sum(is.na(compApple))
#We see that now we only have 1 missing value, removing that won't be affecting
#our results at all.
compApple = na.omit(compApple)
sum(is.na(compApple))

normalize= function(x) { return((x-min(x))/(max(x)-min(x)))}


nums = select_if(compApple, is.numeric)


#Lets create a subset of apps with more than 100 Reviews
apple_subset <- compApple[compApple$apple.Reviews > 100,]
barplot(sort(table(apple_subset$apple.Reviews)))
boxplot(apple_subset$apple.Reviews)

#Various data visualizations to get better image of dataset
hist(apple_subset$apple.Reviews)
#No unusual values found
ggplot(data = apple_subset) +
  geom_bar(mapping = aes(x = apple.Free))

apple_subset %>% 
  count(apple.Content_Rating, apple.Free) %>%  
  ggplot(mapping = aes(x = apple.Content_Rating, y = apple.Free)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data=apple_subset, aes(x=apple.Average_User_Rating, y=apple.Reviews, color=apple.Free)) + 
  geom_point()

ggplot(data=apple_subset, aes(x=apple.Free, y=apple.Reviews)) + 
  geom_boxplot(fill="steelblue")


#Very few outliers, we will remove them using the Tukey method
tukey <- boxplot.stats(apple_subset$apple.Reviews)$stats
iqr <- tukey[4] - tukey[2]
lower <- tukey[2] - 1.5 * iqr
upper <- tukey[4] + 1.5 * iqr
newsub <- apple_subset[apple_subset$apple.Reviews > lower & apple_subset$apple.Reviews < upper, ]
boxplot(newsub$apple.Reviews)
#We check for normality in Reviews using hist, data is rightly skewed
hist(newsub$apple.Reviews)
#Wiser to use non-parametric tests
barplot(sort(table(newsub$apple.Reviews)))

#Creating a factor variable called rating class where apps rated 0-2.99 are considered Bad
# 3-4 are considered Good and 4-5 are considered Great
rating_class = cut(newsub$apple.Average_User_Rating, c(0,2.99,4,5), labels = c("Bad", "Good", "Great"))
table(rating_class)
#--------------------------------------------------------------------------------------------------------
#Checking some basic info about Paid and Free apps concerning Reviews and Avg User Rating
newsub %>%
  group_by(newsub$apple.Free) %>%
  summarise(count = n(), min_rev = min(apple.Reviews), max_rev = max(apple.Reviews),
            mean_rev = mean(apple.Reviews), median_rev = median(apple.Reviews))
newsub %>%
  group_by(newsub$apple.Free) %>%
  summarise(count = n(), min_rate = min(apple.Average_User_Rating), 
            max_rate = max(apple.Average_User_Rating), 
            mean_rate = mean(apple.Average_User_Rating), 
            median_rate = median(apple.Average_User_Rating))


hist(newsub$apple.Average_User_Rating)


#Lets check is the is significant difference in Reviews between Paid and Free apps
wilcox.test(newsub$apple.Reviews ~ newsub$apple.Free)
# p-value <0.05 so there is significant difference between the two.
#Checking the same about Avg user rating
wilcox.test(newsub$apple.Average_User_Rating ~ newsub$apple.Free)
# p-value <0.05 so there is significant difference between the two.


plot(newsub$apple.Reviews, newsub$apple.Average_User_Rating)
scatterplot3d::scatterplot3d(newsub$apple.Reviews, 
                             newsub$apple.Average_User_Rating, newsub$apple.apple.Price)


#Correlation test for numerics, we see that there is a small correlation
#between Reviews and Avg User Rating
corrplot(cor(select_if(newsub, is.numeric)), method="square")

#Brown-Forsythe Test for Variance homogeneity check, non-par, equivalent of Levene's Test
newsub %>%
  group_by(apple.Free) %>%
  summarize(Variance=var(apple.Average_User_Rating))
bf.test(apple.Average_User_Rating ~ apple.Free, data= newsub)
#The data does not meet the assumption of variance homogeneity

boxplot(newsub$apple.Average_User_Rating ~ newsub$apple.Free, xlab = "Free/Paid",
        ylab = "Average User Rating", main = "Avg User Rating ~ Free/Paid Apps Boxplot")
#As done earlier
wilcox.test(newsub$apple.Average_User_Rating ~ newsub$apple.Free)
# p-value <0.05 so there is significant difference between the two.

#Normality Check using QQ-plots
par(mfrow=c(1,2))
qqPlot(newsub$apple.Average_User_Rating[newsub$apple.Free=="True"], ylab = "Avg User Rating", main = "QQ-Plot in Avg User Rating for Free Apps")
qqPlot(newsub$apple.Average_User_Rating[newsub$apple.Free=="False"], ylab = "Avg User Rating", main = "QQ-Plot in Avg User Rating for Paid Apps")
par(mfrow=c(1,1))



#Kruskal-Wallis test, non-par equivalent to ANOVA to test for differences between the 3 classes
kruskal.test(apple.Reviews ~ rating_class, data = newsub)
#p-value<0.05,there is significant evidence of differences between the groups

#Kruskal-Wallis Multiple Comparisons Test
kruskalmc(apple.Reviews ~ rating_class + apple.Free, data = newsub)

#Normalizing the data we will be using
newlmdata= data.frame(newsub$apple.Average_User_Rating,newsub$apple.Reviews, 
                      newsub$apple.apple.Price, newsub$apple.apple.Size_Bytes)
newlmdata=normalize(newlmdata)

set.seed(123)
sample = newlmdata[sample(nrow(newlmdata), size = 10000),]
setnames(sample, old = c('newsub.apple.Average_User_Rating','newsub.apple.Reviews',
                         'newsub.apple.apple.Price','newsub.apple.apple.Size_Bytes'), 
         new = c('avg_rating','reviews','price','size'))

#Simple Linear Regression
model = lm(reviews~ size , data = sample)
#Residuals vs Fitted plot
plot(model, 1)
#Independence assumption
dwtest(model)
#Based on Res vs Fitted plot, the mean of the residuals is almost 0

#Scale-Location plot to check for residual's constant variance
plot(model, 3)
ncvTest(model)
#p-value < 0.05 so non-constant vars

#Checking if the issue continues across categories.
ggplot(sample, aes(x = fitted(model), y = residuals(model))) +
  geom_point() +
  facet_wrap(~ rating_class, scales = "free") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residual Plots for Different Groups")
#Pattern continues across categories so we need to find the error in the model
#We will use non-parametric methods.

#SVR
#Training the data
partsample <- sample.split(sample$reviews, SplitRatio = 0.7)
train  <- subset(sample, partsample == TRUE)
test   <- subset(sample, partsample == FALSE)
model_reg=svm(reviews~.,data = train)
print(model_reg)
pred = predict(model_reg, test)
mse <- mean((pred - test$reviews)^2)
mae <- mean(abs(pred - test$reviews))
r2 <- cor(pred, test$reviews)^2
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r2, "\n")
#Only 3% of our data is explained by this model.


#Ordinal Logistic Reg
set.seed(123)

newlmdata$free = newsub$apple.Free
newlmdata$rating_class = rating_class
logsample=newlmdata[sample(nrow(newlmdata), size = 10000),]
logmodel = polr(rating_class~ reviews +price, data = logsample)
summary(logmodel)
scatterplotMatrix(~ reviews +price, data = logsample)


#PCA

# Standardize the data
data_std = scale(nums)
# Perform PCA
stdpca= prcomp(data_std,center = TRUE, scale. = TRUE)
# Get the proportion of variance explained by each principal component
pca_var= stdpca$sdev^2/sum(stdpca$sdev^2)
# Plot the scree plot
plot(pca_var,type = "b", pch = 19, xlab = "Principal Component", 
     ylab= "Proportion of Variance Explained",main = "Scree Plot")
# Identify the number of principal components to use
pc_cutoff = 0.8
pc_sum = cumsum(pca_var)
n_pc = sum(pc_sum <= pc_cutoff) + 1
# Transform the data into the reduced-dimensional space of the principal components
data_pca = predict(stdpca, newdata = data_std)[, 1:n_pc]
# Perform k-means clustering
k = 3 # set the number of clusters
kmeans_model = kmeans(data_pca, centers = k)
data_clustered <- nums %>% 
  mutate(cluster = as.factor(kmeans_model$cluster),
         PC1 = data_pca[,1],
         PC2 = data_pca[,2])
# Plot the data in the reduced-dimensional space of the first two principal components
ggplot(data_clustered, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  xlab(paste0("PC1 (", round(pca_var[1]*100), "%)")) +
  ylab(paste0("PC2 (", round(pca_var[2]*100), "%)")) +
  ggtitle("PCA + K-means Clustering")


library(reshape2)
# Extract loadings matrix
stdloadings = stdpca$rotation
# Convert loadings data frame from wide to long format
loadings_long <- melt(stdloadings)
# Visualize loadings as a heatmap
ggplot(loadings_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#Profiling
profvis({
  completedData = complete(tempData,1)
  sum(is.na(completedData))
  
  compApple = data.frame(apple[c(1,2,3,5,7,8,9,10,11)], completedData)
  sum(is.na(compApple))
  #We see that now we only have 1 missing value, removing that won't be affecting
  #our results at all.
  compApple = na.omit(compApple)
  sum(is.na(compApple))
  
  normalize= function(x) { return((x-min(x))/(max(x)-min(x)))}
  
  
  nums = select_if(compApple, is.numeric)
  
  
  #Lets create a subset of apps with more than 100 Reviews
  apple_subset <- compApple[compApple$apple.Reviews > 100,]
  barplot(sort(table(apple_subset$apple.Reviews)))
  boxplot(apple_subset$apple.Reviews)
  
  #Various data visualizations to get better image of dataset
  hist(apple_subset$apple.Reviews)
  #No unusual values found
  ggplot(data = apple_subset) +
    geom_bar(mapping = aes(x = apple.Free))
  
  apple_subset %>% 
    count(apple.Content_Rating, apple.Free) %>%  
    ggplot(mapping = aes(x = apple.Content_Rating, y = apple.Free)) +
    geom_tile(mapping = aes(fill = n))
  
  ggplot(data=apple_subset, aes(x=apple.Average_User_Rating, y=apple.Reviews, color=apple.Free)) + 
    geom_point()
  
  ggplot(data=apple_subset, aes(x=apple.Free, y=apple.Reviews)) + 
    geom_boxplot(fill="steelblue")
  
  
  #Very few outliers, we will remove them using the Tukey method
  tukey <- boxplot.stats(apple_subset$apple.Reviews)$stats
  iqr <- tukey[4] - tukey[2]
  lower <- tukey[2] - 1.5 * iqr
  upper <- tukey[4] + 1.5 * iqr
  newsub <- apple_subset[apple_subset$apple.Reviews > lower & apple_subset$apple.Reviews < upper, ]
  boxplot(newsub$apple.Reviews)
  #We check for normality in Reviews using hist, data is rightly skewed
  hist(newsub$apple.Reviews)
  #Wiser to use non-parametric tests
  barplot(sort(table(newsub$apple.Reviews)))
  
  #Creating a factor variable called rating class where apps rated 0-2.99 are considered Bad
  # 3-4 are considered Good and 4-5 are considered Great
  rating_class = cut(newsub$apple.Average_User_Rating, c(0,2.99,4,5), labels = c("Bad", "Good", "Great"))
  table(rating_class)
  #--------------------------------------------------------------------------------------------------------
  #Checking some basic info about Paid and Free apps concerning Reviews and Avg User Rating
  newsub %>%
    group_by(newsub$apple.Free) %>%
    summarise(count = n(), min_rev = min(apple.Reviews), max_rev = max(apple.Reviews),
              mean_rev = mean(apple.Reviews), median_rev = median(apple.Reviews))
  newsub %>%
    group_by(newsub$apple.Free) %>%
    summarise(count = n(), min_rate = min(apple.Average_User_Rating), 
              max_rate = max(apple.Average_User_Rating), 
              mean_rate = mean(apple.Average_User_Rating), 
              median_rate = median(apple.Average_User_Rating))
  
  
  hist(newsub$apple.Average_User_Rating)
  
  
  #Lets check is the is significant difference in Reviews between Paid and Free apps
  wilcox.test(newsub$apple.Reviews ~ newsub$apple.Free)
  # p-value <0.05 so there is significant difference between the two.
  #Checking the same about Avg user rating
  wilcox.test(newsub$apple.Average_User_Rating ~ newsub$apple.Free)
  # p-value <0.05 so there is significant difference between the two.
  
  
  plot(newsub$apple.Reviews, newsub$apple.Average_User_Rating)
  scatterplot3d::scatterplot3d(newsub$apple.Reviews, 
                               newsub$apple.Average_User_Rating, newsub$apple.apple.Price)
  
  
  #Correlation test for numerics, we see that there is a small correlation
  #between Reviews and Avg User Rating
  corrplot(cor(select_if(newsub, is.numeric)), method="square")
  
  #Brown-Forsythe Test for Variance homogeneity check, non-par, equivalent of Levene's Test
  newsub %>%
    group_by(apple.Free) %>%
    summarize(Variance=var(apple.Average_User_Rating))
  bf.test(apple.Average_User_Rating ~ apple.Free, data= newsub)
  #The data does not meet the assumption of variance homogeneity
  
  boxplot(newsub$apple.Average_User_Rating ~ newsub$apple.Free, xlab = "Free/Paid",
          ylab = "Average User Rating", main = "Avg User Rating ~ Free/Paid Apps Boxplot")
  #As done earlier
  wilcox.test(newsub$apple.Average_User_Rating ~ newsub$apple.Free)
  # p-value <0.05 so there is significant difference between the two.
  
  #Normality Check using QQ-plots
  par(mfrow=c(1,2))
  qqPlot(newsub$apple.Average_User_Rating[newsub$apple.Free=="True"], ylab = "Avg User Rating", main = "QQ-Plot in Avg User Rating for Free Apps")
  qqPlot(newsub$apple.Average_User_Rating[newsub$apple.Free=="False"], ylab = "Avg User Rating", main = "QQ-Plot in Avg User Rating for Paid Apps")
  par(mfrow=c(1,1))
  
  
  
  #Kruskal-Wallis test, non-par equivalent to ANOVA to test for differences between the 3 classes
  kruskal.test(apple.Reviews ~ rating_class, data = newsub)
  #p-value<0.05,there is significant evidence of differences between the groups
  
  #Kruskal-Wallis Multiple Comparisons Test
  kruskalmc(apple.Reviews ~ rating_class + apple.Free, data = newsub)
  
  #Normalizing the data we will be using
  newlmdata= data.frame(newsub$apple.Average_User_Rating,newsub$apple.Reviews, 
                        newsub$apple.apple.Price, newsub$apple.apple.Size_Bytes)
  newlmdata=normalize(newlmdata)
  
  set.seed(123)
  sample = newlmdata[sample(nrow(newlmdata), size = 10000),]
  setnames(sample, old = c('newsub.apple.Average_User_Rating','newsub.apple.Reviews',
                           'newsub.apple.apple.Price','newsub.apple.apple.Size_Bytes'), 
           new = c('avg_rating','reviews','price','size'))
  
  #Simple Linear Regression
  model = lm(reviews~ size , data = sample)
  #Residuals vs Fitted plot
  plot(model, 1)
  #Independence assumption
  dwtest(model)
  #Based on Res vs Fitted plot, the mean of the residuals is almost 0
  
  #Scale-Location plot to check for residual's constant variance
  plot(model, 3)
  ncvTest(model)
  #p-value < 0.05 so non-constant vars
  
  #Checking if the issue continues across categories.
  ggplot(sample, aes(x = fitted(model), y = residuals(model))) +
    geom_point() +
    facet_wrap(~ rating_class, scales = "free") +
    labs(x = "Fitted Values", y = "Residuals") +
    ggtitle("Residual Plots for Different Groups")
  #Pattern continues across categories so we need to find the error in the model
  #We will use non-parametric methods.
  
  #SVR
  #Training the data
  partsample <- sample.split(sample$reviews, SplitRatio = 0.7)
  train  <- subset(sample, partsample == TRUE)
  test   <- subset(sample, partsample == FALSE)
  model_reg=svm(reviews~.,data = train)
  print(model_reg)
  pred = predict(model_reg, test)
  mse <- mean((pred - test$reviews)^2)
  mae <- mean(abs(pred - test$reviews))
  r2 <- cor(pred, test$reviews)^2
  cat("MSE:", mse, "\n")
  cat("MAE:", mae, "\n")
  cat("R-squared:", r2, "\n")
  #Only 3% of our data is explained by this model.
  
  
  #Ordinal Logistic Reg
  set.seed(123)
  
  newlmdata$free = newsub$apple.Free
  newlmdata$rating_class = rating_class
  logsample=newlmdata[sample(nrow(newlmdata), size = 10000),]
  logmodel = polr(rating_class~ reviews +price, data = logsample)
  summary(logmodel)
  scatterplotMatrix(~ reviews +price, data = logsample)
  
  
  #PCA
  library(stats)
  library(factoextra)
  library(cluster)
  
  # Standardize the data
  data_std = scale(nums)
  # Perform PCA
  stdpca= prcomp(data_std,center = TRUE, scale. = TRUE)
  # Get the proportion of variance explained by each principal component
  pca_var= stdpca$sdev^2/sum(stdpca$sdev^2)
  # Plot the scree plot
  plot(pca_var,type = "b", pch = 19, xlab = "Principal Component", 
       ylab= "Proportion of Variance Explained",main = "Scree Plot")
  # Identify the number of principal components to use
  pc_cutoff = 0.8
  pc_sum = cumsum(pca_var)
  n_pc = sum(pc_sum <= pc_cutoff) + 1
  # Transform the data into the reduced-dimensional space of the principal components
  data_pca = predict(stdpca, newdata = data_std)[, 1:n_pc]
  # Perform k-means clustering
  k = 3 # set the number of clusters
  kmeans_model = kmeans(data_pca, centers = k)
  data_clustered <- nums %>% 
    mutate(cluster = as.factor(kmeans_model$cluster),
           PC1 = data_pca[,1],
           PC2 = data_pca[,2])
  # Plot the data in the reduced-dimensional space of the first two principal components
  ggplot(data_clustered, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point() +
    xlab(paste0("PC1 (", round(pca_var[1]*100), "%)")) +
    ylab(paste0("PC2 (", round(pca_var[2]*100), "%)")) +
    ggtitle("PCA + K-means Clustering")
  
  
  library(reshape2)
  # Extract loadings matrix
  stdloadings = stdpca$rotation
  # Convert loadings data frame from wide to long format
  loadings_long <- melt(stdloadings)
  # Visualize loadings as a heatmap
  ggplot(loadings_long, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
})
#The most time consuming function is the 3D scatterplot. That could be omitted as it
# doesn't really give us any new information but rather just ensures validity of what we already know.
#Additionally, the split into training and test sets is the second more time-consuming
#function and also the most memory intensive. 