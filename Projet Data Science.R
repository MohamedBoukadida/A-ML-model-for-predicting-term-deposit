library(dplyr)
bank <- read.csv("C:/Users/LENOVO/Desktop/bank.csv", sep=";")
View(bank)

# Encoding the target feature as factor
bank$y= factor(bank$y,
               levels = c("no","yes"),
               labels = c(0, 1))


# Encoding features as factor
bank$marital = factor(bank$marital,
levels = c('single', 'married', 'divorced'),
labels = c(0, 1, 2))

bank$education = factor(bank$education,
levels = c("unknown","primary","secondary","tertiary"),
labels = c(0, 1, 2, 3))

bank$default= factor(bank$default,
levels = c("no","yes"),
labels = c(0, 1))

bank$housing= factor(bank$housing,
levels = c("no","yes"),
labels = c(0, 1))

bank$loan= factor(bank$loan,
levels = c("no","yes"),
labels = c(0, 1))

bank$contact= factor(bank$contact,
levels = c("unknown","telephone","cellular"),
labels = c(0, 1, 2))

bank$month= factor(bank$month,
levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","nov","oct","dec"),
labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

bank$poutcome= factor(bank$poutcome,
levels = c("unknown","other","failure","success"),
labels = c(0, 1, 2, 3))

bank$job= factor(bank$job,
levels = c("admin.","unknown","unemployed","management","housemaid","entrepreneur","student","blue-collar","self-employed","retired","technician","services"),
labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))



# Function mix max normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


bank[1] <- as.data.frame(lapply(bank[1], min_max_norm))
bank[6] <- as.data.frame(lapply(bank[6], min_max_norm))
bank[12] <- as.data.frame(lapply(bank[12], min_max_norm))
bank[14] <- as.data.frame(lapply(bank[14], min_max_norm))

# Splitting Data
set.seed(1029)

## Remove rows that do not have target variable values
final <- bank[!(is.na(bank$y)),]

final$y <- factor(final$y)

library(caTools)

split <- sample.split(final$y, SplitRatio = 0.75)
train <- subset(final, split == TRUE)
test <- subset(final, split == FALSE)

# Visualization matrice corr
mat.cors = bank.cors <- cor(bank[sapply(bank, is.numeric)])
library(corrplot)
corrplot(mat.cors, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Model randomforest
library(randomForest)
rf1 <- randomForest(y ~ ., data=train, ntree=1000, proximity=TRUE)
rf2 <- randomForest(y ~ ., data=train, ntree=2000, proximity=TRUE)
rf3 <- randomForest(y ~ ., data=train, ntree=3000, proximity=TRUE)

# Logsitic regression
lr <- glm(y ~.,family=binomial(link='logit'),data=train)

# Model decision tree
library(tree)
library(rpart)
library(rpart.plot)
library(caret)
tree <- rpart(y ~., data = train)
rpart.plot(tree)
tree <- rpart(y ~., data = train,cp=0.07444)
p <- predict(tree, train, type = 'class')
dt <- confusionMatrix(p, train$y)


