library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)


# 1. READ IN THE DATA
df <- read.csv('data/Fish.csv')
head(df)



# 2. DATA ANALYSIS
# check for missing values
any(is.na(df))

# Weight vs Height plot
ggplot(data=df, aes(x=Weight, y=Height)) + 
  geom_point(aes(color=Species, size=10, alpha=0.7))

# correlation check
corrgram(df, lower.panel=panel.shade, upper.panel=panel.cor)



# 3. TRAIN/TEST SPLIT
set.seed(42)

sampleSplit <- sample.split(Y=df$Weight, SplitRatio=0.7)
trainSet <- subset(x=df, sampleSplit==TRUE)
testSet <- subset(x=df, sampleSplit==FALSE)



# 4. TRAIN THE MODEL
model <- lm(formula=Weight ~ ., data=trainSet)
summary(model)

# visualize residuals
modelResiduals <- as.data.frame(residuals(model))
ggplot(modelResiduals, aes(residuals(model))) + 
  geom_histogram(fill='deepskyblue', color='black')



# 5. MAKE PREDICTIONS
preds <- predict(model, testSet)



# 6. EVALUATE PREDICTIONS
modelEval <- cbind(testSet$Weight, preds)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
head(modelEval)

mse <- mean((modelEval$Actual - modelEval$Predicted)^2)
mse

rmse <- sqrt(mse)
rmse