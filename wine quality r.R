#explanatory analysis
wine <- read.csv('C:\\Users\\dapao\\Desktop\\winequality-red.csv')
wine$group = ifelse(wine$quality>5, 1, 0)
names(wine)
summary(wine$group)
boxplot(wine)
head(wine)
dim(wine)
sum(is.na(wine))
hist(quality)
hist(group)
library(corrplot)
cor(wine)
corrplot.mixed(cor(wine),order='AOE')

#discussion
train <- (1:1279)
typeof(train)
length(train)
test <- (1280:1599)
data.train<-wine[1:1279,]
data.test <-wine[1280:1599,]

glm.fit1 = glm(wine$group==1 ~ fixed.acidity+volatile.acidity+citric.acid+
                residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide
              +sulphates+alcohol, data=wine, family = binomial, subset=train)
summary(glm.fit1)

glm.fit2 = glm(wine$group==1 ~ volatile.acidity+total.sulfur.dioxide
               +sulphates+alcohol, data=wine, family = binomial, subset=train)
summary(glm.fit2)
summary(glm.fit2)$coef
glm.probs=predict(glm.fit2, data.test, type="response")
glm.pred <- rep(0, 320)
glm.pred[glm.probs>0.5] <- 1
table(glm.pred,data.test$group)
mean(glm.pred == data.test$group)
mean(glm.pred != data.test$group)

null<- glm(wine$group==1 ~ 1, family=binomial, data=wine, subset=train)

full<- glm(wine$group==1 ~ fixed.acidity+volatile.acidity+citric.acid+
             residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide
           +sulphates+alcohol, family=binomial, data=wine, subset=train)
step(null, scope=list(upper=full),direction='both')

glm.fitva<- glm(wine$group==1 ~ volatile.acidity+
                 total.sulfur.dioxide+chlorides+free.sulfur.dioxide
               +sulphates+alcohol, family=binomial, data=wine, subset=train)
summary(glm.fitva)

glm.probs=predict(glm.fitva, data.test, type="response")
glm.pred <- rep(0, 320)
glm.pred[glm.probs>0.5] <- 1
table(glm.pred,data.test$group)
mean(glm.pred == data.test$group)
mean(glm.pred != data.test$group)

glm.fit3 = glm(wine$group==1 ~ volatile.acidity+alcohol, 
              data=wine, family = binomial, subset=train)
summary(glm.fit3)

glm.probs=predict(glm.fit3, data.test, type="response")
glm.pred <- rep(0, 320)
glm.pred[glm.probs>0.5] <- 1
table(glm.pred,data.test$group)
mean(glm.pred == data.test$group)
mean(glm.pred != data.test$group)

library(car)
vif(glm.fit3)
vif(glm.fit2)
AIC(glm.fit3, glm.fit2)
BIC(glm.fit3, glm.fit2)

#cross validation
library(caret)
trainControl= trainControl(method='cv', number=5)
wine$group=as.factor(wine$group)
model1 <- train(group ~ volatile.acidity+alcohol, 
                data = wine, trControl=trainControl, method='glm', 
                family=binomial(link=logit), metric='Accuracy')
model1
model2<- train(group ~ volatile.acidity+total.sulfur.dioxide
               +sulphates+alcohol, trControl=trainControl, data=wine,
               method='glm', family=binomial(link=logit), metric='Accuracy')
model2

trainControl= trainControl(method='cv', number=10)
wine$group=as.factor(wine$group)
model1 <- train(group ~ volatile.acidity+alcohol, 
                data = wine, trControl=trainControl, method='glm', 
                family=binomial(link=logit), metric='Accuracy')
model1
model2<- train(group ~ volatile.acidity+total.sulfur.dioxide
               +sulphates+alcohol, trControl=trainControl, data=wine,
               method='glm', family=binomial(link=logit), metric='Accuracy')
model2