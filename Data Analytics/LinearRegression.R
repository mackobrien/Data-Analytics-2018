library(readr)
train <- read_csv("~/Desktop/train.csv")
head(train)
attach(train)
plot(Pclass)

glm.train.Titanic=glm(Survived~Pclass+Sex+Age,data=train, family=binomial)
summary(glm.train.Titanic)

glm.probs.train=predict(glm.train.Titanic, type="response")
glm.probs.train[1:10]
contrasts(Survived)

n=nrow(train)
n
glm.pred.train=rep("0", 891)
glm.pred.train[glm.probs.train>0.5]="1"
table(glm.pred.train,Survived)

trainID=(PassengerId<445)
train.445=train[!trainID,]

dim(train.445)
Survived.445=Survived[!trainID]
glm.fit.train=glm(Survived~Pclass+Sex+Age, data=train, family=binomial, subset=trainID)
glm.probs.train1=predict(glm.fit.train,train.445,type="response")
glm.pred.train1=rep("0",447)
glm.pred.train1[glm.probs.train1>0.5]="1"
table(glm.pred.train1,Survived.445)
mean(glm.pred.train1==Survived.445)
