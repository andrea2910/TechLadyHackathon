#presentation code
install.packages(c("ggplot2","e1071","mlbench","randomForest","MASS","rpart","rpart.plot",
                   "class"))
#packages to run our code
library(ggplot2) #plotting
library(e1071) # naive bayes
library(mlbench) #data for naive byaes
library(randomForest) #random Forest
library(MASS) # cats data set 
library(rpart) #decision trees
library(rpart.plot) #decision trees
library(class) #knn

#supervised learning
data<- women
ggplot(data=data, aes(x=height, y=weight)) + geom_point(color="red") + theme_bw(base_size=18)

#unsupervised learning
data <- data.frame(x=c(0,0,50,49,3,51),y=c(0,1,50,51,4,52))
ggplot(data=data, aes(x=x,y=y)) + geom_point() + theme_bw(base_size=18)

#lm
data <- mtcars
m1 <- lm(mpg~hp,data=data)
summary(m1)
eq <- as.character(as.expression(substitute(italic(y) == a + b %.% italic(x), 
                         list(a= format(coef(m1)[1],digits=2),
                              b= format(coef(m1)[2],digits=2))))) #create the text for our model
ggplot(data=data, aes(x=hp,y=mpg)) + geom_point()  +  
  geom_smooth(method = "lm", se=FALSE, color="blue") +  theme_bw(base_size=18) + 
  geom_text(x=200, y=30,label=eq,parse=TRUE,size=10) #add our equation to the graph

#logistic reggression
data <- data.frame(hours=c(seq(0.5,5.5,by=0.25)),
                   pass=c(rep(0,6),1,0,1,0,1,0,1,0,rep(1,7)))
m2 <- glm(pass~hours, data=data,family="binomial")
summary(m2)
eq2 <- as.character(as.expression(substitute(italic("log odds") == a + b %.% italic(x), 
                                             list(a= format(coef(m2)[1],digits=2),
                                                  b= format(coef(m2)[2],digits=2)))))
ggplot(data=data,aes(x=hours, y=pass)) + geom_point() + 
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  geom_text(x=1,y=0.5,label=eq2,parse=TRUE) + theme_bw(base_size=18)

#classification tree  - predict a type of deformation called kyphosis after surgery
# from an age in months and number of vertebrae involved and highest vertebrae operated
# on 
data <- kyphosis
#grow our tree
fit <- rpart( Kyphosis~Age+Number+Start, method="class", data=data)
pred <- predict(fit,data,type="class")
table(true=data$Kyphosis, pred=pred)
prp(fit) #83%


#randomForests
data <- kyphosis
rf <- randomForest(Kyphosis~Age+Number+Start,data=data,ntree=1000)
pred <- predict(rf, data)
table(true=data$Kyphosis, pred=pred) #98%
varImpPlot(rf)
mean(rf$err.rate[,1])
getTree(rf,1,labelVar=T)

#svm
data <- cats
ggplot(data=data,aes(x=Bwt,y=Hwt)) + geom_point(aes(color=factor(Sex)))
m3 <- svm(Sex~Bwt+Hwt,data=data,type='C',kernel='linear')
summary(m3)
pred <- fitted(m3)
table(pred, data$Sex)
plot(m3, data)

#naive bayes
data(HouseVotes84)
data <- HouseVotes84
ggplot(data=data, aes(Class))+geom_bar() + theme_bw(base_size=18)
ggplot(data=data, aes(x=V3, fill=Class)) + geom_bar() + theme_bw(base_size=18)
nb_model <- naiveBayes(Class~.,data = data)
pred <- predict(nb_model,data[,-1])
table(prediction=pred,true=data[,1])

#knn
data <- iris
pred10  <- knn(train=data[,1:4],test=data[,1:4],cl=data$Species,k=10)
pred3 <- knn(train=data[,1:4],test=data[,1:4],cl=data$Species,k=3)
table(true=data$Species, pred3=pred3)
table(true=data$Species, pred10=pred10)

# kmeans
data <- data.frame(x=rnorm(100,sd = 0.3),y=rnorm(100, mean = 1, sd = 0.3))
ggplot(data=data, aes(x=x,y=y)) + geom_point() + theme_bw(base_size = 18)
cl <- kmeans(data, 5, nstart = 25)
data$cluster = as.factor(cl$cluster)
ggplot(data=data, aes(x=x,y=y,color=cluster)) + geom_point() + theme_bw(base_size = 18)

#PCA
data <- USArrests
pc <- princomp(~Murder+Assault+UrbanPop, data=data,cor=FALSE)
screeplot(pc,type="lines")
biplot(pc)