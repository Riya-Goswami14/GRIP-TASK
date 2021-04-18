## AUTHOR: RIYA GOSWAMI

## GRIP@The Sparks Foundation

## 'TASK#6 : PREDICTION USING DECISION TREE ALGORITHM'

## **TASK: TO CREATE A DECISION TREE CLASSIFIER AND VISUALIZE IT GRAPHICALLY** ##

# *STEP 1: LOAD THE DATASET*

## Here we are using IRIS Dataset for the given task.
data("iris")
data=iris
data

dim(data)
str(data)
# **COMMENT: THE IRISDATA HAS 150 OBSERVATIONS WITH 5 VARIABLES 'Sepal.Length','Sepal.width','Petal.Length','Petal.Width' AND 'Species' WHICH IS A VECTOR OF CHARACTER TYPE- "SETOSA","VERSICOLOR","VIRGINICA"**
  
# *STEP 2- DATA PARTITION*  

set.seed(123)
sample=sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2)) #Prob=0.8 for training data, Prob=0.2 for test data.
table(sample)
# We divide the data of 150observations into 2 parts- Trained data consists of 121 observations while test data consists of 29 observations.
train=data[sample==1,]
test=data[sample==2,]
train
dim(train)
test
dim(test)

# *STEP 3- DECISION TREE  MODEL*

#Specify the libraries
library(party)
library(rpart)
library(rpart.plot)

#Develop the model
tree=rpart(Species~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width ,data=train)
tree

# *STEP 4- VISUALIZATION OF DECISION TREE*

rpart.plot(tree,type=5)

# *STEP 5-PREDICTION USINGOBTAINED MODEL*
#Predict the observations from trained data into one of the Species
predict(tree,train)

# **COMMENT: ABOVE OUTPUT SUGGESTS THAT FOR A GIVEN OBSERVATION, THE TREE CLASSIFIES INTO ONE OF THE SPECIES WITH MAXIMUM PROBABILITY( SAY OBS. NO 144, THERE IS 0.9047 PROBABILITY THAT IT WILL BE CLASSIFIED INTO 'VIRGINICA' SPECIES)**
  
 ## We give observation at random to predict the class of Species ir can belong.
New1=data.frame(Sepal.Length=c(4.6),Sepal.Width=c(3.1),Petal.Length=c(1.7),Petal.Width=c(0.2))
pred1=predict(tree,New1)
pred1
New2=data.frame(Sepal.Length=c(5.0),Sepal.Width=c(2.8),Petal.Length=c(1.7),Petal.Width=c(0.8))
pred2=predict(tree,New2)
pred2
New3=data.frame(Sepal.Length=c(5.0,5.2,3.6,4.3,2.7),Sepal.Width=c(2.8,4.3,1.7,2.3,1.4),Petal.Length=c(1.7,1.5,4.9,2.6,2.8),Petal.Width=c(0.8,0.2,0.5,0.7,0.9))
pred3=predict(tree,New3)
pred3

# *STEP 6-MISCLASSIFICATION ERRORS*
p1=predict(tree,train,type="class")
t1=table(p1,train$Species)
t1
# ** COMMENT: THE OFF DIAGONALELEMENTS GIVES THE MISCLASSIFICATION FIGURES FOR THE DATA IN TRAINING MODEL THAT HAS WRONGLY BEEM CLASSIFIED. IT SUGGESTS THAT 4 OBSERVATIONS HAVE BEEN CLASSIFIED TO VERSICOLOR WHILE ACTUALLY IT BELONGS TO VERGINICA SPECIES.**
   
## TO CALCULATE MISCLASSIFICATION (FOR TRAINED DATA):
m1=1-sum(diag(t1))/sum(t1)
m1

# **COMMENT: THIS IMPLIES THAT MISCLASSIFICATION ERROR IS 4% WHILE THE MODEL IS 96% ACCURATE FOR THE TRAINING DATA**
  
 
# *MISCLASSIFICATION ERROR ON TEST DATA*
p2=predict(tree,test,type="class")
t2=table(p2,test$Species)
t2

# **COMMENT: THE OFF DIAGONAL ELEMENTS GIVES THE MISCLASSIFICATION FIGURES FOR THE DATA IN TEST MODEL THAT HAS WRONGLY BEEN CLASSIFIED. IT SUGGESTS THAT 2 OBSERVATIONS HAVE BEEN CLASSIFIED TO VIRGINICA WHILE ACTUALLY IT BELONGS TO VERSICOLR SPECIES.*

## TO CALCULATE MISSCLASSIFICATION ( FOR TEST DATA):
m2=1-sum(diag(t2))/sum(t2)
m2

# **COMMENT : THIS IMPLIES THAT MISCLASSIFICATION ERROR IS 7% WHILE THE MODEL IS 93% ACCURATE FOR THE TEST DATA.**
  
   
  
  


