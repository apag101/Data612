


#("recommenderlab", dependencies=TRUE)
library(recommenderlab)
library(ggplot2)
library(tidyr)
library(MASS)
library(dplyr)
library(reshape2)
library(pROC)
library(caret)
library(pROC)
library(jsonlite)
library(stringr)
library(devtools)
library(sparklyr)


ld<-function()
{
    urate<- read.csv("https://raw.githubusercontent.com/apag101/Data612/master/Projects/Project2/BX-Book-Ratings.csv", sep=";",header = TRUE)
    books<- read.csv("https://raw.githubusercontent.com/apag101/Data612/master/Projects/Project2/BX-Books.csv", sep=";", header = TRUE)

surate<-subset(urate, Book.Rating !=0 & as.integer(User.ID) <501)

lisbn<-length(unique(surate$ISBN))
luser<-length(unique(surate$User.ID))
lbook<-length(surate$Book.Rating)

#Build  Matrix
set.seed(123)
sdata.mat <- matrix(data=surate$Book.Rating,ncol=length(unique(surate$ISBN)),nrow=length(unique(surate$User.ID)))
rownames(sdata.mat)<-c(paste(unique(surate$User.ID)))
colnames(sdata.mat)<-c(paste(unique(surate$ISBN)))
srdata.mat<-as(sdata.mat, "realRatingMatrix")
srdata.mat

#Split Data
which_train<- sample(x=c(TRUE, FALSE), size=nrow(srdata.mat), replace=TRUE, prob=c(0.8,0.2))
data_train <- srdata.mat[which_train,]
data_test <- srdata.mat[!which_train,]

#Item Based Collaborative Train
ibc_model<-Recommender(data= data_train, method = "IBCF", parameter = list(k=30))
model_details<-getModel(ibc_model)
n_items_top<-30
image(model_details$sim[1:n_items_top,1:n_items_top], main="Heatmap of the first rows and columns")
col_sums<-colSums(model_details$sim>0)
qplot(col_sums)+stat_bin(binwidth = 1) + ggtitle("Distribution of column count")
which_max<-order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]

books%>%
    filter (ISBN %in% c(colnames(model_details$sim)[which_max]))%>%
    select (Book.Title)

#Item Based Collaborative Predict Test
n_recommend <- 6
ibc_predict<- predict(object=ibc_model, newdata= data_test, n=n_recommend)
ibc_matrix <-sapply(ibc_predict@itemLabels, function(x){
    colnames(srdata.mat)[x]
})
ibc<-ibc_matrix[1:6]

i<-as.data.frame(
c(books%>%
    filter (ISBN %in% c(names(ibc)))%>%
    select (Book.Title),

books%>%
    filter (ISBN %in% c(colnames(data_test@data)[1:6]))%>%
    select (Book.Title)))

#User based Collaborative filtering
ubc_model<-Recommender(data= data_train, method = "UBCF")
ubc_model
model_details<-getModel(ubc_model)
n_items_top<-30
image(model_details$data[1:n_items_top,1:n_items_top], main="Heatmap of the first rows and columns")
col_sums<-colSums(model_details$data)
qplot(col_sums)+stat_bin(binwidth = 1) + ggtitle("Distribution of column count")
which_max<-order(col_sums, decreasing = TRUE)[1:6]
colnames(model_details$data)[which_max]
model_details$data

books%>%
    filter (ISBN %in% c(colnames(model_details$data)[which_max]))%>%
    select (Book.Title)


#User Based Collaborative Predict Test
n_recommend <- 6
ubc_predict<- predict(object=ubc_model, newdata= data_test, n=n_recommend)
ubc_matrix <-sapply(ubc_predict@itemLabels, function(x){
    rownames(srdata.mat)[x]
})

ubc<-ubc_matrix[1:6]

u<- as.data.frame(
c(books%>%
    filter (ISBN %in% c(names(ubc)))%>%
    select (Book.Title),

books%>%
    filter (ISBN %in% c(colnames(data_test@data)[1:6]))%>%
    select (Book.Title)))
  
  }
dbst<-system.time(ld())
dbst


