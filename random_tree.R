library(dplyr)
library(tidyverse)
library(BBmisc)


###################################################################################
#                                                                                 #
#                               RANDOM TREE                                       #
#                                                                                 #
###################################################################################

split_val <- function(random_feature,x){
  split_val <- median(x[,random_feature], na.rm=T)
  make_leaf <- F
  
  left_tree_y_indices <- which(x[,random_feature] <= split_val)
  right_tree_y_indices <- which(x[,random_feature] > split_val)
  
  x_shape <- dim(x)[1]
  
  if (length(left_tree_y_indices) == x_shape || length(right_tree_y_indices) == x_shape) {
    split_val <- mean(x[,random_feature], na.rm=T)
    left_tree_y_indices <- which(x[,random_feature] <= split_val)
    right_tree_y_indices <- which(x[,random_feature] > split_val)
    
    if (length(left_tree_y_indices) == x_shape || length(right_tree_y_indices) == x_shape) {
      make_leaf = T
    }
    
  }
  
  return (list(split_val, left_tree_y_indices, right_tree_y_indices, make_leaf))
  
}

random_feature <- function(num_features) {
  feature <- sample(1:num_features,1)
  return (feature)
}

# TODO - make leaves check

make_leaves <- function(x,y) {
  leafs <- leaf_size
  
  # NOT SURE if this functions like ranom.randint line 71.
  indices <- sample(1:dim(x)[1],leafs, replace=T)
  leaf_data_x <- x[indices,]
  leaf_data_y <- y[indices]
  return (list(leaf_data_x, leaf_data_y))
}

build_tree <- function(x,y) {
  
  if (dim(x)[1] <= leaf_size) {
    leaf_val <- mean(y,na.rm=T)
    if (NA %in% leaf_val || NaN %in% leaf_val) {
      leaf_val <- 0
    }
    return (list(-1, leaf_val, NA, NA))
  }
  
  if (length(unique(y))==1) {
    leaf_val <- y[1]
    if (NA %in% leaf_val || NaN %in% leaf_val) {
      leaf_val <- 0
    }    
    return (list(-1, leaf_val, NA, NA))
  }
  
  else {
    i = random_feature(n_x_features)
    split_val_data <- split_val(i, x)
    split_val = split_val_data[[1]]
    y_split_left <- split_val_data[[2]]
    y_split_right <- split_val_data[[3]]
    make_leaf <- split_val_data[[4]]
    
    if (make_leaf==T) {
      x_y_leaves = make_leaves(x,y)
      x <- data.frame(x_y_leaves[[1]])
      y <- x_y_leaves[[2]]
      y_split_left <- 1:length(x)
      y_split_right <- y_split_left
    }
    

    left_tree <- build_tree(x[which(x[,i] <= split_val),], y[y_split_left])
    # print(class(left_tree))
    if (class(left_tree) != "data.frame") {
      left_tree <- as.data.frame(matrix(left_tree,ncol=4))
    }
    
    right_tree <- build_tree(x[which(x[,i] <= split_val),], y[y_split_right])
    # print(class(right_tree))    
    if (class(right_tree) != "data.frame") {
      right_tree <- as.data.frame(matrix(right_tree,ncol=4))
    }
    
    root_row <- matrix(list(i, split_val, 1, dim(left_tree)[1]+1),ncol=4)
    root <- as.data.frame(root_row)
    model <- bind_rows(root,left_tree,right_tree)
    return (model)
    
  }
}

train <- function(x,y) {
  t <- build_tree(x,y)
  #if (class(t) == "list") browser()
  t <- as.data.frame(t)
  return (t)
}

query_tree <- function(x,t,row=1) {
  # t is a a random tree
  node <- as.numeric(unlist(unlist(t[row,])))
  
  
  if (node[1] == -1){ #leaf
    return (node[2])
  }
  
  else {
    factor = x[,as.integer(node[1])]
    split_val <- node[2]
    
    if (factor <= split_val) {
      row <- row + node[3]
    }
    
    else if (factor > split_val) {
      row <- row + node[4]
    }
    
    y <- query_tree(x, t, row = as.integer(row))
    
    return (y)
  }
}

query <- function(x,t) {
  # t is a tree to be queried
  y <- list()
  for (i in 1:nrow(x)){
    yi <- query_tree(x[i,], t, 1)
    # classify by rounding
    yi <- round(yi,0)
    y <- append(y,yi)    
  }
  
  return (y)
}


# load 
setwd("/Users/matthewhull/r/apple_watch_workouts")
# load("apple_health_data.RData")
load("data/fit_data.RData")
df <- final_df[,2:ncol(final_df)] #eliminate id column


### load prepare train/test data

indices <- sample(1:nrow(final_df),nrow(final_df))
train_indices <- indices[1:round(length(indices)*.9,0)]
test_indices <- indices[round(length(indices)*.9)+1:round(length(indices)*.1,0)]

train_x <- df[train_indices,1:(ncol(df)-1)]
test_x <- df[test_indices,1:(ncol(df)-1)]
train_y <- df[train_indices,ncol(df)]
test_y <- df[test_indices,ncol(df)]

n_x_features <- ncol(train_x)


###################################################################################
#                                                                                 #
#                               SINGLE RT                                         #
#                                                                                 #
###################################################################################
# init params
leaf_size <- 5
tree <- NA

# train & query a single tree
tree <- train(train_x, train_y)
y_query <- query(test_x, tree)

# combine model and test data for comparison
# TODO - wrap this function to quickly measure accuracy.
yy <- data.frame(matrix(y_query,ncol=1))
yy <- cbind(yy,test_y)
colnames(yy) <- c('model', 'test')
#correct answers
matches <- subset(yy, model==test)
acc <- nrow(matches) / nrow(yy)
print(paste("accuracy: ", round(acc,3),"%"))


# train & query 10 models using k-folds cross-validation
# k=10
cross_validate <- function(k=10, leaves=5) {
  model_accuracy_df <- data.frame(matrix(ncol=3,nrow=10))
  colnames(model_accuracy_df) <- c("model","train","test")
  for (i in 1:k) {

    
    indices <- sample(1:nrow(final_df),nrow(final_df))
    train_indices <- indices[1:round(length(indices)*.9,0)]
    test_indices <- indices[round(length(indices)*.9)+1:round(length(indices)*.1,0)]
    
    train_x <- df[train_indices,1:(ncol(df)-1)]
    test_x <- df[test_indices,1:(ncol(df)-1)]
    train_y <- df[train_indices,ncol(df)]
    test_y <- df[test_indices,ncol(df)]
    
    n_x_features <- ncol(train_x)    
    
    
    leaf_size <- leaves
    tree <- NA
    
    # train & query a single tree on test data
    tree <- train(train_x, train_y)
    y_query <- query(test_x, tree)
    
    # combine model and test data for comparison
    yy <- data.frame(matrix(y_query,ncol=1))
    yy <- cbind(yy,test_y)
    colnames(yy) <- c('model', 'test')
    #correct answers
    matches <- subset(yy, model==test)
    test_acc <- nrow(matches) / nrow(yy)
    test_acc <- round(test_acc,3)
    
    
    # query on train data
    y_query_train <- query(train_x, tree)
    
    yy_train <- data.frame(matrix(y_query_train,ncol=1))
    yy_train <- cbind(yy_train, train_y)
    colnames(yy_train) <- c('model', 'train')
    matches_train <- subset(yy_train, model==train)
    train_acc <- nrow(matches_train) / nrow(yy_train)
    train_acc <- round(train_acc, 3)
    

    
    model_accuracy_df[i,] = c(i,train_acc,test_acc)
  }
  return(model_accuracy_df)
}

model_accuracy_df <- cross_validate(leaves=2)

ggplot(model_accuracy_df) + 
  geom_boxplot(aes("Test",test),alpha=.7) +
  geom_point(aes("Test",test)) +
  geom_boxplot(aes("Train",train),alpha=.7) +
  geom_point(aes("Train",train)) +
  ylab("accuracy") +
  xlab("data") +
  ggtitle("RT Accuracy, K-Folds=10, leaves=7")
  

###################################################################################
#                                                                                 #
#                               RANDOM FOREST                                     #
#                                                                                 #
###################################################################################


forest <- function(x, size=20, leaves=5) {
  
  # grow n numbers of random trees as a forest
  # for tree in forest
  #   random number of rows 50:100 of training data
  #   slice data based on rows
  #   train tree
  #   add tree to forest
  
  # params:
  # X is a data frame
  # all cols except last are features
  # last col is Y (labels)
  forest <- list()
  
  ncol_x <- ncol(x)
  nrow_x <- nrow(x)
  
  leaf_size <- leaves
  
  for (i in 1:size){
    
    rows <- sample(1:nrow_x,sample(10:nrow_x))
    train_x <- x[rows,1:ncol_x-1]
    train_y <- x[rows,ncol_x]
    n_x_features <- ncol_x-1        
    tree <- train(train_x, train_y)  
    forest[[i]] <- tree
  }
  
  return (forest)
}

query_forest <- function(data, a_forest){
  # query forest
  # get answers for each set of points
  # average or median answer is final answer
  # we use threshold < .90 as a false, else true.
  # might need to adjust threshold.
  # return answers
  
  forest_size <- length(a_forest)
  
  answer_forest <- data.frame(matrix(nrow=nrow(test_x),ncol=forest_size))
  
  for (i in 1:forest_size){
    a_tree <- a_forest[[i]]
    answers <- query(data, a_tree)
    answers <- unlist(answers)
    answer_forest[,i] <- answers
  }
  
  funx <- function(x,threshold=.5) {
    if (x < threshold) {
      return (0)
    }
    return (1)
  }
  
  cl <- rowMeans(answer_forest)
  
  fa <- lapply(cl, funx, .90) # should threshold be median?
  fa <- unlist(fa)
  
  return (fa)
}

measure_accuracy <- function(answers,solutions) {
  # answers - list from the learner
  # solutions - list of test y data
  df <- data.frame(matrix(answers, ncol=1))
  df <- cbind(df, solutions)
  colnames(df) <- c('answers', 'solutions')
  correct_answers <- subset(df, answers==solutions)
  accuracy <- nrow(correct_answers) / nrow(df)
  accuracy <- round(accuracy, 3)
  return (accuracy)
}

###################################################################################
#                                                                                 #
#                               BUILD FORESTS                                     #
#                                                                                 #
###################################################################################


# experiment 1 - forest accuracy by size
# set up a list of forest sizes
# for size in forest size train forest
# measure accuracy of forest
# plot accuracies
#forest_sizes <- seq(20,200,by=10)
forest_sizes <- seq(5,150,5)
exp1 <- data.frame(matrix(nrow=length(forest_sizes),ncol=2))
colnames(exp1) <- c('forest_size','accuracy')
for (i in 1:length(forest_sizes)) {
  this_size <- forest_sizes[i]
  f <- forest(x=df, this_size)
  forest_answers <- query_forest(test_x, f)
  forest_acc <- measure_accuracy(forest_answers,test_y)
  exp1[i,] <- c(this_size,forest_acc)
}

fm <- lm(data=exp1, accuracy ~ forest_size) #grab the line
ggplot(exp1, mapping=aes(forest_size,accuracy)) + 
  geom_point() +
  geom_smooth() +
#  geom_abline(intercept=coef(fm)[[1]],slope=coef(fm)[[2]]) +
  ggtitle("Accuracy by Forest Size") +
  xlab("Forest Size") +
  ylab("Accuracy")
  


# experiment 2 - forest accuracy by leaf size
# choose optimum forest size from experiment 1
# set up a list of leaf sizes
# train fixed size forests at varying leaf size
# measure accuracies
# plot accuracies
leaf_sizes <- seq(2,20,1)
exp2 <- data.frame(matrix(nrow=length(leaf_sizes),ncol=2))
colnames(exp2) <- c('leaf_size', 'accuracy')
for (i in 1:length(leaf_sizes)) {
  this_size <- leaf_sizes[i]
  f <- forest(x=df, 20, leaves=this_size)
  forest_answers <- query_forest(test_x, f)
  forest_acc <- measure_accuracy(forest_answers, test_y)
  exp2[i,] <- c(this_size, forest_acc)
}

fn <- lm(data=exp2, accuracy ~ leaf_size) # grab the line
ggplot(exp2, mapping=aes(leaf_size, accuracy)) +
  geom_point() +
#  geom_abline(intercept=coef(fn)[[1]],slope=coef(fn)[[2]]) +
  geom_smooth() +
  ggtitle("Accuracy by Leaf Size") +
  xlab("Leaf Size") +
  ylab("Accuracy")


# experiment 3 - forest accuracy by training data size
# vary training data sizes using forest size & leaf size
# from experiments 1 & 2
# measure & plot accuracies


save(ls,file="data/random_tree.Rdata")
