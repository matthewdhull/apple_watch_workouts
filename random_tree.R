library(dplyr)
library(tidyverse)
library(BBmisc)

setwd("/Users/matthewhull/r/apple_watch_health_analysis")
load("apple_health_data.RData")
df <- final_df
leaf_size <- 5

tree <- NA

### load prepare train/test data

indices <- sample(1:nrow(final_df),nrow(final_df))
train_indices <- indices[1:round(length(indices)*.9,0)]
test_indices <- indices[round(length(indices)*.9)+1:round(length(indices)*.1,0)]

train_x <- df[train_indices,2:(ncol(df)-1)]
test_x <- df[test_indices,2:(ncol(df)-1)]
train_y <- df[train_indices,ncol(df)]
test_y <- df[test_indices,ncol(df)]

n_x_features <- ncol(train_x)


split_val <- function(random_feature,x){
  split_val <- median(x[,random_feature])
  make_leaf <- F
  
  left_tree_y_indices <- which(x[,random_feature] <= split_val)
  right_tree_y_indices <- which(x[,random_feature] > split_val)
  
  x_shape <- dim(x)[1]
  
  if (length(left_tree_y_indices) == x_shape || length(right_tree_y_indices) == x_shape) {
    split_val <- mean(x[,random_feature])
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
    y = mean(y,na.rm=T)
    return (list(-1, y, NA, NA))
  }
  
  if (length(unique(y))==1) {
    y = y[1]
    return (list(-1, y, NA, NA))
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
  tree <- build_tree(x,y)
}



tree <- train(train_x, train_y)


query_tree <- function(x,tree,row=1) {
  node <- as.numeric(unlist(unlist(tree[row,])))
  

  
  
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
    
    y <- query_tree(x, tree, row = as.integer(row))
    
    return (y)
  }
}

query <- function(x,tree) {
  y <- list()
  for (i in 1:nrow(x)){
    yi <- query_tree(x[i,], tree, 1)
    y <- append(y,yi)    
  }
  
  return (y)
}

y_query <- query(test_x, tree)


