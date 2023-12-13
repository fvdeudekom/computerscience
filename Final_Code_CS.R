##### Computer Science Duplicate Detection
##### Fons van Deudekom 512176

# load packages 
install.packages("rjson, jsonlite") # To import the JSON data set
library(rjson)
library(jsonlite)

# load data
zip_file_path <- "C:/Users/Home/Documents/TVs-all-merged.zip"
json_file_name <- "TVs-all-merged.json"
datainlists <- fromJSON(unz(zip_file_path, json_file_name))

# split each dataset into a list of data frames with one row each
split_data <- function(dataset) {
  lapply(seq_len(nrow(dataset)), function(i) dataset[i, , drop = FALSE])
}
data_split <- lapply(datainlists, split_data)

#number of unique tv's
n_unique_TV <- length(datainlists)

# unnest the list to remove one level of nesting
full_data <- unlist(data_split, recursive = FALSE)
n <- length(full_data)

# number of total duplicates
n_duplicates <- n - n_unique_TV

# create vector of titles
full_titles <- vector("numeric", n)
for (i in 1:n) {
  title <- full_data[[i]][["title"]]
  if (!is.null(full_data[[i]][["featuresMap"]][["Brand"]]) && !is.na(full_data[[i]][["featuresMap"]][["Brand"]])) {
    title <- paste(title, full_data[[i]][["featuresMap"]][["Brand"]])
  }
  if (!is.null(full_data[[i]][["featuresMap"]][["Brand Name"]]) && !is.na(full_data[[i]][["featuresMap"]][["Brand Name"]])) {
    title <- paste(title, full_data[[i]][["featuresMap"]][["Brand Name"]])
  }
  full_titles[i] <- tolower(title)
}
# adjust title words 
full_titles <- gsub('inch| inch|-inch| -inch|inches| inches|-inches| -inches|\"','inch',full_titles)
full_titles <- gsub('hz| hz|-hz| -hz|hertz| hertz|-hertz','hz',full_titles)
full_titles <- gsub(',|  |, ',' ',full_titles)
full_titles<- gsub('3-d|3 d|3/d','3d',full_titles)
full_titles<- gsub('diagonal|diag.','diag',full_titles)

# check all duplicates and number of duplicates of total dataset
full_duplicate_matrix <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if ((full_data[[i]][["modelID"]] == full_data[[j]][["modelID"]]) && (i != j)) {
      full_duplicate_matrix[i,j] <-  1
    }
  }
}

# create brand vector to store brands of all observations
for (x in 1:n) {
  if (!is.null(full_data[[x]][["featuresMap"]][["Brand"]]) &&
      !is.na(full_data[[x]][["featuresMap"]][["Brand"]])) {
    brand_vector[x] <- tolower(full_data[[x]][["featuresMap"]][["Brand"]])
  } 
  if (!is.null(full_data[[x]][["featuresMap"]][["Brand Name"]]) &&
      !is.na(full_data[[x]][["featuresMap"]][["Brand Name"]])) {
    brand_vector[x] <- tolower(full_data[[x]][["featuresMap"]][["Brand Name"]])
  }
}

# to store results
count_train <- 1
count_test <- 1
# output vector for training will contain ((number of r) * bootstrap * (#thresholds)) number of rows
train_output_matrix <- matrix(0, 120, 11)
test_output_matrix <- matrix(0, 40, 11)
best_threshold_vector <- vector("numeric", 40)
range <- c(2,5,10,16,20,25,100,400)
for (r in range) {
  lsh <- function(data, titles, threshold) {
    # count true duplicates for data by putting them into a matrix 
    n <- length(data)
    duplicate_matrix <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        if ((data[[i]][["modelID"]] == data[[j]][["modelID"]]) && (i != j)) {
          duplicate_matrix[i,j] <-  1
        }
      }
    }
    duplicate_matrix[lower.tri(duplicate_matrix, diag = TRUE)] <- 0

    # create model words vector and add brands
    brands <- character(0)
    for(i in 1:n) {
      if (!is.null(data[[i]][["featuresMap"]][["Brand"]]) && !is.na(data[[i]][["featuresMap"]][["Brand"]])) {
        if (!(tolower((data[[i]][["featuresMap"]][["Brand"]])) %in% brands)) {
          brands <- append(brands, tolower(data[[i]][["featuresMap"]][["Brand"]]))
        }
      }
      if (!is.null(data[[i]][["featuresMap"]][["Brand Name"]]) && !is.na(data[[i]][["featuresMap"]][["Brand Name"]])) {
        if (!(tolower((data[[i]][["featuresMap"]][["Brand Name"]])) %in% brands)) {
          brands <- append(brands, tolower(data[[i]][["featuresMap"]][["Brand Name"]]))
        }
      }
    }
    brands <- gsub(' ','',brands)
    
    model_words <- vector("numeric")
    for (i in 1:n){
      model_words <- unique(append(model_words, strsplit(titles[i], "\\s+")[[1]]))
    }
    model_words <- model_words[(regexpr("[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*", model_words) > 0)]
    model_words <- append(model_words, brands)
    n_model_words <- length(model_words)
    
    # create binary matrix where element is one if observation's title description consists model word
    binary_matrix <- matrix(0, n, n_model_words)
    for (i in 1:length(data)) {
      for (j in 1:length(model_words)) {
        if (model_words[j] %in% strsplit(titles[i], "\\s+")[[1]]) {
          binary_matrix[i,j] <- 1
        }
      }
    }
    binary_matrix <- t(binary_matrix)
    
    # determine permutations 
    
    n_hashings <- 800
    min_hashing_matrix <- matrix(0, n_model_words, n_hashings)
    for (i in 1:n_hashings) {
      set.seed(i)
      # unique permutation 
      min_hashing_matrix[,i] <- sample(seq(1:n_model_words), replace = FALSE)
    }
    
    # determine signature matrix
    signature_matrix <- matrix(Inf, n_hashings, n)
    for (i in 1:n_model_words) {
      for (j in 1:n) {
        if (binary_matrix[i,j] == 1) {
          signature_matrix[,j] <- apply(cbind(signature_matrix[, j], min_hashing_matrix[i, ]), 1, min)
          
        }
      }
    }
    
    
    b <- n_hashings/r
    candidate_matrix <- matrix(0, n, n)
    for (i in 1:b) {
      if (i < b) {
        # extract a band from the signature_matrix based on the current iteration
        band <- signature_matrix[seq_len(n_hashings)[seq_len(n_hashings) %% r == 1][i]:seq_len(n_hashings)[seq_len(n_hashings) %% r == 0][i], ] 
      }
      if (i == b) {
        # extract the final band from the signature_matrix 
        band <- signature_matrix[seq_len(n_hashings)[seq_len(n_hashings) %% r == 1][i]:n_hashings, ] 
      }
      # initiate buckets for each band but combining rows to a string
      buckets <- sapply(1:n, function(j) paste(toString(band[, j]), collapse = ""))
      for (j in 1:length(buckets)) {
        # find candidate pairs within the same bucket
        candidate_pairs <- which(buckets %in% buckets[[j]])
        for (k in 1:length(candidate_pairs)) {
          # check candidate pair is not the same as the current index and adjust candidate matrix
          if (candidate_pairs[k] != j) {
            candidate_matrix[j, candidate_pairs[k]] <- 1
          }
        }
      }
    }
    
    candidate_matrix[lower.tri(candidate_matrix, diag = TRUE)] <- 0
    
    
    updated_candidate_matrix <- candidate_matrix
    # remove if candidates have different brands or when candidates have the same shop
    for (i in 1:n) {
      for (j in 1:n) {
        if (updated_candidate_matrix[i,j] == 1) {
          if ((!is.na(brand_vector[i])) && (!is.na(brand_vector[j]))) {
            if (brand_vector[i] != brand_vector[j]) {
              updated_candidate_matrix[i,j] <- 0
            }
          }
          if (data[[i]][["shop"]] == data[[j]][["shop"]]) {
            updated_candidate_matrix[i,j] <- 0
          }
        }
      }
    }
    
    
    # iterate through all pairs of observations and check similarity using Jaccard
    for (i in 1:n) {
      for (j in 1:n) {
        if (updated_candidate_matrix[i, j] == 1) {
          
          # calculate Jaccard Similarity 
          similarity <- (sum((binary_matrix[, i] == 1) & (binary_matrix[, j] == 1)))/(sum((binary_matrix[, i] == 1) | (binary_matrix[, j] == 1)))
          
          # check if similarity is below the threshold
          if (similarity < threshold) {
            updated_candidate_matrix[i, j] <- 0
          }
        }
      }
    }
    
    # evaluation measurements
    TP <- sum(duplicate_matrix == 1 & updated_candidate_matrix == 1)
    FP <- sum(duplicate_matrix == 0 & updated_candidate_matrix == 1)
    FN <- sum(duplicate_matrix == 1 & updated_candidate_matrix == 0)
    TN <- sum(duplicate_matrix == 0 & updated_candidate_matrix == 0)
    
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    accuracy <- (TP + TN) / sum(!is.na(updated_candidate_matrix))
    PC <- TP/sum(duplicate_matrix)
    PQ <- TP/sum(candidate_matrix)
    F1 <- (2*precision*recall)/(precision+recall)
    F1_star <- (2*PC*PQ)/(PC+PQ)
    fraction_of_comparisons <- sum(candidate_matrix)/((n*(n-1))/2)
    #print(c(TP, FP, FN, TN, precision, recall, F1, PC, PQ, F1_star, fraction_of_comparisons))
    return <- c(TP, FP, FN, TN, precision, recall, F1, PC, PQ, F1_star, fraction_of_comparisons)
    
  }
  
  for (bootstrap in 1:5) {
    
    n <- 1624
    n_train <- 1024
    n_test <- n - n_train
    set.seed(bootstrap)
    # generate random indices for training and testing sets
    train_indices <- sample(1:n, n_train)
    test_indices <- setdiff(1:n, train_indices)
    
    # extract titles
    titles_train <- full_titles[train_indices]
    titles_test <- full_titles[test_indices]
    
    # create training and testing data sets
    data_train <- full_data[train_indices]
    data_test <- full_data[test_indices]
    
    best_threshold <- 0
    # tune threshold value 
    thresholds <- c(0.7,0.8,0.9)
    best_F1 <- 0
    for (threshold in thresholds) {
      output_train <- lsh(data_train, titles_train, threshold)
      train_output_matrix[count_train,] <- output_train
      if (output_train[7] > best_F1) {
        best_F1 <- output_train[7]
        best_threshold <- threshold
      }
      output_train <- 0 
      count_train <- count_train + 1
    }
    output_test <- lsh(data_test, titles_test, best_threshold)
    test_output_matrix[count_test,] <- output_test
    best_threshold_vector[count_test] <- best_threshold
    count_test <- count_test + 1
  }
}

# determine average test results (over bootstraps)
copy_test_output <- test_output_matrix
# some values where recall and precision are zero
copy_test_output[is.nan(copy_test_output)] <- 0

sum_matrix <- matrix(0, nrow = 8, ncol = ncol(copy_test_output))

for (i in 1:8) {
  start_row <- (i - 1) * 5 + 1
  end_row <- start_row + 4
  sum_matrix[i, ] <- colSums(copy_test_output[start_row:end_row, ])
}
# result matrix for testing, averaged over bootstrap
avg_matrix <- sum_matrix / 5

# plots
col_8 <- avg_matrix[, 8]
col_9 <- avg_matrix[, 9]
col_10 <- avg_matrix[, 10]
col_11 <- avg_matrix[, 11]



png("plot_col_8_vs_col_11.png")
plot(col_11, col_8, ylab = "Pair Completeness (PC)", xlab = "Fraction of comparisons", type = "l", xlim = c(0, 0.2), ylim = c(0, 0.4))
dev.off()

png("plot_col_9_vs_col_11.png")
plot(col_11, col_9, ylab = "Pair Quality (PQ)", xlab = "Fraction of comparisons", type = "l", xlim = c(0, 0.1), ylim = c(0, 0.15))
dev.off()

png("plot_col_10_vs_col_11.png")
plot(col_11, col_10, ylab = "F1*", xlab = "Fraction of comparisons", type = "l", xlim = c(0, 0.1), ylim = c(0, 0.15))
dev.off()

