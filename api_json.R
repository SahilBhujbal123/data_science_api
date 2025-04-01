library(plumber)
# library(tidyverse)
library(stringr)
library(tm)
# library(caret)
# library(readr)
# library(openxlsx)
library(stringdist)

# Load the trained model
model_rf <- readRDS("APImodel_1.rds")

# Preprocessing function
preprocess_text <- function(text) {
  text <- tolower(as.character(text))
  text <- gsub("[^a-z0-9\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  
  words <- unlist(strsplit(text, "\\s+"))
  stopwords_custom <- c(stopwords("en"), "and", "the", "such", "also", "for", "with", "that", "this", "those", "these", "there", "here", "from")
  words_clean <- words[!(words %in% stopwords_custom) & nchar(words) > 2]
  
  return(trimws(paste(words_clean, collapse = " ")))
}

# Cosine Similarity Function
compute_cosine_similarity <- function(a, b) {
  a_vec <- strsplit(a, " ")[[1]]
  b_vec <- strsplit(b, " ")[[1]]
  terms <- unique(c(a_vec, b_vec))
  a_freq <- table(factor(a_vec, levels = terms))
  b_freq <- table(factor(b_vec, levels = terms))
  cos_sim <- sum(a_freq * b_freq) / (sqrt(sum(a_freq^2)) * sqrt(sum(b_freq^2)))
  return(ifelse(is.nan(cos_sim), 0, cos_sim))
}

# Round to nearest 0.5
round_to_nearest_half <- function(x) {
  return(round(x * 2) / 2)
}

# Score prediction function
predictScore <- function(model_answer, student_answer) {
  model_answer <- preprocess_text(model_answer)
  student_answer <- preprocess_text(student_answer)
  
  cosine_similarity <- compute_cosine_similarity(model_answer, student_answer)
  length_ratio <- nchar(student_answer) / nchar(model_answer)
  
  new_data <- data.frame(Cosine_Similarity = cosine_similarity, Length_Ratio = length_ratio)
  predicted_score <- predict(model_rf, newdata = new_data)
  rounded_score <- round_to_nearest_half(predicted_score)
  
  return(rounded_score)
}

# API Endpoint: Single Question-Answer Scoring
#* @param model_answer The reference model answer
#* @param student_answer The student's response
#* @post /score_answer
function(model_answer, student_answer) {
  return(list(score = predictScore(model_answer, student_answer)))
}

# API Endpoint: Batch Processing (CSV Upload)
#* @post /score_csv
#* @parser multi
function(req, res) {
  if (is.null(req$body) || length(req$body) == 0) {
    res$status <- 400
    return(list(error = "No file uploaded!"))
  }
  
  uploaded_file <- req$body[[1]]
  if (is.null(uploaded_file$filename) || is.null(uploaded_file$value)) {
    res$status <- 400
    return(list(error = "Invalid file upload!"))
  }
  
  file_content <- rawToChar(uploaded_file$value)
  df <- tryCatch({
    read.csv(text = file_content, stringsAsFactors = FALSE)
  }, error = function(e) {
    res$status <- 400
    return(list(error = "Failed to read CSV content!"))
  })
  
  required_cols <- c("Question", "Model_Answer", "Student_Answer")
  if (!all(required_cols %in% colnames(df))) {
    res$status <- 400
    return(list(error = "CSV must contain columns: Question, Model_Answer, Student_Answer"))
  }
  
  df$Score <- sapply(1:nrow(df), function(i) {
    predictScore(df$Model_Answer[i], df$Student_Answer[i])
  })
  
  output_file <- "scored_output.csv"
  write.csv(df, output_file, row.names = FALSE)
  
  return(list(message = "File processed successfully!", output_file = output_file, data = df))
}

# API Endpoint: JSON Input Processing
#* @post /score_json
#* @param req The JSON input with questions, model answers, and student answers
#* @parser json
function(req, res) {
  if (is.null(req$postBody) || length(req$postBody) == 0) {
    res$status <- 400
    return(list(error = "No JSON data received!"))
  }
  print("request body is:")
  print(req$body)  ## remove later
  print(colnames(req$body))
  
  # Parse JSON body
  input_data <- tryCatch({
    df <- as.data.frame(jsonlite::fromJSON(req$postBody, simplifyDataFrame = TRUE))
    df
  }, error = function(e) {
    res$status <- 400
    return(list(error = "Invalid JSON format!"))
  })
  print("parsed jason is:")
  print(input_data)  ## remove later
  print(colnames(input_data))
  
  required_cols <- c("Question", "Model_Answer", "Student_Answer")
  if (!all(required_cols %in% colnames(input_data))) {
    res$status <- 400
    return(list(error = "JSON must contain 'Question', 'Model_Answer', 'Student_Answer'"))
  }
  
  
  
  # Calculate scores
  input_data$Score <- sapply(1:nrow(input_data), function(i) {
    predictScore(input_data$Model_Answer[i], input_data$Student_Answer[i])
  })
  
  return(input_data)
}

