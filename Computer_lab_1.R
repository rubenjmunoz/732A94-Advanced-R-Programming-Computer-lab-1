name <- "Ruben Munoz"
liuid <-  ""

my_num_vector <- function() {
  return(c(log10(11), cos(pi / 5), exp(pi / 3), (1173 %% 7) / 19))
}

filter_my_vector <- function(x, leq) {
  f <- function(x, leq) {
    if (x >= leq) {
      return(NA)
    }
    else{
      return(x)
    }
  }
  return(unlist(lapply(x, f, leq)))
}

dot_prod <- function(a, b) {
  return(sum(a * b))
}

approx_e <- function(N) {
  return(sum(unlist(lapply(c(0:N), function(x)
    1 / factorial(x)))))
}

my_magic_matrix <- function() {
  return(matrix(c(4, 9, 2, 3, 5, 7, 8, 1, 6), nrow = 3, byrow = TRUE))
}

calculate_elements <- function(A) {
  return(length(A))
}

row_to_zero <- function(A, i) {
  A[i, ] <- 0
  return(A)
}

add_elements_to_matrix <- function(A, x, i, j) {
  A[i, j] <- A[i, j] + x
  return(A)
}

my_magic_list <- function() {
  return(list(info = "my own list", my_num_vector(), my_magic_matrix()))
}

change_info <- function(x, text) {
  x[["info"]] <- text
  return(x)
}

add_note <- function(x, note) {
  x[["note"]] <- note
  return(x)
}

sum_numeric_parts <- function(x) {
  return(as.numeric(sum(unlist(lapply(x, function(x)if(!is.character(x))return(sum(x)))))))
}

my_data.frame <- function(){
  return(data.frame(id = c(1,2,3), name = c("John", "Lisa", "Azra"), income = c(7.30, 0.00, 15.21), rich = c(FALSE, FALSE, TRUE)))
}

sort_head <- function(df, var.name, n){
  return(df[order(df[var.name], decreasing = TRUE)[1:n],])
}

add_median_variable <- function(df, j) {
  m = median(df[ ,j])
  f <- function(x) {
    if (x < m) {
      return("Smaller")
    }
    else if (x > m) {
      return("Greater")
    }
    else{
      return("Median")
    }
  }
  df[["compared_to_median"]] <- unlist(lapply(df[,j], f)) 
  return(df)
}

analyze_columns <- function(df, j){
  f1 <- function(x){
    return(c(mean = mean(df[, x]), median = median(df[, x]), sd = sd(df[, x])))
  }
  l <- lapply(j, f1)
  f2 <- function(x){
    return(colnames(df[x]))
  }
  n <- unlist(lapply(j, f2))
  names(l) <- n
  l[["correlation_matrix"]] <- cor(df[n])
  return(l)
}