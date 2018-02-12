#Activity 5: The Sorting Hat!

# 1. make studentmaker function
## student maker takes in the name of a person and returns a student object,
## which is a vector of length five, giving random values for ambition, intelligence, courage, and effort.
studentmaker <- function(person){
  name1 <- person
  ambition1 <- sample(1:100,1)
  intelligence1 <- sample(1:100,1)
  courage1 <- sample(1:100,1)
  effort1 <- sample(1:100,1)
  student <- list(name = name1,courage = courage1,ambition = ambition1,intelligence = intelligence1,effort = effort1)
  class(student) <- "student"
  return(student)
}
harry <- studentmaker("Harry")

sort.student <- function(x, y){
  if(length(y) != 16 | nrow(y) != 4){
    return("Second argument must be a 4x4 matrix")
  }
  a <-  matrix(c(x$courage, x$ambition, x$intelligence, x$effort), nrow = 1)
  b <-  y%*%t(a)
  if (b[1,1] == max(b)){
    return("GRIFFINDOR!")
  } else if (b[2,1] == max(b)){
    return("SYLTHERIN!")
  } else if (b[3,1] == max(b)){
    return("RAVENCLAW!")
  } else if (b[4,1] == max(b)){
    return("HUFFLEPUFF!")
  } else{
    return("I can't decide!")
  }
}

# Test function
generic_matrix <- matrix(1:16, nrow = 4)
sort(harry, y = generic_matrix)
