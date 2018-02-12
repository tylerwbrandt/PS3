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

# 2. Create a student method for the sort function
## sort.student takes in a student and a 4x4 matrix, and uses matrix multiplication
## to decide which group to assign the student to.
## When running the sort.student method, must write sort(student, y = matrix).
## Otherwise, R will think the second argument is for the "decreasing" argument.
sort.student <- function(x, y){
  if(length(y) != 16 | nrow(y) != 4){
    return("Second argument must be a 4x4 matrix")
  }
  a <-  matrix(c(x$courage, x$ambition, x$intelligence, x$effort), nrow = 1)
  b <-  y%*%t(a)
  if (b[1,1] == max(b)){
    return("GRYFFINDOR!")
  } else if (b[2,1] == max(b)){
    return("SYLTHERIN!")
  } else if (b[3,1] == max(b)){
    return("RAVENCLAW!")
  } else if (b[4,1] == max(b)){
    return("HUFFLEPUFF!")
  }
}

# Test function
generic_matrix <- matrix(sample(1:16, 16), nrow = 4)
sort(harry, y = generic_matrix)


# 3. Change the sort function so that it alters the students and includes two classes
sort.student <- function(x, y){
  if(length(y) != 16 | nrow(y) != 4){
    return("Second argument must be a 4x4 matrix")
  }
  a <-  matrix(c(x$courage, x$ambition, x$intelligence, x$effort), nrow = 1)
  b <-  y%*%t(a)
  if (b[1,1] == max(b)){
    class(x) <- "Gryffindor"
    return(x)
  } else if (b[2,1] == max(b)){
    class(x) <- "Slytherin"
    return(x)
  } else if (b[3,1] == max(b)){
    class(x) <- "Ravenclaw"
    return(x)
  } else if (b[4,1] == max(b)){
    class(x) <- "Hufflepuff"
    return(x)
  }
}

# Test function
harry <- sort(harry, y = generic_matrix)
harry

# 4. Create new environments and a generic function that sends students to their environments.

# Create four new environments
Gryffindor_Tower <- new.env()
Black_Lake <- new.env()
Ravenclaw_Tower <- new.env()
Basement <- new.env()

# Create curfew function
## curfew will take in an object of class Gryffindor, Slytherin, Ravenclaw, or Hufflepuff.
## It will move that object to its dormitory (environment) based on the class that it was assigned
curfew <- function(x){
  UseMethod("curfew", x)
}

## Create Gryffindor method
curfew.Gryffindor <- function(x){
  y <- deparse(substitute(x))
  assign(y, get(y), Gryffindor_Tower)
  rm(list = y, envir = .GlobalEnv)
}

## Create Slytherin method
curfew.Slytherin <- function(x){
  y <- deparse(substitute(x))
  assign(y, get(y), Black_Lake)
  rm(list = y, envir = .GlobalEnv)
}

## Create Ravenclaw method
curfew.Ravenclaw <- function(x){
  y <- deparse(substitute(x))
  assign(y, get(y), Ravenclaw_Tower)
  rm(list = y, envir = .GlobalEnv)
}

## Create Hufflepuff method
curfew.Hufflepuff <- function(x){
  y <- deparse(substitute(x))
  assign(y, get(y), Basement)
  rm(list = y, envir = .GlobalEnv)
}


# Test curfew
curfew(harry)
ls(Gryffindor_Tower)
ls(Black_Lake)
ls(Ravenclaw_Tower)
ls(Basement)
ls(globalenv())

