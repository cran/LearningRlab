#  mean.R
#  Created by Jose Manuel Gomez on 12/8/20.
#

###############################
#####-AUXILIARY FUNCTIONS-#####
###############################

#Print a vector of data
drawVector <- function(buffer){
  for(i in 1:length(buffer)){
    if(i == length(buffer)){
      cat(buffer[i])
    } else {
      cat(buffer[i], ",")
    }
  }
}

#Wait for user activity
getUserAction <- function(){
  #init loop variables
  resp <- "n"
  
  while(resp == "n"){
    cat("Enter natural numbers separated by a space and press enter at the end\n")
    buffer_aux <- readline(prompt = "")
    buffer <- na.omit(as.numeric(unlist(strsplit(buffer_aux, " "))))
    cat("\nYou summited the following : ")
    drawVector(buffer)
    cat("\nAre the data correct? any/n")
    resp <- readline(prompt = "")
    
  }
  return(buffer)
}

#Load the images for interactive functions
initImages <- function(path){
  #needed librarys for read images
  #magick library instalation needed
  img <- image_read(path)
  plot(img)
}

######################
########-MEAN-########
######################

#mean developed in C
meanC <- function(x) {
  
  # res <- .C("meanC", as.double(x), as.integer(0), length(x))
  # print(res[2])
  
}

#principal mean function
mean_ <- function(x){
  x <- as.vector(x)
  suma <- 0
  for(i in 1:length(x)){
    suma <- suma + x[i]
  }
  res <- (suma / length(x))
  return(res)
}


#explained example function
explain.mean <- function(x){
  x <- as.vector(x)
  suma <- 0
  cat(bold("\n__MEAN CALCULUS__ \n"))
  cat("\nThe mean of a dataset is calculated by the sum of the values divided by the number of values. We'll give the user an example for better comprension.\n")
  cat(green("\nFormula -> (x1 + x2 +..+xn) / num_elements\n"))
  cat(green("xn: valor of elements to dataset\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
  cat("\nThe content of the vector is: ")
  for(i in 1:length(x)){
    if( i == length(x)){
      cat(x[i])
      suma <- suma + x[i]
    } else {
      cat(x[i], ",")
      suma <- suma + x[i]
    }
  }
  res <- (suma / length(x))
  cat("\n")
  cat("Now we need to add each element of the vector/dataset\n")
  cat("The sum of the elements is: ", blue(suma), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> ", blue(suma), "/", blue(length(x)) , " = ", bold(res))
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.mean function to practice.\n")
}

#interactive function
interactive.mean <- function(){
  initImages("https://i.imgur.com/O2kShzJ.png")
  #init vars
  cont <- 0
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #mean result
  res <- mean_(buffer)
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the mean calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(res,3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct...")
      if(cont == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont > 2){
        cat(yellow("\nHint 2 -> add each element of your vector and divide it by the length\n\n"))
      }
    }
  }
}


######################
####GEOMETRIC MEAN-###
######################

#principal mean function
geometricMean_ <- function(x){
  x <- as.vector(x)
  producto <- 1
  for(i in 1:length(x)){
    producto <- producto * x[i]
  }
  res <- (producto)^(1/length(x))
  return(res)
}

#explained example function
explain.geometricMean <- function(x){
  x <- as.vector(x)
  producto <- 1
  cat(bold("\n__GEOMETRIC MEAN CALCULUS__ \n"))
  cat("\nThe geometric mean of a dataset is calculated by multiplying each element of the dataset and raising the result to 1 divided by the number of elements in the dataset (the nth root). 
    We'll give the user an example for better comprension.\n")
  cat(green("\nFormula -> (x1 * x2 *..* xn)^( 1 / num_elements)\n"))
  cat(green("xn: valor of elements to dataset\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
  cat("\nThe content of the vector is: ")
  for(i in 1:length(x)){
    if( i == length(x)){
      cat(x[i])
      producto <- producto * x[i]
    } else {
      cat(x[i], ",")
      producto <- producto * x[i]
    }
  }
  res <- ((producto)^( 1 / length(x)))
  cat("\n")
  cat("Now we need to multiply each element of the vector/dataset\n")
  cat("The product of the elements is: ", blue(producto), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> (", blue(producto), ") ^ ( 1 /", blue(length(x)) , ") = ", bold(res))
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.geometricMean function to practice.\n")
}

#interactive function
interactive.geometricMean <- function(){
  initImages("https://economipedia.com/wp-content/uploads/formula-media-geom%C3%A9trica.jpg")
  #init vars
  cont <- 0
  
  buffer = getUserAction()
  
  #mean result
  res <- geometricMean_(buffer)
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the geometric mean calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(res,3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct...")
      if(cont == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont > 2){
        cat(yellow("\nHint 2 -> add each element of your vector and divide it by the length\n\n"))
      }
    }
  }
}

######################
########-MODE-########
######################

#principal mode function
mode_ <- function(x){
  v <- sort(as.vector(x))
  poliMod <- numeric()
  flag <- 1
  max <- 1
  mode <- v[0]
  #save the most common element in the vector
  for(i in 1:length(v)){
    aux <- v[i]
    cont <- 0
    for(j in 1:length(v)){
      #match
      if(aux == v[j]){
        cont <- cont + 1
      }
    }
    #select mode
    if(cont > max){
      mode <- aux
      max <- cont
    }
  }
  
  vect_frec = as.vector(table(v))
  v_aux = unique(sort(v))
  for(i in 1:length(vect_frec)){
    if(vect_frec[i] == max){
      poliMod = append(poliMod, v_aux[i])
    }
  }
  
  cat("Factor appears ",max," times in the vector.\n")
  
  if(length(poliMod) == 1){
    cat("Unique mode ")
    return(mode)
  } else {
    cat("Multiples modes ")
    return(poliMod)
  }
}


#explained example function
explain.mode <- function(x){
  v <- as.vector(x)
  cat(bold("\n__MODE CALCULUS__ \n"))
  cat("\nThe mode of a dataset is calculated by looking for the most repeated value in the dataset. If in a group there are two or several scores with the same frequency and that frequency is the maximum, the distribution is bimodal or multimodal, that is, it has several modes.\n")
  cat(green("\nFormula -> Most repeated value of [Data]\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst step : search the most repeated value\n")
  cat("\nThe content of the vector is: ")
  
  drawVector(v)
  
  cat("\n")
  max <- 1
  mode <- v[0]
  #save the most common element in the vector
  for(i in 1:length(v)){
    aux <- v[i]
    cont <- 0
    for(j in 1:length(v)){
      #match
      if(aux == v[j]){
        cont <- cont + 1
      }
    }
    #select mode
    if(cont > max){
      mode <- aux
      max <- cont
    }
  }
  cat("Factor " , bold(mode) , " appears ", blue(max)," times in the vector.\n")
  
  cat("\nSecond step : check the dataset looking for a value with the same maximum frequency\n")
  cat("\nIf there are only 1 unique most repeated value, it is the mode.\n")
  cat("If there are 2 values repeated with the same maximum frequency each value represents the mode. Bimodal dataset\n")
  cat("If there are more than 2 values repeated with the same maximum frequency, it is a Multimodal dataset\n")
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.mode function to practice.\n")
}

#interactive function
interactive.mode <- function(){
  #initImages("https://i.imgur.com/KWFunWd.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the mode calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(mode_(buffer),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      cat(yellow("\nHint -> Psst!... Take a closer look at the value most often\n\n"))
    }
    
  }
  
}

######################
#######-MEDIAN-#######
######################

#principal median function
median_ <- function(x){
  x <- as.vector(x)
  #sort the vector
  cat("\nSorted vector: ")
  x_sorted <- sort(x)
  cat(x_sorted, "\n")
  #check if pair number of elements
  pair <- as.logical()
  pair <- FALSE
  if(length(x_sorted) %% 2 == 0){
    pair <- TRUE
  }
  #pair number of elements
  if(pair){
    aux <- x_sorted[(length(x) / 2)]
    aux2 <- x_sorted[((length(x) / 2) + 1)]
    res <- ((aux + aux2) / 2)
  } else{
    res <- x_sorted[(length(x_sorted) / 2) + 0.5]
  }
  cat("\n")
  return(res)
}


#explained example function
explain.median <- function(x){
  v <- as.vector(x)
  cat(bold("\n__MEDIAN CALCULUS__ \n"))
  cat("\nThe median of a dataset is the value in the middle of the sorted data. It's important to know that the data must be sorted. If the dataset has a pair number of elements, we should select both in the middle to add each other and get divided by two. If the dataset has a no pair number of elements, we should select the one in the middle.\n")
  cat(green("\nFormula -> 1/2(n+1) where n -> vector size\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst step : identify if the vector has a pair number of elements\n")
  cat("\nThe content of the vector is: ")
  #vector sorted
  v <- sort(v)
  
  drawVector(v)
  
  cat("\n")
  cat("\nSecond step: depending of the number of elements\n")
  if(length(v) %% 2 == 0){
    cat("\nIt has a PAIR number of elements (", blue(length(v)), ")\n")
    #median calculus
    aux <- v[(length(v) / 2)]
    cat("\nWe take the 'n/2' element -> ", blue(aux))
    aux2 <- v[((length(v) / 2) + 1)]
    cat("\nWe take the '(n/2)+1' element -> ", blue(aux2))
    res <- ((aux + aux2) / 2)
    cat("\nNow we add each other and divided it by two")
    cat("\n(",aux," + ", aux2, ") / 2")
    cat("\nThe result is : ", bold(res))
  }else {
    cat("\nIt has a ODD number of elements (", blue(length(v)), ")\n")
    res <- v[(length(v) / 2) + 0.5]
    cat("\nWe take the 'n/2' approaching up element")
    cat("\nThe result is : ", bold(res))
  }
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.median function to practice.\n")
}


#interactive function
interactive.median <- function(){
  initImages("https://i.imgur.com/6rQKiIp.png")
  #init vars
  cont_aux <- 0
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the mode calculus for your data : ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(median_(buffer), 3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux == 2 ){
        cat(yellow("\nHint 2 -> The element at the middle of the dataset\n\n"))
      }
      else if(cont_aux > 2){
        cat(yellow("\nHint 3 -> Check if it has a pair number of elements or not...\n\n"))
      }
    }
  }
}

#########################
### STANDARD DEVIATION ###
#########################

#principal standard deviation function
standardDeviation_ <- function(x){
  x <- as.vector(x)
  mean = mean_(x)
  sum <- 0
  for(i in 1:length(x)){
    sum <- sum + (x[i] - mean)^2
  }
  res <- sqrt(sum/length(x))
  #class(res) <- "standard deviation"
  return(res)
}

#explain example function
explain.standardDeviation <- function(x){
  x <- as.vector(x)
  
  cat(bold("\n__STANDARD DEVIATION CALCULUS__ \n"))
  cat("\nThe standard deviation of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset and finally making the square root on the result. We'll give the user an example for better comprension.\n")
  cat(green("\nFormula ->  square_root ((Summation(each_element - Mean)^2) / num_elements)\n"))
  cat(green("\nMean -> (x1 + x2 +..+xn) / n\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
  cat("\nThe content of the vector is: ")
  
  drawVector(x)
  
  mean <- mean_(x)
  cat("\nThe mean of dataset is...", blue(mean))
  cat("\nThe square of the diference between each number and the mean of dataset is: ")
  suma <- 0
  for(i in 1:length(x)){
    square <- ((x[i] - mean) ^ 2)
    if( i == length(x)){
      cat(square)
      suma <- suma + square
    } else {
      cat(square, ",")
      suma <- suma + square
    }
  }
  res <- sqrt(suma/length(x))
  cat("\nNow we need to add each element of the vector/dataset\n")
  cat("The sum of the squares is: ", blue(suma), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> (", suma, "/", length(x) , ") ^ (1/2) = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.standardDeviation function to practice.\n")
}

#interactive function
interactive.standardDeviation <- function(){
  #initImages("https://imgur.com/cUgFVoa.jpg")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the standard deviation calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(standardDeviation_(buffer),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> The standard deviation of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset and finally making the square root on the result.\n\n"))
      }
      
    }
  }
}


#########################
###     VARIANCE      ###
#########################

#principal variance function
variance_ <- function(x){
  x <- as.vector(x)
  
  mean <- mean_(x)
  suma <- 0
  for(i in 1:length(x)){
    suma <- suma + (x[i] - mean)^2
  }
  res <- (suma/(length(x)-1))
  return(res)
}


#explain example function
explain.variance <- function(x){
  x <- as.vector(x)
  
  cat(bold("\n__VARIANCE CALCULUS__ \n"))
  cat("\nThe variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
  cat(green("\nFormula ->  (Summation(each_element - Mean)^2) / num_elements\n"))
  cat(green("\nMean -> (x1 + x2 +..+xn) / n\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
  cat("\nThe content of the vector is: ")
  
  drawVector(x)
  
  mean <- mean_(x)
  cat("\nThe mean of dataset is...", blue(mean))
  cat("\nThe square of the diference between each number and the mean of dataset is: ")
  suma <- 0
  for(i in 1:length(x)){
    square <- ((x[i] - mean) ^ 2)
    if( i == length(x)){
      cat(square)
      suma <- suma + square
    } else {
      cat(square, ",")
      suma <- suma + square
    }
  }
  res <- (suma/(length(x)-1))
  cat("\nNow we need to add each element of the vector/dataset\n")
  cat("The sum of the squares is: ", blue(suma), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> ", suma, "/", length(x)-1 , " = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.variance function to practice.\n")
}

#interactive function
interactive.variance <- function(){
  initImages("https://miro.medium.com/max/666/0*ovSFlxj9RJMgtQoX.png")
  #init vars
  cont_aux <- 0
  
  buffer = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(variance_(buffer),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> The variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset.\n\n"))
      }
      
    }
  }
}

#########################
### AVERAGE DEVIATION ###
#########################

#principal average deviation function
averageDeviation_ <- function(x){
  x <- as.vector(x)
  
  mean = mean_(x)
  suma <- 0
  for(i in 1:length(x)){
    suma <- suma + abs(x[i] - mean)
  }
  res <- (suma/length(x))
  return(res)
}


#explain sample function
explain.averageDeviation <- function(x){
  x <- as.vector(x)
  
  cat(bold("\n__AVERAGE DEVIATION CALCULUS__ \n"))
  cat("\nThe average deviation of a dataset is calculated by adding the absolute value of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
  cat(green("\nFormula ->  (Summation(abs(each_element - mean))) / num_elements\n"))
  cat(green("\nMean -> (x1 + x2 +..+xn) / num_elements\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
  cat("\nThe content of the vector is: ")
  
  drawVector(x)
  
  mean <- mean_(x)
  cat("\nThe mean of dataset is...", blue(mean))
  cat("\nThe absolute value of the diference between each number and the mean of dataset is: ")
  suma <- 0
  for(i in 1:length(x)){
    absolute <- abs(x[i] - mean)
    if( i == length(x)){
      cat(absolute)
      suma <- suma + absolute
    } else {
      cat(absolute, ",")
      suma <- suma + absolute
    }
  }
  res <- (suma/length(x))
  cat("\nNow we need to add each element of the vector/dataset\n")
  cat("The sum of the squares is: ", blue(suma), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> ", suma, "/", length(x) , " = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.averageDeviation function to practice.\n")
}

#interactive function
interactive.averageDeviation <- function(){
  initImages("https://imgur.com/J138Unu.jpg")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the average deviation calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(averageDeviation_(buffer),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check the function 'explain.averageDeviation'\n\n"))
      }
      
    }
  }
}


######################
######-QUARTILES-#####
######################

#principal quartile functions
quartile_ <- function(x){
  #data must be sorted
  vec <- sort(as.vector(x))
  size <- round((length(vec)+1)/2)
  mid1 <- vec[c(1:size)]
  mid2 <- vec[c(size:length(vec))]
  
  q0 <- vec[c(1)]
  q1 <- median_(mid1)
  q2 <- median_(x)
  q3 <- median_(mid2)
  q4 <- vec[c(length(vec))]
  
  res <- c(q0,q1,q2,q3,q4)
  names(res) <- c("Q0","Q1","Q2","Q3","Q4")
  return(res)
}

#explained sample function
explain.quartile <- function(x){
  #crayon library needed
  cat(bold("\n__QUARTILES CALCULUS__ \n"))
  cat("\nThe quartile divides the dataset in 4 parts as equal as possible.\n")
  cat(green("\nFormula -> First quartile (Q1) as the median of the first half of values. \n"))
  cat(green("             Second quartile (Q2) as the median of the series itself.\n"))
  cat(green("             Third quartile (Q3) as the median of the second half of values. \n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nStep 1: The vector must be sorted.\n")
  
  drawVector(x)
  
  cat("\n")
  cat("\nStep 2: Calculated the quartiles \n")
  
  vec <- sort(as.vector(x))
  size <- round((length(vec)+1)/2)
  mid1 <- vec[c(1:size)]
  mid2 <- vec[c(size:length(vec))]
  
  #ceiling round up the value
  #Quartiles 1, 2 & 3
  q1 <- ceiling((1 * length(vec)) / 4)
  q2 <- ceiling((2 * length(vec)) / 4)
  q3 <- ceiling((3 * length(vec)) / 4)
  
  
  
  q1_ <- median_(mid1)
  cat("\nQ1 -> (median ", mid1, ")  = ", q1_)
  q2_ <- median_(x)
  cat("\nQ1 -> (median ", vec, ")  = ", q2_)
  q3_ <- median_(mid2)
  cat("\nQ1 -> (median ", mid2, ")  = ", q3_)
  
  cat("\n\nVisualization with colors:\n")
  
  #visualization with colors
  for(i in 1:q1){
    cat(vec[i], ",")
  }
  for(i in (q1+1):(q2)){
    cat(blue(vec[i], ","))
  }
  for(i in (q2+1):(q3)){
    cat(green(vec[i], ","))
  }
  for(i in (q3+1):length(vec)){
    if(i == (length(vec))){
      cat(red(vec[i]), "\n")
    } else{
      cat(red(vec[i], ","))
    }
  }
  #displaying results
  cat("\nQ1 -> ", q1_)
  cat(blue(" || Q2 -> ", q2_))
  cat(green(" || Q3 -> ", q3_))
  cat(red(" || Q4 -> onwards"))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.quartile function to practice.\n")
}

#interactive function
interactive.quartile <- function(){
  initImages("https://imgur.com/ZlGsVmn.jpg")
  #init vars
  cont_aux <- 0
  cont_aux_lim <- 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #show data sorted
  buffer = sort(buffer)
  cat("\nData sorted : ")
  
  drawVector(buffer)
  
  vec <- sort(as.vector(buffer))
  size <- round((length(vec)+1)/2)
  mid1 <- vec[c(1:size)]
  mid2 <- vec[c(size:length(vec))]
  
  #quartiles calculus
  q1 <- median_(mid1)
  q2 <- median_(buffer)
  q3 <- median_(mid2)
  #element which represents the quartile limit
  q1_lim <- buffer[ceiling((1 * length(buffer)) / 4)]
  q2_lim <- buffer[ceiling((2 * length(buffer)) / 4)]
  q3_lim <- buffer[ceiling((3 * length(buffer)) / 4)]
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  
  
  #checking loop
  #QUARTILE 1
  flag_q1 <- 1
  while(flag_q1 == 1){
    cat("Please, insert the result of the Quartil 1 calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    q1_resp <- as.numeric(readline(prompt = ""))
    if(q1_resp == q1){
      flag_q1 <- 0
      cat(italic("\nQuartile 1 correct!\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 2 ){
        cat(yellow("\nHint 2 -> (size/4)\n\n"))
      }
      
    }
  }
  
  #checking loop
  #QUARTILE 2
  flag_q2 <- 1
  while(flag_q2 == 1){
    cat("Please, insert the result of the Quantil 2 calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    q2_resp <- as.numeric(readline(prompt = ""))
    if(q2_resp == q2){
      flag_q2 <- 0
      cat(italic("\nQuartile 2 correct!\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 2 ){
        cat(yellow("\nHint 2 -> The 2th Quartile is the median\n\n"))
      }
      
    }
  }
  
  #checking loop
  #QUARTILE 3
  flag_q3 <- 1
  while(flag_q3 == 1){
    cat("Please, insert the result of the Quantil 3 calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    q3_resp <- as.numeric(readline(prompt = ""))
    if(q3_resp == q3){
      flag_q3 <- 0
      cat(italic("\nQuartile 3 correct!\n"))
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 2 ){
        cat(yellow("\nHint 2 -> (3 * size)/4\n\n"))
      }
      
    }
  }
  
  cat("\n\nNow identify the number which represents the limit of each quartil\n\n")
  
  #checking loop
  #QUARTILE 1 LIMIT
  flag_q1_lim <- 1
  while(flag_q1_lim == 1){
    cat("Please, insert the number which represents the Quartile 1 limit for your data : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    q1_resp <- as.numeric(readline(prompt = ""))
    if(q1_resp == q1_lim){
      flag_q1_lim <- 0
      cat(italic("\nQuartile 1 limit correct!\n"))
    } else {
      cont_aux_lim <- cont_aux_lim + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... round up the result of the 1st Quartile\n\n"))
      }
    }
  }
  
  #checking loop
  #QUARTILE 2 LIMIT
  flag_q2_lim <- 1
  while(flag_q2_lim == 1){
    cat("Please, insert the number which represents the Quartile 2 limit for your data : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    q2_resp <- as.numeric(readline(prompt = ""))
    if(q2_resp == q2_lim){
      flag_q2_lim <- 0
      cat(italic("\nQuartile 2 limit correct!\n"))
    } else {
      cont_aux_lim <- cont_aux_lim + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... round up the result of the 2nd Quartile\n\n"))
      }
    }
  }
  
  #checking loop
  #QUARTILE 3 LIMIT
  flag_q3_lim <- 1
  while(flag_q3_lim == 1){
    cat("Please, insert the number which represents the Quartile 3 limit for your data : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    q3_resp <- as.numeric(readline(prompt = ""))
    if(q3_resp == q3_lim){
      flag_q3_lim <- 0
      cat(italic("\nQuartile 3 limit correct!\n"))
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux_lim <- cont_aux_lim + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... round up the result of the 3rd Quartile\n\n"))
      }
    }
  }
  
  cat(italic("\n\nWell done, you've got it!\n\n"))
  
}


######################
#####-PERCENTILES-####
######################

#principal percentile function
percentile_ <- function(x,p){
  if( p <= 1 ){
    #data must be sorted
    data <- as.vector(sort(x))
    size <- length(x)
    
    perc_pos <- (size * p)
    int_div <- (perc_pos %% 1)
    #redondeo
    perc_posRound = ceiling(perc_pos)
    
    if (int_div  != 0 ){
      perc_sol=data[ceiling(perc_pos)]
    }else{
      if( perc_pos == size ){
        perc_sol=data[ceiling(perc_pos)] 
      }else{
        perc_sol= (data[perc_posRound] + data[perc_posRound + 1]) / 2
      }
    }
    
    cat("Percentile ",p*100,"% = ",perc_sol, "\n")
  }else{
    cat("Error, the percentile has to be less o equal than 1")
  }
}

#explained sample function
explain.percentile <- function(x){
  #crayon library needed
  cat(bold("\n__PERCENTILES CALCULUS__ \n"))
  data <- sort(as.vector(x))
  size <- length(data)
  cat("\nThe percentile divides the dataset in 100 parts.\nThe percentile indicates, once the data is ordered from least to greatest, the value of the variable below which a given percentage is located on the data\n")
  cat(green("\nFormula x -> (k * N ) / 100 where k -> [1-100] and N -> vector size\n"))
  cat(green("\nIf rest of x is diference to 0, the value of its percentile will be the position of the quotient of the previous operation.  \n"))
  cat(green("\nIn the opposite case and being 0 will be the sum of the elements whose value is the quotient and following, less in the case of the 100% percentile that will be the last element.  \n"))
  
  cat(bold("\n__Use Example__\n"))
  cat("\nStep 1: The vector must be sorted.\n")
  
  drawVector(data)
  
  cat("\n")
  cat("\nStep 2: Apply the formula (k * N) / 100 where 'k' is [1-100]\n")
  cat("\nWe will calculate the percentiles 1,25,37,50,92 in this example\n")
  
  perc_array <- array(data = NA, dim = 100) #the percentil in our data
  perc_pos_array <- array(data = NA, dim = 100) #the real value of the percentil
  perc_posRound_array <- array(data = NA, dim = 100) #the value rounded up for locate it
  
  #function calculates percentiles [1-100]
  for(i in 1:100){
    perc_pos_array[i] = ((size * i) / 100)
    perc_posRound_array[i] = ceiling(perc_pos_array[i])
    perc_array[i] = data[perc_posRound_array[i]]
  }
  
  cat("\nPercentile 1 -> (1 * ", size , ") / 100 = ", perc_pos_array[1] , "\n")
  cat("\t.Round up the value to locate it in the vector -> ", perc_pos_array[1], " ~ ", perc_posRound_array[1],"\n")
  cat("\t..In our data, the value is = ")
  for(i in 1:size){
    if(i == size){
      if(data[i]==perc_array[1]){
        cat(red(data[i]))
      } else {
        cat(data[i])
      }
    } else{
      if(data[i]==perc_array[1]){
        cat(red(data[i]), ",")
      } else {
        cat(data[i], ",")
      }
    }
  }
  cat("\n")
  
  cat("\nPercentile 25 -> (25 * ", size , ") / 100 = ", perc_pos_array[25] , "\n")
  cat("\t.Round up the value to locate it in the vector -> ", perc_pos_array[25], " ~ ", perc_posRound_array[25],"\n")
  cat("\t..In our data, the value is = ")
  for(i in 1:size){
    if(i == size){
      if(data[i]==perc_array[25]){
        cat(red(data[i]))
      } else {
        cat(data[i])
      }
    } else{
      if(data[i]==perc_array[25]){
        cat(red(data[i]), ",")
      } else {
        cat(data[i], ",")
      }
    }
  }
  cat("\n")
  
  cat("\nPercentile 37 -> (37 * ", size , ") / 100 = ", perc_pos_array[37] , "\n")
  cat("\t.Round up the value to locate it in the vector -> ", perc_pos_array[37], " ~ ", perc_posRound_array[37],"\n")
  cat("\t..In our data, the value is = ")
  for(i in 1:size){
    if(i == size){
      if(data[i]==perc_array[37]){
        cat(red(data[i]))
      } else {
        cat(data[i])
      }
    } else{
      if(data[i]==perc_array[37]){
        cat(red(data[i]), ",")
      } else {
        cat(data[i], ",")
      }
    }
  }
  cat("\n")
  
  cat("\nPercentile 50 -> (50 * ", size , ") / 100 = ", perc_pos_array[50] , "\n")
  cat("\t.Round up the value to locate it in the vector -> ", perc_pos_array[50], " ~ ", perc_posRound_array[50],"\n")
  cat("\t..In our data, the value is = ")
  for(i in 1:size){
    if(i == size){
      if(data[i]==perc_array[50]){
        cat(red(data[i]))
      } else {
        cat(data[i])
      }
    } else{
      if(data[i]==perc_array[50]){
        cat(red(data[i]), ",")
      } else {
        cat(data[i], ",")
      }
    }
  }
  cat("\n")
  
  cat("\nPercentile 92 -> (92 * ", size , ") / 100 = ", perc_pos_array[92] , "\n")
  cat("\t.Round up the value to locate it in the vector -> ", perc_pos_array[92], " ~ ", perc_posRound_array[92],"\n")
  cat("\t..In our data, the value is = ")
  for(i in 1:size){
    if(i == size){
      if(data[i]==perc_array[92]){
        cat(red(data[i]))
      } else {
        cat(data[i])
      }
    } else{
      if(data[i]==perc_array[92]){
        cat(red(data[i]), ",")
      } else {
        cat(data[i], ",")
      }
    }
  }
  cat("\n")
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.percentile function to practice.\n")
  
}

#interactive function
interactive.percentile <- function(){
  initImages("https://wikimedia.org/api/rest_v1/media/math/render/svg/08a1a01d551abc3eb3c2f178a2fc4a8462160d32")
  #init vars
  rand_percentile = sample(1:100,1)
  cont = 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #show data sorted
  buffer = sort(buffer)
  cat("\nData sorted : ")
  
  drawVector(buffer)
  
  perc = rand_percentile/100
  
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the ", rand_percentile ,"% percentile for your data : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    
    resp_percPos <- as.numeric(readline(prompt = ""))
    if(resp_percPos == percentile_(buffer, perc)){
      cat("\nCorrect!\n")
      flag <- 0
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct... Try again")
      if(cont >= 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
    }
  }
}


######################
#-ABSOLUTE FRECUENCY-#
######################

#principal absolute frecuency function
frecuency_abs <- function(v,x){
  v <- as.vector(v)
  x <- as.integer(x)
  count = 0
  for(i in 1:length(v)){
    if(v[i] == x){
      count = count + 1
    }
  }
  return(count)
}

#explained sample function
explain.absolute_frecuency <- function(v,x){
  #crayon library needed
  cat(bold("\n__ABSOLUTE FRECUENCY CALCULUS__ \n"))
  data <- sort(as.vector(v))
  size <- length(v)
  cat("\nThe absolute frequency (Ni) of a value Xi is the number of times the value is in the set (X1, X2, ..., XN)\n")
  cat(green("\nFormula -> N1 + N2 + N3 + ... + Nk -> Nk = X (Where 'X' is the element we want to examine)\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nAll we need to do is count the number of times that the element ", x ," appears in our data set\n")
  cat("\nOur data set: ")
  
  drawVector(data)
  
  count = frecuency_abs(v,x)
  cat("\n\nNow count the number of times that the element ", blue(x) ," appears: ", bold(count), "\n")
  for(i in 1:size){
    if(i == size){
      if(v[i] == x){
        cat(red(v[i]))
      } else{
        cat(v[i])
      }
    } else{
      if(v[i] == x){
        cat(red(v[i],","))
      } else{
        cat(v[i], ",")
      }
    }
  }
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.absolute_frecuency function to practice.\n")
}

#interactive function
interactive.absolute_frecuency <- function(){
  initImages("https://imgur.com/SrtNH1t.jpg")
  #init vars
  cont = 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #show data sorted
  buffer_sort = sort(buffer)
  cat("\nData sorted : ")
  
  drawVector(buffer_sort)
  
  size = length(buffer)
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  rand <- sample(buffer,1)
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the absolute frecuency (Fi) of the data '", rand ,"' : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    resp <- as.numeric(readline(prompt = ""))
    if(resp == frecuency_abs(buffer,rand)){
      cat(bold("\n\nWell done !\n\n"))
      flag <- 0
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct... Try again")
      if(cont >= 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
    }
  }
}



######################
#-RELATIVE FRECUENCY-#
######################

#principal relative frecuency function
frecuency_relative <- function(v,x){
  v <- as.vector(v)
  x <- as.integer(x)
  f_abs = frecuency_abs(v,x)
  f_rel = (f_abs/length(v))
  return(f_rel)
}


#explained sample function
explain.relative_frecuency <- function(v,x){
  #crayon library needed
  cat(bold("\n__RELATIVE FRECUENCY CALCULUS__ \n"))
  data <- sort(as.vector(v))
  size <- length(v)
  cat("\nThe relative frequency is the quotient between the absolute frequency of a certain value and the total number of data\n")
  cat(green("\nFormula -> (Abs_frec(X) / N ) -> Where 'X' is the element we want to examine\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nStep 1: count the number of times that the element ", blue(x) ," appears in our data set\n")
  cat("\nOur data set: ")
  
  drawVector(data)
  
  count = frecuency_abs(v,x)
  cat("\n\nNow count the number of times that the element ", blue(x) ," appears: ", blue(count), "\n")
  for(i in 1:size){
    if(i == size){
      if(v[i] == x){
        cat(red(v[i]))
      } else{
        cat(v[i])
      }
    } else{
      if(v[i] == x){
        cat(red(v[i],","))
      } else{
        cat(v[i], ",")
      }
    }
  }
  cat("\nStep 2: divide it by the length of the data set\n")
  #calculate the result
  rel_frec = frecuency_relative(v,x)
  cat("\nSolution --> relative_frecuency = (absolute_frecuency(x) / length(data)) = ", count, " / ", size, " = ", bold(rel_frec), ".\n")
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.relative_frecuency function to practice.\n")
}

#interactive function
interactive.relative_frecuency <- function(){
  initImages("https://imgur.com/dziOYno.jpg")
  #init vars
  cont = 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #show data sorted
  buffer_sort = sort(buffer)
  cat("\nData sorted : ")
  
  drawVector(buffer_sort)
  
  size = length(buffer)
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  rand <- sample(buffer,1)
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the relative frecuency of the data '", rand ,"' : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    cat("If the number has decimals, round to the 3rd\n")
    resp <- as.numeric(readline(prompt = ""))
    if(resp == round(frecuency_relative(buffer,rand), 3)){
      cat(bold("\n\nWell done !\n\n"))
      flag <- 0
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct... Try again")
      if(cont >= 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
    }
  }
}


#########################
#-RELATIVE FRECUENCY ACUM-#
#########################

#principal function
frecuency_relative_acum <- function(v,x){
  if(is.element(x,v)){
    v <- sort(as.vector(v))
    x <- as.integer(x)
    count = 0
    
    for(i in 1:x){
      if(is.element(i,v)){
        count = count + frecuency_abs(v,i)
      }
    }
    
    res = count / length(v)
    return(res)
  } else{
    cat("Not found element [",x,"]\n")
  }
}

#explained sample function
explain.relative_acum_frecuency <- function(v,x){
  #crayon library needed
  cat(bold("\n__RELATIVE ACUMULATED FRECUENCY CALCULUS__ \n"))
  data <- sort(as.vector(v))
  size <- length(v)
  cat("\nThe relative acumulated frequency is the quotient between the sum of the absolute frequency of the values minors or equals than the value we want to examine, and the total number of data\n")
  cat(green("\nFormula -> (Summation(abs_frecuency <= X) / N ) -> Where 'X' is the element we want to examine\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nStep 1: count the number of times that the elements minors or equals than ", blue(x) ," appears in our data set\n")
  cat("\nOur data set: ")
  
  drawVector(data)
  
  count = frecuency_absolute_acum(data,x)
  cat("\n\nNumber of times that elements minors or equals to ", blue(x) ," appears = ", blue(count), "\n")
  for(i in 1:size){
    if(i == size){
      if(v[i] == x || v[i] < x){
        cat(red(v[i]))
      } else{
        cat(v[i])
      }
    } else{
      if(v[i] == x || v[i] < x){
        cat(red(v[i],","))
      } else{
        cat(v[i], ",")
      }
    }
  }
  cat("\nStep 2: divide it by the length of the data set\n")
  #calculate the result
  rel_frec_acum = frecuency_relative_acum(data,x)
  cat("\nSolution --> relative_frecuency_acum = (Summation(abs_frecuency <= X) / length(data)) = ", count, " / ", size, " = ", bold(rel_frec_acum), ".\n")
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.relative_acum_frecuency function to practice.\n")
}

#interactive function
interactive.relative_acum_frecuency <- function(){
  initImages("https://imgur.com/SYPzHlZ.jpg")
  #init vars
  cont = 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #show data sorted
  buffer = sort(buffer)
  cat("\nData sorted : ")
  
  drawVector(buffer)
  
  size = length(buffer)
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  rand <- sample(buffer,1)
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the relative acumulated frecuency of the data '", blue(rand) ,"' : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    cat("If the number has decimals, round to the 3rd\n")
    
    resp <- as.numeric(readline(prompt = ""))
    if(resp == round(frecuency_relative_acum(buffer,rand), 3)){
      cat(bold("\n\nWell done !\n\n"))
      flag <- 0
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct... Try again")
      if(cont == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      } else if(cont > 1){
        cat(yellow("\nHint 2 -> Hey! remember that the maximum value for the relative acumulated frecuency is 1!\n"))
      }
    }
  }
}

#########################
#-ABSOLUTE FRECUENCY ACUM-#
#########################

#principal function
frecuency_absolute_acum <- function(v,x){
  if(is.element(x,v)){
    v <- sort(as.vector(v))
    x <- as.integer(x)
    count = 0
    
    for(i in 1:x){
      if(is.element(i,v)){
        count = count + frecuency_abs(v,i)
      }
    }
    
    res = count
    return(res)
  } else{
    cat("Not found element [",x,"]\n")
  }
}

#explained sample function
explain.absolute_acum_frecuency <- function(v,x){
  #crayon library needed
  cat(bold("\n__ABSOLUTE ACUMULATED FRECUENCY CALCULUS__ \n"))
  data <- sort(as.vector(v))
  size <- length(v)
  cat("\nThe absolute acumulated frequency is the sum of the absolute frequency of the values minors or equals than the value we want to examine\n")
  cat(green("\nFormula -> Summation(abs_frecuency <= X ) -> Where 'X' is the element we want to examine\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nStep 1: count the number of times that the elements minors or equals than ", blue(x) ," appears in our data set\n")
  cat("\nOur data set: ")
  
  drawVector(data)
  
  count = frecuency_absolute_acum(data,x)
  cat("\n\nNumber of times that elements minors or equals to ", blue(x) ," appears = ", blue(count), "\n")
  for(i in 1:size){
    if(i == size){
      if(v[i] == x || v[i] < x){
        cat(red(v[i]))
      } else{
        cat(v[i])
      }
    } else{
      if(v[i] == x || v[i] < x){
        cat(red(v[i],","))
      } else{
        cat(v[i], ",")
      }
    }
  }
  
  cat("\nSolution --> absolute_frecuency_acum = Summation(abs_frecuency <= X)  = ", bold(count), ".\n")
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.absolute_acum_frecuency function to practice.\n")
}

#interactive function
interactive.absolute_acum_frecuency <- function(){
  initImages("https://imgur.com/nYzYn1N.jpg")
  #init vars
  cont = 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #show data sorted
  buffer = sort(buffer)
  cat("\nData sorted : ")
  
  drawVector(buffer)
  
  size = length(buffer)
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  rand <- sample(buffer,1)
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the absolute acumulated frecuency of the data '", rand ,"' : ")
    cat("\n(remember your data) -> ", buffer , "\n")
    cat("If the number has decimals, round to the 3rd\n")
    
    resp <- as.numeric(readline(prompt = ""))
    if(resp == round(frecuency_absolute_acum(buffer,rand), 3)){
      cat(bold("\n\nWell done !\n\n"))
      flag <- 0
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct... Try again")
      if(cont == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      } else if(cont > 1){
        cat(yellow("\nHint 2 -> Hey! remember that the maximum value for the absolute acumulated frecuency is 1!\n"))
      }
    }
  }
}


#########################
### -HARMONIC MEDIAN- ###
#########################

#principal harmonic median function
harmonicMean_ <- function(x){
  x <- as.vector(x)
  #sort the vector
  cat("\nSorted vector: ")
  x_sorted <- sort(x)
  cat(x_sorted, "\n")
  #function
  denominator = 0
  
  for(i in x_sorted){
    denominator = denominator + (1 / i)
  }
  res <- length(x_sorted)/(denominator)
  cat("\n")
  return(res)
}

#explained example function
explain.harmonicMean <- function(x){
  x <- as.vector(x)
  x_sorted <- sort(x)
  denominator <- 0
  cat(bold("\n__HARMONIC MEAN CALCULUS__ \n"))
  cat("\nThe harmonic mean of a dataset is calculated by the number of values by divided the inverse sum of the values . We'll give the user an example for better comprension.\n")
  cat(green("\nFormula -> num_elements/ (1/x1 + 1/x2 +..+ 1/xn) \n"))
  cat(green("xn: valor of elements to dataset\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the content of the dataset/vector of numbers\n")
  cat("\nThe content of the vector is: ")
  for(i in x_sorted){
    if( i == length(x_sorted)){
      cat(x[i])
    } else {
      cat(x[i], ",")
    }
    denominator <- denominator + (1/i)
  }
  res <- (length(x)/denominator)
  cat("\n")
  cat("The invert sum of the elements is: ", blue(denominator), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> ", blue(length(x)), "/", blue(denominator), " = ", bold(res))
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.harmonicMean function to practice.\n")
}

#interactive function
interactive.harmonicMean <- function(){
  initImages("https://www.universoformulas.com/imagenes/formulas/estadistica/descriptiva/media-armonica.jpg")
  #init vars
  cont <- 0
  
  cat("\nInsert your data set:\n")
  buffer = getUserAction()
  
  #mean result
  res <- harmonicMean_(buffer)
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the harmonic mean calculus for your data : ")
    cat("\nIf the number has decimals, round to the 3rd\n")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(res,3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont <- cont + 1
      cat("Ups, that might not be correct...")
      if(cont == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont > 2){
        cat(yellow("\nHint 2 -> add each element of your vector and divide it by the length\n\n"))
      }
    }
  }
}



###########################
###     COVARIANCE      ###
###########################

#principal covariance function
covariance_ <- function(x, y){
  x <- as.vector(x)
  y <- as.vector(y)
  
  meanx <- mean_(x)
  meany <- mean_(y)
  
  sum <- 0
  
  for(i in 1:length(x)){
    sum <- sum + ((x[i] - meanx)*(y[i] - meany))
  }
  
  res <- (sum/length(x))
  return(res)
}

#explain example function
explain.covariance <- function(x,y){
  x <- as.vector(x)
  
  cat(bold("\n__COVARIANCE CALCULUS__ \n"))
  cat("\nThe covariance of a dataset is calculated by product of sum of elements of x minus the mean's x and sum elements of y minus the mean's y. All of then divide by size of anyone dataset.\n")
  cat(green("\nFormula ->  ((Summation(each_element(x) - Mean(x))) * Summation(each_element(y) - Mean(y))) / num_elements\n"))
  cat(green("\nMean -> (x1 + x2 +..+xn) / n\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
  cat("\nThe contents of the vectors are: ")
  
  drawVector(x)
  drawVector(y)
  
  meanx <- mean_(x)
  meany <- mean_(y)
  cat("\nThe mean of x dataset is...", blue(meanx))
  cat("\nThe mean of y dataset is...", blue(meany))
  cat("\nThe difference of elements by their means: ")
  sumax <- 0
  for(i in x){
    if( i == length(x)){
      cat(i - meanx)
      sumax <- sumax + (i - meanx)
    } else {
      cat(i - meanx, ",")
      sumax <- sumax + (i - meanx)
    }
  }
  sumay <- 0
  for(i in y){
    if( i == length(y)){
      cat(i - meany)
      sumay <- sumay + (i - meany)
    } else {
      cat(i - meany, ",")
      sumay <- sumay + (i - meany)
    }
  }
  
  res <- ((sumax*sumay)/length(x))
  cat("\nNow we need to add each element of the vector/dataset\n")
  cat("The sum of the elements of x is: ", blue(sumax), "\n")
  cat("The sum of the elements of y is: ", blue(sumay), "\n")
  cat("\nNext step, get the number of elements that we've examined")
  cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
  cat("\nFormula applied -> (", sumax, "*",sumay , ") /", length(x), " = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.covariance function to practice.\n")
}

#interactive function
interactive.covariance <- function(){
  initImages("https://economipedia.com/wp-content/uploads/F%C3%B3rmula-de-la-Covarianza-300x55.jpg")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your first data set:\n")
  x = getUserAction()
  
  cat("\nInsert your second data set:\n")
  y = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(covariance_(x,y),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> The covariance of a dataset is calculated by product of sum of elements of x minus the mean's x and sum elements of y minus the mean's y. All of then divide by size of anyone dataset.\n\n"))
      }
      
    }
  }
}


#######################################
### PEARSON CORRELATION COEFFICIENT ###
#######################################

#principal correlation coefficient function
pearson_ <- function(x, y){
  x <- as.vector(x)
  y <- as.vector(y)
  
  res <- (covariance_(x,y)/(standardDeviation_(x)*standardDeviation_(y)))
  return(res)
}


#explain example function
explain.pearson <- function(x,y){
  x <- as.vector(x)
  
  cat(bold("\n__PEARSON CORRELATION COEFFICIENT__ \n"))
  cat("\nPearson's correlation coefficient is the covariance of the two variables divided by the product of their standard deviations.It has a value between +1 and -1. A value of +1 is total positive linear correlation, 0 is no linear correlation, and -1 is total negative linear correlation.\n")
  cat(green("\nFormula ->  (covariance(x,y) / (standardDeviation(x) * standardDeviation(y))\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
  cat("\nThe contents of the vectors are: ")
  
  drawVector(x)
  drawVector(y)
  
  covar <- covariance_(x,y)
  sDX <- standardDeviation_(x)
  sDY <- standardDeviation_(y)
  
  
  res <- (covar/(sDX*sDY))
  cat("The value of covariance: ", blue(covar), "\n")
  cat("The standard deviation of the elements of x is: ", blue(sDX), "\n")
  cat("The standard deviation of the elements of y is: ", blue(sDY), "\n")
  cat("\nFormula applied -> (", covar, "/ (",sDX , " * ", sDY, ") = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.pearson function to practice.\n")
}

#interactive function
interactive.pearson <- function(){
  initImages("https://i.stack.imgur.com/Jy6Vg.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your first data set:\n")
  x = getUserAction()
  cat("\nInsert your second data set:\n")
  y = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(pearson_(x,y),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }else if(cont_aux == 2){
        cat(yellow("\nHint -> It has a value between +1 and -1 -->\n\n"))
      }
      else if(cont_aux > 2 ){
        cat(yellow("\nHint 2 -> Pearson's correlation coefficient is the covariance of the two variables divided by the product of their standard deviations.It has a value between +1 and -1. A value of +1 is total positive linear correlation, 0 is no linear correlation, and -1 is total negative linear correlation.\n\n"))
      }
      
    }
  }
}


################################
### COEFFICIENT OF VARIATION ###
################################

#principal coefficient of variation function
cv_ <- function(x){
  x <- as.vector(x)
  res <- (standardDeviation_(x)/mean_(x))
  return(res)
}


#explain example function
explain.cv <- function(x){
  x <- as.vector(x)
  
  cat(bold("\n__COEFFICIENT OF VARIATION__ \n"))
  cat("\nThe coefficient of variation (CV) is defined as the ratio of the standard deviation to the mean.\n")
  cat(green("\nFormula ->  (standardDeviation(x) / mean(x))\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
  cat("\nThe contents of the vector is: ")
  
  drawVector(x)
  
  sDX <- standardDeviation_(x)
  meanx <- mean_(x)
  res <- (sDX/(meanx))
  
  cat("The standard deviation of the elements of x is: ", blue(sDX), "\n")
  cat("The value of mean: ", blue(meanx), "\n")
  cat("\nFormula applied -> (", sDX, "/ ",meanx , " = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.cv function to practice.\n")
}

#interactive function
interactive.cv <- function(){
  initImages("https://cdn.citl.illinois.edu/courses/kines401/lesson1_lectures/lecture1_p5/images/objects/obj26-3.jpg")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your data set:\n")
  x = getUserAction()
  buffer = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(cv_(buffer),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> The coefficient of variation (CV) is defined as the ratio of the standard deviation to the mean.\n\n"))
      }
      
    }
  }
}

################################
###      LAPLACE`S RULE      ###
################################

#principal function
laplace_ <- function(x,y){
  
  res <- (length(x)/length(y))
  return(res)
}


#explain example function
explain.laplace <- function(x,y){
  x <- as.vector(x)
  
  cat(bold("\n__LAPLACE`S RULE __ \n"))
  cat("\nLaplace's rule as the quotient between the number of favorable cases to A, and that of all possible results of the experiment.\n")
  cat(green("\nFormula ->  (Cases favorable to A / All possible results)\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
  cat("\nThe contents of the vector is: ")
  
  drawVector(x)
  
  casesF <- length(x)
  casesT <- length(y)
  res <- (casesF/casesT)
  
  cat("Favorables cases: ", blue(casesF), "\n")
  cat("All possible results: ", blue(casesT), "\n")
  cat("\nFormula applied -> (", casesF, "/ ",casesT , " = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.laplace function to practice.\n")
}

#interactive function
interactive.laplace <- function(){
  initImages("https://img.webme.com/pic/f/fundamentosestadisticos/eba02e_cbcbeecd008b4053a3743baff59d7a99.jpg_256.jpg")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your first data set:\n")
  x = getUserAction()
  cat("\nInsert your second data set:\n")
  y = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(laplace_(x,y),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Laplace's rule as the quotient between the number of favorable cases to A, and that of all possible results of the experiment.\n\n"))
      }
      
    }
  }
}

################################
###  BIONOMIAL DISTRIBUTION  ###
################################

#principal function
binomial_ <- function(n,x,p){
  
  res <- (factorial(n) / (factorial(x) * factorial(n-x))) * (p ^ x) * (1 - p)^(n - x)
  return(res)
}


#explain example function
explain.binomial <- function(n,x,p){
  cat(bold("\n__BIONOMIAL DISTRIBUTION__ \n"))
  cat("\nBinomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments, each asking a yes or no question, and each with its 
        own Boolean-valued outcome: success (with probability p) or failure (with probability q = 1 - p)\n")
  cat(green("\nFormula ->  ((factorial(n) / (factorial(x) * factorial(n-x))) * (p ^ x) * (1 - p)^(n - x))\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the n, the number of trials\n")
  cat("In this case n=",n,"\n")
  cat("\nSecond, we need to know the p, probability of success.\n")
  cat("In this case p=",p,"\n")
  cat("\nFinally, we need to know the x, binomial random variable\n")
  cat("In this case x=",x,"\n")
  
  res <- binomial_(n,x,p)
  
  cat("\nFormula applied -> (factorial(",n,") / (factorial(",x,") * factorial(",n,"-",x,"))) * (",p," ^ ",x,") * (1 - ",p,")^(",n," - ",x,") = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.binomial function to practice.\n")
}

#interactive function
interactive.binomial <- function(){
  initImages("https://phhp-faculty-cantrell.sites.medinfo.ufl.edu/files/2013/02/mod8-binomform.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert the n, the number of trials\n")
  n = getUserAction()
  cat("\nInsert p, probability of success.\n")
  p = getUserAction()
  cat("\nInsert the x, binomial random variable\n")
  x = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == binomial_(n,x,p)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong\n\n"))
      }
      
    }
  }
}

################################
###   POISSON DISTRIBUTION   ###
################################

#principal function
poisson_ <- function(k,lam){
  
  e <- 2.718281828459045235360
  res <- ((e ^ (- lam)) * (lam ^ k )) / (factorial(k))
  return(res)
}

#explain example function
explain.poisson <- function(k,lam){
  e <- 2.718281828459045235360
  cat(bold("\n__POISSON DISTRIBUTION__ \n"))
  cat("\nPoisson distribution that expresses the probability of a given number of events occurring in a fixed interval of time or space if these events occur with a known constant mean rate and independently of the time since the last event\n")
  cat(green("\nFormula ->  ((e ^ (- lam)) * (lam ^ k)) / factorial(k)\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the e, the s Euler's number\n")
  cat("In this case e=",e," \n")
  cat("\nSecond, we need to know the lam, it is a positive parameter that represents the number of times the phenomenon is expected to occur during a given interval.\n")
  cat("In this case lam=",lam,"\n")
  cat("\nFinally, we need to know the k, the number of occurrences.\n")
  cat("In this case k=",k,"\n")
  
  res <- poisson_(k,lam)
  
  cat("\nFormula applied -> ((",e,"  ^ (- ",lam,")) * (",lam," ^ ",k,")) / factorial(",k,") = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.poisson function to practice.\n")
}

#interactive function
interactive.poisson <- function(){
  initImages("https://i2.wp.com/makemeanalyst.com/wp-content/uploads/2017/05/Poisson-Distribution-Formula.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert the lam, parameter that represents the number of times.\n")
  lam = getUserAction()
  cat("\nInsert k, the number of occurrences.\n")
  k = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == poisson_(k,lam)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
      }
      
    }
  }
}


################################
###   NORMAL DISTRIBUTION   ###
################################

#principal function
normal_ <- function(x){
  
  e <- 2.718281828459045235360
  pi <- 3.14159265358979323846
  res <- (1/(2*pi)^(1/2)) * (e)^((-x^2)/2)
  return(res)
}

#explain example function
explain.normal <- function(x){
  pi <- 3.14159265358979323846
  e <- 2.718281828459045235360
  cat(bold("\n__NORMAL DISTRIBUTION__ \n"))
  cat("\n The standard normal distribution is one that has the mean value of zero, M = 0, and the standard deviation of unity, Sigma = 1.
Its density function is:\n")
  cat(green("\nFormula ->  (1/(2pi)^(1/2)) * (e)^((-x^2)/2)\n"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the e, the s Euler's number\n")
  cat("In this case e=",e," \n")
  cat("\nFinally, we need to know pi, the number pi.\n")
  cat("In this case pi=",pi,"\n")
  
  res <- normal_(x)
  
  cat("\nFormula applied -> (1/(2*",pi,")^(1/2)) * (",e,")^((-",x,"^2)/2) = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.normal function to practice.\n")
}

#interactive function
interactive.normal<- function(){
  initImages("https://www.ztable.net/wp-content/uploads/2020/08/cdf.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert your data set:\n")
  x = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == normal_(x)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
      }
      
    }
  }
}


################################
###  T-STUDENT DISTRIBUTION  ###
################################

#principal function
tstudent_ <- function(x,u,s,n){
  res <- (x-u)/(s/(n)^(1/2))
  return(res)
}

#explain example function
explain.tstudent <- function(x,u,s,n){
  cat(bold("\n__T-STUDENT DISTRIBUTION__ \n"))
  cat("\nT-student is a probability distribution that arises from the problem of estimating the mean of a normally distributed population when the sample size is small.\n")
  cat(green("\nFormula ->  (x-u)/(s/(n)^(1/2))"))
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need to know the x, is sample mean\n")
  cat("In this case x=",x," \n")
  cat("\nSecond, we need to know the u, is population mean\n")
  cat("In this case u=",u," \n")
  cat("\nNext, we need to know the s, is population standard deviation\n")
  cat("In this case s=",s," \n")
  cat("\nFinally, we need to know the n, is sample size.\n")
  cat("In this case n=",n,"\n")
  
  res <- tstudent_(x,u,s,n)
  
  cat("\nFormula applied -> (",x," - ",u,")/(",s,"/(",n,")^(1/2)) = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.tstudent function to practice.\n")
}

#interactive function
interactive.tstudent <- function(){
  initImages("https://dataz4s.com/wp-content/uploads/2020/03/01.-Students-t-distribution.-Z-formula-normal-distribution.jpg")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert the the x (sample mean)\n")
  x = getUserAction()
  cat("\nInsert the the u (population mean)\n")
  u = getUserAction()
  cat("\nInsert the the  s (population standard deviation)\n")
  s = getUserAction()
  cat("\nInsert the the  n (is sample size)\n")
  n = getUserAction()
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(tstudent_(x,u,s,n),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
      }
      
    }
  }
}


############################################
###  CALCUATED CHI-SQUARED DISTRIBUTION  ###
############################################

#principal function
chisquared_ <- function(x,y){
  sizex <- length(x)
  sizey <- length(y)
  total = 0
  if (sizex == sizey){
    for(i in 1:sizex){
      total <- total + ((x[i]-y[i])^2)/y[i]
    }
    res <- total
    return(res)
  }else{
    cat("Size of sample is not correct")
  }
}

#explain example function
explain.chisquared <- function(x,y){
  cat(bold("\n__CALCUATED CHI-SQUARED DISTRIBUTION__ \n"))
  cat("\nCalculated chi-squared is a probability distribution that serves to manifest tests in hypothesis of frequencies, this test compares observed frequencies with those expected frequencies.\n")
  cat(green("\nFormula ->  ((x[1]-y[1])^2)/y[1] + ((x[2]-y[2])^2)/y[2] + ... + ((x[n]-y[n])^2)/y[n]"))
  
  sizex <- length(x)
  sizey <- length(y)
  
  if (sizex == sizey){
    cat(bold("\n__Use Example__\n"))
    cat("\nFirst of all, we need to know the contents of the datasets/vectors of numbers\n")
    cat("\nThe contents of the vector x is: \n")
    drawVector(x)
    
    cat("\nThe contents of the vector y is: \n")
    drawVector(y)
    cat("\n")
    
    res <- chisquared_(x,y)
    total = 0
    cat("\nFormula applied ->")
    for(i in 1:sizex){
      chi <- ((x[i]-y[i])^2)/y[i]
      total <- total + chi
      if(i == sizex){
        cat(red(chi))
      } else{
        cat(red(" ",chi,"+"))
      }
    }
    
    cat(red(" = "))
    cat(bold(total))
    
    cat("\nNow try by your own! :D\n")
    cat("\nUse interactive.chisquared function to practice.\n")
    
  }else{
    cat("Size of sample is not correct")
  }
}

#interactive function
interactive.chisquared <- function(){
  initImages("https://www.whatissixsigma.net/wp-content/uploads/2019/04/ChiSquaredTest-Image_1.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert the dataset x:\n")
  x = getUserAction()
  cat("\nInsert the dataset y:\n")
  y = getUserAction()
  
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(chisquared_(x,y),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
      }
      
    }
  }
}


###############################
###  F FISHER DISTRIBUTION  ###
###############################

#principal function
fisher_ <- function(x,y){
  meant <- (mean_(x) + mean_(y))/2
  sx2 <- 2 * (((mean_(x)-meant)^2) + ((mean_(y)-meant)^2))
  sw2 <- (variance_(x) + variance_(y))/ 2
  res <- sx2/sw2
  return(res)
}

#explain example function
explain.fisher <- function(x,y){
  cat(bold("\n__ F FISHER DISTRIBUTION__ \n"))
  cat("\nF-Fisher distribution is a continuous probability distribution that arises frequently as the null distribution of a test statistic.\n")
  cat(green("\nFormula -> sx2/sw2\n"))
  cat(green("\nsx2 <- 2 * (((mean_(x)-meant)^2) + ((mean_(y)-meant)^2))"))
  cat(green("\n(variance_(x) + variance_(y))/ 2"))
  
  cat(bold("\n__Use Example__\n"))
  cat("\nFirst of all, we need two datasets.")
  cat("\n Dateset x: \n")
  drawVector(x)
  cat("\n Dateset x: \n")
  drawVector(y)
  
  
  
  meant <- (mean_(x) + mean_(y))/2
  sx2 <- 2 * (((mean_(x)-meant)^2) + ((mean_(y)-meant)^2))
  sw2 <- (variance_(x) + variance_(y))/ 2
  res <- fisher_(x,y)
  
  cat("\nFormula applied -> (",sx2,"/",sw2,") = ", bold(res))
  
  cat("\nNow try by your own! :D\n")
  cat("\nUse interactive.fisher function to practice.\n")
  
}

#interactive function
interactive.fisher <- function(){
  initImages("https://www.monografias.com/trabajos91/prueba-hipotesis-f-fisher-empleando-excel-y-winstats/image005.png")
  #init vars
  cont_aux <- 0
  
  cat("\nInsert the first dataset:\n")
  x = getUserAction()
  cat("\nInsert the second dataset:\n")
  y = getUserAction()
  
  
  cat("\nOK! Next Move !!\n")
  #flag for correct answer -> 1 = NO_OK, 0 = OK
  flag <- 1
  
  #checking loop
  while(flag == 1){
    cat("Please, insert the result of the variance calculus for your data (if the result has decimal part, round to the 3rd): ")
    usr_resp <- as.numeric(readline(prompt = ""))
    if(usr_resp == round(fisher_(x,y),3)){
      flag <- 0
      cat(bold("\n\nWell done !\n\n"))
    } else {
      cont_aux <- cont_aux + 1
      cat("Ups, that might not be correct...")
      if(cont_aux == 1){
        cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
      }
      else if(cont_aux > 1 ){
        cat(yellow("\nHint 2 -> Check that you are entering your result correctly. It's easy to be wrong.\n\n"))
      }
      
    }
  }
}