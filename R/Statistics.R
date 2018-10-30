#  mean.R
#  Created by Dennis Monheimius on 6/7/18.
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
    cat(green("\nFormula -> (x1 + x2 +..+xn) / num_elements\n"))
    cat("\nThe mean of a dataset is calculated by adding each element of the dataset and dividing the result by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
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
  cat(green("\nFormula -> (x1 * x2 *..* xn)^( 1 / num_elements\n"))
  cat("\nThe geometric mean of a dataset is calculated by multiplying each element of the dataset and raising the result to 1 divided by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
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
  #initImages("https://i.imgur.com/O2kShzJ.png")
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
    cat(green("\nFormula -> Most repeated value of [Data]\n"))
    cat("\nThe mode of a dataset is calculated by looking for the most repeated value in the dataset. If in a group there are two or several scores with the same frequency and that frequency is the maximum, the distribution is bimodal or multimodal, that is, it has several modes.\n")
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
            if(cont_aux == 1){
                cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
            }
            else if(cont_aux > 2){
                cat(yellow("\nHint 2 -> Look for the most common element\n\n"))
            }
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

#interactive function
interactive.median <- function(){
    initImages("https://i.imgur.com/6rQKiIp.png")
    #init vars
    cont_aux <- 0
    
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

#explained example function
explain.median <- function(x){
    v <- as.vector(x)
    cat(bold("\n__MEDIAN CALCULUS__ \n"))
    cat(green("\nFormula -> 1/2(n+1) where n -> vector size\n"))
    cat("\nThe median of a dataset is the value in the middle of the sorted data. It's important to know that the data must be sorted. If the dataset has a pair number of elements, we should select both in the middle to add each other and get divided by two. If the dataset has a no pair number of elements, we should select the one in the middle.\n")
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
    cat(green("\nFormula ->  square_root ((Summation(each_element - mean)^2) / num_elements)\n"))
    cat("\nThe standard deviation of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset and finally making the square root on the result. We'll give the user an example for better comprension.\n")
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
    res <- (suma/length(x))
    return(res)
}


#explain example function
explain.variance <- function(x){
    x <- as.vector(x)
    
    cat(bold("\n__VARIANCE CALCULUS__ \n"))
    cat(green("\nFormula ->  (Summation(each_element - mean)^2) / num_elements\n"))
    cat("\nThe variance of a dataset is calculated by adding the square of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
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
    res <- (suma/length(x))
    cat("\nNow we need to add each element of the vector/dataset\n")
    cat("The sum of the squares is: ", blue(suma), "\n")
    cat("\nNext step, get the number of elements that we've examined")
    cat("\nThe length of the vector is ", blue(length(x)), "elements\n")
    cat("\nFormula applied -> ", suma, "/", length(x) , " = ", bold(res))
    
    cat("\nNow try by your own! :D\n")
    cat("\nUse interactive.variance function to practice.\n")
}

#interactive function
interactive.variance <- function(){
    #initImages("https://imgur.com/AEQPoEx.jpg")
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
    cat(green("\nFormula ->  (Summation(abs(each_element - mean))) / num_elements\n"))
    cat("\nThe average deviation of a dataset is calculated by adding the absolute value of the diference between each element and the mean of the dataset. This sum will be dividing by the number of elements in the dataset. We'll give the user an example for better comprension.\n")
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
    #initImages("https://imgur.com/J138Unu.jpg")
    #init vars
    cont_aux <- 0
    
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
    size <- length(x)
    
    q1 <- ((1 * size) / 4)
    q2 <- ((2 * size) / 4)
    q3 <- ((3 * size) / 4)
    
    res <- c(q1,q2,q3)
    names(res) <- c("Q1", "Q2", "Q3")
    return(res)
    
}

#explained sample function
explain.quartile <- function(x){
    #crayon library needed
    cat(bold("\n__QUARTILES CALCULUS__ \n"))
    vec <- sort(as.vector(x))
    size <- length(x)
    cat(green("\nFormula -> (k * N ) / 4 where k -> 1,2,3 and N -> vector size\n"))
    cat("\nThe quartile divides the dataset in 4 parts as equal as possible.\n")
    cat(bold("\n__Use Example__\n"))
    cat("\nStep 1: The vector must be sorted.\n")
    
    drawVector(vec)
    
    cat("\n")
    cat("\nStep 2: Apply the formula (k * N) / 4 where 'k' is [1-3]\n")
    
    #ceiling round up the value
    #Quartiles 1, 2 & 3
    q1 <- ceiling((1 * size) / 4)
    q2 <- ceiling((2 * size) / 4)
    q3 <- ceiling((3 * size) / 4)
    
    q1_ <- ((1 * size) / 4)
    cat("\nQ1 -> (1 * ", size, ") / 4 = ", q1_)
    q2_ <- ((2 * size) / 4)
    cat("\nQ2 -> (2 * ", size, ") / 4 = ", q2_)
    q3_ <- ((3 * size) / 4)
    cat("\nQ3 -> (3 * ", size, ") / 4 = ", q3_)
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
    for(i in (q3+1):size){
        if(i == (size)){
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
    #initImages("https://imgur.com/ZlGsVmn.jpg")
    #init vars
    cont_aux <- 0
    cont_aux_lim <- 0
    
    buffer = getUserAction()
    
    #show data sorted
    buffer = sort(buffer)
    cat("\nData sorted : ")
    
    drawVector(buffer)
    
    size = length(buffer)
    #quartiles calculus
    q1 <- round(((1 * size) / 4),3)
    q2 <- round(((2 * size) / 4), 3)
    q3 <- round(((3 * size) / 4), 3)
    #element which represents the quartile limit
    q1_lim <- buffer[ceiling((1 * size) / 4)]
    q2_lim <- buffer[ceiling((2 * size) / 4)]
    q3_lim <- buffer[ceiling((3 * size) / 4)]
    
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

percentile_ <- function(x){
    #data must be sorted
    data <- as.vector(sort(x))
    size <- length(x)
    perc_array <- array(data = NA, dim = 10)
    
    #function calculates percentiles {10,20,30,40,50,60,70,80,90,100}
    for(i in 1:10){
        perc_pos = ((size * (i*10)) / 100)
        perc_posRound = ceiling(perc_pos)
        perc_array[i] = data[perc_posRound]
    }
    #returned values
    for(i in 1:10){
        cat("Percentile[",i*10,"] = ",perc_array[i], "\n")
    }
}

#explained sample function
explain.percentile <- function(x){
    #crayon library needed
    cat(bold("\n__PERCENTILES CALCULUS__ \n"))
    data <- sort(as.vector(x))
    size <- length(data)
    cat(green("\nFormula -> (k * N ) / 100 where k -> [1-100] and N -> vector size\n"))
    cat("\nThe percentile divides the dataset in 100 parts.\nThe percentile indicates, once the data is ordered from least to greatest, the value of the variable below which a given percentage is located on the data\n")
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
    #initImages("https://imgur.com/hgD6Tz7.jpg")
    #init vars
    rand_percentile = sample(1:100,1)
    cont = 0
    
    buffer = getUserAction()
    
    #show data sorted
    buffer = sort(buffer)
    cat("\nData sorted : ")
    
    drawVector(buffer)
    
    size = length(buffer)
    
    #percentiles calculus
    perc_array <- array(data = NA, dim = 100) #the percentil in our data
    perc_pos_array <- array(data = NA, dim = 100) #the real value of the percentil
    perc_posRound_array <- array(data = NA, dim = 100) #the value rounded up for locate it
    
    data <- as.vector(buffer)
    
    #function calculates percentiles [1-100]
    for(i in 1:100){
        perc_pos_array[i] = ((size * i) / 100)
        perc_posRound_array[i] = ceiling(perc_pos_array[i])
        perc_array[i] = data[perc_posRound_array[i]]
    }
    
    cat("\nOK! Next Move !!\n")
    #flag for correct answer -> 1 = NO_OK, 0 = OK
    flag <- 1
    
    #checking loop
    while(flag == 1){
        cat("Please, insert the result of the ", rand_percentile ," percentile for your data : ")
        cat("\n(remember your data) -> ", buffer , "\n")
        
        resp_percPos <- as.numeric(readline(prompt = ""))
        if(resp_percPos == perc_pos_array[rand_percentile]){
            cat("\nCorrect!\n")
            cat("Please, indicate the value of your data which represents the percentile calculated: ")
            cat("\n(remember your data) -> ", buffer , "\n")
            resp_perc <- as.numeric(readline(prompt = ""))
            if(resp_perc == perc_array[rand_percentile]){
                cat(bold("\n\nWell done, you've got it!\n\n"))
                flag <- 0
            } else{
                cat("Ups, that might not be correct... Try again")
                cat(yellow("\nHint -> Psst!... Round up the value to the next integer. Look at the value of that position in your data\n\n"))
            }
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
    cat(green("\nFormula -> N1 + N2 + N3 + ... + Nk -> Nk = X (Where 'X' is the element we want to examine)\n"))
    cat("\nThe absolute frequency (Ni) of a value Xi is the number of times the value is in the set (X1, X2, ..., XN)\n")
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
    cat(green("\nFormula -> (Abs_frec(X) / N ) -> Where 'X' is the element we want to examine\n"))
    cat("\nThe relative frequency is the quotient between the absolute frequency of a certain value and the total number of data\n")
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
    cat(green("\nFormula -> (Summation(abs_frecuency <= X) / N ) -> Where 'X' is the element we want to examine\n"))
    cat("\nThe relative acumulated frequency is the quotient between the sum of the absolute frequency of the values minors or equals than the value we want to examine, and the total number of data\n")
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
    cat(green("\nFormula -> Summation(abs_frecuency <= X ) -> Where 'X' is the element we want to examine\n"))
    cat("\nThe absolute acumulated frequency is the sum of the absolute frequency of the values minors or equals than the value we want to examine\n")
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
