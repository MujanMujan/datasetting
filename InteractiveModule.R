if(!require("dplyr")) install.packages("dplyr")
if(!require("stringr")) install.packages("stringr")

library(dplyr)
library(stringr)










# Import data function -----> ----------------------------------------------------------------------------------------------------

importData <- function(sep = ",", header = TRUE, sheet = 1, skip = 0, na = "") {
  
  tryCatch({
    dataset_type <- "csv"
    dataset_path <- readline(prompt = "pls enter your dataset path (without qoute): ")
    cat("supported types : csv, txt, xls, xlsx, spss, stata, sas, minitab")
    dataset_type <- readline(prompt = "pls enter dataset type: ")
    switch (dataset_type,
            "csv" ={ myFile <- read.csv(dataset_path)} ,
            "txt" ={ myFile <- read.table(dataset_path, header = header , sep = sep)} ,
            "xls" ={ myFile <- read_excel(dataset_path, sheet = sheet, skip = skip, na = na)} ,
            "xlsx" ={myFile <- read_excel(dataset_path, sheet = sheet, skip = skip, na = na)} ,
            "spss" ={myFile <- read.spss(dataset_path, to.data.frame = TRUE, use.value.labels = FALSE)} ,
            "stata" ={myFile <- read.dta(dataset_path)} ,
            "sas" ={myFile <- read.sas7bdat(dataset_path)} ,
            "minitab" ={myFile <- read.mtp(dataset_path) }
    ) #Switch Ended
    return(myFile)}, error = function(e){
      cat("you got an error ! enter the correct path!\n")
      importData()
    })
} 
#---- <----importData Function Ended ! --------------------------------------------------------------------------------------------










#---Report Function ----------- > -------------------------------------------------------------------------------------------------
report <- function(obj) {
  lst <- list(Class = class(obj), Dimentions = dim(obj), Structures = capture.output(str(obj)),
              Glimpse = capture.output(glimpse(obj)), 
              'Names of Columns' = names(obj), 'Summary of dataset' = summary(obj))
  return(lst)
}
#---- <----Report Function Ended !--------------------------------------------------------------------------------------------------










#---ConvertDataType Function------------> ------------------------------------------------------------------------------------------

convertDataType <- function(obj) {
  vector_of_columns_type <- NULL
  for (i in 1:length(obj)) {
    vector_of_columns_type[i] <- class(obj[[i]])
  }
  vector_of_columns_name <- names(obj)
  cat("the following are columns name and columns type of object in : seprate !\n")
  for (i in 1:length(obj)) {
    cat(vector_of_columns_name[i], ": " , vector_of_columns_type[i],"\n")
  }
  
  user_input <- readline(prompt = "what columns you want to change ? enter the names by seprate it with coma (,) : ")
  user_input <- strsplit(user_input, ",")
  user_input <- user_input[[1]]
  user_input <- str_trim(user_input)
  
  cat("columns you want to change :\n", user_input, "\n")
  
  cat("types that supports : character, double, integer, logical")
  type_of_column <- readline(prompt = "what type you want ? ")
  indecses_of_columns <- match(user_input, names(obj))
  
  switch (type_of_column,
          "character" = {obj[indecses_of_columns] <- as.data.frame(sapply(obj[indecses_of_columns], as.character), stringsAsFactors = FALSE)},
          "double"    = {obj[indecses_of_columns] <- as.data.frame(sapply(obj[indecses_of_columns], as.double), stringsAsFactors = FALSE)},
          "integer"   = {obj[indecses_of_columns] <- as.data.frame(sapply(obj[indecses_of_columns], as.integer), stringsAsFactors = FALSE)},
          "logical"   = {obj[indecses_of_columns] <- as.data.frame(sapply(obj[indecses_of_columns], as.logical), stringsAsFactors = FALSE)}
  )
  
  return(obj)
}
#----ConvertDataType Function Ended ! --------------------------------------------------------------------------------------------










#----SelectColumns Function--------> ---------------------------------------------------------------------------------------------
selectColumns <- function(obj) {
  cat("\nThese are your columns name : ", names(obj), "\n")
  user_input <- readline(prompt = "\nWhich columns you want to select ? enter the names by seprate it with coma (,) : ")
  user_input <- strsplit(user_input, ",")
  user_input <- user_input[[1]]
  user_input <- str_trim(user_input)
  
  cat("columns you want to select :\n", user_input, "\n")
  indecses_of_columns <- match(user_input, names(obj))
  
  obj <- obj[indecses_of_columns]
  
  return(obj)
}
#----SelectColumns Function Ended ! ----------------------------------------------------------------------------------------------










#----Select Rows Function-------> ------------------------------------------------------------------------------------------------
selectRows <- function(obj) {
  
  
  cat("\n1. select rows by numerical range\n2. select rows by condition")
  user_choise <- readline(prompt = "1 or 2 ? ")
  
  if (user_choise == "1") {
    start <- as.integer(readline(prompt = "enter the number of BEGIN the range of rows (like 1) : ") )
    end <- as.integer(readline(prompt = "enter the number of END the range of rows (like 10) : ") )
    ifile <- obj[start:end, ]
    return(ifile)
  } else if (user_choise == "2") {
    
    loop <- TRUE
    while(loop) {
      key <- readline(prompt = "enter your key column : ")
      condition <- readline(prompt = "enter the condition like '>= 20'  : ")
      cat("finally condition is : ", key , condition)
      ok <- readline(prompt = "is it ok ? [yes/no] : ")
      
      if (ok == "no") {
        loop <- TRUE
      } else {
        loop <- FALSE
      }
    }
    condition <- strsplit(condition, " ")
    condition_sign <- condition[[1]][1]
    condition_value <- as.numeric(condition[[1]][2])
    
    switch (condition_sign,
            "==" = {ifile <- obj[ obj [ , key] == condition_value, ]},
            ">=" = {ifile <- obj[ obj [ , key] >= condition_value, ]},
            "<=" = {ifile <- obj[ obj [ , key] <= condition_value, ]},
            ">"  = {ifile <- obj[ obj [ , key] > condition_value, ]},
            "<"  = {ifile <- obj[ obj [ , key] < condition_value, ]}
    )
    return(ifile) 
  }
}
#----SelectRows Function Ended !--------------------------------------------------------------------------------------------------









#----Data Cleaning Phase----------------------------------------------------------------------------------------------------------
#----Summary_of_NA Function---------> --------------------------------------------------------------------------------------------
summary_of_NA <- function(obj) {
  
  #get count and ratio of NA in columns
  cat("Summary of NA in columns (count & ratio):\n")
  total_of_na <- 0
  for (i in 1:ncol(obj)) {
    count_of_na_in_col <- sum(is.na(obj[,i]))
    total_of_na <- count_of_na_in_col + total_of_na
    ratio_of_columns <- sum(is.na(obj[,i]))/(length(obj[,i])) * 100
    if (ratio_of_columns >= 50) {
      warning <- "#Warning"
    } else {
      warning <- ""
    }
    cat(names(obj)[i], ": ", count_of_na_in_col ,"\tratio: ", ratio_of_columns , "%\t", warning ,"\n")
  }
  
  
  #get count and ratio of NA in Rows
  
  user_answer <- readline(prompt = "do you sure to see all of rows ratio ? [yes/no]: ")
  
  if (user_answer == "yes") {
    cat("\n\nSummary of NA in rows (count and ratio:\n")
    for (i in 1:nrow(obj)) {
      ratio_of_rows <- sum(is.na(obj[i,]))/(length(obj[i,])) * 100
      if (ratio_of_rows >= 50) {
        warning <- "#Warning"
      } else {
        warning <- ""
      }
      cat("Row",i, ": ", sum(is.na(obj[i,])),"\tratio: ", ratio_of_rows, "%\t",warning,"\n")
    }
  }
  
  total_of_field <- ncol(obj) * nrow(obj)
  cat("\n\nratio of total(whole dataset) : ", (total_of_na/total_of_field) * 100, "% is NA\n")
  
  
}
#----Summary_of_NA Function Ended !-----------------------------------------------------------------------------------------------










#-----Remove Rows Function -------------------------------------------------------------------------------------------------------

removeRows <- function(obj) {
  options(warn = -1)
  tryCatch({
    rate_for_remove <- readline(prompt = "enter your maximum rate for removing (between 0 - 100): ")
    rate_for_remove <- as.integer(rate_for_remove)
    count_of_removed <- 0
    
    for (i in 1:nrow(obj)) {
      ratio_of_row <- sum(is.na(obj[i,]))/(length(obj[i,])) * 100
      if (ratio_of_row >= rate_for_remove) {
        obj <- obj[-i,]
        count_of_removed <- count_of_removed + 1
      }
    }
    cat("you ignored", count_of_removed - 1, "row(s)\n")
    return(obj)
  }, error = function(e) {
    cat("enter integer!\n")
    del(obj)
  })
  
}

#-----remove Rows Function Ended ! -----------------------------------------------------------------------------------------------











#-----fillInHand Function --------------------------------------------------------------------------------------------------------

fillInHand <- function(obj) {
  cat("i frist finding all missing values (NA) and allows you to fill it\nit extermly not recommended!!\n")
  user_answer <- readline(prompt = "are you sure to countinue ? [yes/no]: ")
  
  if (user_answer == "yes") {
    
    for (i in 1:nrow(obj)) {
      for (j in 1:ncol(obj)){
        if (is.na(obj[i,j])) {
          cat("missing value found in row", i, "and column", j, "('", names(obj)[j], "')\n")
          cat("all of row", i,  "values are: ", obj[,j])
          value <- readline(prompt = "what is your value for this field: ")
          obj[i,j] <- value 
        }
        
      } # j for ended
    } # i for ended
  } # if ended 
  else {
    cat("you backed to missing values treatments menu!\n")
    return(obj)
    
  }
  cat("be careful!, all values stored in character type, if you want change that select 'Convert Columns Type' in main menu\n")
  return(obj)
  
} # function ended

#-----fillInHand Function Ended !-------------------------------------------------------------------------------------------------










#-----fillMissingWithConstant Function -------------------------------------------------------------------------------------------

fillMissingWithConstant <- function(obj) {
  
  cat("I finding missing values and fill it by your constant value\n")
  constant <- readline(prompt = "enter your constant to fill missing values: ")
  cat("task started...\n")
  for (i in 1:nrow(obj)) {
    for (j in 1:ncol(obj)){
      if (is.na(obj[i,j])) {
        obj[i,j] <- constant 
      }
      
    } # j for ended
  } # i for ended
  
  cat("task succesfully completed\n")
  return(obj)
  
} # function ended

#-----fillMissingWithConstant Function Ended !------------------------------------------------------------------------------------










#-----fillMissingWithStatistics Function -----------------------------------------------------------------------------------------

fillMissingWithStatistics <- function(obj) {
  
  cat("I finding missing values and fill it by statistics resualt \n")
  cat("Supported Statistics: 1. median\t2. mean\n")
  statics <- readline(prompt = "enter your choose to fill missing values (enter number): ")
  cat("task started...\n")
  if (statics == "1") {
    for (i in 1:nrow(obj)) {
      for (j in 1:ncol(obj)){
        if (is.na(obj[i,j])) {
          obj[i,j] <- median(obj[,j], na.rm = TRUE)
        }
      } # j for ended
    } # i for ended
  } # if ended
  else if ( statics == "2") {
    for (i in 1:nrow(obj)) {
      for (j in 1:ncol(obj)){
        if (is.na(obj[i,j])) {
          obj[i,j] <- mean(obj[,j], na.rm = TRUE)
        }
      } # j for ended
    } # i for ended
  } # else if ended
  else {
    cat("you entered wrong number, try again!\n")
    return(obj)
  }
  cat("task succesfully completed\n")
  return(obj)
  
} # function ended

#-----fillMissingWithStatistics Function Ended !----------------------------------------------------------------------------------











#-----fillingByCategory Function--------------------------------------------------------------------------------------------------

fillingByCategory <- function(obj) {
  loop <- TRUE
  while (loop) {
    cat("this is your columns name: ", names(obj))
    column <- readline(prompt = "enter your column: ")
    column <- match(column, names(obj))
    if(is.na(column)) {
      cat("be careful, enter the correct name!, try again!\n")
    } else {loop <- FALSE}
  }
  
  
  loop <- TRUE
  while (loop) {
    rows_of_object <- nrow(obj)
    cat_count <- readline("har daste chandta satr bashe? (enter integer): ")
    cat_count <- as.integer(cat_count)
    
    if(is.na(cat_count)) {
      cat("be careful, enter the integer!!, try again\n\n")  
    } else {loop <- FALSE}
  }
  
  
  for_limit <- ceiling(nrow(obj) / cat_count)
  x <- 1
  y <- cat_count
  
  for (i in 1:for_limit) {
    
    for (j in 1:length(obj[x:y,column])) {
      if (is.na(obj[x:y, column][j])) {
        obj[x:y, column][j] <- mean(obj[x:y,column], na.rm = TRUE)
      }
    }
    
    x <- 1 + y
    y <- x + cat_count - 1
    while (y > nrow(obj)) {
      y <- y - 1
    }
    
  }
  
  return(obj)
  
}

#-----fillingByCategory Function Ended !------------------------------------------------------------------------------------------











#-----missingValueTreat Function--------------------------------------------------------------------------------------------------

missingValuesTreat <- function(obj) {
  cat("OK!\n1. Remove row(s) by ratio percentage\n2. Fill in hand (not recommended!)\n
      3. Use constant value to fill missing values\n4. Using statistics to fill missing values\n5. Use statistics (only mean) in specific category\n")
  user_answer <- readline(prompt = "Which? ")
  
  switch(user_answer,
         "1" = {myFile <- removeRows(obj)},
         "2" = {myFile <- fillInHand(obj)},
         "3" = {myFile <- fillMissingWithConstant(obj)},
         "4" = {myFile <- fillMissingWithStatistics(obj)},
         "5" = {myFile <- fillingByCategory(obj)}
         
         stop(cat("select 1 to 6 or enter 0 to exit\n"), missingValuesTreat(obj)) 
         )
}









#----missingValueTreat Function Ended !-------------------------------------------------------------------------------------------












#-----dataCleaning Function----- > -----------------------------------------------------------------------------------------------


dataCleaning <- function(obj) {
  cat("welcome to data cleaning pahse!\nselect your option:\n")
  cat("1. Missing Value Treatment\n")
  user_answer <- readline(prompt = "what was your choose ? ")
  
  switch(user_answer,
         "1" = { myFile <- missingValuesTreat(obj) }
         )
  
}








#-----dataCleaning Function Ended !-----------------------------------------------------------------------------------------------











#-----MUJAN FUNCTION : -----------------------------------------------------------------------------------------------------------

Mujan <- function() {
  cat("Welcome to Interactive module to preparing dataset !\nCreator: Mujan\n\n\nyour current directiry is", getwd())
  while(TRUE) {
    cat("\n1. Import Data\n2. Report Data\n3. Convert Column's Type\n
        4. Select Specific Columns\n5. Select Specific Rows\n6. NA Summary\n7. Data Cleaning0. exit")
    answer <- readline(prompt = "What is your choose : ")
    switch (answer,
            "1" = {datum <- importData()
            my_dataset <<- datum 
            print("the dataset returned in variable called 'my_dataset'")},
            "2" = {x <- readline(prompt = "do you want the report of your last dataset(my_dataset)? [yes/no] : ")
            tryCatch({
            if (x == "yes"){
              report_of_dataset <<- report(datum)
              print(report_of_dataset)
              print("the result also save in varible called 'report_of_dataset'")
            } else {
              datum <- importData()
              report_of_dataset <<- report(datum)
              print(report_of_dataset)
              print("the result also save in varible called 'report_of_dataset'")
              cat("be careful! if you want have a sample of input dataset in your environment, you must use frist option (Import Data) \n")
            }}, error = function(e) {
              cat("the dataset does not exist ! you must import one frist!\n")
              datum <- importData()
              report_of_dataset <<- report(datum)
              print(report_of_dataset)
              print("the result also save in varible called 'report_of_dataset'")
              cat("be careful! if you want have a sample of input dataset in your environment, you must use frist option (Import Data) \n")
            })
              },
            "3" = {x <- readline(prompt = "are you want work with your last dataset (my_dataset)? [yes/no] : ")
            if (x == "no") {
              datum <- importData()
            } else if (x == "yes") {
              my_dataset <<- convertDataType(datum)
              print("the modified dataset returned in variable called 'my_dataset'")
            }},
            "4" = {x <- readline(prompt = "are you want work with your last dataset (my_dataset)? [yes/no] : ")
            if (x == "no") {
              datum <- importData()
            } else if (x == "yes") {
              my_dataset <- selectColumns(datum)
              user_answer <- readline(prompt = "do you want apply changes in 'my_dataset' ? ")
              if (user_answer == "yes") {
                my_dataset <<- my_dataset
              } else if (user_answer == "no") {
                new_cdataset <<- my_dataset
                cat("the new dataset (new_cdataset) was created !\n\n\n")
              }
            }},
            "5" = {x <- readline(prompt = "are you want work with your last dataset (my_dataset)? [yes/no] : ")
            if (x == "no") {
              datum <- importData()
            } else if (x == "yes") {
              my_dataset <- selectRows(datum)
              user_answer <- readline(prompt = "do you want apply changes in 'my_dataset' ? ")
              if (user_answer == "yes") {
                my_dataset <<- my_dataset
              } else if (user_answer == "no") {
                new_rdataset <<- my_dataset
                cat("the new dataset (new_rdataset) was created !\n\n\n")
              }
            }},
            "0" = {cat("GoodBye :)")
              break},
            "6" = {x <- readline(prompt = "are you want work with your last dataset (my_dataset)? [yes/no] : ")
            if (x == "no") {
              datum <- importData()
            } else if (x == "yes") {
              summary_of_NA(datum)
            }},
            "7" = {x <- readline(prompt = "are you want work with your last dataset (my_dataset)? [yes/no] : ")
                  tryCatch({
                    if (x == "no") {
                      datum <- importData()
                      cat("be careful! if you want have a sample of input dataset in your environment, you must use frist option (Import Data) \n")
                      dataCleaning(datum)
                    } else if (x == "yes") {
                      dataCleaning(datum)
                    }
                  })                  
             }
    ) #Switch ended
    
  } #While loop ended
  
  
  
}


