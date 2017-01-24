importData <- function(sep = ",", header = TRUE, sheet = 1, skip = 0, na = "") {
  
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
  return(myFile)        
} #importData Function Ended !


report <- function(obj) {
  lst <- list(Class = class(obj), Dimentions = dim(obj), Structures = capture.output(str(obj)),
              Glimpse = capture.output(glimpse(obj)), 
              'Names of Columns' = names(obj), 'Summary of dataset' = summary(obj))
  return(lst)
}


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









Mujan <- function() {
  cat("Welcome to Interactive module to preparing dataset !\nCreator: Mujan\n\n\nyour current directiry is", getwd())
  while(TRUE) {
  cat("\n1. Import Data\n2. Report Data\n3. Convert Column's Type\n4. Select Specific Columns\n5. Select Specific Rows\n6. NA Summary\n0. exit")
  answer <- readline(prompt = "What is your choose : ")
  switch (answer,
          "1" = {datum <- importData()
                 my_dataset <<- datum 
                 print("the dataset returned in variable called 'my_dataset'")},
          "2" = {x <- readline(prompt = "do you want the report of your last dataset(my_dataset)? [yes/no] : ")
                if (x == "yes"){
                  report_of_dataset <<- report(datum)
                  print(report_of_dataset)
                  print("the result also save in varible called 'report_of_dataset'")
                } else {
                  datum <- importData()
                  report_of_dataset <<- report(datum)
                  print(report_of_dataset)
                  print("the result also save in varible called 'report_of_dataset'")
                }},
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
                }}
  ) #Switch ended

  } #While loop ended
 
  

}


