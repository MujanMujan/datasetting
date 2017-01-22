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






Mujan <- function() {
  cat("Welcome to Interactive module to preparing dataset !\nCreator: Mujan\n")
  while(TRUE) {
  cat("\n1. Import Data\n2. Report Data\n3. Convert Column's Type")
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
                }
                  
                  }
  ) #Switch ended

  } #While loop ended
 
  

}


