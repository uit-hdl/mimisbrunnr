# -----------------------------------------------------------
#
# This script contain basic tools to manipulate strings,
# find dates, get automatic naming, and things of the like
#
# -----------------------------------------------------------

library(foreign)   # For reading SPSS files
library(lubridate) # For dealing with proper dates

############################
# NUMBERS MANIPULATION
# -- number2binary()     Transform an integer to binary
# -- myIsInteger()       Check if a number is an integer, the R version doesn't 
#                        works properly.
# -- logit2prob()        Transform a logit probability into a normal probability
# -- checkStrictGrow()   Checks if a list of numbers is strictly growing
# -- scaleValues()       Transform all values in [a,b] to values between [a', b']
############################
{
  # Transform a number into binary
  # -- number is the integer that you want to transform
  # -- noBits is how many bits you want to gets. ie:
  #        14 = 1110
  #        noBits = 7 -> 0001110
  #        noBits = 2 -> 10
  # Positive numbers leads with 0s while negative numbers leads with 1s
  number2binary = function(number, noBits) {
    
    binary_vector = rev(as.numeric(intToBits(number)))
    
    if(missing(noBits)) return(binary_vector)
    
    else binary_vector[-(1:(length(binary_vector) - noBits))]
    
  }
  
  # Check if a number is an integer, since the R version is confusing
  myIsInteger <- function(x){
    
    return (x%%1==0)
    
  }
  
  # Get logit probability into normal probability
  logit2prob <- function(logit){
    odds = exp(logit)
    prob = odds / (1 + odds)
    return(prob)
  }
  
  # Checks if a list of numbers is strictly growing
  checkStrictGrow <- function(myVector){
    
    growing        = TRUE
    currentMaximum = myVector[1]
    vectorSize     = length(myVector)
    
    if(vectorSize > 1){
      for(i in 2:vectorSize){
        
        if(myVector[i] <= currentMaximum) growing = FALSE
        
        currentMaximum = myVector[i]
        
      }    
    }
    
    return (growing)
    
  }
  
  
  # Scale a bunch of values that has this range:
  #  [A , B]
  #
  # to this other range:
  #  [C , D]
  scaleValues <- function(values, newMinimum, newMaximum){
    
    maximumOriginalRange = max(values)
    minimumOriginalRange = min(values)
    deltaRange = maximumOriginalRange - minimumOriginalRange
    
    newValues = ((values - minimumOriginalRange) / deltaRange) *  (newMaximum - newMinimum) + newMinimum
    
    return(newValues)
    
  }
  
}

############################
# DATES MANIPULATION
# isDate()     Tells if a variable is a date
# calc_age()   Find the years in between two dates
############################
{
  
  # Check if a value is date value
  isDate <- function(x) inherits(x, 'Date')
  
  # Calculate an age in years (decimal) from a given reference date
  #
  # Date birthDate ; the birthday of someone
  # Date refDate   ; the reference date from birthDate. If you give a previous
  #                  date you will get a negative number.
  #                  Default to the time in which you run the function.
  #
  # return         ; a float with how many years have pass from the brithDate to
  #                  the refDate.
  #
  # Example:
  # 
  #    now = as.Date("1970-01-01")
  #    later = as.Date("1971-10-31")
  #    calc_age(now, later)
  #
  #    > 1.833333
  calc_age <- function(birthDate, refDate = Sys.Date()) {
    
    require(lubridate)
    
    period = as.period(interval(birthDate, refDate), unit = "year")
    
    totalAge    = 0
    totalYears  = period$year
    totalMonths = period$month
    totalDays   = period$day
    
    totalMonths = totalMonths + (totalDays/30)
    
    totalAge    = totalYears + (totalMonths/12)
    
    
    return(totalAge)
    
  }
  
}

############################
# STRINGS MANIPULATION
# -- trim()                   Delete start and end spaces in a string
# -- cleanWeirdCharacters()   Delete ":$%'/\,""<>[]() " from a string
# -- writeLinesBN()           For a given file, makes sure that '/n' characters
#                             actually breaks the line (which R doesn't do)
############################
{
  # Delete trailing and leading whitespaces from a string
  #
  # String x ; the string you want to trim
  #
  # return     String without trailing and leading spaces
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  # Delete the strange characters from a string and transform the result into a
  # ISO_8859-2 string. White spaces are transformed into "_" characters instead
  #
  # This is intended to use to clean filenames and filepath incompatible 
  # characters.
  #
  # List of characters:
  #
  # : $ % ' / \ , " " (white space)  < > [ ] ( )
  #
  # String weirdString ; The string that you want to clean
  #
  # return             ; a new string with the weirdesness removed
  #
  # Example
  #
  #     cleanWeirdCharacters("This(is),a, very$$ weird$:String")
  #
  #     > "Thisisa_very_weirdString"
  cleanWeirdCharacters <- function(weirdString){
    
    wierdString <- as.character(weirdString)
    weirdString <- gsub(":", "", weirdString)
    weirdString <- gsub("\\$", "", weirdString)
    weirdString <- gsub("%", "", weirdString)
    weirdString <- gsub("'", "", weirdString)
    weirdString <- gsub("/", "", weirdString)
    weirdString <- gsub("\"", "", weirdString)
    weirdString <- gsub(",", "", weirdString)
    weirdString <- gsub(" ", "_", weirdString)
    weirdString <- gsub("<", "", weirdString)
    weirdString <- gsub(">", "", weirdString)
    weirdString <- gsub("[()]", "", weirdString)
    
    weirdString <- gsub("\\[", "", weirdString)
    weirdString <- gsub("\\]", "", weirdString)
    
    #weirdString <- gsub("log\\(", "", weirdString)
    #weirdString <- gsub("\\(\\)", "_", weirdString)
    #weirdString <- gsub("log\\)", "", weirdString)
    #weirdString <- gsub("log\\[", "", weirdString)
    #weirdString <- gsub("log\\]", "", weirdString)
    #weirdString <- gsub("\\s*\\([^\\)]+\\)","",as.character(weirdString))
    iconv(weirdString, to = "ISO_8859-2")
    
    return(weirdString)
  }
  
  
  # Write lines into file but properly breaking lines for \n characters
  # -- No append, overwrite the file always
  writeLinesBN <- function(myString, myFileConn){
    
    # Separate the string into a list of strings
    allStringsList = strsplit(myString, "\n")[[1]]
    
    # Write each line individually
    writeLines(allStringsList, myFileConn)
    close(myFileConn)
    
  }
  
    # Write lines into file but properly breaking lines for \n characters
    # Is different from the previous function because this one takes the filepath
    # and create, and close, the file connection inside the function
    # -- No append, overwrite the file always
    writeLinesDisk <- function(myString, myFilePath){
    
        # Create the file connection
        myFileConn = file(myFilePath, 'w')
      
        # Separate the string into a list of strings
        allStringsList = strsplit(myString, "\n")[[1]]
    
        # Write each line individually
        writeLines(allStringsList, myFileConn)
        
        # Close the connection
        close(myFileConn)
    
    }
  
  
}

############################
# DATA MANIPULATION
# -- getCategoricalVariables() For a given DF, tells you which columns are
#                              categorical
# -- getDFFromSPSS()           Return a R DF from a .sav filepath
# -- getIndexVector()          For a given vector, tells which column is the
#                              given name.
# -- getIndexDF()              For a given DF, tells which column is the given
#                              name.
# -- transposeDF()             Transpose a DF properly, and not the weird thing
#                              that stupid R does in the base code
############################
{
  # For a given dataframe, return a boolean vector telling which variables
  # are categoricals and which one arent.
  getCategoricalVariables <- function(variablesDF){
    
    # Get the dimmension
    totalColumns = ncol(variablesDF)
    
    # Get the class of each column
    myClasses = sapply(variablesDF, class)
    
    # Simplify types to boolean
    # - FALSE = Numerical
    # - TRUE  = Categorical
    categoricalColumns = rep(FALSE,totalColumns)
    for(i in 1:totalColumns){
      
      if(myClasses[[i]] == "character" || myClasses[[i]] == "factor"){
        categoricalColumns[i] = TRUE
        
      }
      
    }
    
    return(categoricalColumns)
    
  }
  
  # From a file in disk with .sav extension (SPSS) gets a DF with the information
  # that is ready to use in R.
  # -- filepath is where you have the .sav file
  # -- saveCSVCopy is another path where you want to save a CSV copy of this data
  getDFFromSPSS <- function(filepath, saveCSVCopy = NULL){
    
    data = read.spss(filepath, to.data.frame=TRUE)
    
    if(!is.null(saveCSVCopy)){
      write.csv(data, file = saveCSVCopy, row.names = FALSE)
    }
    
    return (data)
    
  }

 
    getIndexVector <- function(myName, myVector){
    
        return (grep(paste0("^",myName,"$"), myVector))
        
    }
  
    getIndexDF    <- function(myName, myDF){
        
        myVector = colnames(myDF)
        
        return (grep(paste0("^",myName,"$"), myVector))
        
    }
    
    getIndex      <- function(myName, myDatastructure){
        
        myReturnIndex = -1
        
        if(is.data.frame(myDatastructure) == TRUE){
            
            myReturnIndex = getIndexDF(myName, myDatastructure)
            
        }
        else{
            
            myReturnIndex = getIndexVector(myName, myDatastructure)
            
        }
    
        return ( myReturnIndex )

    }
    
    transposeDF   <- function(myDF){
        
        # Get the original metadata
        originalNRows    = nrow(myDF)
        originalNColumns = ncol(myDF)
        originalNames    = colnames(myDF)
        
        # Prepare the new metadata
        newNRows    = originalNColumns - 1
        newNColumns = originalNRows + 1
        
        # Create the new DF
        trasposeDF = DF(newNRows, newNColumns)
        # -- Gives the column names
        colnames(trasposeDF) = c(originalNames[1], myDF[,1])
        # -- Gives the row names
        trasposeDF[,1] = colnames(myDF)[2:originalNColumns]

        # Start putting data in each place
        for(j in 2:originalNColumns){
        
            for(i in 1:originalNRows){
                
                trasposeDF[(j-1),(i+1)] = myDF[i,j]
                
            }    
            
        }
        
        return(trasposeDF)
        
    }
    
    
 }




############################
# FORMATING
# -- getAlignment()          How many white spaces you need after a string to
#                            align.
# -- getAsterkisPValue()     For a float representing a p-value, return the
#                            asterisk format.
# -- getProgressCharacters() For a float, return a progress bar filled
#                            accordingly.
############################
{
  # Count how many spaces you need to put in between a String and a number, so
  # a bunch of lines with String + Number looks aligned.
  #
  # Cat1: 59                   Cat1:       59
  # Category 2: 100   ---->    Category 2: 100
  # myCat3: 39                 myCat3:     39
  #
  # Return the number of spaces after each string in a list, with minimum 1
  # Important, a NA or NULL must not be in the string list. But it can be "NA" or
  # "NULL"
  #
  # This function doesn't change the string itself.
  getAlignment <- function(stringList){
    
    stringLengths = nchar(stringList)
    maximumLength = max(stringLengths)
    totalSpaces   = maximumLength - stringLengths + 2 # 1 character for the ":" after category name, 1 character for the " " after the ":"
    
    return(totalSpaces)
    
  }
  
    # Return number of asterisk for p-values
    #
    # From (0.05   to 1]      return "ns"
    # From (0.01   to 0.05]   return "*"
    # From (0.001  to 0.01]   return "**"
    # From [0.0001 to 0.001]  return "***"
    # From [0.00001 to 0.001] return "****"
    getAsterkisPValue <- function(x, nsEmpty = FALSE){
    
        result = NA
        # Check wether we have a value or a vector of values
        if(length(x) == 1){
            
            result = "ns"
            if(nsEmpty == TRUE) result = ""
    
            if(!is.na(x)){
      
                if(x < 0.05)   result = "*"
                if(x < 0.01)   result = "**"
                if(x < 0.001)  result = "***"
                if(x < 0.0001) result = "****"
      
            }
            
        }
        else{
            
            result = rep("ns", length(x))
            if(nsEmpty == TRUE) result = rep("", length(x))
            
            for(i in 1:length(x)){
                
                if(!is.na(x[i])){
      
                    if(x[i] < 0.05)   result[i] = "*"
                    if(x[i] < 0.01)   result[i] = "**"
                    if(x[i] < 0.001)  result[i] = "***"
                    if(x[i] < 0.0001) result[i] = "****"
      
                }
                
            }
            
            
        }

        return(result)
    
     }
  
  # Give you an ACSII progress bar
  # x: float in percentage, ie: 45.332 (without the % of course)
  getProgressCharacters <- function(x){
    
    #cat("\014")
    baseLine     = "--------- -------- ----------"
    totalChars   = nchar(baseLine)
    totalBars    = ceiling(totalChars * x / 100)
    barsString   = rep("|",totalBars)
    barsString   = paste(barsString, sep = '', collapse='')
    barsString   = paste(barsString, " ",round(x,2), "%", sep = "")
    currentChars = nchar(barsString)
    addChars     = totalChars - currentChars
    if(addChars < 0) addChars = 0
    extraChars   = rep("_",addChars)
    extraChars   = paste(extraChars, sep = '', collapse='')
    barsString   = paste(barsString, extraChars, sep = "")
    
    return(barsString)
    
  }
  
}

############################
# OS in/out
# -- checkIfFolder()          Tells if a path is a folder
# -- automaticFilePath()      For a folder and something you want to save there, 
#                             gives an automatic name for what you want to save.
# -- getFileExtension()       For a filepath, return the extension (if any)
# -- getFileName()            For a filepath, return the file name (no extension)
# -- getFileFolder()          For a filepath, return the complete folder path
# -- getFilePathNoExtension() For a filepath, return the same path with no extension
############################
{
  
  # Check if a string is a filename or a folder name
  #
  # Simply check that it ends with a .xxxxx extension or not
  # Right now it only checks for PNGs since is the only image format that matters
  # (aside from vector images, which doesn't work in ggplot2)
  #
  # Return
  # 
  #     TRUE  = This is a filepath of a folder (/home/user/doc)
  #     FALSE = This is a filepath of a file   (/home/user/doc/myImage.png)
  checkIfFolder <- function (myFileString){
    
    result = TRUE
    
    # If the file string is NULL, we don't have a file
    if(is.null(myFileString)){
      
      result = FALSE
      
    }
    else{
      
      # If the file string doesn't ends in PNG, or TEX, we don't have a file
      if( grepl("\\.png$", myFileString) == FALSE &&
          grepl("\\.tex$", myFileString) == FALSE   ){
        
        result = FALSE
      }
      
    }
    
    return (result)
    
  }
  
  
  # Gives an automatic filepath to save your file.
  #
  # This is very useful when you want to generate filepath for doing a lot of
  # plots, analysis or whatever, and give consistent naming rules.
  #
  # -- If your filePath is a file, do nothing and return the same filepath.
  #
  # -- If your filePath is a folder, it make up a filepath for you inside that
  #    folder with these rules:
  #
  #     <File_Type> + <Table Name> + <List of names from the relevant variables> +
  #     <extension type>
  #
  #     Example:
  #
  #     "/../tables/BMIPlot_myPatientsDataFrame_Sex_BMI.png"
  # 
  #    "/../tables/LatexTable_myPatientsDataFrame.tex"
  #
  # String filePath       ; Either a filePath with a file or a folder (read above)
  #
  # DataFrame myDataFrame ; (NULL) The data frame that you want to use for
  #                                automatic name giving
  #
  # String tableName      ; (NULL) The name of myDataFrame. You can't get this
  #                                automatically with deparse inside the function.
  #                                so you need to call deparse before the function
  #                                if you want the same name as the variable, or,
  #                                you can override the name of the dataframe by
  #                                giving whatever name you want here.
  #
  # String fileType       ; (NULL) What kind of file you are trying to generate.
  #                                It recommended that you don't leave this to
  #                                NULL as default. The options are:
  #
  #                                NULL = You have no idea of what you are
  #                                       generating, but the function will try
  #                                       to generate a meaningful name anyway
  #                                       with prefix "Unknown_File_Type"
  #
  #                                images = You are making an image, probably a
  #                                          plot from the plotting library.
  #
  #                                             "AbsBarplot"
  #                                             "RelBarplot"
  #                                             "LongAbsBarplot"
  #                                             "BMIPlot"
  #                                             "Boxplot"
  #                                             "RechabilityBoxplot"
  #                                             "Histogram"
  #                                             "Density"
  #                                             "CategoricalHistogram"
  #                                             "CategoricalDensity"
  #                                             "pValuesHeatmap"
  #                                             "QQ"
  #                                             "Scatterplot"
  #                                             "Tableplot"
  #                                             "SimulaitonLineplot"
  #
  #                                latex = You are making a latex file. It
  #                                          could be a file that contain an
  #                                          image or a table, but a .tex file
  #                                          nevertheless.
  #
  #                                             "LatexTable"
  #                                             "LatexImage"
  #
  #                                tables = A .txt table. Probably you run
  #                                         already all the relevant functions
  #                                         to make this txt human friendly.
  #
  #                                             "txtTable"
  #
  #
  #                                Finally, if you didn't gave NULL, but you gave
  #                                an option that is not in the list of option,
  #                                you will get a "Invalid_File_Type" prefix.
  
  # Int variableIndexX    ; (NULL) The column index of myDataFrame that you want
  #                                to use for the automatic name giving. You can
  #                                have up to 3 variables depending of the type
  #                                of plot or file that you are making
  
  # bool rootPath ; (FALSE) ignore the extension and return the filePath with no
  #                         extension. Usefull to generate automatic names for
  #                         the same analysis where you are going to have a .png,
  #                         .tex, and so on.
  
  automaticFilePath <- function(filePath, rootPath = FALSE,
                                myDataFrame = NULL, tableName = NULL,
                                fileType = NULL, variableIndex1 = NULL,
                                variableIndex2 = NULL, variableIndex3 = NULL){
    
    # This is the final string variable to return
    finalPath = filePath
    
    # If we have a complete filepath, do nothing.
    # Otherwise, gives an automatic name accordingly
    haveFile = checkIfFolder(filePath)
    if(haveFile == FALSE){
      
      # Defaults
      fileExtension = ""
      
      # If you didn't go for NULL filetype, check if the filetype you have is a
      # valid one and give the proper file extension.
      if(!is.null(fileType)){
        
        # (these list are sorted alphabetically)
        
        validImages = c("AbsBarplot",
                        "BMIPlot",
                        "Boxplot",
                        "CategoricalBoxplot",
                        "CategoricalDensity",
                        "CategoricalHeatmap",
                        "CategoricalHistogram",
                        "CombinedRelBarplot",
                        "CombinedAbsBarplot",
                        "CategoricalHeatmap",
                        "Density",
                        "DoubleCategoricalBoxplot",
                        "Histogram",
                        "LongAbsBarplot",
                        "pValuesHeatmap",
                        "QQ",
                        "RechabilityBoxplot",
                        "RelBarplot",
                        "SimulationLinePlot",
                        "Scatterplot",
                        "Tableplot")
        
        validAnalysis = c("XiTable", "MetaXiTable")
        
        validLatex  = c("LatexImage",
                        "LatexTable")
        
        validTables = c("txtTable")
        
        validHTML   = c("HTMLTable")
        
        if(fileType %in% validImages)   fileExtension = ".png"
        if(fileType %in% validLatex)    fileExtension = ".tex"
        if(fileType %in% validTables)   fileExtension = ".txt"
        if(fileType %in% validAnalysis) fileExtension = ".tex"
        if(fileType %in% validHTML)     fileExtension = ".html"
        
        # If you fail all the check, you made a human error
        if(fileExtension == "") fileType = "Invalid_File_Type"
        
      }
      else{
        
        fileType      = "Unknown_File_Type" # Gives you a very generic plot name
        # if you don't provide one
      }
      
      # The name of your dataframe variable is given as a parameter
      #
      # Note that if you do this:
      #
      #    tableName = deparse(substitute(myDataFrame))
      #
      # It will not copy the original dataframe name, but the name of which
      # whatever function you are calling this function. So you need to pass the
      # tableName manually. Also note that R doesn't need to pass variables as
      # reference for performance. (or so they say so at CRAN)
      #
      # In any case, give an automatic name if you don't have one
      if(is.null(tableName)){
        
        tableName = "NoTableNameGiven"
        
      }
      
      
      # Name of each of the variables
      name1 = colnames(myDataFrame)[variableIndex1] # It doesn't matter if it is NULL, you get an empty string
      name2 = colnames(myDataFrame)[variableIndex2]
      name3 = colnames(myDataFrame)[variableIndex3]
      
      # Depending of your file type, you return one type of string or another
      # In any case, put everything together in this variable
      fileName = ""
      
      # Unknown and Invalid
      if(fileType == "Unknown_File_Type")    fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
      if(fileType == "Invalid_File_Type")    fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
      # TXT
      # HTML
      if(fileType == "HTMLTable")            fileName = paste(fileType, "_", tableName,                                     sep="")
      # Latex
      if(fileType == "LatexTable")           fileName = paste(fileType, "_", tableName,                                     sep="")
      # Images
      #     0 Variables (Specials)
      if(fileType == "RechabilityBoxplot")   fileName = paste(fileType, "_", tableName,                                     sep="")
      if(fileType == "TablePlot")            fileName = paste(fileType, "_", tableName,                                     sep="")
      if(fileType == "SimulationLinePlot")   fileName = paste(fileType, "_", tableName,                                     sep="")
      
      #     1 Variable
      #         Categoricals
      if(fileType == "AbsBarplot")           fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      if(fileType == "LongAbsBarplot")       fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      if(fileType == "RelBarplot")           fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      #         Numericals
      if(fileType == "Boxplot")              fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      if(fileType == "Density")              fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      if(fileType == "Histogram")            fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      if(fileType == "QQ")                   fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
      #     2 Variables
      #         Categorical + Categorical
      if(fileType == "CombinedRelBarplot")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "CombinedAbsBarplot")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "XiTable")              fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "CategoricalHeatmap")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      #         Categorical + Numerical
      if(fileType == "BMIPlot")              fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "CategoricalBoxplot")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "CategoricalHistogram") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "CategoricalDensity")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      if(fileType == "pValuesHeatmap")       fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      #         Numerical   + Numerical
      if(fileType == "Scatterplot")          fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
      #     3 Variables
      #         Numerical + Categorical + Categorical
      if(fileType == "DoubleCategoricalBoxplot") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
      
      #         Numerical + Numerical + Categorical
      if(fileType == "CombineRegression")        fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
      #     Several combination of Variables
      if(fileType == "MetaXiTable")              fileName = paste(fileType, "_", tableName,                                     sep="")
      
      # Merge all the information into a variable and return it
      # -- If you have an actual filepath, use that (TODO: FIX THIS!! This has nothing to do with roots)
      if(rootPath == TRUE){
        finalPath = paste(filePath, fileName, sep="")
      }
      # -- If you have a folder
      else{
        
        # If you have one of the special analysis where you generate several
        # tables and images, each with it own name
        if(fileType == "XiTable"){
          
          finalPath      = vector("list", length = 5)
          finalPath[[1]] = paste0(filePath, fileName, "_frequency_", name1, fileExtension)    
          finalPath[[2]] = paste0(filePath, fileName, "_frequency_",  name2, fileExtension)    
          finalPath[[3]] = paste0(filePath, fileName, "_absolute",   fileExtension)    
          finalPath[[4]] = paste0(filePath, fileName, "_relative",   fileExtension)    
          finalPath[[5]] = paste0(filePath, fileName, "_difference", fileExtension)    
          finalPath[[6]] = paste0(filePath, fileName, "_pvalues",    fileExtension)    
          finalPath[[7]] = paste0(filePath, fileName, "_summary",    fileExtension)    
          
        }
        # In any other case, return what you need
        else{
          finalPath = paste(filePath, fileName, fileExtension, sep="")    
        }
      }
    }
    
    return(finalPath)
    
  }
  
  
  # Get the extension of a filePath
  #
  # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
  #
  # return extension including the dot, if exist
  #
  # ie:
  # 
  # "../asfaf/asdfas/asf/myFile.txt"   -> ".txt"
  # "../asfaf/asdfas/asf/myFile"       -> ""
  # "../asfaf/asdfas/asf/myFile."      -> "."
  # "../asfaf/asdfas/asf/my.Fi.le.txt" -> ".txt"
  #
  getFileExtension <- function(filePath){
    
    myExtension = ""
    
    texFilePathSplitted       = strsplit(filePath, "/")[[1]] # Get rid of the filePath and get only the fileName
    texRelativeLocationPath   = texFilePathSplitted[length(texFilePathSplitted)]
    
    texFilePathExtension      = strsplit(texRelativeLocationPath, "\\.")[[1]]
    
    if(length(texFilePathExtension) > 1){
      
      myExtension = paste(".", texFilePathExtension[length(texFilePathExtension)], sep='')
      
    }
    
    return(myExtension)
    
  }
 
  
  # Get the fileName with the extension
  # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
  # Return "myFile.txt"
  getFileName <- function(filePath, includeExtension = TRUE){
    
    filePathSplitted = strsplit(filePath, "/")[[1]] # Get rid of the filePath and get only the fileName
    fileName         = filePathSplitted[length(filePathSplitted)]
    
    if(includeExtension == FALSE){
      
      myFileExtension         = getFileExtension(filePath)
      totalCharFileExtension  = nchar(myFileExtension)
      totalCharFileName       = nchar(fileName)
      fileName                = substr(fileName, 1, totalCharFileName - totalCharFileExtension)
      
    }
    
    return (fileName)
    
  }
  
  # Get the folder of the filePath where a file is located
  # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
  # Return "../asfaf/asdfas/asf"
  # You can return "../asfaf/asdfas/asf/" if lastSlash = TRUE
  getFileFolder <- function(filePath, includeLastSlash = FALSE){
    
    myFileName        = getFileName(filePath)
    totalCharFileName = nchar(myFileName)
    totalCharFilePath = nchar(filePath)
    myFileFolder      = substr(filePath, 1, totalCharFilePath - totalCharFileName)
    
    if(includeLastSlash == FALSE) myFileFolder = gsub('.{1}$', '', myFileFolder)
    
    return(myFileFolder)
  }
  
  # Get the filePath and delete the extension
  # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
  # Return  "../asfaf/asdfas/asf/myFile"
  getFilePathNoExtension <- function(filePath){
    
    myFileExtension        = getFileExtension(filePath)
    totalCharFileExtension = nchar(myFileExtension)
    totalCharFilePath      = nchar(filePath)
    fileName               = substr(filePath, 1, totalCharFilePath - totalCharFileExtension)
    
    return(fileName)
    
  }
   
}

############################
# MEDICINE
# -- getATCInfo()      For a ATC medical code, get some info about it
# -- getICD10Info()    For a ICD10 code, get some info about it
# -- getHormonalType() For a given hormonal contraceptive brand, return the type
#                      of hormonal contraceptive that it is (ie, Progestin,
#                      Progestin + Estradiol, Non-hormonal, or Unknown)
############################
{
  # For a ATC medical code, get some info about it
  getATCInfo <- function(atcCode){
    
    drugFamily = paste0("ATC_CODE_BAD_FORMAT_", atcCode)
    
    #Get the first letter
    firstLetter = substring(atcCode, 1, 1) # R is a horrible language, can't reference chars in strings, and can't do pop() in strings
    
    # R is a very horrible language that doesn't have switch
    if(firstLetter == "A") drugFamily = "Alimentary tract and metabolism"
    if(firstLetter == "B") drugFamily = "Blood and blood forming organs"
    if(firstLetter == "C") drugFamily = "Cardiovascular system"
    if(firstLetter == "D") drugFamily = "Dermatologicals"
    if(firstLetter == "G") drugFamily = "Genito-urinary system and sex hormones"
    if(firstLetter == "H") drugFamily = "Systemic hormonal preparations, excluding sex hormones and insulins"
    if(firstLetter == "J") drugFamily = "Antiinfectives for systemic use"
    if(firstLetter == "L") drugFamily = "Antineoplastic and immunomodulating agents"
    if(firstLetter == "M") drugFamily = "Musculo-skeletal system"
    if(firstLetter == "N") drugFamily = "Nervous system"
    if(firstLetter == "P") drugFamily = "Antiparasitic products, insecticides and repellents"
    if(firstLetter == "R") drugFamily = "Respiratory system"
    if(firstLetter == "S") drugFamily = "Sensory organs"
    if(firstLetter == "V") drugFamily = "Various"
    
    return(drugFamily)
  }
  
  # For a ICD10 code, get some info about it
  getICD10Info <- function(icd10Code){
    
    diseaseFamily = paste0("Unknown_Code_", icd10Code)
    
    #Get the first letter
    firstLetter = substring(icd10Code, 1, 1) # R is a horrible language, can't reference chars in strings, and can't do pop() in strings
    #Get the next 2 numbers
    numericCode = as.numeric(substring(icd10Code, 2, 2))
    
    # R is a very horrible language that doesn't have switch
    if(firstLetter == "A") diseaseFamily  = "Infectious and parasitic"
    if(firstLetter == "B") diseaseFamily  = "Infectious and parasitic"
    if(firstLetter == "C") diseaseFamily  = "Neoplasms"
    if(firstLetter == "D"){
      if(numericCode <  50) diseaseFamily = "Neoplasms"
      if(numericCode >= 50) diseaseFamily = "Blood and blood-forming organs"
    } 
    
    if(firstLetter == "E") diseaseFamily  = "Endocrine, nutritional and metabolic"
    if(firstLetter == "F") diseaseFamily  = "Mental and behavioural"
    if(firstLetter == "G") diseaseFamily  = "Nervous system"
    if(firstLetter == "H"){
      if(numericCode <  60) diseaseFamily = "Eye and adnexa"
      if(numericCode >= 60) diseaseFamily = "Ear and mastoid process"
    }
    
    if(firstLetter == "I") diseaseFamily  = "Circulatory system"  
    if(firstLetter == "J") diseaseFamily  = "Respiratory system"
    if(firstLetter == "K") diseaseFamily  = "Digestive system"
    if(firstLetter == "L") diseaseFamily  = "Skin and subcutaneous tissue"
    
    if(firstLetter == "M") diseaseFamily  = "Musculoskeletal system and connective tissue"
    if(firstLetter == "N") diseaseFamily  = "Genitourinary system"
    if(firstLetter == "O") diseaseFamily  = "Pregnancy, childbirth and the puerperium"
    if(firstLetter == "P") diseaseFamily  = "Perinatal period"
    
    if(firstLetter == "Q") diseaseFamily  = "Congenital malformations, deformations and chromosomal abnormalities"
    if(firstLetter == "R") diseaseFamily  = "Not elsewhere classified"
    if(firstLetter == "S") diseaseFamily  = "Injury, poisoning and certain other consequences of external causes"
    if(firstLetter == "T") diseaseFamily  = "Injury, poisoning and certain other consequences of external causes"
    if(firstLetter == "U") diseaseFamily  = "Codes for special purposes "
    
    if(firstLetter == "V") diseaseFamily  = "External causes of morbidity and mortality"
    if(firstLetter == "W") diseaseFamily  = "External causes of morbidity and mortality"
    if(firstLetter == "X") diseaseFamily  = "External causes of morbidity and mortality"
    if(firstLetter == "Y") diseaseFamily  = "External causes of morbidity and mortality"
    
    if(firstLetter == "Z") diseaseFamily  = "Factors influencing health status and contact with health services"
    if(firstLetter == "Ñ") diseaseFamily  = "Review ICD10 Code manually "
    
    return(diseaseFamily)
    
  }
  
  # For a given hormonal contraceptive brand, return whether it is Progesting,
  # combination of progestin and estradiol, or unknown type of HC.
  #
  # The rules for the hormonal column goes as follows
  #
  #     Non-Hormonal:        Condons
  #
  #     Progestin-only:      Cerazette, Nexplanon, Depo-provera, Implanon
  #
  #     Progestin-Estradiol: 
  # 
  #         Low Estradiol:   Mercilon, Yasminelle, Loette 28, Nuvaring,
  #
  #         High Estradiol:  Marvelon, Yasmin, Microgynon, Oralcon, Diane,
  #                          Synfase, Evra, Zyrona
  #
  #     Unknown:             Any other brand/type 
  getHormonalType <- function(brandName){
    
    hormonalType = "Unknown"
    
    if(brandName == "Condons")      hormonalType = "Non-Hormonal"
    
    if(brandName == "Cerazette")    hormonalType = "Progestin"
    if(brandName == "Nexplanon")    hormonalType = "Progestin"
    if(brandName == "Depo-provera") hormonalType = "Progestin"
    if(brandName == "Implanon")     hormonalType = "Progestin"
    
    if(brandName == "Mercilon")     hormonalType = "Low Estradiol"
    if(brandName == "Yasminelle")   hormonalType = "Low Estradiol"
    if(brandName == "Loette")       hormonalType = "Low Estradiol"
    if(brandName == "Loette 28")    hormonalType = "Low Estradiol"
    if(brandName == "Nuvaring")     hormonalType = "Low Estradiol"
    
    if(brandName == "Marvelon")     hormonalType = "High Estradiol"
    if(brandName == "Yasmin")       hormonalType = "High Estradiol"
    if(brandName == "Microgynon")   hormonalType = "High Estradiol"
    if(brandName == "Oralcon")      hormonalType = "High Estradiol"
    if(brandName == "Diane")        hormonalType = "High Estradiol"
    if(brandName == "Synfase")      hormonalType = "High Estradiol"
    if(brandName == "Evra")         hormonalType = "High Estradiol"
    if(brandName == "Zyrona")       hormonalType = "High Estradiol"
    
    return(hormonalType)
    
  }
}


############################
# AUXILIAR
# -- distance2D() Get the distance between two 2D points
############################
{

    distance2D <- function(x1,x2,y1,y2){
    
        return (sqrt((x1-x2)^2 + (y1-y2)^2))
        
    }
        
}

# This go into another file
# Function that goes a easiest interface that looks like more proper C++ objects
DF <- function(n_row, n_col, defaultValue = NA){
  
  myNewDF = data.frame(matrix(defaultValue, nrow = n_row, ncol = n_col))
  return(myNewDF)
}

newList <- function(mylength){
    
    myNewList = vector("list", length = mylength)
    return(myNewList)
    
}






# Find all the unique integers values in between those column and the maximum count of each per column
#
# ie
#
# A B C D E
# 1 2 3 4 5
# 2 3 3 3 4
# 1 1 1 0 2
#
# with leftColumn = 2 and rightColumn = 4 ( B to D)
#
# Unique Integers: 0 1 2 3 4
#       Max Total: 1 1 1 2 1
maximumInteger <- function(tableBase, leftColumnIndex, rightColumnIndex){

    # This is used in the histogram for finding proper bin size, but
    # I have no idea of why. It does works though
        
    uniqueIntegers = NULL
    totalColumns   = rightColumnIndex - leftColumnIndex + 1
    
    # Find the unique integers
    for (i in 1:totalColumns) {
      currentIndex   = leftColumnIndex + i - 1
      currentUniques = unique(completeTable[,currentIndex])
      uniqueIntegers = unique(c(uniqueIntegers, currentUniques))
      
    }
    
    totalUniques = length(uniqueIntegers)
    totalForEach = rep(0, totalUniques)
    maxForEach   = rep(0, totalUniques)
    
    # Count each
    for (i in 1:totalUniques) {
      
      currentUnique = uniqueIntegers[i]
      currentTotal  = 0
      
      for (j in 1:totalColumns) {
        
        currentIndex   = leftColumnIndex + j - 1
        currentTotal   = sum(tableBase[,currentIndex] == currentUnique, na.rm = TRUE)
        maxForEach[i]  = max(maxForEach[i], currentTotal)
        
      }
      
    }
    
    return(maxForEach)
    
}
 

