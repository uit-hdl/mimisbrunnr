library(ggplot2)
library(rcompanion)   # For the multiple logistic regression (https://cran.r-project.org/src/contrib/Archive/rcompanion/)

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("./tools/toolsBasic.R",       encoding="utf-8")
source("./tools/toolsNetworks.R",    encoding="utf-8")
source("./tools/toolsLatex.R",       encoding="utf-8")
source("./tools/toolsSummarizers.R", encoding="utf-8")
source("./tools/toolsPlotting.R",    encoding="utf-8")



# ---------------------------------
#     ONE VARIABLE ANALYSIS
# ---------------------------------

# For a given vector of numbers return the CI for a given alpha (typically 95%)
#
# Return (low interval, average, high interval)
getCI <- function(myVector, alpha = 0.95){

        #input sample size, sample mean, and sample standard deviation
        n    = length(myVector)
        xbar = mean(myVector)
        s    = sd(myVector)

        #calculate margin of error
        margin = qt(  (1-((1-alpha)/2)) , df = n - 1) * s / sqrt(n)

        #calculate lower and upper bounds of confidence interval
        low  = xbar - margin
        high = xbar + margin    
        
        return(c(low,xbar,high))
    
}


# For a given table and column, return the type of variable that thiis column contain.
getColumnType <- function(tableBase, index){}


# ---------------------------------
#     MULTIPLE VARIABLES ANALYSIS
# ---------------------------------
{

  # ---------------------------------
  #     TWO CATEGORICAL ANALYSIS
  # ---------------------------------
  {
    

    
    # -----------------------------------------------
    #     Xi^2 TABLES
    # -----------------------------------------------
    {
      
      # Make a xi^2 table based on a group Index (ie Sex {man, woman}) with
      # respect another variable that is also categorical (ie: 
      # Smoke {no, some, always})
      #
      # For 2x2 tables it will perform a Yates correction
      #
      # Return two dataframes
      #
      # The first one with the absolute count for each combination
      #
      # ________| no | some | always |
      # | Man   | 3  |  5   |  10    |
      # -----------------------------
      # | Woman | 0  |  1   |  5    |
      #
      # The second one with the actual p-values
      #
      # ________| no | some | always |
      # | Man   | 3  |  5   |  10    |
      # -----------------------------
      # | Woman | 0  |  1   |  5    |
      #
      # Also write this two tables into latex at the given tableFilePath
      #
      # tableFilePath is either a FOLDER, or a list with the names that you wish
      # to give to the latex tables that will be generated from here
      #
        # If you have selected two variables with either less than 2 categories,
        # return 0,0 and print a warning message
        categoricalXi <- function(tableBase, groupingIndex, categoricalIndex,
                                  tableFilePath     = NULL,
                                  includeUnknown    = FALSE,
                                  overrideTableName = NULL,
                                  overrideCaption   = NULL,
                                  supressWarnings   = FALSE,
                                  logFile           = NULL){
        
            # The results dataframes goes here
            {
                myReturn = vector("list", length = 10)
                myReturn[[1]]  = 0  # Frequency table for the grouping (Rows)
                myReturn[[2]]  = 0  # Frequency table for the categorical (columns)
                myReturn[[3]]  = 0  # Absolute count for each combination
                myReturn[[4]]  = 0  # Relative count for each combination
                myReturn[[5]]  = 0  # Difference from theoretical distribution
                myReturn[[6]]  = 0  # Binomial test for each cell
                myReturn[[7]]  = 0  # The ultra-summary deluxe for Xi tables 
                myReturn[[8]]  = -1 # This number tells you if there was an error (-1)
                                    # Or if everything went fine, p-value (0,1)          
                myReturn[[9]]  = -1 # This is just a nice looking summary of frequencies
                myReturn[[10]]  = NA # In here we return the suggested filenames for the previous tables
            }

            # Init variables
            {
          
                # Plot type
                myFileType = "XiTable"
          
                # Table name
                myTableName = deparse(substitute(tableBase))
          
                if(!is.null(overrideTableName)){
                    myTableName = overrideTableName
            
                }
          
            }
        
            # Get an automatic tables name if you don't have a proper one
            # And if you ask for one
            {
          
                if(!is.null(tableFilePath)){
                
                    tableFilePath = automaticFilePath(tableFilePath, myDataFrame = tableBase,
                                                      tableName = myTableName, fileType = myFileType,
                                                      variableIndex1 = groupingIndex,
                                                      variableIndex2 = categoricalIndex)                
                }

            }
        
            # Delete the "Unknown" rows if necessary
            if(includeUnknown == FALSE){
          
                print("Delete Unknowns!")
                
                print(paste0("Before deleting ", nrow(tableBase)))
                
                tableBase = tableBase[tableBase[,groupingIndex]    != "Unknown",]
          
                tableBase = tableBase[tableBase[,categoricalIndex] != "Unknown",]
          
                print(paste0("After deleting ", nrow(tableBase)))
            }

            # Convert the numerical in categories if needed
            # -- For the grouping    variable (rows)
            {
          groupingVariableType = class(tableBase[,groupingIndex])
          if(groupingVariableType != "character" && groupingVariableType != "factor") { 
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,groupingIndex]))
            myLevels = as.character(myLevels)
            
            tableBase[,groupingIndex] = as.character(tableBase[,groupingIndex])
            tableBase[,groupingIndex] = factor(tableBase[,groupingIndex], levels = myLevels)
           
            # Tell the user that something wonky happens
            if(supressWarnings == FALSE){
              
              print("WARNING!!")
              print("Doing the xi-tables for:")
              print(myTableName)
              print("With index")
              print(groupingIndex)
              print("I found a numerical variable")
              print("I transformed into categorical automatically and did the xi-table anyway")
              print("Maybe you wanted to do something else?")
              
            }
             
          }
          
        }
            # -- For the categorical variable (columns)
            {
          categoricalVariableType = class(tableBase[,categoricalIndex])
          if(categoricalVariableType != "character" && categoricalVariableType != "factor") { 
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,categoricalIndex]))
            myLevels = as.character(myLevels)
            
            tableBase[,categoricalIndex] = as.character(tableBase[,categoricalIndex])
            tableBase[,categoricalIndex] = factor(tableBase[,categoricalIndex], levels = myLevels)
            
            # Tell the user that something wonky happens
            if(supressWarnings == FALSE){
              
              print("WARNING!!")
              print("Doing the xi-tables for:")
              print(myTableName)
              print("With index")
              print(categoricalIndex)
              print("I found a numerical variable")
              print("I transformed into categorical automatically and did the xi-table anyway")
              print("Maybe you wanted to do something else?")
              
            }
            
          }
          
        }

            # Get info about different categories
            {
          
                # Factors, I hate you, that's why I'm using the summarize function
                # that has everything inside done properly.
                currentSummaryA = summarizeCategorical(tableBase, groupingIndex,    sorted="none")
                currentSummaryB = summarizeCategorical(tableBase, categoricalIndex, sorted="none")

                myCategoriesA   = as.character(currentSummaryA[,1])
                nCategoriesA    = length(myCategoriesA)
                groupingNameA   = colnames(tableBase)[groupingIndex]
          
                myCategoriesB   = as.character(currentSummaryB[,1])
                nCategoriesB    = length(myCategoriesB)
                groupingNameB   = colnames(tableBase)[categoricalIndex]
                
                # We need to respect the factor order, but if the unknown are
                # deleted, but still exist, we need to take them from the variable
                # list names as well
                if(includeUnknown == FALSE){

                    if("Unknown" %in% myCategoriesA){
                        
                        myCategoriesA = myCategoriesA["Unknown" != myCategoriesA]
                        nCategoriesA  = nCategoriesA - 1
                    }

                    if("Unknown" %in% myCategoriesB){
                        
                        myCategoriesB = myCategoriesB["Unknown" != myCategoriesB]
                        nCategoriesB  = nCategoriesB - 1
                        
                    }

                }
                
            }
        
            # Make sure that you have at least 2 categories in each group
            # -- If you have less than 2 in either, print a warning
            if(nCategoriesA<2 || nCategoriesB <2){
          
          if(supressWarnings == FALSE){
            print("WARNING: Making xi² tables")
            print("You gave me two indexes and one of them have less than 2 categories")
            print(myTableName)
            print(groupingIndex)
            print(categoricalIndex)            
          }

        }
            # -- Otherwise, continue as normal
            else{

                # Count how many rows in the DB we have
                totalRows = nrow(tableBase)
          
                # Count how many are for each modality of the grouping variable
                uniquesDFA           =  data.frame(matrix(NA, nrow = nCategoriesA, ncol = 3))
                colnames(uniquesDFA) = c(groupingNameA, "NTotal", "NFreq")
          
                uniquesDFB           =  data.frame(matrix(NA, nrow = nCategoriesB, ncol = 3))
                colnames(uniquesDFB) = c(groupingNameB, "NTotal", "NFreq")
          
                # Make the df with the absolute , relative, diff, and binomial test
                xiDF           =  data.frame(matrix(0, nrow = nCategoriesA, ncol = nCategoriesB + 1))
                colnames(xiDF) = c("Absolute", myCategoriesB)

                # Init some of the other tables that we will use later, they have the same amount of cells
                # and the same names in the rows and columns
                xiFqcDF  = xiDF
                xiDiffDF = xiDF
                xiBiDF   = xiDF
          
                # Rename the top square with the proper table title
                colnames(xiFqcDF)[1]  = "Relative"  
                colnames(xiDiffDF)[1] = "Difference"
                colnames(xiBiDF)[1]   = "Binomial"
          
                # Count the combinations
                # -- For each of the grouping modalities (rows)
                for(i in 1:nCategoriesA){
            
            # Init variables
            currentModality = myCategoriesA[i]
            uniquesDFA[[i,1]]  = currentModality
            xiDF[[i,1]]        = currentModality
            xiFqcDF[[i,1]]     = currentModality
            xiDiffDF[[i,1]]    = currentModality
            xiBiDF[[i,1]]      = currentModality

            # Subset of the tableBase with respect the grouping modality
            currentModalityOnlyTable  = subset(tableBase, tableBase[,groupingIndex] == currentModality)
                        
            # Count the absolute and relative frequencies
            totalModality     = nrow(currentModalityOnlyTable)
            fqcModality       = totalModality/totalRows
            
            # Add it to the total frequency table
            uniquesDFA[[i,2]]  = totalModality
            uniquesDFA[[i,3]]  = fqcModality
            
            # -- For each of the categorical modalities (column)
            for(j in 1:nCategoriesB){
              
              # Init variables  
              destinyModality        = myCategoriesB[j]
              
              # Count how many of these two you have in the table
              # and write it down in the proper DF
              # -- From this subset, count how many do you have of the categorical modality
              destinyModalityTotal      = sum(currentModalityOnlyTable[,categoricalIndex] == destinyModality, na.rm = TRUE)
              # -- Write it down
              xiDF[[i,1+j]]    = destinyModalityTotal
              xiFqcDF[[i,1+j]] = destinyModalityTotal / totalRows
              
            }
            
          }
          
                # Count the uniques for category B
                for(j in 1:nCategoriesB){
            
                # Init variables
                currentModality   = myCategoriesB[j]
                uniquesDFB[[j,1]] = currentModality

                # Subset of the tableBase with respect the grouping modality
                currentModalityOnlyTable  = subset(tableBase, tableBase[,categoricalIndex] == currentModality)
            
                # Count the absolute and relative frequencies
                totalModality     = nrow(currentModalityOnlyTable)
                fqcModality       = totalModality/totalRows
            
                # Add it to the table
                uniquesDFB[[j,2]]  = totalModality
                uniquesDFB[[j,3]]  = fqcModality
            
            }

          # Now that each of the the uniqueDF tables are complete,
          # we can do the Binomial test in each cell
          # for each cell
          for(i in 1:nCategoriesA){
            for(j in 1:nCategoriesB){

              # First, we need to see how much is above/bellow what the theoretical value is suppose to be
              # The theoretical value is the Frequency of the Row x Frequency of the Column
              theoreticalValue = uniquesDFA[[i,3]] * uniquesDFB[[j,3]]
              # The actual vallue is what we have in the Frequency table
              actualValue = xiFqcDF[[i,1+j]]
              # See how much is above the rest
              xiDiffDF[[i,1+j]]  = actualValue / theoreticalValue 
              # Do the binomial test for that cell, it doesn't matter if you do it by rows or columns
              xiBiDF[[i,1+j]]    = binom.test(xiDF[[i,1+j]], uniquesDFA[[i,2]], uniquesDFA[[i,3]], alternative = "two.sided")$p.value
              #                               how many     , row sum         ,  probability

              print(actualValue)
              print(theoreticalValue)
              print(xiDiffDF)
              
              # Addjust p-values to be negative which represent UNDER BIAS
              if(xiDiffDF[[i,1+j]] < 1){

                # If the value is very close to 0 (around e^-300), R will round it to 0
                # We need to keep the 0s that are negative
                if(xiBiDF[[i,1+j]]      < 1/10^250) xiBiDF[[i,1+j]]      = 1/10^250
                # In any case, flip the sign
                xiBiDF[[i,1+j]]      = (-1) * xiBiDF[[i,1+j]]


              }

            }

          }
          
          # The table is complete, now we can make the proper R analysis
 
          # Do the Chi² Test for the whole contingency table
          xiTotalResults = chisq.test(xiDF[,-1])
          pValue         = xiTotalResults$p.value

          
          
          # Make the ultra summary table
          {
            summaryDF      = data.frame(matrix(0, nrow = nCategoriesA + 2, ncol = (nCategoriesB*2) + 3))
            
            # -- Init the table labels and names
            {
              # ---- First and last column
              colnames(summaryDF)[1]                   =  signif(pValue,1) 
              colnames(summaryDF)[ncol(summaryDF) - 1] = "Total"
              colnames(summaryDF)[ncol(summaryDF)]     = "Freq"
              # ---- Rest of columns
              for(j in 1:(nCategoriesB*2)){
                
                if(j %% 2 == 0) colnames(summaryDF)[j+1] = ""
                else colnames(summaryDF)[j+1] = myCategoriesB[floor(j/2)+1]
                
              }
              # ---- Last rows
              summaryDF[(nCategoriesA+1), 1] = "Total"
              summaryDF[(nCategoriesA+2), 1] = "Freq"
              # ---- Each of the rows
              for(i in 1:nCategoriesA){
                
                summaryDF[i, 1] = myCategoriesA[i]
                
              }
            }
            # -- Fill the data that we already have, absolute numbers and totals
            {
              # ---- Remove the very last cell, which is Total by Total (it doesn't make sense)
              {
                summaryDF[nrow(summaryDF)-1 , ncol(summaryDF) - 1] = totalRows
                summaryDF[nrow(summaryDF)-1 , ncol(summaryDF)    ] = ""
                summaryDF[nrow(summaryDF)   , ncol(summaryDF) - 1] = ""
                summaryDF[nrow(summaryDF)   , ncol(summaryDF)    ] = 1
              }

              # ---- Total and frequency of each row
              for(i in 1:nCategoriesA){
                
                summaryDF[i, ncol(summaryDF) - 1] = uniquesDFA[i,2]
                summaryDF[i, ncol(summaryDF) ]    = round(uniquesDFA[i,3],2)
                
              }
              
              # ---- Fill the absolute numbers from the xiDF table
              for(i in 1:nCategoriesA){
                
                for(j in 1:nCategoriesB){
                  
                  summaryDF[i,(j*2)] = xiDF[[i,1+j]]
                  
                }
                
              }
              
              # ---- Total of each column
              for(j in 1:nCategoriesB){
                
                summaryDF[nrow(summaryDF)-1,(j*2)] = sum(summaryDF[,(j*2)])
                summaryDF[nrow(summaryDF),  (j*2)] = round(uniquesDFB[j,3],2)
                
              }
              
              
            }
            # -- Finally, fill the extra info that tells you if something grows or shrink
            #    We only add the extra info in places where is significant, so we
            #    use the binomial table for that
            
            # In here we save another type of summary  
            overviewDF = summaryDF
              
            # ---- Fill the cells that are not the total
            for(i in 1:nCategoriesA){
              
              for(j in 1:nCategoriesB){
                
                # Same value as in the absolute table
                # Also add the theoretical values we should get
                
                summaryDF[i,(j*2)]      = paste0(xiDF[[i,1+j]] , "/" , floor(totalRows * uniquesDFA[i,3] * uniquesDFB[j,3]) )
                overviewDF[i,(j*2)]     = xiDF[[i,1+j]] 
                overviewDF[i,((j*2)+1)] = paste0("(" , round(xiFqcDF[[i,1+j]],2) , ")")
                
                # Add a little "+"/"-" in the next cell indicating something is wrong
                if(xiBiDF[[i,1+j]] > -0.05 && xiBiDF[[i,1+j]] < 0.05){
                  
                  # Add a lot of +/- if the difference is too big
                  signPower = 1
                  if (abs(xiBiDF[[i,1+j]])<0.01)  signPower = 2
                  if (abs(xiBiDF[[i,1+j]])<0.001) signPower = 3
                  
                  if(xiBiDF[[i,1+j]] > 0){
                    
                    summaryDF[i,(j*2)+1] = paste(rep("+", signPower), collapse="") # Seriously, R is horrible!
                    
                  }
                  else{
                    summaryDF[i,(j*2)+1] = paste(rep("-", signPower), collapse="")
                  }
                  
                }
                else{
                  summaryDF[i,(j*2)+1] = ""
                }
                
              }
            }
            
            # ---- Delete the 0's in the last "Total" row
            #      This is never significant since is just the description
            for(j in 1:nCategoriesB){
              
              summaryDF[nrow(summaryDF)-1,1 + (j*2)] = ""
              summaryDF[nrow(summaryDF),  1 + (j*2)] = ""
              
            }
            
            # ---- Move the frequency numbers +1 to the right
            #      Also delete the extra 0's in Total
            for(j in 1:nCategoriesB){
            
                overviewDF[nrow(summaryDF)-1,1 + (j*2)] = ""
                overviewDF[nrow(overviewDF), 1 + (j*2)] = overviewDF[nrow(overviewDF), (j*2)]
                overviewDF[nrow(overviewDF), (j*2)]     = ""
                
                #print(overviewDF)
                        
            }
            
            
            
          }

          # Addjust the tableFilePath return value because R is horrible
          # and if you return a NULL it shrink the vector size instead of having
          # A PROPER OBJECT which value is null
          if(is.null(tableFilePath)) tableFilePath = NA
          
          # Fill the return vector
          myReturn[[1]]  = uniquesDFA
          myReturn[[2]]  = uniquesDFB
          myReturn[[3]]  = xiDF
          myReturn[[4]]  = xiFqcDF
          myReturn[[5]]  = xiDiffDF
          myReturn[[6]]  = xiBiDF
          myReturn[[7]]  = summaryDF
          myReturn[[8]]  = pValue
          myReturn[[9]]  = overviewDF
          myReturn[[10]] = tableFilePath 
          
          
        }

            return (myReturn)
        
        }
      
      
        # This function takes a table and a list of explanatory indexes and 
        # another list of target indexes and it runs all the xi² combinations
        #
        # Returns:
        #    - a simple dataframe with the results
        simpleXi <- function(tableBase, explanatoryIndexesList, targetIndexesList){
          
            # Init some variables
            totalExplanatoryIndexes   = length(explanatoryIndexesList)
            totalTargetIndexes        = length(targetIndexesList)

            # Init the dataframe where we put the results
            # 
            # R is horrible, there was a bug here where it was use completeTable
            # instead of tableBase, and other variable names for the explanatory
            # and target lists. Because R doesn't care about variable type,
            # since completeTable is not declared as a global variable, it
            # should have been a pre-compilation error easy to fix instead of
            # making me lost 20 mins finding this.
            #
            # I should make an script that looks at variable declaration within
            # function and fix all of that forever.
            xi2SummaryDF = readyDFVariables(tableBase, explanatoryIndexesList, targetIndexesList)
            
            for(i in 1:totalExplanatoryIndexes){
            
                currentExplanatoryIndex = explanatoryIndexesList[i]
                
                for(j in 1:totalTargetIndexes){
              
                    currentTargetIndex = targetIndexesList[j]
                    xiResults = categoricalXi(tableBase, currentExplanatoryIndex, currentTargetIndex)

                    xi2SummaryDF[i,j+1] = xiResults[[8]]

                }
                    
            }
            
            return(xi2SummaryDF)
            
        }
      
        # This function takes a table and a list of explanatory indexes and 
        # another list of target indexes and it runs all the xi² combinations
        #
        # Returns:
        #    - a list of dataframes with the full summary of each combination
        completeXi <- function(tableBase, explanatoryIndexesList, targetIndexesList){
          
            # Init some variables
            totalExplanatoryIndexes   = length(explanatoryIndexesList)
            totalTargetIndexes        = length(targetIndexesList)

            # Init the dataframe where we put the results
            xi2CompelteDF = newList(totalExplanatoryIndexes * totalTargetIndexes)

            currentIndex  = 1
            
            for(i in 1:totalExplanatoryIndexes){
            
                currentExplanatoryIndex = explanatoryIndexesList[i]
                
                for(j in 1:totalTargetIndexes){
              
                    currentTargetIndex = targetIndexesList[j]
                    xiResults = categoricalXi(tableBase, currentExplanatoryIndex, currentTargetIndex)

                    xi2CompelteDF[[currentIndex]] = xiResults[[7]]

                    currentIndex = currentIndex + 1 
                }
                    
            }
            
            return(xi2CompelteDF)
            
        }
        
      
      
      
      #         |  Nasal               |  Throat
      # ________| Positive | Negative | Positive | Negative |
      # Sex
      # -----------------------------------------------------
      # | Man   | 3  |  5   |  10    |
      # -----------------------------
      # | Woman | 0  |  1   |  5    |
      metasummaryXi <- function(tableBase, aVector, bVector, filePath,
                                includeUnknown    = FALSE,
                                overrideTableName = NULL,
                                overrideCaption   = NULL,
                                supressWarnings   = FALSE,
                                logFile           = NULL){


        # Init variables
        {
          
          # Plot type
          myFileType = "MetaXiTable"
          
          # Table name
          myTableName = deparse(substitute(tableBase))
          
          if(!is.null(overrideTableName)){
            myTableName = overrideTableName
            
          }
          
          latexMetaString = "\\begin{table}[!htbp] \n \\centering \n \\resizebox{\\textwidth}{!}{ \n \\begin{tabular}{l|" # stupid scape characters in R make everything more complicated to read ~~, sometimes is \\x , sometimes is \n. I just want a verbatim copy of the string you stupid syntax of a language!
           
        }
        
        # Get an automatic tables name if you don't have a proper one
        {
          
          tableFilePath = automaticFilePath(filePath, myDataFrame = tableBase,
                                            tableName = myTableName, fileType = myFileType,
                                            variableIndex1 = NULL,
                                            variableIndex2 = NULL)
          
        }
        
        # Count how much information we have
        {
          # How many rows we have in the original table, regardless of whether
          # we take the Unknown for analysis or not
          totalOriginalRows = nrow(tableBase)
          
          # How many grouping variables we have and names
          totalAs = length(aVector)
          ANames  = colnames(tableBase)[aVector]
          
          # How many categorical variables we have
          totalBs = length(bVector)        
          BNames  = colnames(tableBase)[bVector]
          
          # Get how many categories we have in total in all the grouping variables
          totalAModalities = 0
          for(i in 1:totalAs){
            
            # Delete the "Unknown" rows if necessary
            # if(includeUnknown == FALSE){
            #   
            #   tableBase = tableBase[tableBase[,aVector[i]] != "Unknown" ,]
            #   
            # }
            
            currentSummaryA       = summarizeCategorical(tableBase, aVector[i] , sorted="none")  
            
            if(includeUnknown == FALSE){
            
              if("Unknown" %in% currentSummaryA[,1]){
                
                currentSummaryA = currentSummaryA[currentSummaryA[,1] != "Unknown",]

              }
              
            }
            
            totalAModalities      = totalAModalities + nrow(currentSummaryA)
            
          }
          
          # Get how many categories we have in total in all the categorical variables
          totalBModalities = 0
          for(j in 1:totalBs){
            
            # Delete the "Unknown" rows if necessary
            # if(includeUnknown == FALSE){
            #   
            #   tableBase = tableBase[tableBase[,bVector[j]]    != "Unknown",]
            # 
            # }
            
            currentSummaryB       = summarizeCategorical(tableBase, bVector[j], sorted="none")  
            
            if(includeUnknown == FALSE){
              
              if("Unknown" %in% currentSummaryB[,1]){
                
                currentSummaryB = currentSummaryB[currentSummaryB[,1] != "Unknown",]
                
              }
              
            }
            
            totalBModalities      = totalBModalities + nrow(currentSummaryB)
            
          }   
        }
        
        # Prepare the dataframe where we are going to put everything
        {
          # -- It has:
          #
          #         COLUMNS:
          #
          #         - Total and Frequency (common)
          #         - 3 column per modality in the B vector
          #             Actual
          #             Theoretical
          #             Sign
          #         - One extra column for variable names
          #
          #         ROWS:
          #
          #         - One row per A variable with the name of the categorical variable
          #         - One row per each of the A modalities x2 (total and frequency)
          #         - Total and Frequency (common)
          #         - Two extra rows for the columns labels
          
          
          myTotalRows    = totalAs + totalAModalities * 2 + 4
          myTotalColumns = 3 + (3*totalBModalities)
          
          # Generate the dataframe with empty columns names
          summaryDF            =  data.frame(matrix(NA, nrow = myTotalRows, ncol = myTotalColumns))
          colnames(summaryDF)  = (1:myTotalColumns) # Give indexes to the column names, we will change this later
          
          # Init the labels in the summary
          
          # -- The very first cell, and the second are blanks
          summaryDF[1,1] = ""
          summaryDF[2,1] = ""
          
          # -- The last cells titles in the columns are the Total and Frequency
          summaryDF[1,myTotalColumns - 1] = ""
          summaryDF[1,myTotalColumns]     = ""
          summaryDF[2,myTotalColumns - 1] = "Total"
          summaryDF[2,myTotalColumns]     = "Freq"        
        }
        
        # -- FILL ROWS
        {
          currentRowIndex = 3 # the first two rows are blank
          for(i in 1:totalAs){
            
            # Write the name of the variable in the DF
            summaryDF[currentRowIndex,1] = ANames[i]
            
            # Make all other cells in this row blank ""
            for(k in 2:myTotalColumns){
              summaryDF[currentRowIndex,k] = ""  
            }
            
            # Continue to the next row
            currentRowIndex = currentRowIndex + 1  
            
            # Get each of the modalities
            currentSummaryA = summarizeCategorical(tableBase, aVector[i], sorted="none")  

            if(includeUnknown == FALSE){
              
              if("Unknown" %in% currentSummaryA[,1]){
                
                currentSummaryA = currentSummaryA[currentSummaryA[,1] != "Unknown",]
                
              }
              
            }
            
            #print(currentSummaryA)
            
            # For each modality, write it in the dataframe
            for(k in 1:nrow(currentSummaryA)){
              
              # Write the modality
              summaryDF[currentRowIndex,1] = as.character(currentSummaryA[k,1])
              currentRowIndex = currentRowIndex + 1
              # Write a blank space
              summaryDF[currentRowIndex,1] = ""
              currentRowIndex = currentRowIndex + 1
              
              
            }
            
            
          }
          summaryDF[currentRowIndex,1] = "Total"
          currentRowIndex = currentRowIndex + 1  
          summaryDF[currentRowIndex,1] = "Freq"
          currentRowIndex = currentRowIndex + 1          
        }

        # -- FILL COLUMNS
        {
          
          currentColumnIndex = 2 # The first column is the row names
          
          # For each target variable
          for(j in 1:totalBs){
            
            # Save the current index position for later
            temporalCurrentIndex = currentColumnIndex

            # Get each of the modalities
            currentSummaryB = summarizeCategorical(tableBase, bVector[j], sorted="none")  
            
            if(includeUnknown == FALSE){
              
              if("Unknown" %in% currentSummaryB[,1]){
                
                currentSummaryB = currentSummaryB[currentSummaryB[,1] != "Unknown",]
                
              }
              
            }
            
            # For each modality, write it in the dataframe
            for(k in 1:nrow(currentSummaryB)){
              
              # Modality name
              summaryDF[2,currentColumnIndex] = as.character(currentSummaryB[k,1])
              # Make a blank in the variables names, which will be corrected later
              summaryDF[1,currentColumnIndex] = ""
              # Advance
              currentColumnIndex = currentColumnIndex + 1

              # Two blanks spaces where necessary
              summaryDF[2,currentColumnIndex] = ""
              summaryDF[1,currentColumnIndex] = ""
              currentColumnIndex = currentColumnIndex + 1
              summaryDF[2,currentColumnIndex] = ""
              summaryDF[1,currentColumnIndex] = ""
              currentColumnIndex = currentColumnIndex + 1
             
              # Add the column info to the latex table
              latexMetaString = paste0(latexMetaString, "lll")
               
            }
            
            # Set the name of the variable
            summaryDF[1,temporalCurrentIndex] = BNames[j]
            
            # Add the column line to the latex table
            latexMetaString = paste0(latexMetaString, "|")
            
            
          }
          
          # Add the final column lines to the latex table with the totals
          latexMetaString = paste0(latexMetaString, "ll|} \n")
          
          
        }

        # At this point, the summaryDF is completely initialize and ready to be filled
        # So we start doing that now one by one combination
        # -----------------------------------------------------------------------------
        {
          variableRowIndex    = 3 # We skip the first two blanks
          variableColumnIndex = 2 # The first column is the categories and modalities text 
          
          # Do a quick run through the columns and fill the total/frequency of each column
          {
            
            # For each column variables
            for(j in 1:totalBs){
              
              currentSummaryB  = summarizeCategorical(tableBase, bVector[j], sorted="none")  
              
              if(includeUnknown == FALSE){
                
                if("Unknown" %in% currentSummaryB[,1]){
                  
                  currentSummaryB = currentSummaryB[currentSummaryB[,1] != "Unknown",]
                  
                }
                
              }
              
              totalBModalities = nrow(currentSummaryB)

              # For each of the modalities of each variable
              for(l in 1:totalBModalities){

                # Fill the total and frequency for that variable
                summaryDF[(myTotalRows-1), variableColumnIndex + ((l-1)*3)] = currentSummaryB[l,2]
                summaryDF[myTotalRows,     variableColumnIndex + ((l-1)*3)] = paste0(as.character(round(currentSummaryB[l,3]*100,1)), "%")
                # Fill the blanks
                summaryDF[(myTotalRows-1), variableColumnIndex+((l-1)*3) + 1] = ""
                summaryDF[(myTotalRows-1), variableColumnIndex+((l-1)*3) + 2] = ""
                summaryDF[myTotalRows,     variableColumnIndex+((l-1)*3) + 1] = ""
                summaryDF[myTotalRows,     variableColumnIndex+((l-1)*3) + 2] = ""
                
              }
          
              # Do the next variable
              variableColumnIndex = variableColumnIndex + (totalBModalities * 3)
                  
            }
            
          }
          
          # Reset the column index back to the first again
          variableColumnIndex = 2 # The first column is the categories and modalities text 
          
          # For each row variable
          for(i in 1:totalAs){
            
            # Get the first A modalities
            currentSummaryA  = summarizeCategorical(tableBase, aVector[i], sorted="none")  
            
            if(includeUnknown == FALSE){
              
              if("Unknown" %in% currentSummaryA[,1]){
                
                currentSummaryA = currentSummaryA[currentSummaryA[,1] != "Unknown",]
                
              }
              
            }
            
            totalAModalities = nrow(currentSummaryA)
            
            # And for each target
            variableColumnIndex = 2 # The first column is the categories and modalities text 
            for(j in 1:totalBs){
              
              # Get the second A modalities
              currentSummaryB  = summarizeCategorical(tableBase, bVector[j], sorted="none")  
              
              if(includeUnknown == FALSE){
                
                if("Unknown" %in% currentSummaryB[,1]){
                  
                  currentSummaryB = currentSummaryB[currentSummaryB[,1] != "Unknown",]
                  
                }
                
              }
              
              totalBModalities = nrow(currentSummaryB)
              
              # Run the appropriate xi-analysis
              currentResults = categoricalXi(tableBase, aVector[i], bVector[j], "",
                                             includeUnknown    = includeUnknown,
                                             overrideTableName = overrideTableName,
                                             overrideCaption   = overrideCaption,
                                             supressWarnings   = supressWarnings,
                                             logFile           = logFile)
              
              # We only care about the results 7, it has all the information we need
              relevantSummary = currentResults[[7]]

              # Xi² test result, add it to the variable name
              #myXiResult = colnames(currentResults[[7]])[1]
              myXiResult = currentResults[[8]]
              myXiResult = as.numeric(round(myXiResult,4))
              summaryDF[variableRowIndex,variableColumnIndex] = paste0( "(pv: ", as.character(myXiResult), " ) ")

              # print("----------------------")
              # print( paste0(" DEBUG: ",myTableName,"      ")
              # print("----------------------")
              # print( paste0("Got a xi result of ", myXiResult))
              # print( paste0("Using indexes ", aVector[i], " and ", bVector[j]))
                            
              # Now find the appropriate frequencies and symbol for each cell
              # For each of the modalities of A
              for(k in 1:totalAModalities){
                
                
                # Fill the total and frequency for that variable
                summaryDF[variableRowIndex+((k-1)*2) + 1, (myTotalColumns-1)] = currentSummaryA[k,2]
                summaryDF[variableRowIndex+((k-1)*2) + 1,  myTotalColumns]    = paste0( as.character(round(currentSummaryA[k,3]*100,1)), "%")
                # Fill the blanks
                summaryDF[variableRowIndex+((k-1)*2) + 2, (myTotalColumns-1)] = ""
                summaryDF[variableRowIndex+((k-1)*2) + 2,  myTotalColumns]    = ""
                
                
                # For each of the modalities of B
                for(l in 1:totalBModalities){
                  
                  # Get the real count, theoretical count,
                  myRealCount        = as.numeric(strsplit(currentResults[[7]][k,l*2], split = "/")[[1]][1])
                  myTheoreticalCount = as.numeric(strsplit(currentResults[[7]][k,l*2], split = "/")[[1]][2])
                  
                  # real frequency, theoretical frequency,
                  myRealFrequency        = round(myRealCount*100/totalOriginalRows,1)
                  myTheoreticalFrequency = round(myTheoreticalCount*100/totalOriginalRows,1)
                  
                  # p-values with sign
                  mySigns     = currentResults[[7]][k,(l*2) + 1]
                  if(myXiResult > 0.05) mySigns = "" # If the p-value is not relevant, don't bother with the symbols
                  else{
                    # print("--- PRE ---")
                    # print(mySigns)
                    #mySigns = str_replace_all(mySigns, "+", "▴")  # str_replace_all doesn't work because "+" is not a valid character, because R was designed by a maniac! Aaaagg, I hate you so much
                    #mySigns = str_replace_all(mySigns, "-", "▾")
                    #ySigns = gsub("+", "▴", mySigns)
                    #mySigns = gsub("-", "▾", mySigns)                    
                    # print("--- POST ---")
                    # print(mySigns)
                    
                    # I need to hardcode this because character replacement with +/- sign doesn't make sense with R 
                    
                    if(mySigns == "+")   mySigns   = "▴"
                    if(mySigns == "++")  mySigns  = "▴▴"
                    if(mySigns == "+++") mySigns = "▴▴▴"
                    
                    
                    if(mySigns == "-")   mySigns   = "▾"
                    if(mySigns == "--")  mySigns  = "▾▾"
                    if(mySigns == "---") mySigns = "▾▾▾"
                    
                  }

                  
                  
                  
                  # Write those in the proper cell in our metasummary
                  # -- Counts
                  summaryDF[variableRowIndex + 1 + (k-1)*2,  variableColumnIndex + (l-1)*3]     = myRealCount
                  summaryDF[variableRowIndex + 1 + (k-1)*2,  variableColumnIndex + (l-1)*3 + 1] = paste0( "(",myTheoreticalCount,")")
                  # -- Relative
                  summaryDF[variableRowIndex + 2 + (k-1)*2,  variableColumnIndex + (l-1)*3]     = paste0(myRealFrequency,"%")
                  summaryDF[variableRowIndex + 2 + (k-1)*2,  variableColumnIndex + (l-1)*3 + 1] = paste0( "(",myTheoreticalFrequency,"%)")
                  # -- P-signs
                  summaryDF[variableRowIndex + 1 + (k-1)*2,  variableColumnIndex + (l-1)*3 + 2] = mySigns
                  # -- Blank under P-signs
                  summaryDF[variableRowIndex + 1 + (k-1)*2 + 1,  variableColumnIndex + (l-1)*3 + 2] = ""
                  
                }
              }
              
              variableColumnIndex = variableColumnIndex + (totalBModalities * 3)
              
            }
            
            
            
            # Update the variable index
            variableRowIndex = variableRowIndex + (totalAModalities * 2) + 1
            
          }
          
          # Fill the total count at the very last cells
          summaryDF[myTotalRows-1, myTotalColumns-1] = totalOriginalRows
          summaryDF[myTotalRows,   myTotalColumns-1] = ""
          summaryDF[myTotalRows-1, myTotalColumns]   = ""
          summaryDF[myTotalRows,   myTotalColumns]   = "100%"   
        }
        

        # At this point, the summaryDF is complete
        # Finally, we write the results into a proper latex table
        # -----------------------------------------------------------------------------
        latexFilePath = writeTableLATEX(summaryDF, tableFilePath,
                                        tableCaption="Meta-summary") # We get the filename from here, but we are going to overwrite it manually now
        
        
        # For each line in the final summary
        for (i in 1:nrow(summaryDF)) {
          
          # If we are in the first line
          # | Name B1 .... | Name B2 ... |   |
          if(i == 1){
            
            # The entire row is white (for now, change later)
            rowColorString = "\\rowcolor[HTML]{FFFFC7}"
          
            # Add the first &, the cell to the left is empty
            ampString = " \\cellcolor[HTML]{FFFFFF} &"
            
            # For each variable
            for(j in 1:totalBs){
              
              
              # Get the original name
              rawColName = BNames[j]
              
              # Transform weird string characters into proper latex characters
              # -- %
              if (grepl("%", rawColName)) rawColName = gsub("%", "\\\\%", rawColName)
              # -- _  
              if (grepl("_", rawColName)) rawColName = gsub("_", "$\\\\_$", rawColName)
              
              # Add the name, and 6 & which is each column
              ampString = paste0(ampString, rawColName , paste(as.character(rep("&", 6 )), sep="", collapse="")) # I hate R syntax, this is unnecessarely complicated
              
            }
            # There is the TOTAL and FREQ column missing
            # But we have one extra & already, so only add one more
            ampString = paste0(ampString, " \\cellcolor[HTML]{9AFF99} & \\cellcolor[HTML]{9AFF99} ")
            
            # Add the \\ that indicate the end of the line
            latexMetaString = paste0(latexMetaString, rowColorString, ampString, "\\\\ \n")
            
                
          }
          
          # If we are in the second line
          if(i == 2){
            
            # The entire row is white (for now, change later)
            rowColorString = "\\rowcolor[HTML]{FFFFC7}"
            
            # Add the first &, the cell to the left is empty
            ampString = " \\cellcolor[HTML]{FFFFFF} &"
            
            # For each variable
            for(j in 1:totalBs){
            
              # Get each of the modalities
              currentSummaryB = summarizeCategorical(tableBase, bVector[j], sorted="none")  
              
              if(includeUnknown == FALSE){
                
                if("Unknown" %in% currentSummaryB[,1]){
                  
                  currentSummaryB = currentSummaryB[currentSummaryB[,1] != "Unknown",]
                  
                }
                
              }
              
              # For each modality, write it in the dataframe
              for(k in 1:nrow(currentSummaryB)){
                
                # Modality name
                currentModalityName = as.character(currentSummaryB[k,1])
                
                # Add the name, and 3 & which is each total, theoretical, and p-value
                ampString = paste0(ampString, currentModalityName, paste(as.character(rep("&", 3 )), sep="", collapse=""))
                
              }
               
            }
            # There is the TOTAL and FREQ column missing
            # But we have one extra & already, so only add one more
            
            ampString = paste0(ampString, " \\cellcolor[HTML]{9AFF99} Total & \\cellcolor[HTML]{9AFF99} Freq")
            
            # Add the \\ that indicate the end of the line
            # Plus a full strike separation line
            latexMetaString = paste0(latexMetaString, rowColorString, ampString, "\\\\ \\hline \n")
            
            
          }
          
          # If we are in any other line that is not 1,2,-2, or -1
          if(i != 1 && i!=2 && i != (nrow(summaryDF) - 1) && i != nrow(summaryDF)){

            ampString   = ""
            variableRow = FALSE
            
            # If you are in a variable row, you need to color that line red
            # Otherwise, leave it white
            if( grepl("pv", summaryDF[i,2]) == TRUE ){
            
              variableRow = TRUE
              ampString = "\\rowcolor[HTML]{FFCCC9}"
                
            }

            # Just copy the rest of the summaryDF for now (we will complicate it later)
            for (j in 1:ncol(summaryDF)){
              
              # Change the % to proper latex if it is exist
              currentCell = gsub("%", "\\\\%",               summaryDF[i,j])
              
              
              # If we have parenthesis...
              # (and not a variable row)
              # Make it slighly grey
              if( grepl("\\(", summaryDF[i,j]) == TRUE && variableRow == FALSE ){
                
                currentCell = paste0("{\\color[HTML]{9B9B9B}", currentCell, "}")
                
              }
              
              
              # If we have triangles...
              # Change the triangle up to proper latex, and make it green
              if( grepl("▴", summaryDF[i,j]) == TRUE ){
              
                currentCell = gsub("▴", "$\\\\blacktriangle$",     currentCell)
                currentCell = paste0("{\\color[HTML]{009901}", currentCell, "}")
                
              }

              if( grepl("▾", summaryDF[i,j]) == TRUE ){
                
                currentCell = gsub("▾", "$\\\\blacktriangledown$", currentCell)  
                currentCell = paste0("{\\color[HTML]{FE0000}", currentCell, "}")
                
              }
              
              
              
              
              ampString = paste0(ampString, currentCell , "&")
              
            }
            
            # There is an extra & here, delete that
            ampString = substr(ampString,1,nchar(ampString)-1)
            
            # The last line might or might not have a strike
            if(i != (nrow(summaryDF) - 2) ){
              # Add the end of line \\
              latexMetaString = paste0(latexMetaString, ampString, "\\\\ \n")              
            }
            else{
              
              latexMetaString = paste0(latexMetaString, ampString, "\\\\ \\hline \n")              
              
            }
            

            
          }
          
          # If we are in the before last line
          if(i == (nrow(summaryDF) - 1)){

            ampString = ""

            # This case is pretty much the same as for the rest of the DF, but with another color for the row
            rowColorString = " \\cellcolor[HTML]{9AFF99}"
            
            # Just copy the rest of the summaryDF for now (we will complicate it later)
            for (j in 1:ncol(summaryDF)){
              
              currentCell = gsub("%", "\\\\%", summaryDF[i,j])
              ampString = paste0(ampString, currentCell , "&")
              
            }
            
            # There is an extra & here, delete that
            ampString = substr(ampString,1,nchar(ampString)-1)
            
            # Add the end of line \\
            latexMetaString = paste0(latexMetaString, rowColorString, ampString, "\\\\ \n")
            
             
          }
          
          # If we are in the last line
          if(i == nrow(summaryDF)){
           
            ampString = ""
            
            # Same as before, but with no strike line here
             
            # This case is pretty much the same as for the rest of the DF, but with another color for the row
            rowColorString = " \\cellcolor[HTML]{9AFF99}"
            
            # Just copy the rest of the summaryDF for now (we will complicate it later)
            for (j in 1:ncol(summaryDF)){
              
              currentCell = gsub("%", "\\\\%", summaryDF[i,j])
              ampString = paste0(ampString, currentCell , "&")
              
            }
            
            # There is an extra & here, delete that
            ampString = substr(ampString,1,nchar(ampString)-1)
            
            # We don't have \\ in the last line
            latexMetaString = paste0(latexMetaString, rowColorString, ampString, "\n")
            
          }
            
        }
        
        # Add the ending strings
        tableCaptionLatex = ""
        if(!is.null(overrideCaption)) tableCaptionLatex = paste("    \\caption{",overrideCaption,"}", sep="")
        
        latexMetaString = paste0(latexMetaString, "\\end{tabular} \n } \n",tableCaptionLatex," \\end{table} \n")
        
        # We have the string here, and we have the file where to write it,
        # then do so
        fileConn = file(latexFilePath, 'w')
        writeLinesBN(paste(latexMetaString, "\n",
                           sep=""), fileConn)

        # Encapsulate everything and return
        myReturn = vector("list", length = 2)
        myReturn[[1]] = summaryDF
        myReturn[[2]] = latexFilePath
        
        return(myReturn)
        
      }
      
      
      
    }
    
    
 
  }
  
 
  # ---------------------------------------------------------
  #     ONE CATEGORICAL ONE NUMERICAL VARIABLE
  # ---------------------------------------------------------
  {

        # ---------------------------------
        #   p-value  Numerical analysis
        # ---------------------------------
        simpleCategoricalPValue <- function(tableBase, groupIndex, variableIndex, skipUnknowns = FALSE){
            
            # Get the name of the table, save it for later
            myTableName = deparse(substitute(tableBase))
            
            # This is the value that we need to return including coding error
            # or whatever happens.
            pValueReturn = -1
            
            # Create a copy of the data that we can modify freely
            {
            
                copyTable = tableBase
                
                # Take away the NA rows in the grouping index
                keepTheseRows = rep(TRUE, nrow(tableBase))    
                keepTheseRows = keepTheseRows & (!is.na(tableBase[,groupIndex]))
                copyTable = copyTable[keepTheseRows,]
                       
                # Take away the Unknown values if any, and if you want
                if(skipUnknowns == TRUE){
                
                    keepTheseRows = rep(TRUE, nrow(copyTable))    
                    keepTheseRows = keepTheseRows & (  copyTable[,groupIndex] != "Unknown" )
                    copyTable = copyTable[keepTheseRows,]
                    
                }
                
                # We don't care about the NA in variables for now, they are filtered
                # away automatically by each function, and we will check that we have
                # a valid standard deviation and sufficient categories anyway
                
            }
            
            # Now count how many categories do you have, in order to do an
            # ANOVA or t-test (or something else, according to preconditions)
            myCategories  = unique(copyTable[,groupIndex])
            nCategories   = length(myCategories)
            groupingName  = colnames(copyTable)[groupIndex]
            numericalName = colnames(copyTable)[variableIndex]    
            
            
                            # Check out the preconditions data
          
                              #         tTestString = paste(" T-Test performed under these conditions:",
                              # "     + Assumed unequal variance (Welsh df)",
                              # "     + Two Sided",
                              # "     + Mu = 0",
                              # "",
                              # " - Whas your population sampled randomly properly?",
                              # "     If not try Resampling: http://finzi.psych.upenn.edu/R/library/coin/doc/coin.pdf",
                              # " - Whas your data non normally distributed?",
                              # "     For non-parametric test try Mann-Whitney U, Wilcoxon Signed Rank, Kruskal Wallis, and Friedman tests.",
                              # " - Are you doing a lot of T-Test at the same time?",
                              # "     Consider doing an ANOVA instead, and make sure you run a post-hoc if you don't.",
                              # sep="\n")
            
            
            # print(myCategories)
            # print(nCategories)
            # print(groupingName)
            # print(numericalName)
            
            # In the case we do a T-Test:
            if(nCategories == 2){
                
                # Init the centralities, even though it could be impossible to do them
                centralitiesDF           =  data.frame(matrix(NA, nrow = nCategories, ncol = 2 + 1))
                colnames(centralitiesDF) = c("ID", "Mean", "Median")
                centralitiesDF[,1]       = myCategories
                
                # Create as many subsets as there are categories, and put them into this list
                subsetsDF     = rep(list(data.frame(NULL)), nCategories)
                for(i in 1:nCategories){
            
                    subsetsDF[[i]] = subset(copyTable, copyTable[,groupIndex] == as.character(myCategories[i]))
            
                }
        
                # For each category (start at 1 because we want to find all centralities)
                for(i in 1:nCategories){
          
                    samplesI      = subsetsDF[[i]][,variableIndex]
          
                    centralitiesDF[i,2] = mean(samplesI,   na.rm = TRUE)
                    centralitiesDF[i,3] = median(samplesI, na.rm = TRUE)
          
                }
                
                # Count that you have enough data to do a T-Test
                {
                  samplesI      = subsetsDF[[1]][,variableIndex]
                  samplesJ      = subsetsDF[[2]][,variableIndex]
                  samplesI      = samplesI[!is.na(samplesI)]
                  samplesJ      = samplesJ[!is.na(samplesJ)]
                  totalSamplesI = length(samplesI)
                  totalSamplesJ = length(samplesJ)
                  sdSamplesI    = sd(samplesI)
                  sdSamplesJ    = sd(samplesJ)
                }
                
                # If you do...
                if( (totalSamplesI >= 2) && (totalSamplesJ >= 2) ){
                  
                    # Count that not everything is the same
                    sdSamplesI    = sd(samplesI)
                    sdSamplesJ    = sd(samplesJ)
                  
                    # If you do, then you can run a T-Test
                    if((sdSamplesI > 0) && (sdSamplesJ > 0) ){
                    
                        pValueReturn = t.test(subsetsDF[[1]][,variableIndex], subsetsDF[[2]][,variableIndex])$p.value
                    
                    }
                    else{
                    
                        pValueReturn = -3    
                        
                    }
 
                }
                else{
                
                    pValueReturn = -2
                        
                }
                
                
            }
            
            # In the case we do an ANOVA:
            if(nCategories > 2){
                
                anovaDF     = copyTable[,c(groupIndex,variableIndex)]  
                # Delete the NA rows
                anovaDF = anovaDF[is.na(anovaDF[,2]) == FALSE,]
                anovaDF = anovaDF[is.na(anovaDF[,1]) == FALSE,]

                # Gives names because R sucks and can't give numerical indexes    
                colnames(anovaDF) = c("Categorical", "Numerical")
    
                anovaResult = aov(Numerical ~ Categorical, data = anovaDF)

                pValueReturn = summary(anovaResult)[[1]][["Pr(>F)"]][1]

                
            }
            
            # In the case that we have nothing and we screw up
            if(nCategories < 2){
                
                print( "WARNING!! Can't generate p-value")
                print( "          Only one categorie was found")
                print( "          To make a p-value I need at least two categories with two different values in each")
                print( "          This was the attemp: ")
                print( paste("          Table:              ", myTableName,                                                  sep=''))
                print( paste("          Category variable:  ", groupingName , " only found ", as.character(myCategories[1]), sep='' ))
                print( paste("          Numerical variable: ", numericalName,                                                sep=''))
                
            }
            
            
            return(pValueReturn)
            
   
            
            
        }
      
      
    # ---------------------------------
    #          BOXPLOTS
    # ---------------------------------
    {



    }

    # ---------------------------------
    #          HISTOGRAM
    # ---------------------------------
    {

      doCategoricalHistogramPlot <- function(tableBase, variableIndex, categoryIndex, plotFilePath,
                                             totalBins = NULL, binsWidth = NULL, oneTickPerBin = TRUE,
                                             normalizeYMaximum = NULL, normalizeXMaximum = NULL,
                                             writeValues = TRUE,
                                             plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                             colorsVector = NULL,
                                             plotTheme = NULL,
                                             overrideTableName = NULL,
                                             overrideCaption = NULL){

        # Init variables
        {
          myPlotType = "CategoricalHistogram"
          myTableName = deparse(substitute(tableBase))

          if(!is.null(overrideTableName)){
            myTableName = overrideTableName

          }

          myTotalBins  = -1
          myBinsWidth  = -1

          minimumValue = min(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
          maximumValue = max(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
          deltaValue   = maximumValue - minimumValue

          upperValue   = max(maximumInteger(tableBase, variableIndex, variableIndex))
          lowerValue   = 0
          delta2Value  = upperValue - lowerValue

        }

        # Get info about different categories and numerical variables
        {
          myCategories  = unique(tableBase[,categoryIndex])
          nCategories   = length(myCategories)
          groupingName  = colnames(tableBase)[categoryIndex]
          numericalName = colnames(tableBase)[variableIndex]
        }
        
        # Get an automatic name if you don't have a proper one
        {

          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = categoryIndex, variableIndex2 = variableIndex)
          
        }

        # ---- Prepare the defaults
        {
          
          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }

        # ---- Prepare the defaults
        {
          
          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # Get the theme information
        themeData = getThemeParameters(plotTheme)

        # What to do with the number of bins, and wide of each bin
        {
          # ---- If neither of then are not initialize
          if(is.null(totalBins) && is.null(binsWidth)){

            myTotalBins  = 30
            myBinsWidth  = deltaValue/myTotalBins

          }

          # ---- If at least one is initialize
          else{

            # ---- If BOTH are init, this is a ERROR from the user
            if(!is.null(totalBins) && !is.null(binsWidth)){

              print("----")
              print("WARNING!: In the function doing the histogram")
              print("----")
              print("You have choosen to initialize both the number of bins, and how wid each bin is.")
              print("I must choose only one of the two given values.")
              print("I default to the number of bins.")
              print("Please correct this in your code so you don't see this message again.")
              print("----")

              myTotalBins  = 30
              myBinsWidth  = deltaValue/myTotalBins

            }

            else{

              # ---- If only total number of bins is init.
              if(!is.null(totalBins)){

                myTotalBins = totalBins
                myBinsWidth = deltaValue/myTotalBins

              }

              else{

                # ---- If only bin width is init.

                myBinsWidth  = binsWidth
                myTotalBins  = ceiling(deltaValue/myBinsWidth) + 1

              }

            }

          }

        }

        # Figure it out special numbers of the plot
        {

          # Where to put the breaks
          xBreaksEvery = myBinsWidth

          # The coordinates where to pain the plot
          expandXPercent = 0.1
          leftLimit     = minimumValue - (deltaValue * expandXPercent)
          rightLimit    = maximumValue + (deltaValue * expandXPercent)

          expandYPercent = 0.1
          upperLimit    = upperValue + (delta2Value * expandYPercent)
          lowerLimit    = lowerValue # - (delta2Value * expandYPercent)

          if(!is.null(normalizeXMaximum)) rightLimit    = normalizeXMaximum
          if(!is.null(normalizeYMaximum)) upperLimit    = normalizeYMaximum

        }

        # Do the plot
        {
          myPlot = ggplot(data=tableBase, aes(tableBase[,variableIndex], fill = tableBase[,categoryIndex])) +

                          # Do the histogram
                          #geom_histogram(bins = myTotalBins + 1, fill="black", col="grey") +
                          geom_histogram(binwidth = binsWidth, col="black") +
                          scale_fill_manual(values=colorsVector) +

                          # Limit the plot to the maximum and minimum
                          # coord_cartesian(xlim=c(minimumValue,maximumValue)) +
                          # Limit the plot to the maximum and minimum
                          coord_cartesian(xlim=c(leftLimit,rightLimit), ylim=c(lowerLimit,upperLimit)) +

                          # Add each of the individual breaks
                          #scale_x_continuous(breaks = seq(minimumValue , maximumValue , xBreaksEvery), lim = c(minimumValue - 1, maximumValue + 1)) +
                          scale_x_continuous(breaks = seq(minimumValue , maximumValue , xBreaksEvery)) +

                          # Create titles and subtitles
                          labs(title    = plotTitle,
                               subtitle = plotSubtitle,
                               fill     = groupingName,
                               caption  = plotCaption,
                               x = plotXLabel, y = plotYLabel) +

                          # Apply the theme
                          theme(panel.background   = themeData[[1]],
                                axis.line          = themeData[[2]],
                                panel.grid.major.y = themeData[[3]],
                                panel.grid.major.x = themeData[[4]],
                                legend.position    = themeData[[5]])

          # If you want to write the values on top of each bar (DEFAULT)
          if(writeValues == TRUE){

              maximumHeight  = max(hist( tableBase[,variableIndex], breaks=seq(minimumValue, maximumValue, by = xBreaksEvery), plot=FALSE)$counts)
              barHeightLimit = ceiling(maximumHeight * 0.05)

              # With black text on top of each column
              myPlot = myPlot + stat_bin(aes(y=..count.. , label = ifelse(..count.. > barHeightLimit, ..count.. , "" )  ),
                                         binwidth = binsWidth, geom="text", vjust=+1.5) # Write values for bars that are hight enought only



          }


        }

        # ---- Save the image
        imageWidth = ceiling(myTotalBins/2)
        ggsave(imageFilePath, plot = myPlot, width = imageWidth)
        latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return
        myReturn = vector("list", length = 3)
        myReturn[[1]] = myPlot
        myReturn[[2]] = imageFilePath
        myReturn[[3]] = latexFilePath
        
        return (myReturn)
        
        
      }


    }

    # ---------------------------------
    #          DENSITY
    # ---------------------------------
    {
      doCategoricalDensityPlot <- function(tableBase, variableIndex, categoryIndex, plotFilePath = NULL,
                                           plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                           colorsVector = NULL, plotTheme = NULL,
                                           borderPlot = FALSE, rotatePlot = FALSE,
                                           overrideTableName = NULL,
                                           overrideCaption = NULL){

        # Init variables
        {
          myPlotType = "CategoricalDensity"
          myTableName = deparse(substitute(tableBase))
          
          if(!is.null(overrideTableName)){
            myTableName = overrideTableName
            
          }          
        }

        # Get an automatic name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = categoryIndex, variableIndex2 = variableIndex)
          
        }

        # Get the theme information
        themeData = getThemeParameters(plotTheme)
        
        # Get info about different categories
        {
          myCategories  = unique(tableBase[,categoryIndex])
          nCategories   = length(myCategories)
          groupingName  = colnames(tableBase)[categoryIndex]
          numericalName = colnames(tableBase)[variableIndex]
          totalRows     = nrow(tableBase)          
        }
        
        
        # ---- Prepare the defaults
        {
          
          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }

        # Do the plot
        {
          
          densityPlot = ggplot(data = tableBase, aes(x = tableBase[,variableIndex], fill = tableBase[,categoryIndex])) +
            
            # Add the density plot
            geom_density(alpha=0.8) +
            scale_fill_manual(values=colorsVector) +
            
            # Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 color    = groupingName,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]])

          # Something about the border
          if(borderPlot == TRUE)
            densityPlot = densityPlot + border()
          
          # Rotate the plot if asked
          if(rotatePlot == TRUE )
            densityPlot = densityPlot + coord_flip()
          
        }
        
        # Save the image
        imageWidth = 8
        ggsave(imageFilePath, plot = densityPlot, width = imageWidth)
        latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return
        myReturn = vector("list", length = 3)
        myReturn[[1]] = densityPlot
        myReturn[[2]] = imageFilePath
        myReturn[[3]] = latexFilePath
        
        return (myReturn)
        
      }  
      
    }

    # ---------------------------------
    #          SPECIAL
    # ---------------------------------
    {
      # ---- BMI for each grouping
      doBMIPlot <- function(tableBase, BMIindex, groupIndex, plotFilePath,
                            colorsVector = NULL,
                            plotTitle = NULL, plotSubtitle = NULL,
                            plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                            plotTheme = NULL,
                            overrideTableName = NULL,
                            overrideCaption   = NULL){

        # Init variables
        myPlotType = "BMIPlot"
        myTableName = deparse(substitute(tableBase))

        if(!is.null(overrideTableName)){
          myTableName = overrideTableName

        }

        # Get an automatic name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = groupIndex, variableIndex2 = BMIindex)
          
        }

        # Get the theme information
        themeData = getThemeParameters(plotTheme)

        # Get info about different categories
        {
          myCategories  = unique(tableBase[,groupIndex])
          nCategories   = length(myCategories)
          groupingName  = colnames(tableBase)[groupIndex]
          numericalName = "BMI"
          totalRows     = nrow(tableBase)          
        }


        # ---- Prepare the defaults
        {

          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # ---- Find the maximum density
        myMaxDensity = max(density(tableBase[,BMIIndex], na.rm=TRUE)$y)

        for (i in 1:nCategories) {

          subTable      = tableBase[tableBase[,groupIndex] == myCategories[i],]
          subMaxDensity = max(density(subTable[,BMIIndex], na.rm=TRUE)$y)
          myMaxDensity  = max(myMaxDensity, subMaxDensity)

        }

        lowerCoordinate = myMaxDensity * -0.02
        middleCoordinate = lowerCoordinate/2

        # Do the plot
        {
          myBMIPlot = ggplot(tableBase, aes(x = tableBase[,BMIIndex],  fill = tableBase[,groupIndex]), colour = "Black") +

                             # Background rectangles
                             geom_rect(xmin=0,    xmax=18.5, ymin=-Inf, ymax=Inf, fill = "#ffffbb", color="black", alpha=0.05) +
                             geom_rect(xmin=18.5, xmax=25,   ymin=-Inf, ymax=Inf, fill = "#bbff9e", color="black", alpha=0.05) +
                             geom_rect(xmin=25,   xmax=30,   ymin=-Inf, ymax=Inf, fill = "#ffe3bb", color="black", alpha=0.05) +
                             geom_rect(xmin=30,   xmax=Inf,  ymin=-Inf, ymax=Inf, fill = "#ffbbbb", color="black", alpha=0.05) +

                             # Labels for the rectangles
                             geom_text(x = 14.5, y = middleCoordinate, label="Underweight", size=4, color="black", vjust = 1.5) +
                             geom_text(x = 21.5, y = middleCoordinate, label="Healthy",     size=4, color="black", vjust = 1.5) +
                             geom_text(x = 27.5, y = middleCoordinate, label="Overweight",  size=4, color="black", vjust = 1.5) +
                             geom_text(x = 33,   y = middleCoordinate, label="Obese",       size=4, color="black", vjust = 1.5) +

                             # Do a density plot
                             geom_density(alpha = 0.6) +

                             # Specify colors
                             scale_color_manual(values = colorsVector, na.value = COLOR_NA) +
                             scale_fill_manual(values  = colorsVector, na.value = COLOR_NA) +

                             # Set the limits for the IMC
                             xlim(12, 35) +
                             ylim(lowerCoordinate , myMaxDensity * 1.1) +

                             # Create titles and subtitles
                             labs(title    = plotTitle,
                                  subtitle = plotSubtitle,
                                  caption  = plotCaption,
                                  color    = groupingName,
                                  fill     = groupingName,
                                  x = plotXLabel, y = plotYLabel) +

                            # Apply the theme
                            theme(panel.background   = themeData[[1]],
                                  axis.line          = themeData[[2]],
                                  panel.grid.major.y = themeData[[3]],
                                  panel.grid.major.x = themeData[[4]])
        }

        # Save the image
        imageWidth    = 8
        ggsave(imageFilePath, plot = myBMIPlot, width = imageWidth)
        latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return
        myReturn = vector("list", length = 3)
        myReturn[[1]] = myBMIPlot
        myReturn[[2]] = imageFilePath
        myReturn[[3]] = latexFilePath
        
        return (myReturn)

      }
    }

  }


  # ---------------------------------------------------------
  #     TWO NUMERICALS VARIABLES
  # ---------------------------------------------------------
  {
    
    # Get two numerical columns and make the regression between then
    doSimpleRegression <- function(tableBase, independentColumnIndex, dependentColumnIndex, plotFilePath,
                                   colorsVector = NULL, showRegressionLine = TRUE,
                                   ymin = NULL, ymax = NULL,
                                   plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                   plotTheme = NULL,
                                   overrideTableName = NULL){
      
      # Define plot type
      myPlotType  = "Scatterplot"
      myTableName = deparse(substitute(tableBase))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }
      
      
      
      # Get an automatic name if you don't have a proper one
      {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = independentColumnIndex, 
                                                 variableIndex2 = dependentColumnIndex)
          
      }
      
      # Get the theme information
      themeData = getThemeParameters(plotTheme)
      
      # Get info about the variable
      numericalNameA = colnames(tableBase)[independentColumnIndex]
      numericalNameB = colnames(tableBase)[dependentColumnIndex]
      imageWidth     = 3
      
      # Prepare the defaults
      
      
      defaultVector = getBiNumericalDefaults(numericalNameA, numericalNameB, 
                                             colorsVector = colorsVector,
                                             plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                             plotCaption = plotCaption, plotXLabel = plotXLabel,
                                             plotYLabel = plotYLabel, fileType = "Scatterplot")
      
      colorsVector   = defaultVector[[1]]
      plotTitle      = defaultVector[[2]][1]
      plotSubtitle   = defaultVector[[3]][1]
      plotCaption    = defaultVector[[4]][1]
      plotXLabel     = defaultVector[[5]][1]
      plotYLabel     = defaultVector[[6]][1]
      
      # Init basic variables
      returnR2 = 0
      myModel  = 0
      
      # Find how many samples we have
      totalSampleIndependent = sum(!is.na(tableBase[,independentColumnIndex]))
      totalSampleDependent   = sum(!is.na(tableBase[,dependentColumnIndex]))
      
      # If we have enought do the plot, otherwise, skip the whole process
      if( totalSampleIndependent >= 2 && totalSampleDependent >= 2){
        
        # Check that the values match in the same position of the vector
        tableComplete <- sum((!is.na(tableBase[,independentColumnIndex])) & (!is.na(tableBase[,dependentColumnIndex])))
        
        if(tableComplete >=2 ){
          
          doPlots = TRUE
          
        }
        else{
          doPlots = FALSE
          
          print("-------------------------------------------------------")
          print("I can't make the regression of these two variables")
          print("There are enought samples in each variable group, but they don't coincide in the same row")
          print("")
          print("For example:")
          print("  A  |  B  |")
          print(" Na  |  1  |")
          print(" Na  |  2  |")
          print("  3  |  Na  |")
          print("  4  |  Na  |")
          print("-------------------------------------------------------")
          print("Columns:")
          print(paste("independent:",independentColumnIndex))
          print(paste("dependent:"  ,dependentColumnIndex))
          print("Total data found:")
          print(totalSampleIndependent)
          print(totalSampleDependent)
          print("-------------------------------------------------------")
          
        }
        
        
      }
      else{
        doPlots <- FALSE
        
        print("-------------------------------------------------------")
        print("I can't make the regression of these two variables")
        print("I don't have enough samples, need at least 2 for each variable")
        print("-------------------------------------------------------")
        print("Columns:")
        print(paste("independent:",independentColumnIndex))
        print(paste("dependent:"  ,dependentColumnIndex))
        print("Total data found:")
        print(totalSampleIndependent)
        print(totalSampleDependent)
        print("-------------------------------------------------------")
      }
      
      # If we have enought points...
      if(doPlots){
          
        # Delete the rows where are NAs, we have already check that this doesn't let you with empty sets
        naDependent   = is.na(tableBase[,dependentColumnIndex])
        naIndependent = is.na(tableBase[,independentColumnIndex])
        naRows        = naDependent | naIndependent
        tableBase     = tableBase[!naRows,]
        
        # Make the X² + X + C model
        myModel = lm(tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex] + I(tableBase[,independentColumnIndex]^2), tableBase)
        
        # Get the R2 value
        myR2value = summary(myModel)$r.squared
        
        # Get the P-value
        myPvalue  = summary(myModel)$coefficients[2,4]
        
        # The returning value have all the digits
        returnR2  = myR2value
        
        # The value that we are going to print in the plot has only 2 decimals (ie R^2 = 0.94)
        myR2value = format(myR2value, digits = 2)
        r2String  = paste("R2: ", myR2value, sep = " ")
        myPvalue  = format(myPvalue, digits = 4)
        PString   = paste("p-v: ", myPvalue, sep = " ")
        if(myPvalue == 0) PString = "p-v < 0.0001"
        
        # Do a scatter plot
        #--------------------------------------------------------------------
        ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex])) +
          
          # With hollow points
          geom_point(shape=1) +
          
          # With confidence interval of 95%
          geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
          
          # Add the R2 text
          geom_text(aes(x=-Inf,y=Inf,hjust=0,  vjust=1,    label= r2String), color="red") +
          geom_text(aes(x=-Inf,y=Inf,hjust=0,  vjust=2.5, label= PString),  color="red") +      
    
          # Create titles and subtitles
          labs(title    = plotTitle,
               subtitle = plotSubtitle,
               caption  = plotCaption,
               x = plotXLabel, y = plotYLabel)
        
        print(imageFilePath)
        
        ggsave(imageFilePath, width = 8)
        
      }
      
      return (myModel)
      
    }
    
    
    
    
  }
  
  # ---------------------------------------------------------
  #     MULTIPLE CATEGORICAL AND MULTIPLE NUMERICAL
  #
  #     If your dependent variable is continuous (and the residuals are normally distributed),
  #     but all of your independent variables are categorical, this is just an ANOVA.
  #
  #     If your dependent variable is categorical and your independent variables are continuous,
  #     this would be logistic regression (possibly binary, ordinal, or multinomial, depending).
  #
  #     If both your dependent variable and your independent variables are categorical variables,
  #     you can still use logistic regression—it's kind of the ANOVA-ish version of LR.

  
  # ---------------------------------------------------------
  {
    
    # This function gets a bunch of explicative variables.
    # Those which are categorical are transformed into independent dummy
    # variables ie:
    #
    #            Age Sex
    #            18  Female
    #            48  Male
    #            20  Male
    #
    #            Age Sex_Female Sex_Male
    #             18          1        0
    #             48          0        1
    #             20          0        1
    #
    # explicativeVariableDF is a dataframe with the variables that you want to use
    #
    # depedentVariableVector is the resulting vector (it doesn't matter if it is categorical or not)
    #
    # deleteUnknowns (string) If you have categories that has unknown strings to
    #                         notate unknown category ie:
    #
    #                                 BMI
    #                             -----------
    #                             Underweight
    #                             Overweight
    #                             Unknown
    #                             Healthy
    #                             Healthy
    #                 
    #                         And you want to exclude them from analysis, then
    #                         gives here whatever string you have use to notate
    #                         the unknown categories ie "Unknown".
    #
    # maxCombos (int) Is how many possible combinations of variables do you want
    #                 to try automatically. If the total possible combinations
    #                 Is to high, the function will stop, and you will get some
    #                 clues as to what you should delete to make the model
    #                 more handy
    multipleLogisticRegression <-function(explicativeVariablesDF, dependentVariableVector, deleteUnknowns = NULL, maxCombos = 100000){
      
      # Get the dimensions
      totalColumns = ncol(explicativeVariablesDF)
      totalRows    = nrow(explicativeVariablesDF)
      
      # First we need to figure it out which variables are categoricals and which one isn't
      categoricalDataVector = getCategoricalVariables(explicativeVariablesDF)
        
      # Get a DF with only the categorical values if any, and transform it to dummy variables
      # Add it to the explicative DF and delete the old variables
      totalCategorical = sum(categoricalDataVector)
      if(totalCategorical > 0){
        
        # Create the dummy DF
        categoricalDF               = explicativeVariablesDF[,categoricalDataVector]
        dummyExplicativeVariablesDF = fastDummies::dummy_cols(categoricalDF, remove_most_frequent_dummy = TRUE)
        dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(1:totalCategorical)]

        # If the user wants to, we must delete the unknown columns from the analysis
        if(!is.null(deleteUnknowns)){
          
          unknownColumns = grep(deleteUnknowns, colnames(dummyExplicativeVariablesDF))
          dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(unknownColumns)]
          
        }
        
        # Add a single column to the explicative variables to ensure that we
        # don't delete the entire DF ( I hate you R ), when we delete the
        # categorical columns
        explicativeVariablesDF$IHateYouR = 0
        
        # Delete the old categorical variables, whatever is left is only
        # numerical variables (if any)
        explicativeVariablesDF = explicativeVariablesDF[,!categoricalDataVector]
        
        # Add it to the original DF with all the variables
        explicativeVariablesDF = cbind(explicativeVariablesDF,dummyExplicativeVariablesDF)
      
        # Delete the extra column
        explicativeVariablesDF$IHateYouR = NULL
        
        
      }

      # From here we only have numerical values in the DF
      explicativeVariablesDF$Target = dependentVariableVector
      logisticReadyDF               = explicativeVariablesDF #alias, so I don't have to rewrite variables names

      # Get the new updated dimensions
      totalColumns = ncol(logisticReadyDF)
      totalExplicativeVariables = totalColumns - 1

      # Clean the rows that has NA values if any
      {
        keepTheseRows = rep(TRUE, totalRows)
        for (i in 1:totalColumns){
          keepTheseRows = keepTheseRows & (!is.na(logisticReadyDF[,i]))
          
        }
        logisticReadyDF  = logisticReadyDF[keepTheseRows,]
      }

      # Get the new updated dimensions
      totalRows    = nrow(logisticReadyDF)
      
      # We need to clean the columns names and clean any weird character
      # because R hates us, and it also hates proper programming
      currentNames = colnames(logisticReadyDF)
      newNames     = cleanWeirdCharacters(currentNames)
      colnames(logisticReadyDF) = newNames
      
      # Create the formula for the GLM programatically (stupid R syntax -_- )
      {
        valid.names = names(logisticReadyDF)[names(logisticReadyDF) != "Target"]  # all but group

        formulaString = "Target ~ "
        for(i in 1:length(valid.names)){
          formulaString = paste0(formulaString, " ", valid.names[i], " +") 
        }
        formulaString = substr(formulaString,1,nchar(formulaString)-1)
        myFormula = as.formula(formulaString)
      }
      
      # We are going to try to male all possible models if possible.
      totalBruteRows = (2^totalExplicativeVariables) - 1
      totalBruteCols = totalExplicativeVariables + 3
      
      # Show the step by step and full model summary
      {
        print(" Step by step procedure results ")
        
        model.null = glm(Target ~ 1,
                         data   = logisticReadyDF,
                         family = binomial(link="logit")
        )
        
        model.full = glm(myFormula,
                         data   = logisticReadyDF,
                         family = binomial(link="logit")
        )
        
        step(model.null,
             scope     = list(upper=model.full),
             direction = "both",
             test      = "Chisq",
             data      = logisticReadyDF
        )
        
        print(summary(model.full))      
      }

      # If we have too much models
      if(totalBruteRows > maxCombos){
        
        print("--------------------------------------")
        print("You have way to many possible combination of variables to try this analytically")
        print("")
        print(paste0("Total possible combinations = ", totalBruteRows))
        print("")
        print("Try the summary displayed above to restrict your variables a bit")
        print("--------------------------------------")
       

    }
      
      # If the size is enough, keep going
      else{
        
        print("Else")
        
        # Show the base model with all the columns
        model.full = glm(myFormula,
                         data   = logisticReadyDF,
                         family = binomial(link="logit")
        )
        
        print(summary(model.full))

        # Try all possible combination of models with the given variables
        # Evaluate the model with pseudo R2
        {
          
          # Keep track of the best scores
          bestMcFadden   = -99
          bestCox        = -99
          bestNagelkerke = -99
          
          bestModelDF    = data.frame(matrix(FALSE, nrow = totalBruteRows, ncol = totalBruteCols))
          colnames(bestModelDF) = c(valid.names, "McFadden", "Cox", "Nagelkerke")

          # For each of the possible brute force combinations
          for(i in 1:totalBruteRows){
            
            myCurrentBinary = number2binary(i, totalExplicativeVariables)
            
            # Mark the columns that you are using and init the pseudo R² to -99
            # The -99 means nothing, is just a value to write something in the DF
            # and update it later
            bestModelDF[i,] = c(myCurrentBinary, -99, -99, -99)
            
            # Make the formula and evaluate the model
            currentFormulaString = "Target ~ "
            for(j in 1:totalExplicativeVariables){
              # If it is a valid variable
              if(bestModelDF[i,j] == 1){
                currentFormulaString = paste0(currentFormulaString, " ", valid.names[j], " +")   
              }
            }

            # Delete the last  "+"
            currentFormulaString = substr(currentFormulaString,1,nchar(currentFormulaString)-1)
            myFinalFormula = as.formula(currentFormulaString)
            
            print("Before Full")
            print(myFormula)
            print(myFinalFormula)
            
            # Show the base model with all the columns
            model.full = glm(myFormula,
                             data   = logisticReadyDF,
                             family = binomial(link="logit")
            )
            
            # Make the model
            model.current = glm(formula = myFinalFormula,
                                data    = logisticReadyDF,
                                family  = binomial(link="logit"))
            
            modelEvaluation = nagelkerke(model.current)
            
            bestModelDF[i,(totalBruteCols - 2)] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[1]
            bestModelDF[i,(totalBruteCols - 1)] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[2]
            bestModelDF[i, totalBruteCols     ] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[3]
            
          }
          
          # Update the score of the best model
          bestMcFadden   = max(bestModelDF$McFadden)
          bestCox        = max(bestModelDF$Cox)
          bestNagelkerke = max(bestModelDF$Nagelkerke)
          
          
          print(bestMcFadden)
          print(bestCox)
          print(bestNagelkerke)
          
        }
        
      }
      

    }

    # # Get a dependent variable Vector
    # dependentVariable = completeTable$C_NasalCarrier
    # # dependentVariable = as.character(completeTable$C_NasalCarrier)
    # # dependentVariable = dependentVariable[dependentVariable == "Positive"] = 1
    # # dependentVariable = dependentVariable[dependentVariable == "Negative"] = 0
    # 
    # # Prepare a dataframe with all the explicative variables
    # explicativeDF = completeTable[,c(importantNumericalIndexes, importantCategoricalIndexes)]
    # # Remove the BMI Categorical, because BMI is better
    # explicativeDF$BMICategorical = NULL
    # 
    # # Best model
    # # explicativeDF = completeTable[,c(sexIndex, sportsIndex)]
    # 
    # # # Make the numbers for the model and delete Unknowns
    # # explicativeDF$SexNumerical    = 0
    # # explicativeDF$SportsNumerical = 0
    # # explicativeDF$SexNumerical[explicativeDF$Sex       == "Man"]    = 1
    # # explicativeDF$SportsNumerical[explicativeDF$Sports == "Light"]  = 1
    # # explicativeDF$SportsNumerical[explicativeDF$Sports == "Medium"] = 1
    # # explicativeDF$SportsNumerical[explicativeDF$Sports == "Hard"]   = 2
    # # 
    # # # Delete the stuff from the dependent vector
    # # dependentVariable = dependentVariable[c(explicativeDF$Sports != "Unknown")]
    # # # Delete the stuff from the explicative variables
    # # # -- Unknowns
    # # explicativeDF = explicativeDF[explicativeDF$Sports != "Unknown",]
    # # # --Delete the not numerical columns
    # # explicativeDF$Sex    = NULL
    # # explicativeDF$Sports = NULL
    # 
    # #multipleLogisticRegression(explicativeDF, dependentVariable)
    # multipleLogisticRegression(explicativeDF, dependentVariable, deleteUnknowns="Unknown")
    
  }

}

# ---------------------------------
#     OTHER ANALYSIS
# ---------------------------------
{

  # ---------------------------------
  #     GRAPHS and NETWORKS
  # ---------------------------------
  {

    # Find the homophily for a given graph and a given set of attributes
    # homophily(overallGraph, "Sex"), homophily(overallGraph, "Sex", "Woman")
    homophily <- function(graph,vertex.attr,attr.val=NULL,prop=T){

        #Assign names as vertex attributes for edgelist output#
        V(graph)$name<-vertex_attr(graph,vertex.attr)
        #Get the basic edgelist#
        ee<-get.data.frame(graph)
        #If not specifying on particular attribute value, get percentage (prop=T)#
        #or count (prop=F) of all nodes tied with matching attribute#
        if(is.null(attr.val)){
          ifelse(prop==T,sum(ee[,1]==ee[,2])/nrow(ee),sum(ee[,1]==ee[,2]))
          #If not null, get proportion (prop=T) or count (prop=F) of#
          #edges among nodes with that particular node attribute value#
        } else {
          ifelse(prop==T,sum(ee[,1]==attr.val & ee[,2]==attr.val)/nrow(ee[ee[,1]==attr.val|ee[,2]==attr.val,]),
                 sum(ee[,1]==attr.val & ee[,2]==attr.val))
        }

    }

    # Provide the Xi2 analysis of the variable in the graph
    # Not working well, need to re-structure the code
    depecratedCompleteXi <- function(graph, categoricalIndex){

      # Get the tables
      tableEdges    = igraph::as_data_frame(graph, "edges")
      tableBase     = igraph::as_data_frame(graph, "vertices")
      variableName  = colnames(tableBase)[categoricalIndex]
      totalRows     = nrow(tableBase)
      totalEdges    = nrow(tableEdges)

      # Get info about different categories
      myCategories  = unique(tableBase[,categoricalIndex])
      nCategories   = length(myCategories)
      groupingName  = colnames(tableBase)[categoricalIndex]

      # Count how many uniques for each group (Nodes)
      uniquesDF           =  data.frame(matrix(NA, nrow = nCategories, ncol = 6))
      colnames(uniquesDF) = c(groupingName, "NTotal", "NFreq", "Out", "No", "To")

      for(i in 1:nCategories){

        currentModality   = myCategories[i]
        totalModality     = sum(tableBase[,categoricalIndex] == currentModality, na.rm = TRUE)
        fqcModality       = totalModality/totalRows

        uniquesDF[[i,1]]  = currentModality
        uniquesDF[[i,2]]  = totalModality
        uniquesDF[[i,3]]  = fqcModality

      }

      # Count how many uniques for each group (Edges)
      uniquesEdgesDF        =  uniquesDF
      temporalGraph         = graph
      V(temporalGraph)$name <- vertex_attr(temporalGraph,variableName) # this doesnt work? V(temporalGraph)$names = vertex_attr(temporalGraph,variableName)
      temporalEdges         <- get.data.frame(temporalGraph) #This doesn't work either temporalEdges          = get.data.frame(temporalGraph)
                                                            # These two lines changes ID for attribute, so if you have:
      # 1 --> 3
      # 3 --> 2
      # 2 --> 1
      #
      # 1 = Man
      # 2 = Woman
      # 3 = Woman
      #
      # Man   -> Woman
      # Woman -> Woman
      # Woman -> Man
      
      for(i in 1:nCategories){

        currentModality   = myCategories[i]
        totalModality     = sum(temporalEdges[,1] == currentModality) # Take the froms that coincide with the variable name
        fqcModality       = totalModality/totalEdges

        uniquesEdgesDF[[i,1]]  = currentModality
        uniquesEdgesDF[[i,2]]  = totalModality
        uniquesEdgesDF[[i,3]]  = fqcModality

      }

      # From the edges point of view doesn't make sense to keep these columns
      uniquesEdgesDF$Out = NULL
      uniquesEdgesDF$No  = NULL
      uniquesEdgesDF$To  = NULL

      # Rename the columns
      colnames(uniquesEdgesDF) = c(groupingName, "ETotal", "EFreq")

      # Copy this info into the main summary
      uniquesDF$ETotal = uniquesEdgesDF$ETotal
      uniquesDF$EFreq  = uniquesEdgesDF$EFreq

      # We are going to make the following matrices now:
      # -- uniquesDF (already done)
      #     For each modality:
      #       (TOTAL)     Total Nodes,
      #       (FREQUENCY) Rel Frequency,
      #       (OUT)       How many of them give out at least one nomination,
      #       (NO)        How many give no nominations, 
      #       (TO)        How many of the general population receive a nomination
      #
      # -- xiDF , the absolute number of nodes linking. Each row sum should be equal to (TOTAL)
      #
      # -- xiFqcDF, the relative number of nodes linking. Each row sum should be equal to 1 (100%)
      #
      # -- xiDiffDF, the proportion between the relative frequencies of nomination with respect the actual frequencies in the population
      #              An unbias relationship should be equal to 1.
      #              A bias relationship should be greater than 1 if LIKE or smoller than 1 if DISLIKE
      #
      # -- xiBiDF,   The p-value of the difference obtained with a binomial test
      
      # Make the matrix with the absolute relationships
      xiDF           =  data.frame(matrix(0, nrow = nCategories, ncol = nCategories + 1))
      colnames(xiDF) = c("Total", myCategories)

      # Make another one with the relative frequencies and divergence
      xiFqcDF  = xiDF
      xiDiffDF = xiDF
      xiBiDF   = xiDF

      # Make the same for the edge base Xi analysis
      xiEdgesDF     = xiDF
      xiEdgesFqcDF  = xiDF
      xiEdgesDiffDF = xiDF
      xiEdgesBiDF   = xiDF

      xiEdgesConditionalFqcDF  = xiDF

      # Rename the top square
      colnames(xiFqcDF)[1]  = "N Relative"
      colnames(xiDiffDF)[1] = "N Difference"
      colnames(xiBiDF)[1]   = "N Binomial"
      
      colnames(xiEdgesFqcDF)[1]  = "E Relative"
      colnames(xiEdgesDiffDF)[1] = "E Difference"
      colnames(xiEdgesBiDF)[1]   = "E Binomial"

      #xiEdgesConditionalFqcDF[1,1] = "Rel/Cond"

      # Count who follows who
      for(i in 1:nCategories){

        # Init variables
        currentModality      = myCategories[i]

        xiDF[[i,1]]          = currentModality
        xiFqcDF[[i,1]]       = currentModality
        xiDiffDF[[i,1]]      = currentModality
        xiBiDF[[i,1]]        = currentModality

        xiEdgesDF[[i,1]]     = currentModality
        xiEdgesFqcDF[[i,1]]  = currentModality
        xiEdgesDiffDF[[i,1]] = currentModality
        xiEdgesBiDF[[i,1]]   = currentModality

        #xiEdgesConditionalFqcDF[[i,1]]  = currentModality

        # FOR THE NODES
        
        # Get the list of nodes that coincide with that category and to whom is adressed
        currentNodes        = tableBase[tableBase[,categoricalIndex] == currentModality,1]
        goingSomewhere      = unique(tableEdges[tableEdges$from %in% currentNodes ,]$from)
        goingNowhere        = setdiff(currentNodes , goingSomewhere)
        currentEdgesDestiny = unique(tableEdges[tableEdges$from %in% currentNodes ,]$to)
        totalDestiny        = length(currentEdgesDestiny)

        # Get the list of edges that coincide with that category and to whom is adressed
        uniquesDF[[i,4]]  = length(goingSomewhere)
        uniquesDF[[i,5]]  = length(goingNowhere)
        uniquesDF[[i,6]]  = totalDestiny

        # FOR THE EDGES
        
        currentTotalEdges = uniquesDF$ETotal[i]
        
        # For each of the destiny, find out which modality is
        for(j in 1:nCategories){

          destinyModality        = myCategories[j]
          candidateDestinyNodes  = tableBase[tableBase[,categoricalIndex] == destinyModality,1] # All the IDs for that modality
          destinyNodes           = intersect(currentEdgesDestiny, candidateDestinyNodes) # Intersect all the IDs with the actual destiny IDs

          # Nodes
          xiDF[[i,1+j]]          = length(destinyNodes)              # Copy the count into the main DF
          xiFqcDF[[i,1+j]]       = length(destinyNodes)/totalDestiny # Find the relative count for each
          if(totalDestiny == 0)  xiFqcDF[[i,1+j]] = 0                # If is someone who doesn't nominate anybody ever, set it as 0
                                                                     # Actually not sure what would be the correct in this case

          # Edges
          andEdges               = sum(temporalEdges[,1] == currentModality & temporalEdges[,2] == destinyModality)
          orEdges                = sum(temporalEdges[,1] == currentModality | temporalEdges[,2] == destinyModality)
          xiEdgesDF[[i,1+j]]     = andEdges                   # Absolute count
          xiEdgesFqcDF[[i,1+j]]  = andEdges/currentTotalEdges # Relative count

          #xiEdgesConditionalFqcDF[[i,1+j]]  = andEdges/orEdges   # Relative count conditional

        }

      }

      # print("---")
      # print(xiEdgesDF)
      # print(uniquesDF)
      # print("---")
      
      # Now that the uniqueDF table is complete, we can do the Binomial test
      for(i in 1:nCategories){

        # Init variables
        currentModality   = myCategories[i]

        for(j in 1:nCategories){

          destinyModality       = myCategories[j]

          # print("Trying for...")
          # print(currentModality)
          # print(destinyModality)
          # print("--------------")
          # print("--")
          # print(xiEdgesDF[[i,1+j]])
          # print(uniquesDF[[j,7]])
          # print("--")
          # print("--------------")
          
          
          
          # For the nodes
          xiDiffDF[[i,1+j]]  = xiFqcDF[[i,1+j]]/uniquesDF[[j,3]] # See how much is above the rest
          xiBiDF[[i,1+j]]    = binom.test(xiDF[[i,1+j]],         uniquesDF[[i,6]],     uniquesDF[[i,3]],            alternative = "two.sided")$p.value
                                          # how many           , row sum             , probability
                                          # Actual Connections / Total Connections   / Probability for that group
                                          #                      that goes somewhere /
                                          #                      not total nodes.

          #print(paste0 (xiDF[[i,1+j]] , " / ", uniquesDF[[i,6]], ". p = ", uniquesDF[[i,3]], " = ", xiBiDF[[i,1+j]]  ))
          
          # For the edges
          xiEdgesDiffDF[[i,1+j]] = xiEdgesFqcDF[[i,1+j]]/uniquesDF[[j,8]]
          xiEdgesBiDF[[i,1+j]]   = binom.test(xiEdgesDF[[i,1+j]], uniquesDF[[i,7]], uniquesDF[[i,8]],               alternative = "two.sided")$p.value

          # Addjust p-values to be negative which represent AVOID
          if(xiDiffDF[[i,1+j]] < 1){

            # If the value is very close to 0 (around e^-300), R will round it to 0
            # We need to keep the 0s that are negative
            if(xiBiDF[[i,1+j]]      < 1/10^250) xiBiDF[[i,1+j]]      = 1/10^250
            if(xiEdgesBiDF[[i,1+j]] < 1/10^250) xiEdgesBiDF[[i,1+j]] = 1/10^250
            
            # In any case, flip the sign
            xiBiDF[[i,1+j]]      = (-1) * xiBiDF[[i,1+j]]
            xiEdgesBiDF[[i,1+j]] = (-1) * xiEdgesBiDF[[i,1+j]]

          }

        }

      }

      # Do the Chi² Test for the whole contingency table for both NODES and EDGES
      # print("Xi-Squares")
      pValue  = chisq.test(xiDF[,-1])$p.value
      pValue2 = chisq.test(xiEdgesDF[,-1])$p.value

      myReturn = vector("list", length = 6)
      myReturn[[1]]  = uniquesDF
      myReturn[[2]]  = xiDF
      myReturn[[3]]  = xiFqcDF
      myReturn[[4]]  = xiDiffDF
      myReturn[[5]]  = xiBiDF
      myReturn[[6]]  = pValue
      myReturn[[7]]  = xiEdgesDF
      myReturn[[8]]  = xiEdgesFqcDF
      myReturn[[9]]  = xiEdgesDiffDF
      myReturn[[10]] = xiEdgesBiDF
      myReturn[[11]] = pValue2
      #myReturn[[11]] = xiEdgesConditionalFqcDF

      return (myReturn)

    }

    # Given a Graph and a set of variables that are categorical it gives back summary table
    # with all the homophilies combinations.
    #
    # The result has this structure:
    #
    # Variable        |  Homophily |  Frequency |   Delta |   Sign   |  Diference | P-value | Significance
    # ----------------------------------------------------------
    # Sex                0.83
    # -- Man             0.71         0.54         0.17       +         +0.17       0.003      **
    # -- Woman           0.82         0.46         0.36       +         +0.36       0.221      ns
    # BMI
    # -- Underweight     0.32 ...
    
    completeHomophily <- function(graph, indexList){

      # Init variables
      totalVariables = length(indexList)

      # Get the table
      completeTable  = igraph::as_data_frame(graph, "vertices")
      variablesNames = colnames(completeTable)[indexList]

      # Count how many uniques for each group
      totalRows = 0
      for(i in 1:totalVariables){

        currentIndex = indexList[i]

        totalRows = totalRows + 1 # +1 per variable

        totalRows = totalRows + length(unique(completeTable[,currentIndex])) # + X for each category

      }

      # Create the DF where we are going to put the results
      homoDF               =  DF(totalRows,8)
      colnames(homoDF)     = c("Variable", "Homophily", "Frequency", "Delta", "Sign", "Difference", "P-value", "Significance")

      # Fill the table
      tableIndex = 1
      for(i in 1:totalVariables){

        currentIndex      = indexList[i]
        variableName      = variablesNames[i]
        variableHomophily = homophily(graph, variableName)
        variableUniques   = summarizeCategorical(completeTable, currentIndex, sorted = "none", crop = 0)[,1]
        totalUniques      = length(variableUniques)

        homoDF[tableIndex,1] = variableName
        homoDF[tableIndex,2] = variableHomophily

        tableIndex = tableIndex + 1

        for(j in 1:totalUniques){

          currentModality         = variableUniques[j]
          variableHomophily       = homophily(graph, variableName, currentModality)
          totalModality           = sum(completeTable[,currentIndex] == currentModality, na.rm = TRUE)
          proportionModality      = totalModality / nrow(completeTable)
          totalSameToSameModality = round(variableHomophily * totalModality)
          currentSign             = "+"

          homoDF[tableIndex,1] = paste("--", currentModality, sep="")
          homoDF[tableIndex,2] = variableHomophily
          homoDF[tableIndex,3] = proportionModality
          homoDF[tableIndex,4] = abs(variableHomophily - (proportionModality))
          homoDF[tableIndex,6] = variableHomophily - (proportionModality)
          
          if(homoDF[tableIndex,6] <0) currentSign = "-"
          
          homoDF[tableIndex,5] = currentSign
          
          
          # The binomial test goes as:
          #
          # -- How many women are friends with another women? The variable homophily (ie 0.75 , 75%)
          # -- How many women do we have? The total Modality variable (ie: 500)
          # -- How many women are friends with another women, but in integer form? 0.75 * 500 = 375 round to the nearest integer
          # -- What is the probability of being friend with another women just by random chance? The proportion modality (ie 0.5, 50%)
          #
          #    And we do the two sided as we don't care if it is overrepresented or underrepresented, either case is interesting.          
          homoDF[tableIndex,7] = binom.test( totalSameToSameModality,  totalModality  , p = proportionModality, alternative = "two.sided")$p.value

          
          homoDF[tableIndex,8] = getAsterkisPValue(homoDF[tableIndex,7])
          
          
          
          tableIndex = tableIndex + 1

        }

      }
      
      
      return(homoDF)

    }

    # Given a Graph and a set of variables that are categorical it gives back summary table
    # with only the main variables, no the sub modalities
    partialHomophily <- function(graph, indexList){

      # Init variables
      totalVariables = length(indexList)

      # Get the table
      completeTable  = igraph::as_data_frame(graph, "vertices")
      variablesNames = colnames(completeTable)[indexList]

      # Create the DF where we are going to put the results
      homoDF               =  data.frame(matrix(NA, nrow = totalVariables, ncol = 2))
      colnames(homoDF)     = c("Variable", "Homophily")

      # Fill the table
      for(i in 1:totalVariables){

        currentIndex      = indexList[i]
        variableName      = variablesNames[i]
        variableHomophily = homophily(graph, variableName)

        homoDF[i,1] = variableName
        homoDF[i,2] = variableHomophily

      }

      return(homoDF)

    }

    # Reachability plot
    # A bunch of steps to see how is the coverage of your network after that many steps
    doReachabilityPlot <- function(edgesDF, nodesDF, plotFilePath, directedPlot = FALSE, totalSteps = 15,
                                   plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                   plotTheme = NULL,
                                   overrideTableName = NULL,
                                   overrideCaption = NULL,
                                   ymin = NULL, ymax = NULL){


      # Define plot type
      myPlotType = "RechabilityBoxplot"
      myTableName = deparse(substitute(edgesDF))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }

      # Get the theme information
      themeData = getThemeParameters(plotTheme)
      
      # Create the graph objects and check for distances
      myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
      distanceMap  = distances(myGraph)

      # Get how many nodes we have
      totalNodes   = nrow(nodesDF)

      # We need to create a table like this to make the boxplots:
      # -----------------------------------
      # Node ID \ Steps \ Coverage \
      #    A        1       10%
      #    A        2       15%
      #    B        1        7%
      #    C        3       25%
      #   ...      ...       ...
      #    Z        x        y

      # The easies way to make this, is to make a NumberOfNodes X Steps matrix, and then melt it
      coverageMatrix = data.frame(matrix(0, nrow = totalNodes, ncol = totalSteps))
      colnames(coverageMatrix) = c(1:totalSteps)
      coverageMatrix$ID = nodesDF$ID

      # Now we check all the combinations in the distance map
      for(i in 1:totalNodes){

        for(j in 1:totalSteps){

          # Get the row (or the column, since is undirected it doesn't matter) TODO: Make it directed
          currentRow = distanceMap[i,]

          # Check how many are in the current reach
          totalReached = sum(currentRow <= j)

          # Add that info to the matrix
          coverageMatrix[i,j] = totalReached/totalNodes

        }

      }

      # In here we have the coverage matrix finished, so we melted it
      meltedCoverage = melt(coverageMatrix, id.vars = "ID")

      # Prepare the color vector which is going to be plain grey for all the steps
      boringColorVector = rep("grey", totalSteps)

      # Get an automatic name if you don't have a proper one
      {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = meltedCoverage,
                                               tableName = myTableName, fileType = myPlotType)
        
      }
      
      # We don't care about the ID anymore, so we can throw that away (we care so little that we don't care about deleting it even)

      # Now we do the plot
      # 2 = Number of steps
      # 3 = Coverage
      # ---- Boxplot
      plotResults = doCategoricalBoxPlot (meltedCoverage, 2, 3, imageFilePath,
                                          outlierShape = NA,
                                          colorsVector = boringColorVector, showPValues = FALSE,
                                          ymin = ymin, ymax = ymax,
                                          plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                          plotCaption = plotCaption, plotXLabel = plotXLabel, plotYLabel = plotYLabel,
                                          plotTheme = plotTheme,
                                          overrideTableName = myTableName,
                                          overrideCaption = overrideCaption) 
      
      # Save the image
      # imageWidth = totalSteps 
      # ggsave(, plot = myBoxPlot, width = imageWidth)
      # latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
      
      # Return
      myReturn = vector("list", length = 5)
      myReturn[[1]] = plotResults[[1]]
      myReturn[[2]] = plotResults[[2]]
      myReturn[[3]] = plotResults[[3]]
      myReturn[[4]] = plotResults[[4]]
      myReturn[[5]] = plotResults[[5]]
      
      return (myReturn)

    }

    
    # Simulation plot
    # A bunch of steps to see how a disease advance in your network after that many steps
    doSimulationPlot <- function(edgesDF, nodesDF, plotFilePath, directedPlot = FALSE, totalSteps = 10, totalSimulations = 5,
                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                 plotTheme = NULL,
                                 overrideTableName = NULL,
                                 overrideCaption = NULL,
                                 ymin = NULL, ymax = NULL){
      
      
      # Define plot type
      myPlotType  = "SimulationLinePlot"
      myTableName = deparse(substitute(edgesDF))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }
      
      # Get the theme information
      themeData = getThemeParameters(plotTheme)
      
      # Get an automatic name if you don't have a proper one
      {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = edgesDF,
                                               tableName = myTableName, fileType = myPlotType)
        
      }
      
      print(imageFilePath)
      print("Simulating, please wait...")
      
      # Create the graph objects and check for distances
      myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
      distanceMap  = distances(myGraph)
      
      # Get how many nodes we have
      totalNodes   = nrow(nodesDF)
      
      # Initialize the conditions matrices
      # -- Probability to infect others and be infected yourself by others.
      #    (right now a very simple homogeneous model)
      
      # -- Probability to get a recurrent virus after cured
      #    (herpes type, very simple, now is none)
      #
      # -- Probability of getting immunity
      #    (if somebody try to infect you and fails, do you become immune forever, now is NO)
      #
      # -- Probability of dying
      #    (in each given step, how likely is that you are remove from the infected set, no is ZERO)
      #
      # TODO: There is more, see slides
      
      # CONTAGIOUS MATRIX
      # ----  If you are infected, what is the probability of giving the disease to another
      #       (leave it constant for simplicity, but you can model people giving away disease
      #        to other being more easy/difficult, ie: they live together and person A doesn't
      #        wash his hands ever, agh!)
      giveDeseaseMatrix           = data.frame(matrix(0.3, nrow = totalNodes, ncol = totalNodes))
      colnames(giveDeseaseMatrix) = c(1:totalNodes)
      giveDeseaseMatrix $ID       = nodesDF$ID
      
      # INFECTABILITY MATRIX
      # ---- If you are not infected, what is the probability of receiving the disease if you are expose
      #      (how good your immune system is with respect the person that gives you the disease,
      #       if the person doesn't matter, leave the rows constant)
      receiveDeseaseMatrix           = data.frame(matrix(0.9, nrow = totalNodes, ncol = totalNodes))
      colnames(receiveDeseaseMatrix) = c(1:totalNodes)
      receiveDeseaseMatrix$ID        = nodesDF$ID
      
      # DYING VECTOR
      # ---- If you are infected, what is the probability of dying at any given time
      #      (you can change this to follow a distribution, so you have more chances the more the disease advance for example)
      #      (this is a vector, you don't have more chances of dying depending of how many people you are friend, you
      #       have more probabity of getting infected, but dying is an independent event)
      dyingProbabilityVector = rep(0, totalNodes)
      
      
      # We need to create a table like this to make the lineplot:
      # -----------------------------------
      # Start X  | Start Y \ End X | End Y
      #  1.3        5.3       5.2     6.7
      #          ................
      # (There is going to be TotalSteps x Total Simulation lines)
      evolutionDF                = data.frame(matrix(0, nrow = totalSteps * totalSimulations  , ncol = 4))
      colnames(evolutionDF)      = c("StartX","StartY","EndX","EndY")
      currentEvolutionRow        = 1
      
      # Pick up your starting nodes
      startingNodes = sample(x = nodesDF$ID,size = totalSimulations, replace = FALSE)
      
      # ------------------------------------------------------
      # FOR EACH SIMULATION (each one of the starting points)
      # ------------------------------------------------------
      for(i in 1:totalSimulations){
        
        print("Simulation Started:")
        print(i)
        
        # Initialize everything
        {
          # Get how many nodes we have, this might vary during the simulation if people die
          totalNodes        = nrow(nodesDF)
          currentTotalNodes = totalNodes
          
          # Initialize your set of infected to the first person
          infectedSet = c(startingNodes[i])

          # The dead people come here
          deadPeopleSet = c()
        }
        
        # ------------------------------------------------------
        # For each step in the simulation
        # ------------------------------------------------------
        for(j in 1:totalSteps){
          
          print( paste("        ", round(j/totalSteps,2), sep="") )

          # Save the original infected for later
          lastStepInfected = infectedSet

          # Count how many infected we have
          totalInfected    = length(infectedSet)

          # Check how many people get infected in this step
          # For each infected person (if there are any left)
          if(totalInfected > 0){
            
            # People that have been infected during this step
            newInfected = c()
            
            # For each infected person that we have
            for(k in 1:totalInfected){
              
              # Who is giving the disease
              currentIllPersonID     = infectedSet[k]

              # Check his neighbors
              currentNeighbours = unique(neighbors(myGraph, currentIllPersonID, mode = "all"))
              totalNeighbours   = length(currentNeighbours)

              # Check in the matrix if the person get infected or not based on the die roll
              if(totalNeighbours > 0){
                
                # Make the disease roll
                currentGivingDiseaseRoll = runif(totalNeighbours, 0, 1)
                currentSavingDiseaseRoll = runif(totalNeighbours, 0, 1)
                
                for(l in 1:totalNeighbours){
                  
                  #The person candidate to receive the disease
                  currentNeighbourID = currentNeighbours[l]

                  if( TRUE ){
                    
                    # Get the probabilities for giving and receiving
                    probOfGiving       = giveDeseaseMatrix[   currentIllPersonID, currentNeighbourID]    
                    probOfReceiving    = receiveDeseaseMatrix[currentIllPersonID, currentNeighbourID] 

                    # First if you manage to give it away
                    if(currentGivingDiseaseRoll[l] < probOfGiving){
                      # Second, if the person defended himself
                      # print("...It gave him the disease")
                      if(currentSavingDiseaseRoll[l] < probOfReceiving){
                        # At this point, the neighbor is mark as infected
                        newInfected = union(newInfected, currentNeighbourID)
                        
                      }  
                      else{
                        # print("...but the inmune system counter it")
                      }
                    }
                    else{
                      # print("...but the disease didn't transmit")
                    }
                    
                  }
                  
                }
              }
            } 
            
            # Get how many new people are infected
            totalCurrentInfected = length(newInfected)

            # Add then to the list of infected
            if(totalCurrentInfected > 0) infectedSet = union( infectedSet, newInfected)
            
            totalNewInfected = length(infectedSet)
            
            
          }
          
          # Now we have all the information we need to add a line to the dataframe
          {
            startY = totalInfected/totalNodes    # (old number)
            endY   = totalNewInfected/totalNodes # (new number)
            
            startX = j - 1
            endX   = j
            
            evolutionDF$StartX[currentEvolutionRow] = startX
            evolutionDF$StartY[currentEvolutionRow] = startY
            evolutionDF$EndX[currentEvolutionRow]   = endX
            evolutionDF$EndY[currentEvolutionRow]   = endY
            
            currentEvolutionRow = currentEvolutionRow + 1  
          }
          
          # Now remove the people that die (or go into isolation, or whatever)
          {
            # -- The newly infected people will not die until the next step, so we only eliminate the old ones

            deathRoll           = runif(totalNewInfected, 0, 1)
            deathCandidates     = (nodesDF$ID %in% infectedSet)
            deathCandidates     = dyingProbabilityVector[deathCandidates]
            deathResults        = deathRoll < deathCandidates
            currentDeadPeople   = lastStepInfected[deathResults]
            totalNewDeadPeople  = length(currentDeadPeople)
            
            # -- Add the reaped ones to the death set
            deadPeopleSet   = union(deadPeopleSet, currentDeadPeople)
            totalDeadPeople = length(deadPeopleSet)
            
            # -- Delete those from the infected set
            infectedSet   = setdiff(infectedSet, currentDeadPeople)        
            
          }
          
          # Print some info for debugging
          # print("Step")
          # print(j)
          # print("Accumulated Infected: ")
          # print(totalInfected)
          # print("New:")
          # print(totalCurrentInfected)
          # print("Died:")
          # print(totalNewDeadPeople)
          # print("Accumulated deaths:")
          # print(totalDeadPeople)
          
          
        }
        
        print("Simulation Ended:")
        print(i)
        print(round(i/totalSimulations,2))
        
      }

      # ------------------------------------------------------
      # DO THE PLOT
      # ------------------------------------------------------
      
      # Do the plot with all the lines
      simulationPlot = ggplot(evolutionDF) +
        
        # Draw each line
        geom_segment(aes(x = StartX, y = StartY, xend = EndX, yend = EndY), colour = "red", alpha = 0.1) +
        
        # Draw ALL the steps in the X axys
        scale_x_continuous(breaks=c(0:totalSteps)) +
        
        # Scale the y axis to whatever
        scale_y_continuous(limits=c(ymin, ymax)) +
        
        # Create titles and subtitles
        labs(title    = plotTitle,
             subtitle = plotSubtitle,
             caption  = plotCaption,
             x = plotXLabel, y = plotYLabel) +
        
        # Apply the theme
        theme(panel.background   = themeData[[1]],
              axis.line          = themeData[[2]],
              panel.grid.major.y = themeData[[3]],
              panel.grid.major.x = themeData[[4]])
      
      # Save the image
      imageWidth = totalSteps/2 
      ggsave(imageFilePath, plot = simulationPlot, width = imageWidth)
      latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
      
      # Return
      myReturn = vector("list", length = 3)
      myReturn[[1]] = simulationPlot
      myReturn[[2]] = imageFilePath
      myReturn[[3]] = latexFilePath
      
      return (myReturn)
      
    }
    

    # Simulation plot
    # THIS IS IMPOSSIBLE!!!! to run good on R, need to use Rcpp to make anything good of this
    # A bunch of steps to see how a disease advance in your network after that many steps
    doSimulationPlot2 <- function(edgesDF, nodesDF, plotFilePath, directedPlot = FALSE, totalSteps = 10, totalSimulations = 5,
                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                 plotTheme = NULL,
                                 overrideTableName = NULL,
                                 overrideCaption = NULL,
                                 ymin = NULL, ymax = NULL){

      
      # Define plot type
      myPlotType  = "SimulationLinePlot"
      myTableName = deparse(substitute(edgesDF))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }
  
      # Get the theme information
      themeData = getThemeParameters(plotTheme)

      # Get an automatic name if you don't have a proper one
      {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = edgesDF,
                                               tableName = myTableName, fileType = myPlotType)
        
      }

      print(imageFilePath)
      print("Simulating, please wait...")

      # Create the graph objects and check for distances
      myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
      distanceMap  = distances(myGraph)

      # Get how many nodes we have
      totalNodes   = nrow(nodesDF)

      # Initialize the conditions matrices
      # -- Probability to infect others and be infected yourself by others.
      #    (right now a very simple homogeneous model)
      
      # -- Probability to get a recurrent virus after cured
      #    (herpes type, very simple, now is none)
      #
      # -- Probability of getting immunity
      #    (if somebody try to infect you and fails, do you become immune forever, now is NO)
      #
      # -- Probability of dying
      #    (in each given step, how likely is that you are remove from the infected set, no is ZERO)
      #
      # TODO: There is more, see slides

      # CONTAGIOUS MATRIX
      # ----  If you are infected, what is the probability of giving the disease to another
      #       (leave it constant for simplicity, but you can model people giving away disease
      #        to other being more easy/difficult, ie: they live together and person A doesn't
      #        wash his hands ever, agh!)
      giveDeseaseMatrix           = data.frame(matrix(0.3, nrow = totalNodes, ncol = totalNodes))
      colnames(giveDeseaseMatrix) = c(1:totalNodes)
      giveDeseaseMatrix $ID       = nodesDF$ID

      # INFECTABILITY MATRIX
      # ---- If you are not infected, what is the probability of receiving the disease if you are expose
      #      (how good your immune system is with respect the person that gives you the disease,
      #       if the person doesn't matter, leave the rows constant)
      receiveDeseaseMatrix           = data.frame(matrix(0.9, nrow = totalNodes, ncol = totalNodes))
      colnames(receiveDeseaseMatrix) = c(1:totalNodes)
      receiveDeseaseMatrix$ID        = nodesDF$ID
      
      # DYING VECTOR
      # ---- If you are infected, what is the probability of dying at any given time
      #      (you can change this to follow a distribution, so you have more chances the more the disease advance for example)
      #      (this is a vector, you don't have more chances of dying depending of how many people you are friend, you
      #       have more probabity of getting infected, but dying is an independent event)
      dyingProbabilityVector = rep(0, totalNodes)

      
      # We need to create a table like this to make the lineplot:
      # -----------------------------------
      # Start X  | Start Y \ End X | End Y
      #  1.3        5.3       5.2     6.7
      #          ................
      # (There is going to be TotalSteps x Total Simulation lines)
      evolutionDF                = data.frame(matrix(0, nrow = totalSteps * totalSimulations  , ncol = 4))
      colnames(evolutionDF)      = c("StartX","StartY","EndX","EndY")
      currentEvolutionRow        = 1

      # Pick up your starting nodes
      startingNodes = sample(x = nodesDF$ID,size = totalSimulations, replace = FALSE)

      # ------------------------------------------------------
      # FOR EACH SIMULATION (each one of the starting points)
      # ------------------------------------------------------
      for(i in 1:totalSimulations){

        print("Simulation Started:")
        print(i)

        # Initialize everything
        {
          # Get how many nodes we have, this might vary during the simulation if people die
          totalNodes        = nrow(nodesDF)
          currentTotalNodes = totalNodes

          # Initialize your set of infected to the first person
          #infectedSet = c(startingNodes[i])
          infectedSet2 = rep(FALSE, totalNodes)
          infectedSet2[as.numeric(startingNodes[i])] = TRUE  # Stupid R doesn't want the IDs as numbers but strings :(
                                                             # Like, this whole thing with boolean vector to optimize is silly
                                                             # a real programing language has pointers to optimize all of this :((((

          # The dead people come here
          #deadPeopleSet = c()
          deadPeopleSet2 = rep(FALSE, totalNodes)

          # In here we put the people who are candidate to be infected in whatever current step
          #potentialCurrentStep = c()
        }

        # ------------------------------------------------------
        # For each step in the simulation
        # ------------------------------------------------------
        for(j in 1:totalSteps){

          print( paste("        ", round(j/totalSteps,2), sep="") )
          print("A")
          # Update how many people we have left
          #currentTotalNodes = totalNodes - length(deadPeopleSet)
          #currentTotalNodes = totalNodes - sum(deadPeopleSet2)
          print("B")
          # Save the original infected for later
          #lastStepInfected = infectedSet
          lastStepInfected2 = infectedSet2
          print("C")
          # Count how many infected we have
          #totalInfected    = length(infectedSet)
          totalInfected      = sum(infectedSet2)
          currentInfectedIDs = nodesDF[infectedSet2,]$ID
          print( paste("        ", round(j/totalSteps,2), sep="") )
          print("D")
          # Check how many people get infected in this step
          # For each infected person (if there are any left)
          if(totalInfected > 0){
          print("D2")  
            # People that have been infected during this step
            newInfected = rep(FALSE, totalNodes)
            print("D3")
            # For each infected person that we have
            for(k in 1:totalInfected){
              print("D31")  
              # Who is giving the disease
              #currentIllPersonID     = infectedSet[k]
              currentIllPersonID = currentInfectedIDs[k]
              currentIllPersonID = as.numeric(currentIllPersonID)
              
              # print("The person that is currently Ill is")
              # print(currentIllPersonID)
              print("D32")
              # Check his neighbors
              results = getFrienshipTypes(currentIllPersonID, overallNetworkDF)
              #currentNeighbours = unique(c(results[[4]], results[[5]]))
              currentNeighbours = results[[4]]
              if (length(currentNeighbours) == 1){
                if(currentNeighbours == 0) currentNeighbours = NULL # Another shitty thing from R, I can't return a constant list of NULLs,
                # because it just delete that element of the list and make it shorter!!!                
              }

              #currentNeighbours = unique(neighbors(overallGraph, currentIllPersonID, mode = "all"))
              totalNeighbours   = length(currentNeighbours)

               # print("Me")
               # print(currentIllPersonID)
               # print("I have these neighbours")
               # print(currentNeighbours)
               
              print("D33")
              # Check in the matrix if the person get infected or not based on the die roll
              if(totalNeighbours > 0){

                # Make the disease roll
                currentGivingDiseaseRoll = runif(totalNeighbours, 0, 1)
                currentSavingDiseaseRoll = runif(totalNeighbours, 0, 1)
                
                for(l in 1:totalNeighbours){
                  
                  #The person candidate to receive the disease
                  currentNeighbourID = currentNeighbours[l]
                  
                  currentNeighbourID = as.numeric(currentNeighbourID)
                  
                  # print("My next neighbour ID")
                  # print(currentNeighbourID)
                  
                  # If the person that we are trying to infect:
                  # -- has already been infected in this step
                  # -- is dead
                  # -- is immune
                  # -- is already infected
                  # Then skip it
                  skipThis = ( newInfected[currentNeighbourID]    ||
                               deadPeopleSet2[currentNeighbourID] ||
                               infectedSet2[currentNeighbourID] )
                  
                  # print("-- skip test --")
                  # print(skipThis)
                  # print("----")
                  # print(newInfected[currentNeighbourID])
                  # print(deadPeopleSet2[currentNeighbourID])
                  # print(infectedSet2[currentNeighbourID])
                  # print("----")
                  
                  # print(paste0(currentIllPersonID, " is trying to infect ", currentNeighbourID))
                  # if(infectedSet2[currentNeighbourID]){
                  #   print("...But he is already infected")
                  # }
                  # if(newInfected[currentNeighbourID]){
                  #   print("...But somebody else infected him in this round")
                  # }
                  # if(deadPeopleSet2[currentNeighbourID]){
                  #   print("...But his friend is already dead")
                  # }
                  
                  
                  if( skipThis == FALSE ){
                    
                    # Get the probabilities for giving and receiving
                    probOfGiving       = giveDeseaseMatrix[   currentIllPersonID, currentNeighbourID]    
                    probOfReceiving    = receiveDeseaseMatrix[currentIllPersonID, currentNeighbourID] 
                    
                    # print(currentIllPersonID)
                    # print(currentNeighbourID)
                    # print(probOfGiving)
                    # print(currentGivingDiseaseRoll[l])
                    
                    # First if you manage to give it away
                    if(currentGivingDiseaseRoll[l] < probOfGiving){
                      # Second, if the person defended himself
                      # print("...It gave him the disease")
                      if(currentSavingDiseaseRoll[l] < probOfReceiving){
                        # At this point, the neighbor is mark as infected
                        newInfected[currentNeighbourID] = TRUE
                        # print( paste0("NEW INFECTED!! ", currentNeighbourID )  )
                      }  
                      else{
                        # print("...but the inmune system counter it")
                      }
                    }
                    else{
                      # print("...but the disease didn't transmit")
                    }
                    
                  }

                }
              }
            } 
            print("D4")
            # Get the ID of the newly infected people
            newInfectedID    = nodesDF[newInfected,]$ID
            #totalNewInfected = length(newInfected)
            totalNewInfected = sum(newInfected)
            print("D5")
            # Add then to the list of infected
            #if(totalNewInfected > 0) infectedSet = union( infectedSet, newInfectedID)
            
            # print("Pre")
            # # print(infectedSet2)
            # print(sum(infectedSet2))
            
            if(totalNewInfected > 0) infectedSet2 = ( infectedSet2 | newInfected)
            
            # print("---")
            # print("Post")
            # #print(infectedSet2)
            # print(sum(infectedSet2))
            
          }
          print("E")
          # Now we have all the information we need to add a line to the dataframe
          {
            startY = totalInfected/totalNodes       # (old number)
            #endY   = length(infectedSet)/currentTotalNodes # (new number)
            endY   = sum(infectedSet2)/totalNodes # (new number)
            startX = j
            endX   = j + 1
            
            evolutionDF$StartX[currentEvolutionRow] = startX
            evolutionDF$StartY[currentEvolutionRow] = startY
            evolutionDF$EndX[currentEvolutionRow]   = endX
            evolutionDF$EndY[currentEvolutionRow]   = endY
            
            currentEvolutionRow = currentEvolutionRow + 1  
          }
          print("F")
          # Now remove the people that die (or go into isolation, or whatever)
          {
            # -- The newly infected people will not die until the next step, so we only eliminate the old ones
            #deathRoll           = runif(totalInfected, 0, 1)
            #deathRoll           = deathRoll < dyingProbabilityVector
            #currentDeadPeople   = lastStepInfected[deathRoll]
            
            # -- Add the reaped ones to the death set
            #deadPeopleSet = union(deadPeopleSet, currentDeadPeople)
            #deadPeopleSet = (deadPeopleSet2 |  currentDeadPeople)
            # -- Delete those from the infected set
            #infectedSet   = setdiff(infectedSet, currentDeadPeople)        
            #infectedSet   = (infectedSet2  currentDeadPeople)        
            
            # infected    ,  # dead  , # Z    (A and B)   (A !xor B)
            # t                t         f       t          t
            # t                f         t       f          f
            # f                t         f       f          f
            # f                f         f       f          t
            # t                f         t       f          f
            
            
            deathRoll           = runif(totalNodes, 0, 1)
            deathRoll           = deathRoll < dyingProbabilityVector # People marked for death
            currentDeadPeople   = (lastStepInfected2 & deathRoll) # Only infected people die
            deadPeopleSet2      = (deadPeopleSet2 |  currentDeadPeople) # Update the people that is dead
            # Remove the people from the infected set
            # I don't know how to do this in R with a single vector operation
            # so into a loop we go.
            # If infected AND dead , infected = FALSE
            # otherwise keep the infected value
            for(z in 1:totalNodes){
              
              if(infectedSet2[z] == TRUE && deadPeopleSet2[z] == TRUE) infectedSet2[z] == FALSE
              
            }

          }

          # # Print some info for debugging
          # print("Step")
          # print(j)
          # print("Accumulated Infected: ")
          # print(sum(infectedSet2))
          # print("New:")
          # print(sum(newInfected))
          # print("Died:")
          # print(sum(currentDeadPeople))
          # print("Accumulated deaths:")
          # print(sum(deadPeopleSet2))
          
          
        }


        #print("Final infected:")
        #print(infectedSet)
        print("Simulation Ended:")
        print(i)
        print(round(i/totalSimulations,2))

      }

      #print(evolutionDF)
      
      # ------------------------------------------------------
      # DO THE PLOT
      # ------------------------------------------------------

      # Do the plot with all the lines
      simulationPlot = ggplot(evolutionDF) +

                       # Draw each line
                       geom_segment(aes(x = StartX, y = StartY, xend = EndX, yend = EndY), colour = "red", alpha = 0.1) +

                       # Draw ALL the steps in the X axys
                       scale_x_continuous(breaks=c(1:totalSteps)) +

                       # Scale the y axis to whatever
                       scale_y_continuous(limits=c(ymin, ymax)) +

                       # Create titles and subtitles
                       labs(title    = plotTitle,
                            subtitle = plotSubtitle,
                            caption  = plotCaption,
                            x = plotXLabel, y = plotYLabel) +

                       # Apply the theme
                       theme(panel.background   = themeData[[1]],
                             axis.line          = themeData[[2]],
                             panel.grid.major.y = themeData[[3]],
                             panel.grid.major.x = themeData[[4]])

      # Save the image
      imageWidth = totalSteps 
      ggsave(imageFilePath, plot = simulationPlot, width = imageWidth)
      latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
      
      # Return
      myReturn = vector("list", length = 3)
      myReturn[[1]] = simulationPlot
      myReturn[[2]] = imageFilePath
      myReturn[[3]] = latexFilePath
      
      return (myReturn)

    }


    # Built in lnam
    # This is the Linear Network Autocorrelation Model that comes with the $LIBRARY
    # although I have no idea of what it does internally.
    #
    # You can add the deviance weight matrix too, as this solve the formula
    #
    #    Yn = W1 * Yn-1 + XB + W2 * e + nu
    #
    # The rho doesn't appear anywhere here, but somehow you get it back.
    #
    # Results
    #
    builtInLNAM <- function(dependentVariablesVector, myExplicativeVariablesDF,
                            friendshipMatrix = NULL, devianceMatrix = NULL,
                            epsilon = 0.01){
      
      # Find the variables dimensions
      totalPeople = nrow(myExplicativeVariablesDF)
      print(totalPeople)
      # Define the matrixes for frienship, devience, and explicative variables
      friendshipW = matrix(0, totalPeople,totalPeople)
      devianceW   = matrix(0, totalPeople,totalPeople)
      explicative = as.matrix(myExplicativeVariablesDF)
      
      if(!is.null(friendshipMatrix)) friendshipW = friendshipMatrix
      if(!is.null(devianceMatrix))   devianceW   = devianceMatrix
      
      # Define the initialization of the rhos, sigmas, betas, nu, e and y0
      # r1    = 0.2
      # r2    = 0.1
      # sigma = 0.1
      # beta  = rnorm(ncol(myExplicativeVariablesDF))
      # nu    = rnorm(totalPeople,0,sigma)     
      # e     = qr.solve(diag(totalPeople),nu) 
      
      r1    = 0.2
      r2    = 0.1
      sigma = 0.1
      beta  = rnorm(ncol(myExplicativeVariablesDF))
      nu    = rnorm(totalPeople,0,sigma)     
      e     = qr.solve(diag(totalPeople),nu) 
      
      # Why is the example giving me Y from W1 and BX????
      # This is true for the first iteration, but this overwrite the y in the code,
      # which makes the variables independent ??????? WHAT????
      #y     = qr.solve(diag(totalPeople) -r1 * friendshipW , explicative %*% beta + e)
      y = dependentVariablesVector
      
      # I need this for now to see how long this takes
      print("Doing the Linear Autocorrelation Model")
      print("I need to do a bunch of matrix multiplication,")
      print(paste0("of size ", totalPeople, ", this is going to take some time"))
      print("I'm not stuck, I'm just slow, please wait...")
      print(paste0("Started at: ", Sys.time()))
      
      
      #Now, fit the autocorrelation model:
      fit = 0
      # -- If we provide W1 and W2
      if(  !is.null(friendshipMatrix)  &&  !is.null(devianceMatrix))
        fit = lnam(y, x = explicative , W1 = friendshipW, W2 = devianceW, tol = epsilon)
      
      # -- If we provide W1
      if(  !is.null(friendshipMatrix)  &&  is.null(devianceMatrix))
        fit = lnam(y,x = explicative ,  W1 = friendshipW, tol = epsilon)
      
      # -- If we provide W2
      if(  is.null(friendshipMatrix)   &&  !is.null(devianceMatrix))
        fit = lnam(y, x = explicative , W2 = devianceW, tol = epsilon)
      
      # -- If we provide nothing, do nothing, and return fit = 0
      
      # I need this for now to see how long this takes
      print(paste0("Finished at: ", Sys.time()))
      
      return(fit)
      
    }
    
    
    # Here comes the function to do the network simulations. In general this is
    # quite straight forward, however we have an special case for a function
    # that we do later.
    #
    # I tried for more than a week to generalize that function into the general
    # case in a simple way, and is simply not possible to make it simple and in
    # a code that is understandable. So for the sake of simplicity, I'm not 
    # making the function generalization, but rather a specific function for a
    # very specific case later on.
    
    # Here come first the general simple cases that are intuitive:
    
    
    # This function does a bunch of simulations and return a vector of size
    # total simulations. Each element of the vector contain how many same to
    # same relationships we have in that simulation.
    #
    # This function doesn't tell you whether the relationships are bias or not
    # it only generate the vector, later on you have to analyize that.
    #
    # There are 3 ways to run the bootstrap vector:
    #
    #     A)
    #
    #        Get the general prevalence. Whatever you give in the categoricalIndex
    #        variable has a prevalence (ie Positive 60% Negative 40%), and that
    #        frequency is apply to everyone in the nodes table.
    #
    #     B)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which has an specific
    #        modality given by overrideFrequencyCategory (ie Sex -> Woman).
    #        This modality has a prevalence likely different from the general
    #        population (ie Positive 60% Negative 40% was the original, but 
    #        women has a Positive 20% and Negative 80%). Then this frequency
    #        is apply to everyone in the table.
    #
    #        The point of this is to compare what would happens if we consider
    #        everybody to have this category. If the result are significant
    #        it means that this modality has a different risk (higher or lower)
    #        than the rest.
    #
    #     C)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which doesn't has an
    #        specific modality given by overrideFrequencyCategory (NULL/NA).
    #
    #        This means that you are going to take each modality (Men, Women)
    #        And apply that prevalence to each element of the node table.
    #        (ie Men Positive 80% Negative 20%, Women Positive 20% and
    #        Negative 80%). Then this frequency is apply to everyone in the table.
    #        accordingly to it modality.
    #
    #
    # tableBase                = which DF do you want
    #
    # tableEdges               = edges with the real relationships
    #                            with "from" , "to" variables, "value" optional
    #
    # categoricalIndex         = For the given DF in tableBase, which column do you
    #                            want to analyze. The column must contain a
    #                            categorical type of variable.
    #                            (ie: SPAType or Carrier Status)
    #
    #                            If you have a numerical variable, you need to
    #                            convert it to categorical first.
    #
    # totalSimulations         = How many simulations do you want to do. As a
    #                            general rule, 1000 simulations is good enough.
    #
    # simulateRelationships    = Whether to use original relationships or simulate
    #                            new random ones. (DEFAULT = FALSE)
    #
    #                            Keeping the same relationships maintain the network
    #                            topology, so you are analyzing if there is a bias
    #                            in the relationships (ie: Do same type of carrier
    #                            are more likely to be friends with each others)
    #
    #                            If you don't keep the relationship, the results
    #                            will have several different meanings depending of
    #                            what you are doing. Consult your local statician
    #                            for more information.
    #                           
    #
    # overrideFrequenciesIndex = Which column are you going to use to build the
    #                            frequency table. The default is the same as
    #                            categoricalIndex, so nothing change by default.
    #
    #                            (ie: Index for Sex, BMI, Smoking, or whatever)
    #
    #                            You might want to do, for example, the analysis
    #                            for smoking, but using the frequency of Sex: MEN
    #                            and Sex: WOMEN, to check whether men or women have
    #                            some sort of bias, higher risk, or whatever.
    #
    # overrideFrequencyCategory = If you want to use the previous variable, I need
    #                             a category to filter by (ie: "Woman", "Yes", "40")
    #                             If you give me an index in the previous variable,
    #                             but this is still NULL (default) I will run
    #                             B2 instead of B1.
    #
    #
    # showProgressBar         =  (String) If not NULL (default), the console will show
    #                            a little progress bar from 0% to 100% telling
    #                            how far we are doing the simulations.
    #
    #                            Beware that the progress bar will clear the console
    #                            of all text.
    #
    #                            If not NULL, you need to add a string here that
    #                            will show up later in console. Recomendation is
    #                            that you give something meaningfull like
    #
    #                            "Doing simulations for school: "
    #
    #                            Is useful to set it to NULL if you use this inside
    #                            another function, of if you don't want to loose the
    #                            text in the console for whatever reason.
    #
    # Return: A vector of size totalSimulations, which how many same to same relationship
    #         where found in each particular simulation
  
    getBootstrapVector <- function(tableBase, tableEdges, categoricalIndex, totalSimulations,
                                   simulateRelationships     = FALSE,
                                   overrideFrequenciesIndex  = NULL,
                                   overrideFrequencyCategory = NULL,
                                   showProgressBar           = NULL){
    
    
        # Prepare the vector with the bootsraps results
        bootstrapVector      = rep(0,totalSimulations)
    
        # Get the basic statistics from the nodes table
        frequencyTable          = summarizeCategorical(tableBase, categoricalIndex)
        totalNodes              = nrow(tableBase)
        
        # Get the basic statistics from the edge table
        totalRelationships      = nrow(tableEdges)
    
        # If you want to override the Frequency table it means that you want
        # to use the categorical index but restricted to only a particular
        # category of another column.
        
        # Cases B and C 
        if(!is.null(overrideFrequenciesIndex)){
      
            # Check that you have a valid category to filter
            #
            # B
            if(!is.null(overrideFrequencyCategory)){
                
                filterTableBase = tableBase[tableBase[,overrideFrequenciesIndex] == overrideFrequencyCategory,]
                frequencyTable  = summarizeCategorical(filterTableBase, categoricalIndex)                    
                
            }
            
            # C There is nothing else to do here, we will fix the frequency
            # tables later as we go.


        }
    
        # Case A
        # This is the default and there is nothing special that need to be done.
        
        # Dataframes we are going to use to run the simulations. In each
        # simulation, we fill the patient table and the relationship table.
        # -- People
        totalPatients           = totalNodes
        patientsTable           = DF(totalPatients, 3)
        colnames(patientsTable) = c("ID", "TargetVariable", "ModalityOverride")
        # -- Relationships
        frienshipDF             = DF(totalRelationships, 4)
        colnames(frienshipDF)   = c("from","to","value","SameRelationship")
    
    
        # For each simulation:
        #
        # ---- Generate the random patient table with their random attributes which 
        #      follows the given frequency rules.
        #
        # ---- Generate the friendship table (or not depending of your function call)
        #
        # ---- Count how many patients that are friends share the same attribute
        
        for (j in 1:totalSimulations) {

            # Print feedback of simulation progress to the user
            if(!is.null(showProgressBar)){

                cat("\014")
                print(showProgressBar)
                print("")
                print(getProgressCharacters((100*j)/totalSimulations))
                                
            }
            
            # Init the patients
            # -- Init the IDs
            patientsTable[,1] = tableBase[,1] 
            # -- Init the random categorical variable by sampling from the pool of
            #    frequencies that you choose in the frequency table
            #    This depends on whether you are in case A, B or C
            #
            #    A and B, we use the frequency table that we already found out
            #    These are the simple case, and the case in which we use the
            #    conditional probability.
            #
            #    C is when we use a frequency table for each modality independently
            #    so we need to adapt to it as we go.
            
            
            # Case A (there is nothing else to do, this case is finish).
            if(is.null(overrideFrequenciesIndex)){
                
                patientsTable[,2] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)    
                
            }
            # Case B and C
            else{
                
                # B (also, there is nothing else to do, this is also a simple case)
                if(!is.null(overrideFrequencyCategory)){
                        
                    patientsTable[,2] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)    
                        
                }
                # C (this is a bit more complicated)
                else{
                    
                    # Give each patient it original modality for the override
                    # (ie, same sex, same categorical BMI, whatever)
                    patientsTable[,3] = tableBase[,overrideFrequenciesIndex] 
                
                    # Find out how many modalities are in the override index
                    myModalities    = getModalities(tableBase, overrideFrequenciesIndex)
                    totalModalities = length(myModalities)
                
                    # Here we create a matrix of samples.
                    # There is one row for each modality of the target category
                    sampleMatrix    = newList(totalModalities)
                    for(i in 1:totalModalities){
                        
                        # Init the row
                        sampleMatrix[[i]] = rep(NA, totalPatients)
                            
                        # Get the modality we are studying
                        currentModality = myModalities[i]
                            
                        specialFilterTable    = tableBase[tableBase[,overrideFrequenciesIndex] == currentModality,]
                        specialFrequencyTable = summarizeCategorical(specialFilterTable, categoricalIndex)                    
                                    
                        sampleMatrix[[i]] = sample(specialFrequencyTable$Modality, size = totalPatients, replace = TRUE, prob = specialFrequencyTable$Relative)     
                             
                    }
                
                    # For each patient, check the modality and apply the
                    # proper sample.
                    for(i in 1:totalPatients){
                            
                        # Get the patient modality
                        currentPatientModality = patientsTable[i,3]
                        
                        # Find that modality in the sample matrix
                        currentModalityIndex   = grep(TRUE, (currentPatientModality == myModalities))
                            
                        # Assign the sample value from the sample matrix
                        patientsTable[i,2] = sampleMatrix[[currentModalityIndex]][i]
                            
                    }                    

                }
                
            }

            # We have now simulated each status of each patient.
            # Now we need to figure it out how many same-to-same relationships
            # are in the simulation.

            # For each relationship
            # -- Simulate a new relationship if needed
            # -- Check if the have the same category.
            for (i in 1:totalRelationships) {
        
                # Get the real relationship From and To values
                # Right here, despise variable name, they are not random yet
                randomFrom = tableEdges[i,1]
                randomTo   = tableEdges[i,2]
        
                # If you need the simulation of relationship, simulate random from and to
                if(simulateRelationships == TRUE){
          
                    # Take a random from and to, that are not the same, and are not already in the list
          
                    # -- Pick the first one
                    candidatesList = patientsTable$ID
                    randomFrom     = sample(candidatesList, 1)
          
                    # -- Take away the first one from the candidates list as you are not suppose to have a relationship with yourself
                    candidatesList = candidatesList[candidatesList != randomFrom]
          
                    # -- Pick the second one
                    # ---- You already have a list of relationships that start with FROM
                    # ---- This list could be empty though
                    # ---- In any case, grab the TOs from that list
                    # ---- Those are the forbidden numbers that you need to take away from the candidate list
                    forbiddenTos = frienshipDF[frienshipDF$from == randomFrom,2]
          
                    # ---- Update the candidate list if there is one or more forbiddens
                    if (length(forbiddenTos) > 0){
            
                        candidatesList = candidatesList[!(candidatesList %in% forbiddenTos)]
            
                    }
          
                    # ---- Finally pick the second one
                    randomTo = sample(candidatesList, 1)
          
                }
        
                # Set this particular relationship into our friendship DF
                frienshipDF$from[i]  = randomFrom
                frienshipDF$to[i]    = randomTo
                frienshipDF$value[i] = 1
        
                # Check if they share same target variable
                targetTypeFrom = patientsTable[patientsTable[,1] == randomFrom,2]
                targetTypeTo   = patientsTable[patientsTable[,1] == randomTo,  2]
            
                # Sometimes you get a random number that doesn't work, is very weird but the bug is there, I'm trying to catch it with this
                # (but It haven't show in sometime so I think is fixed)
                if(is.na((targetTypeFrom == targetTypeTo))){
          
                    print("ALERT!")
                    print(targetTypeFrom)
                    print(targetTypeTo)
                    print(randomFrom)
                    print(randomTo)
                    print(i)
          
          
                }
        
                # Label if both targets have the same category
                frienshipDF$SameRelationship[i] = (targetTypeFrom == targetTypeTo)
        
            }
      
            # Add how many have the same relationship to the result of this simulation
            bootstrapVector[j] = sum(frienshipDF$SameRelationship)
      
        }
    
        return(bootstrapVector)
    
  }  
    
    
    # Special bootstrap for the contraceptives case
    getContraceptiveVector <- function(tableBase, tableEdges, categoricalIndex, totalSimulations,
                                       simulateRelationships     = FALSE,
                                       mySexIndex                = NULL,
                                       myHormonalIndex           = NULL,
                                       showProgressBar           = NULL){
    
    
        # Prepare the vector with the bootsraps results
        bootstrapVector      = rep(0,totalSimulations)
    
        # Get the basic statistics from the nodes table
        frequencyTable          = NA
        totalNodes              = nrow(tableBase)
        
        # Get the basic statistics from the edge table
        totalRelationships      = nrow(tableEdges)

        # Dataframes we are going to use to run the simulations. In each
        # simulation, we fill the patient table and the relationship table.
        # -- People
        totalPatients           = totalNodes
        patientsTable           = DF(totalPatients, 4)
        colnames(patientsTable) = c("ID", "TargetVariable", "Sex", "Contraceptive")
        # -- Relationships
        frienshipDF             = DF(totalRelationships, 4)
        colnames(frienshipDF)   = c("from","to","value","SameRelationship")
    
        # For each simulation:
        #
        # ---- Generate the random patient table with their random attributes which 
        #      follows the given frequency rules.
        #
        # ---- Generate the friendship table (or not depending of your function call)
        #
        # ---- Count how many patients that are friends share the same attribute
        
        for (j in 1:totalSimulations) {

            # Print feedback of simulation progress to the user
            if(!is.null(showProgressBar)){

                cat("\014")
                print(showProgressBar)
                print("")
                print(getProgressCharacters((100*j)/totalSimulations))
                                
            }
            
            # Init the patients
            # -- Init the IDs
            patientsTable[,1] = tableBase[,1] 
        
            # Give each patient it original sex and contraceptives
            patientsTable[,3] = tableBase[,mySexIndex] 
            patientsTable[,4] = tableBase[,myHormonalIndex] 
                
            # Find out how many modalities are in the hormonal index
            myModalities    = getModalities(tableBase, myHormonalIndex)
            totalModalities = length(myModalities)
                
            # Here we create a matrix of samples for women
            sampleMatrix    = newList(totalModalities)
            for(i in 1:totalModalities){
                        
                # Init the row
                sampleMatrix[[i]] = rep(NA, totalPatients)
                            
                # Get the modality we are studying
                currentModality = myModalities[i]
                            
                womenOnlyTableBase    = tableBase[tableBase[,mySexIndex]                        == "Woman",]
                specialFilterTable    = womenOnlyTableBase[womenOnlyTableBase[,myHormonalIndex] == currentModality,]
                specialFrequencyTable = summarizeCategorical(specialFilterTable, categoricalIndex)                    
                                    
                sampleMatrix[[i]] = sample(specialFrequencyTable$Modality, size = totalPatients, replace = TRUE, prob = specialFrequencyTable$Relative)
                             
            }
            
            # Here we create the sample vector for men
            # Men only have one variable of contraceptives which is Non-Hormonal
            {
            
                menSampleVector       = rep(NA, totalPatients)    
                menOnlyTableBase      = tableBase[tableBase[,mySexIndex] == "Man",]
                specialFrequencyTable = summarizeCategorical(menOnlyTableBase, categoricalIndex)                    
                                    
                menSampleVector       = sample(specialFrequencyTable$Modality, size = totalPatients, replace = TRUE, prob = specialFrequencyTable$Relative)    
                
            }
            
            # For each patient, check the sex and contraceptives and apply the
            # proper sample.
            for(i in 1:totalPatients){
                            
                # Get the patient modality
                currentPatientSex = patientsTable[i,3]
                
                # If it is a Man, simply apply the man frequencies
                if(currentPatientSex == "Man"){
                    patientsTable[i,2] = menSampleVector[i]
                }
                # If it is a Woman, check the contraceptive type
                else{
                    
                    # Get the contraceptive
                    currentPatientContraceptive = patientsTable[i,4]

                    # Find that modality in the sample matrix
                    currentModalityIndex = grep(TRUE, (currentPatientContraceptive == myModalities))

                    # Assign the sample value from the sample matrix
                    patientsTable[i,2] = sampleMatrix[[currentModalityIndex]][i]                    
                                        
                }
                            
            }                    

            # We have now simulated each status of each patient.
            # Now we need to figure it out how many same-to-same relationships
            # are in the simulation.

            # For each relationship
            # -- Simulate a new relationship if needed
            # -- Check if the have the same category.
            for (i in 1:totalRelationships) {
        
                # Get the real relationship From and To values
                # Right here, despise variable name, they are not random yet
                randomFrom = tableEdges[i,1]
                randomTo   = tableEdges[i,2]
        
                # If you need the simulation of relationship, simulate random from and to
                if(simulateRelationships == TRUE){
          
                    # Take a random from and to, that are not the same, and are not already in the list
          
                    # -- Pick the first one
                    candidatesList = patientsTable$ID
                    randomFrom     = sample(candidatesList, 1)
          
                    # -- Take away the first one from the candidates list as you are not suppose to have a relationship with yourself
                    candidatesList = candidatesList[candidatesList != randomFrom]
          
                    # -- Pick the second one
                    # ---- You already have a list of relationships that start with FROM
                    # ---- This list could be empty though
                    # ---- In any case, grab the TOs from that list
                    # ---- Those are the forbidden numbers that you need to take away from the candidate list
                    forbiddenTos = frienshipDF[frienshipDF$from == randomFrom,2]
          
                    # ---- Update the candidate list if there is one or more forbiddens
                    if (length(forbiddenTos) > 0){
            
                        candidatesList = candidatesList[!(candidatesList %in% forbiddenTos)]
            
                    }
          
                    # ---- Finally pick the second one
                    randomTo = sample(candidatesList, 1)
          
                }
        
                # Set this particular relationship into our friendship DF
                frienshipDF$from[i]  = randomFrom
                frienshipDF$to[i]    = randomTo
                frienshipDF$value[i] = 1
        
                # Check if they share same target variable
                targetTypeFrom = patientsTable[patientsTable[,1] == randomFrom,2]
                targetTypeTo   = patientsTable[patientsTable[,1] == randomTo,  2]
            
                # Sometimes you get a random number that doesn't work, is very weird but the bug is there, I'm trying to catch it with this
                # (but It haven't show in sometime so I think is fixed)
                if(is.na((targetTypeFrom == targetTypeTo))){
          
                    print("ALERT!")
                    print(targetTypeFrom)
                    print(targetTypeTo)
                    print(randomFrom)
                    print(randomTo)
                    print(i)
          
          
                }
        
                # Label if both targets have the same category
                frienshipDF$SameRelationship[i] = (targetTypeFrom == targetTypeTo)
        
            }
      
            # Add how many have the same relationship to the result of this simulation
            bootstrapVector[j] = sum(frienshipDF$SameRelationship)
      
        }
    
        return(bootstrapVector)
    
  }  
    
    
    
    # This function tells you if your relationships are bias or not
    # Only for categorical variables. It can run the version A, B and C,
    # of the Bootstrap vector.
    #
    # In order to select each version, fill the input variables accordingly.
    #
    # nodesTable    = Dataframe with the information about your nodes
    #                 It can have any structure you want, the only restriction is
    #                 the first column must be the ID column, and it must be
    #                 a numerical ID
    #
    #
    # listOfEdgesDF = List of Dataframes. Each dataframe have a different network
    #                 The dataframe structure goes like this:
    #
    #                 from     to     value 
    #                    1      2         3
    #                    2      3         5
    #                          ...
    #
    #                The "value" column is irrelevant for this analysis, but
    #                is the standard way to save the edges in a network.
    #
    # listOfNetworkNames = List of Strings with the name of each network. The
    #                      default value is NULL and it will be named from 1 to 
    #                      X. Otherwise, if you want proper names, give them to
    #                      the function in this paremeter.
    #
    #
    # listOfConsequenceIndexes = List of Indexes with the variables that you want
    #                            to study. The variables must be categorical, and
    #                            the indexes must be a valid index contain within
    #                            the nodeTable.
    #
    # totalSimulations         = How many simulations you want to run
    #                            There is no default, 10 is good for testing
    #                            And 1000 is good for getting results.
    # 
    # Return:
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Network"               Name of the Network
    #     "Total Relationships",  How many relationships we have
    #     "Equal Relationships",  How many same to same relationships we have
    #  
    #     "MIN",                  Minimum same to same relationships count that we found in the simulation
    #     "Q1",                   Percentile 25 of same to same relationships count that we found in the simulation
    #     "Median",               Median same to same relationships count that we found in the simulation
    #     "Average",              Average same to same relationships count that we found in the simulation
    #     "Q3",                   Percentile 75 same to same relationships count that we found in the simulation
    #     "MAX",                  Maximum same to same relationships count that we found in the simulation
    #     "SD",                   Standard Deviation same to same relationships count that we found in the simulation
    #
    #     "ConsequenceIndex"      The actual name of this column change depending of the name of each consequence index.
    #                             In here we have the actual p-value for each network, on whether your relationship
    #                             is bias or not.
    # 
    doCategoricalBiasAnalysis <- function(nodesTable, listOfEdgesDF,
                                          listOfConsequenceIndexes,
                                          totalSimulations,
                                          overrideFrequenciesIndex  = NULL,
                                          overrideFrequencyCategory = NULL,
                                          listOfNetworksNames = NULL){

        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
        }
        
        # Prepare the blank base DF where we write the results.
        # We will have one of this for each consequence index.
        biasSimulationsDF           =  data.frame(matrix(NA, nrow = totalNetworks, ncol = 10 + 1 ))
        colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Average", "Q3", "MAX", "SD",  "ConsequenceIndex"  )
        for(i in 1:totalNetworks){
            biasSimulationsDF[i,1]  = myNetworkNames[i]
        }
        
        # We have a list of result for each of the consequence index
        # So in here we prepare such list, and give a blank DF to each
        biasResultsList =  newList(totalConsequenceIndexes)
        for( i in 1:totalConsequenceIndexes){
      
            # Init the DF to empty
            biasResultsList[[i]] = biasSimulationsDF

            # Change the name of the variable we are interested in for this DF
            colnames(biasResultsList[[i]])[11] = consequenceNames[i]

        }

        # For each of the consequence index, we do these 1000-ish simulation for
        # each of the networks that you have.
        for (i in 1:totalConsequenceIndexes){
      
            # Get the DF where we save the results for this variable
      
            # R is stupid, why can't I pass a reference? why do I need to use an index
            # here when then an alias to the variable would make everything more
            # readable and efficient??? >:[
            # currentDF = biasResultsList[[i]]
      
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[i]
                currentName  = consequenceNames[i]        
            }

            # For each of the network, do the proper bias analysis
            # For each network ( each network is represented by an edges DF)
            for(j in 1:totalNetworks){
                    
                    # Get the current edges
                    currentEdges = listOfEdgesDF[[j]]
                    
                    # Find out the if "from" - "to" have the same relationship
                    currentEdges$SameRelationship = addEdgeRelationship(currentEdges,  nodesTable, currentIndex)
                    
                    # Find out how many relationships we have
                    currentTotalRelationships          = nrow(currentEdges)
                    
                    # Find out how many same to same relationships we have
                    # This is the real value that we use in the p-value calculation
                    currentTotalSameRelationships      = sum(currentEdges$SameRelationship == TRUE)
                 
                    # Check if carrier have bias friendship towards people with the same carrier status
                    #
                    # -- Prepare the custom message 
                    currentWaitingMessage = paste0( "Doing simulations for ", myNetworkNames[j], " please wait..." )

                    # -- Do the bias analysis
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = overrideFrequenciesIndex,
                                                           overrideFrequencyCategory = overrideFrequencyCategory,
                                                           showProgressBar           = currentWaitingMessage)
                    
                    
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write all this info in the appropriate part of the results
                    biasResultsList[[i]][j,  2 ] = currentTotalRelationships
                    biasResultsList[[i]][j,  3 ] = currentTotalSameRelationships
                    biasResultsList[[i]][j,  4 ] = min(currentBiasResult)
                    biasResultsList[[i]][j,  5 ] = as.integer(summary(currentBiasResult)[2])
                    biasResultsList[[i]][j,  6 ] = median(currentBiasResult)
                    biasResultsList[[i]][j,  7 ] = currentAverage
                    biasResultsList[[i]][j,  8 ] = as.integer(summary(currentBiasResult)[5])
                    biasResultsList[[i]][j,  9 ] = max(currentBiasResult)
                    biasResultsList[[i]][j, 10 ] = currentSD
                    biasResultsList[[i]][j, 11 ] = currentPValue
                       
                }

            
        }
        
                
        # Everything is finish, give back the list of biases and close.
        return(biasResultsList)
        
        
    }
    
    
    # This function tells you if your relationships are bias or not with respect
    # each modality of each categorical variable.
    #
    # It also find the confident interval
    #
    # Only for categorical variables (obviously). It compares the simulations
    # against the results A from the categorical bias function
    #
    # Return
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Variable"
    #     "Modality"
    #     "Index"
    #     "Network"
    #     "Real Relationships"
    #     "Real Same to Same"
    #     "Simulated Unbias Average Same"
    #     "Simulated Unbias Minimum Same"
    #     "Simulated Bias Average Same"
    #     "Simulated Bias SD"
    #     "Target Variable"
    #     "Base Risk"
    #     "Low CI"
    #     "High CI"
    doModalityBiasAnalysis <-function(nodesTable, listOfEdgesDF,
                                      listOfConsequenceIndexes,
                                      listOfExplanatoryIndexes,
                                      totalSimulations,
                                      listOfNetworksNames = NULL,
                                      confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            
            currentModalities = NA
            if(is.null(levels(nodesTable[,explanatoryIndexes[i]])))
                currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            else
                currentModalities      = as.character(levels(nodesTable[,explanatoryIndexes[i]]))            
            
            totalCurrentModalities = length(currentModalities)
            
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }

        # For each consequence variable and each network, we are going to need
        # to Simulate the bias analysis, and get the bias average. We do that
        # now, and save it for later
        biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                   consequenceIndexes,
                                                   totalSimulations,
                                                   listOfNetworksNames = myNetworkNames)

        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        # Get the starting time so we can tell aprox. how much to finish
        startTime = Sys.time()
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    # -- Time to finish
                    currentTime          = Sys.time()
                    secondsFromStart     = as.numeric(currentTime-startTime,units="secs")
                    proportionFromStart  = ((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)
                    percentagePerSecond  = proportionFromStart / secondsFromStart
                    secondsToFinish      = ((1 - proportionFromStart) * secondsFromStart) / proportionFromStart
                    # -- String to user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100 * i / totalModalities ,2), "%" ))
                    print("--------------------")
                    print( paste0(" Total: ",   round(100 * (((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)) , 2),  "%" ))
                    print( paste0(" Finish in: ", secondsToFinish))
                    print("--------------------")

                    # Get the bootstrap vector (version B)
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = currentVariableIndex,
                                                           overrideFrequencyCategory = currentModalityName)

                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }

            }
            
            
        }        

        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][[k]][i,13] = ( (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][[k]][i,14] = ( (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...

                    if(i < totalModalities){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue     = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
                    }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }
    
    # The simulations for the contraceptives is a very special case that can't
    # be put into a function form in a simple way.
    #
    # We need to simulate the bootstrap vector with men using their original
    # prevalence, and women using the prevalence of each contraceptive method.
    # And on top of that, we need to compare with the base of each sex for it
    # original prevalence. Is very chaotic.
    #
    # Hence, I'm copypasting most of the previous code here in this function.
    doContraceptivesCase <-function(nodesTable, listOfEdgesDF,
                                    listOfConsequenceIndexes,
                                    listOfExplanatoryIndexes,
                                    totalSimulations,
                                    mySexIndex,
                                    myHormonalIndex,
                                    listOfNetworksNames = NULL,
                                    confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            
            currentModalities = NA
            if(is.null(levels(nodesTable[,explanatoryIndexes[i]])))
                currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            else
                currentModalities      = as.character(levels(nodesTable[,explanatoryIndexes[i]]))
            
            totalCurrentModalities = length(currentModalities)
        
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }

        # (SPECIAL PART)
        # We want to compare we the case where each men and women have their
        # simulation done with the stratified frequency table (case C)
        biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                   consequenceIndexes,
                                                   totalSimulations,
                                                   overrideFrequenciesIndex = sexIndex,
                                                   listOfNetworksNames      = myNetworkNames)

        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        # Get the starting time so we can tell aprox. how much to finish
        startTime = Sys.time()
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    # -- Time to finish
                    currentTime          = Sys.time()
                    secondsFromStart     = as.numeric(currentTime-startTime,units="secs")
                    proportionFromStart  = ((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)
                    percentagePerSecond  = proportionFromStart / secondsFromStart
                    secondsToFinish      = ((1 - proportionFromStart) * secondsFromStart) / proportionFromStart
                    # -- String to user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100 * i / totalModalities ,2), "%" ))
                    print("--------------------")
                    print( paste0(" Total: ",   round(100 * (((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)) , 2),  "%" ))
                    print( paste0(" Finish in: ", secondsToFinish))
                    print("--------------------")

                    
                    # (SPECIAL PART)
                    #
                    # We need to run a bootstrap where the men stay with constant
                    # men frequency, and women change the frequency according to
                    # the case of contraceptives that we are in
                    currentBiasResult = getContraceptiveVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                               mySexIndex                = mySexIndex,
                                                               myHormonalIndex           = myHormonalIndex)
                    
                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }

            }
            
            
        }        

        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][[k]][i,13] = ( (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][[k]][i,14] = ( (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...

                    if(i < totalModalities){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue     = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
                    }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }
    
    
    
  }


  # ---------------------------------
  #     HEATMAPS
  # ---------------------------------
  {

    doPValuesHeatmap <- function(tableBase, plotFilePath,
                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                 plotTheme = NULL,
                                 overrideTableName = NULL){

      # Define plot type
      myPlotType  = "pValuesHeatmap"
      myTableName = deparse(substitute(tableBase))

      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName

      }

      # Get an automatic name if you don't have a proper one
      {

        genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName = myTableName, plotType = myPlotType)

        if(genericFilePath == plotFilePath) genericFilePath = getFilePathNoExtension(genericFilePath)

        imageFilePath   = paste(genericFilePath,".png",sep="")
        tableFilePath   = paste(genericFilePath,".txt",sep="")
        latexFilePath   = paste(genericFilePath,".tex",sep="")

      }

      # Get the theme information
      themeData = getThemeParameters(plotTheme)

      # Get info about the table
      modalitiesNames = tableBase[,1]
      totalModalities = nrow(tableBase)
      totalX = totalModalities
      totalY = totalModalities


      # Init a matrix with the total for each combination
      matrixCount = matrix(0, nrow = totalY, ncol = totalX)

      # Make a new dataframe so we can draw the heatmap
      # Category X / Category Y / Total / X coordinate / Y coordinate / Color of tile / Size for text
      totalRows              = totalX * totalY
      heatmapTable           = data.frame(matrix(NA, nrow = totalRows, ncol = 8))
      colnames(heatmapTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size", "Label" )
      absoluteCount          = totalModalities

      # Fill the dataframe
      for(i in 1:totalX){

        for(j in 1:totalY){

          # Get the current line and categories
          currentLine = (i-1)*totalY + j

          currentXCategory = modalitiesNames[i]
          currentYCategory = modalitiesNames[j]

          # Init the row in the dataframe
          heatmapTable$CategoryX[currentLine] = currentXCategory
          heatmapTable$CategoryY[currentLine] = currentYCategory

          # Count how many we have
          heatmapTable$Total[currentLine]     = tableBase[[i,j+1]]
          # -- Transform numbers
          # ---- Negative very close to 0 goes to -1
          if(heatmapTable$Total[currentLine] < 0) heatmapTable$Total[currentLine] = -1 - heatmapTable$Total[currentLine]
          if(heatmapTable$Total[currentLine] > 0) heatmapTable$Total[currentLine] =  1 - heatmapTable$Total[currentLine]


          # ---- Positive very close to 0 goes to +1

          # Set the proper X / Y coordinates
          heatmapTable$Xcoordinate[currentLine] = i
          heatmapTable$Ycoordinate[currentLine] = j

          # Set the manual color and size for the text (not in use)
          heatmapTable$myColor[currentLine] = "Red"
          heatmapTable$Size[currentLine]    = 1

          # Set the label
          # heatmapTable$Label[currentLine] = round(heatmapTable$Total[currentLine],2)
          heatmapTable$Label[currentLine] = getAsterkisPValue(tableBase[[i,j+1]])

        }

      }

      cols = brewer.pal(n = 5, name = "RdBu")

      blueColors = brewer.pal(n = 3, name = "PuBu")
      redColors  = brewer.pal(n = 3, name = "OrRd")
      lastBlue   = blueColors[3]
      lastRed    = redColors[3]
      whiteColor = "#FFFFFF"
      blackColor = "#000000"

      #joinColors = c(whiteColor, whiteColor, redColors, lastRed, blackColor, lastBlue, rev(blueColors), whiteColor, whiteColor)

      joinColors = c(rev(redColors), whiteColor, whiteColor, whiteColor, blueColors)

      #print(joinColors)

      ggplot( heatmapTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total )) +

              # The background rectangle
              geom_tile(color = "black") +
              # What is written inside the rectangle
              geom_shadowtext( aes(label= Label), color = "white", fontface = "bold") +
              #geom_shadowtext( aes(label= round(Total, 4)), color = "white", fontface = "bold") +
              #geom_shadowtext( aes(label= Total), color = "white", fontface = "bold") +

              # What is written in the X axys
              geom_text(aes(label = CategoryX, y = 0.3), angle = 90, hjust = 1) +
              # What is written in the Y axys
              geom_text(aes(label = CategoryY, x = 0.3), hjust = 1) +

              # Give an extra space so we can see the labels clearly
              coord_cartesian( xlim = c(-1, totalX + 1),
                               ylim = c(-1, totalY + 1)) +

              # Force the leyend to be in between -1 and +1
              # scale_fill_gradient2(limits = c(-1, 1)) +

              # scale_fill_gradientn(colours = joinColors,
              #                      values  = rescale(c(-1, -0.1, -0.05, -0.01, -0.001, -1/1e100, 0, 1/1e100, 0.001, 0.01, 0.05, 0.1, 1)),
              #                      guide   = "colorbar", limits=c(-0.05, 0.05)) +

              scale_fill_gradientn(colours = joinColors,
                                   values  = rescale(c(-1, -0.999, -0.99, -0.95, -0.94, 0, 0.94, 0.95, 0.99, 0.999, 1)),
                                   guide   = "colorbar", limits=c(-1, 1)) +

              #scale_colour_gradient(limits = c(-1, 1)) +

              #scale_fill_gradient(limits = c(2, 4))

              # Create titles and subtitles
              labs(title    = plotTitle,
                   subtitle = plotSubtitle,
                   caption  = plotCaption,
                   color    = "p-value",
                   x = plotXLabel, y = plotYLabel) +

              # Apply the theme
              # theme(panel.background   = themeData[[1]],
              #       axis.line          = themeData[[2]],
              #       panel.grid.major.y = themeData[[3]],
              #       panel.grid.major.x = themeData[[4]],
              #       legend.position    = themeData[[5]])

              # Remove the background default lines and grey panel
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line        = element_blank(),
                    axis.text.x      = element_blank(),
                    axis.text.y      = element_blank(),
                    axis.ticks       = element_blank())


      # Save everything
      ggsave(imageFilePath)

      print(imageFilePath)

    }

  }

}




