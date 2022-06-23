# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("indexes.R",      encoding="utf-8")

# Rename the complete table for testing porpuses
automaticTable = completeTable

# Analyse how many variables we have theoretically
# -----------------------------------------------------------------------------
{
    # How many columns and rows
    totalColumns = ncol(completeTable)
    totalRows    = nrow(completeTable)

     # How many of each variables
     totalVariables   = nrow(variablesInfoDF)
     totalCategorical = sum(   variablesInfoDF$Type == "categorical"  )
     totalNumerical   = sum(   variablesInfoDF$Type == "numerical"    )
}

# Take away the variables that you don't want to be analyzed automatically
{
  # Examples of these are:
  # ---- the ID might be usefull to print in text, so we keep it in the phenotype table, but that will create 1000 categories which is silly, so we don't do any plot with that column as reference
  # ---- Variables that you already transform it to categories, you will just get duplicated plots.
  # ---- Variables that you already know are not interesting and are in your table for something completely unrelated
  # -----------------------------------------------------------------------------
  variablesInfoDF$Mask                      = TRUE   # Everything included by default
  variablesInfoDF$Mask[IDIndex]             = FALSE  # Don't include the ID in the analysis
  variablesInfoDF$Mask[dateIndexComplete]   = FALSE  # Don't include the Date
  variablesInfoDF$Mask[uniqueIndexComplete] = FALSE  # Don't include the weird group with only one category

  # Take away the columns that have way too many categories and add it to the columns to be deleted
  variablesInfoDF[variablesInfoDF$TotalCategories > CATEGORY_LIMIT,]$Mask = FALSE

  # Reduce the amount of columns for testing
  if(FALSE){
    variablesInfoDF[6:totalVariables,]$Mask = FALSE
  }


}

# Analyse how many variables we have after the masking
# -----------------------------------------------------------------------------
{
  # How many of each variables
  myFinalVariables = (variablesInfoDF$Mask)

  totalUnmaskVariables   = nrow(  myFinalVariables )
  totalUnmaskCategorical = sum(   variablesInfoDF[myFinalVariables,]$Type == "categorical"  )
  totalUnmaskNumerical   = sum(   variablesInfoDF[myFinalVariables,]$Type == "numerical"    )
}

# Make the general descriptive statistics about the numerical variables
# -----------------------------------------------------------------------------
{


}

# Count how many plot of each we are going to make theoretically
totalBoxPlots = totalCategorical * totalNumerical

# Prepare the dataframes with the summaries:
# Also prepare the index to follow each dataframe
#
# We have C categorical variables
# We are in the Ci categorical variable
# We have N numerical variables
# We have K categories inside all categorical variables
# We have Kc categories inside each categorical variable
# We are in the Kcj category of all the Kc possible
#
#
# General information:
totalSimplePValues       = (totalUnmaskNumerical *  (variablesInfoDF[myFinalVariables,]$TotalCategories)^2)

#totalFilteredPValues     = totalSimplePValues * variablesInfoDF[myFinalVariables,]$TotalCategories

allCategoriesCombination = sum( totalUnmaskNumerical *  (variablesInfoDF[myFinalVariables,]$TotalCategories)^2  )

#allPValues               = sum( totalSimplePValues + totalFilteredPValues  )


# Boxplots
boxPlotSummary           =  data.frame(matrix(NA, nrow = allCategoriesCombination , ncol = 8))
#boxPlotSummary           =  data.frame(matrix(NA, nrow = allPValues , ncol = 8))
colnames(boxPlotSummary) = c("Grouping Variable", "Value Variable", "Category A", "Category B", "P-Value", "Asterisks", "Mean A", "Mean B")
boxPlotLogIndex          = 1


# Prepare the TXT logs
{
  # -- Clear the PValues TXT
  pValuesTXTFileConnection = file(pValuesTXT)
  headerLine               = paste( "\"Filtering Variable\" ",
                                    " \"Only\" ",
                                    " \"Grouping Variable\" ",
                                    " \"Value Variable\" ",
                                    " \"Category A\" " ,
                                    " \"Category B\" ",
                                    " \"P-Value\" ",
                                    " \"Asterisks\" ",
                                    " \"Mean A\" ",
                                    " \"Mean B\" ",
                                    " \"Total A\" ",
                                    " \"Total B\"",
                                    sep = ";")
  write( headerLine ,
         file = pValuesTXTFileConnection,
         append = FALSE)
}


# Finally, do everything
# Do everything
# FOR EACH VARIABLE...
for(i in 1:totalVariables){

  totalProgress = round(i/totalVariables,2) * 100
  totalBars     = getProgressCharacters(totalProgress)

  # Show progress bar
  print("--------- PROGRESS ----------")
  print(  paste(  totalBars , "  ", totalProgress, "%", sep="")   )
  print("--------- -------- ----------")

  # Get the variable name
  variableNameI       = variablesInfoDF$VariableID[i]

  # Do stuff only for variables that are NOT MASKED
  if(variablesInfoDF$Mask[i] == TRUE){

    # Set up the special color vector if any
    usingColorVector = NULL
    if(i == sexIndex) usingColorVector = COLOR_VECTOR_SEX

    # --------------------------------------
    # CATEGORICAL VARIABLE
    # --------------------------------------
    {
      if( variablesInfoDF$Type[i] == "categorical"){

        # --------------------------------------------------
        # ONLY ONE CATEGORICAL VARIABLE with nothing else
        # --------------------------------------------------
        {

        }

        # -------------------------------------------
        # ONE CATEGORY WITH SOMETHING ELSE
        # -------------------------------------------
        {

          # For every other column
          for(j in 1:totalVariables){

            # Get the other name
            variableNameJ       = variablesInfoDF$VariableID[j]

            # Again, do stuff only for only NOT MASKED variables
            if(variablesInfoDF$Mask[j] == TRUE){

              # Check that is not the same column we just analyze
              # That wouldn't make any sense for multivariable analysis
              if(i!=j){

                # -----------------------------------------
                # TWO CATEGORIES GROUPED TOGUETHER
                # -----------------------------------------
                {

                }

                # -------------------------------------------------
                # ONE CATEGORICAL VARIABLE WITH NUMERICAL VARIABLE
                # -------------------------------------------------
                {

                  if( variablesInfoDF$Type[j] == "numerical"){

                    # -------------------------------------------------
                    # NO CATEGORY FILTERING
                    # -------------------------------------------------
                    {
                      # Boxplots with p-value
                      if(SAVE_BOX_PLOTS == TRUE){

                        # Do the boxplot
                        {
                          print(paste("Doing boxplot for ",variableNameJ, " grouped by ", variableNameI, sep=""))

                          automaticTitle = paste("Boxplot with P-Value for ",variableNameJ, " grouped by", variableNameI, sep="")

                          myResult = doBoxPlotPValues(completeTable, i, j,  usingColorVector, automaticFolder,
                                                      plotTitle    = automaticTitle,
                                                      plotSubtitle = "automatic plot",
                                                      plotCaption  = "Fit Futures 1",
                                                      plotXLabel   = variableNameI,
                                                      plotYLabel   = "Total")
                        }

                        # Log the results
                        {

                          myMeltedResult = melt(myResult, id.vars="ID")

                          totalPValues = nrow(myMeltedResult)

                          # Copy the results into the megalog
                          for (z in 1:totalPValues) {

                            # General Info
                            currentCategoryA = as.character(myMeltedResult$ID[z])
                            currentCategoryB = as.character(myMeltedResult$variable[z])
                            currentPValue    = myMeltedResult$value[z]
                            if(is.na(currentPValue) == TRUE) currentPValue = 2

                            # Find out the means
                            currentMeanA     = mean(completeTable[completeTable[,i] == currentCategoryA,][,j], na.rm = TRUE)
                            currentMeanB     = mean(completeTable[completeTable[,i] == currentCategoryB,][,j], na.rm = TRUE)

                            # Find out the sample size
                            totalA = sum(!is.na(completeTable[completeTable[,i] == currentCategoryA,][,j]))
                            totalB = sum(!is.na(completeTable[completeTable[,i] == currentCategoryB,][,j]))

                            # Write it down
                            boxPlotSummary$`Grouping Variable`[boxPlotLogIndex] = variableNameI
                            boxPlotSummary$`Value Variable`[boxPlotLogIndex]    = variableNameJ
                            boxPlotSummary$`Category A`[boxPlotLogIndex]        = currentCategoryA
                            boxPlotSummary$`Category B`[boxPlotLogIndex]        = currentCategoryB
                            boxPlotSummary$`P-Value`[boxPlotLogIndex]           = currentPValue
                            boxPlotSummary$Asterisks[boxPlotLogIndex]           = getAsterkisPValue(currentPValue)
                            boxPlotSummary$`Mean A`[boxPlotLogIndex]            = currentMeanA
                            boxPlotSummary$`Mean B`[boxPlotLogIndex]            = currentMeanB

                            # Pass to the next one
                            boxPlotLogIndex = boxPlotLogIndex + 1

                            # Write it down TXT
                            {
                              logThis = TRUE
                              if(LOG_P_SIGNIFICANT_ONLY == TRUE){
                                if(currentPValue > P_SIGNIFICANT_THRESHOLD){
                                  logThis = FALSE
                                }
                                if(totalA < N_SIGNIFICANT_SIZE){
                                  logThis = FALSE
                                }
                                if(totalB < N_SIGNIFICANT_SIZE){
                                  logThis = FALSE
                                }
                              }

                              if(logThis == TRUE){

                                nextLineString = paste( "NO FILTERING",
                                                        "",
                                                        variableNameI,
                                                        variableNameJ,
                                                        currentCategoryA,
                                                        currentCategoryB,
                                                        currentPValue,
                                                        getAsterkisPValue(currentPValue),
                                                        currentMeanA,
                                                        currentMeanB,
                                                        totalA,
                                                        totalB,
                                                        sep = ";")

                                write( nextLineString ,
                                       file   = pValuesTXT,
                                       append = TRUE )
                              }


                            }


                          }
                        }
                      }
                    }



                    # -------------------------------------------------
                    # FILTERING THE TABLE FOR EACH CATEGORY
                    # -------------------------------------------------
                    {}

                    # FOR EACH VARIABLE...
                    for(k in 1:totalVariables){

                      # Get the variable name
                      variableNameK       = variablesInfoDF$VariableID[k]

                      # Do stuff only for variables that are NOT MASKED
                      if(variablesInfoDF$Mask[k] == TRUE){

                        # Do stuff only for categorical variables only
                        if( variablesInfoDF$Type[k] == "categorical"){

                          # Do stuff for variables that are not the current variable
                          if(i != k){

                            currentAllCategories = unique(completeTable[,k])
                            totalAllCategories   = length(currentAllCategories)

                            # For each of the modality in that category
                            for( y in 1:totalAllCategories){

                              currentModality      = as.character(currentAllCategories[y])
                              currentModalityClean = cleanWeirdCharacters(currentModality)
                              myFilterTable        = completeTable[completeTable[,k] == currentModality,]

                              # Do the boxplot
                              {
                                print(paste("Doing boxplot for ",variableNameJ, " grouped by ", variableNameI, " for only ", variableNameK, " equal to ", currentModality, sep=""))

                                automaticTitle     = paste("Automatic Plot: ", currentModality," only. Boxplot with P-Value for ",variableNameJ, " grouped by", variableNameI, sep="")
                                automaticSubtitle  = paste("FILTER BY:", variableNameK, " equal to ", currentModality, sep="")
                                overridedTableName = paste(currentModalityClean,"_only_FOR_",variableNameK, sep='')



                                myResult = doBoxPlotPValues(myFilterTable, i, j,  usingColorVector, automaticFolder,
                                                            overrideTableName = overridedTableName,
                                                            plotTitle    = automaticTitle,
                                                            plotSubtitle = automaticSubtitle,
                                                            plotCaption  = "Fit Futures 1",
                                                            plotXLabel   = variableNameI,
                                                            plotYLabel   = "Total")
                              }
                              # Logging the results
                              {
                                myMeltedResult = melt(myResult, id.vars="ID")
                                totalPValues = nrow(myMeltedResult)

                                for (z in 1:totalPValues) {

                                  # General Info
                                  currentCategoryA = as.character(myMeltedResult$ID[z])
                                  currentCategoryB = as.character(myMeltedResult$variable[z])
                                  currentPValue    = myMeltedResult$value[z]
                                  if(is.na(currentPValue) == TRUE) currentPValue = 2

                                  # Find out the means
                                  currentMeanA     = mean(myFilterTable[myFilterTable[,i] == currentCategoryA,][,j], na.rm = TRUE)
                                  currentMeanB     = mean(myFilterTable[myFilterTable[,i] == currentCategoryB,][,j], na.rm = TRUE)

                                  # Find out the sample size
                                  totalA = sum(!is.na(myFilterTable[myFilterTable[,i] == currentCategoryA,][,j]))
                                  totalB = sum(!is.na(myFilterTable[myFilterTable[,i] == currentCategoryB,][,j]))

                                  # Write it down
                                  {
                                    logThis = TRUE
                                    if(LOG_P_SIGNIFICANT_ONLY == TRUE){
                                      if(currentPValue > P_SIGNIFICANT_THRESHOLD){
                                        logThis = FALSE
                                      }
                                      if(totalA < N_SIGNIFICANT_SIZE){
                                        logThis = FALSE
                                      }
                                      if(totalB < N_SIGNIFICANT_SIZE){
                                        logThis = FALSE
                                      }
                                    }

                                    if(logThis == TRUE){

                                      nextLineString = paste( variableNameK,
                                                              currentModality,
                                                              variableNameI,
                                                              variableNameJ,
                                                              currentCategoryA,
                                                              currentCategoryB,
                                                              currentPValue,
                                                              getAsterkisPValue(currentPValue),
                                                              currentMeanA,
                                                              currentMeanB,
                                                              totalA,
                                                              totalB,
                                                              sep = ";")

                                      write( nextLineString ,
                                             file   = pValuesTXT,
                                             append = TRUE )
                                    }


                                  }

                                }
                              }

                            }

                          }


                        }



                      }

                    }








                  }
                }
              }
            }
          }
        }
      }
    }

    # --------------------------------------
    # NUMERICAL VARIABLE
    # --------------------------------------
    {

    }


  }



}


# We have fill all the logs, now is just writting them
write.csv2(boxPlotSummary, pValuesCSV)

# Close the TXT connections
close(pValuesTXTFileConnection)

