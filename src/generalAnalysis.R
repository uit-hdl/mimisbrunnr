# -----------------------------------------------------------------------------
# This script gives a general descriptive analysis of all the variables.
#
# It serves as an introduction for all the analysis that we do later. Be aware
# that in here we don't do any control whether the variables makes sense or not.
# This is done in the control.R script.
# 
# It generates the following plots
#
#     -- For each categorical variable:
#
#            · Barplot with the absolute count of each category.
#                  If the total categories is greater than 16, you will get a
#                  vertical barplot instead of a horizontal one.
#
#            · Barplot with the absolute count of each category.
#
#
#     -- For each numerical variable
#
#            · Barplot with the absolute count of each category.
#            · Barplot with the absolute count of each category.
# -----------------------------------------------------------------------------

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("indexes.R",      encoding="utf-8")

# -----------------------------------
#     Log for the inputs{} in latex
# -----------------------------------
{
   
   logTXTFileConnection = file(GENERAL_LOG_PATH, 'w')
   logLine              = paste( "GENERAL DATA LOG at: ", RIGHT_NOW, sep = "")
   write( logLine ,
          file = logTXTFileConnection,
          append = FALSE)
}


# Generate the summary metatable
{
   
   # Start with the categoricals
   logLine = paste0("\\subsection{ Categorical variables }" , "\n")
   write( logLine , file   = logTXTFileConnection, append = TRUE )
   
   for(i in 1:totalImportantCategoricalIndexes){
      
      # Init some variables
      currentImportantIndex = importantCategoricalIndexes[i]
      currentVariableName   = colnames(completeTable)[currentImportantIndex]
      currentVariableType   = "categorical"
      
      logLine = paste0("\\subsubsection{",currentVariableName, "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )

      # Prepare the table
      print(currentVariableName)
      currentSummary        = summarizeCategorical(completeTable, currentImportantIndex)
            
      currentTableCaption = paste("Summary of the ",currentVariableType," variable ", currentVariableName, sep="")
      overrideTableName   = paste("Summary_Table_", currentVariableName, sep="")
      
      currentTable = writeTableLATEX(currentSummary, GENERAL_FOLDER, roundMe = 2,
                                     tableCaption = currentTableCaption,
                                     overrideTableName = overrideTableName)  
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(currentTable), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
   }
   
   # Then write the numericals
   logLine = paste0("\\subsection{ Numerical variables }" , "\n")
   write( logLine , file   = logTXTFileConnection, append = TRUE )

   for(i in 1:totalImportantNumericalIndexes){
      
      # Init some variables
      currentImportantIndex = importantNumericalIndexes[i]
      currentVariableName   = colnames(completeTable)[currentImportantIndex]
      currentVariableType   = "numerical"
      
      logLine = paste0("\\subsubsection{",currentVariableName, "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      # Prepare the table
      currentSummary      =  summarizeNumerical(completeTable, currentImportantIndex)
      
      currentTableCaption = paste("Summary of the ",currentVariableType," variable ", currentVariableName, sep="")
      overrideTableName   = paste("Summary_Table_", currentVariableName, sep="")
      
      currentTable = writeTableLATEX(currentSummary, GENERAL_FOLDER, roundMe = 2,
                                     tableCaption = currentTableCaption,
                                     overrideTableName = overrideTableName)  
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(currentTable), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
   }
   
}



# -------------------------------------------------------------
# Doing all the X^2 combinations
# -------------------------------------------------------------
{
   
   # Save the results into this dataframe
   allXiResultsDF              =  data.frame(matrix(NA, nrow = totalImportantCategoricalIndexes, ncol = totalImportantCategoricalIndexes+1))
   colnames(allXiResultsDF)[1] = "Variable"
   
   # For each combination of important indexes
   for(i in 1:totalImportantCategoricalIndexes){
      
      for(j in 1:totalImportantCategoricalIndexes){
      
         # -- A
         currentImportantIndexA = importantCategoricalIndexes[i]
         currentVariableNameA   = colnames(completeTable)[currentImportantIndexA]   
         # -- B
         currentImportantIndexB = importantCategoricalIndexes[j]
         currentVariableNameB   = colnames(completeTable)[currentImportantIndexB]   
         
         # Set the name of the row and column into the proper DF
         allXiResultsDF[i,1]           = currentVariableNameA
         colnames(allXiResultsDF)[1+j] = currentVariableNameB
         
         # If you are comparing with yourself, or soemething that is on the 
         # triangular matrix, do nothing, otherwise continue
         if(i<=j){
            
            allXiResultsDF[i,1+j] = NA
            
         }
         else{
            
            # Do the Xi2 analysis
            myResults = categoricalXi(completeTable, currentImportantIndexA, currentImportantIndexB, GENERAL_FOLDER)
            
            # Save the results
            allXiResultsDF[i,1+j] = myResults[[8]]
            
         }
         

      
      }
      
   }
   
   # Save the table on disk
   xiSummaryTable = writeTableLATEX(allXiResultsDF, GENERAL_FOLDER,
                                    overrideTableName = "Xi_summary",
                                    intervalsNumbers  = INTERVAL_PVALUES,
                                    intervalsColors   = COLOR_INTERVAL_PVALUES,
                                    tableCaption      = "Xi analysis with all the important categorical variables",
                                    roundMe = 2)
   
}


# -----------------------------------
#     Generate plots automatically
# -----------------------------------
# For each important variable
for(i in 1:totalImporantIndexes){
   
   # Init some variables
   currentImportantIndex = importantIndexes[i]
   currentVariableName   = colnames(completeTable)[currentImportantIndex]
   currentVariableType   = variablesInfoDF[variablesInfoDF$VariableID == currentVariableName,]$Type

   logLine = paste0("\\section{",currentVariableName, "}" , "\n")
   write( logLine , file   = logTXTFileConnection, append = TRUE )
      
   # If it is a category
   if(currentVariableType == "categorical"){
      
      print(paste0("Categorical ----> ",currentVariableName))
      print("-----------------------------------------")

      # Do these plots
      # ---- Absolute bar
      # ---- Relative bar
      myListOfBasicPlots = vector("list", length = 2)
      
      {
         
         captionBarPlot = paste0("Barplot for ", currentVariableName,".")
         captionRelPlot = paste0("Pie plot for ", currentVariableName,".")
         
         # Do an absolute bar plot
         print("Doing barplot")
         barplotResults = doBarPlot(completeTable, currentImportantIndex, GENERAL_FOLDER,
                                    colorsVector = myListOfColorVectors[[currentImportantIndex]],
                                    overrideCaption = captionBarPlot,
                                    countingType = "count")
         
         # Do a  relative bar plot
         print("Doing piechart")
         relplotResults = doBarPlot(completeTable, currentImportantIndex, GENERAL_FOLDER,
                                    colorsVector = myListOfColorVectors[[currentImportantIndex]],
                                    overrideCaption = captionRelPlot,
                                    countingType = "identity",
                                    polarCoordinates = TRUE)

         # Add it to the list
         myListOfBasicPlots[[1]] = barplotResults[[1]]
         myListOfBasicPlots[[2]] = relplotResults[[1]]
         
         # Log each one individually
         # logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(barplotResults[[3]]), "}" , "\n")
         # write( logLine , file   = logTXTFileConnection, append = TRUE )
         # 
         # logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResults[[3]]), "}" , "\n")
         # write( logLine , file   = logTXTFileConnection, append = TRUE )
         
      }
      
      # Do the plot composition at 1/4 of the page
      ggarrange(plotlist = myListOfBasicPlots, ncol = 2, nrow = 1)
      basicFileName      = paste0(currentVariableName,"_topComposition.png")
      basicPath          = file.path(paste(GENERAL_FOLDER, basicFileName, sep = ""))
      ggsave(basicPath, width = 21, height = 30*0.25)
      
      basicCaption  = paste0(currentVariableName," basic plots")
      latexFilePath = writeImageLATEX(basicPath, captionText = basicCaption,
                                      pageWidth = 1, pageHeight = 0.25)
      
      logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(latexFilePath), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      
      # Do the combine plots
      # ---- Relative to other categories
      # ---- Boxplots to other numerical
      
      # Save all the plots here
      
      myListOfAdvancePlots = vector("list", length = ( totalImporantIndexes-1 ))
      advancePlotsIndex = 1
      
      for(j in 1:totalImporantIndexes){

         # Don't combine with yourself
         if(i!=j){
            
            currentSecondImportantIndex = importantIndexes[j]
            currentSecondVariableName   = colnames(completeTable)[currentSecondImportantIndex]
            currentSecondVariableType   = variablesInfoDF[variablesInfoDF$VariableID == currentSecondVariableName,]$Type
             
            # logLine = paste0("    \\subsection{ by ",currentSecondVariableName, "}" , "\n")
            # write( logLine , file   = logTXTFileConnection, append = TRUE )
            
            # If the second one is a category
            if(currentSecondVariableType == "categorical"){

               print(paste0("    Categorical ",currentVariableName, " plus categorical ", currentSecondVariableName))
               
               
               print("    ---- Relative barplot")
               captionCombineRelPlot = paste0("Relative barplot for ", currentVariableName," group by ", currentSecondVariableName,".")
               
               relCombinePlotResults = doBarRelativeCombinePlot(completeTable,
                                                                currentSecondImportantIndex,
                                                                currentImportantIndex,
                                                                overrideCaption = captionCombineRelPlot,
                                                                GENERAL_FOLDER,
                                                                colorsVector = myListOfColorVectors[[currentSecondImportantIndex]])
               
               # Save it for the composition
               myListOfAdvancePlots[[advancePlotsIndex]] = relCombinePlotResults[[1]]
               advancePlotsIndex = advancePlotsIndex + 1
               
               # logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relCombinePlotResults[[3]]), "}" , "\n")
               # write( logLine , file   = logTXTFileConnection, append = TRUE )
               
               
            }
            # If the second one is a numeric
            else{
               
               print(paste0("Categorical ",currentVariableName, " plus numerical ", currentSecondVariableName))
               
               tableNamePvalues      = paste0("pvalues_",     currentVariableName, "_", currentSecondVariableName)
               tableNameCentralities = paste0("centralities_",currentVariableName, "_", currentSecondVariableName)
               
               captionPvalues        = paste0("P-values for ",     currentVariableName, " and ", currentSecondVariableName ," table.")
               captionCentralities   = paste0("Centralities for ", currentVariableName, " and ", currentSecondVariableName ," table.")
               
               print("    ---- Boxplot barplot")
               myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                                        currentImportantIndex,
                                                        currentSecondImportantIndex,
                                                        GENERAL_FOLDER,
                                                        showPValues  = FALSE,
                                                        colorsVector = myListOfColorVectors[[currentImportantIndex]])
               # P-Values
               pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                              overrideTableName = tableNamePvalues,
                                              intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                              tableCaption = captionPvalues, roundMe = 2)
               
               # Centralities
               centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                                   overrideTableName = tableNameCentralities,
                                                   tableCaption = captionCentralities, roundMe = 2)

               logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
               write( logLine , file   = logTXTFileConnection, append = TRUE )
               
               logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
               write( logLine , file   = logTXTFileConnection, append = TRUE )
               
               # Save it for the composition
               myListOfAdvancePlots[[advancePlotsIndex]] = myBoxplotResults[[1]]
               advancePlotsIndex = advancePlotsIndex + 1
               
               
            }
            
         }
         
      }
    
      # Do the plot composition at 3/4 of the page
      ggarrange(plotlist = myListOfAdvancePlots, ncol = 2, nrow = ceiling((totalImporantIndexes-1)/2) )
      advanceFileName    = paste0(currentVariableName,"_downComposition.png")
      advancePath        = file.path(paste(GENERAL_FOLDER, advanceFileName, sep = ""))
      ggsave(advancePath, width = 21, height = 30*0.75)
      
      advanceCaption = paste0(currentVariableName," combined plots")
      latexFilePath  = writeImageLATEX(advancePath, captionText = advanceCaption,
                                      pageWidth = 1, pageHeight = 0.75)
      
      logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(latexFilePath), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      
   }
   
   
   # If it is numerical
   # (as right now this is not important, because the only two numericals we have we plot them manually anyway)
   else{
    
      print(paste0("Numerical ",currentVariableName))

      # Do these plots
      if(FALSE){

         # Histograms (probably ugly because of automatic binning)
         print("Histogram")
         doHistogramPlot2(completeTable, currentImportantIndex, GENERAL_FOLDER)

         # Density
         print("Density")
         doDensityPlot(completeTable, currentImportantIndex, GENERAL_FOLDER)
         
         # QQ Plots
         print("QQ")
         doQQPlot(completeTable, currentImportantIndex, GENERAL_FOLDER)
         
      }
      
   }

}




# -----------------------------------
#     Generate plots manually
#
#         - The age histogram
#         - The BMI Density for each other categorical
# -----------------------------------
{
   
   print("Manual plots")
   
   doHistogramPlot2(completeTable, ageIndex, GENERAL_FOLDER,
                    binsWidth = 1)
   
   
   # BMI and Sex
   a = doBMIPlot(completeTable, BMIIndex, sexIndex, GENERAL_FOLDER,
             colorsVector = COLOR_VECTOR_SEX)
   
   # BMI and Sports
   b = doBMIPlot(completeTable, BMIIndex, sportsIndex, GENERAL_FOLDER,
             colorsVector = COLOR_VECTOR_SPORTS,
             plotTitle = "BMI grouped by sport activity")
    
   # BMI and Smoke
   c = doBMIPlot(completeTable, BMIIndex, smokeIndex, GENERAL_FOLDER,
             colorsVector = COLOR_VECTOR_SMOKE,
             plotTitle = "BMI grouped by smoking habit")
   
   # BMI and Carrier
   d = doBMIPlot(completeTable, BMIIndex, carrierIndex, GENERAL_FOLDER,
                 colorsVector = COLOR_VECTOR_CARRIER,
                 plotTitle = "BMI grouped by carrier status")
   
   
   myListOfBMIPlots = vector("list", length = 4)
   myListOfBMIPlots[[1]] = a[[1]]
   myListOfBMIPlots[[2]] = b[[1]]
   myListOfBMIPlots[[3]] = c[[1]]
   myListOfBMIPlots[[4]] = d[[1]]
   
   # Do the plot composition at 3/4 of the page
   ggarrange(plotlist = myListOfBMIPlots, ncol = 2, nrow = 2)
   bmisFileName    = "BMI_composition.png"
   bmisPath        = file.path(paste(GENERAL_FOLDER, bmisFileName, sep = ""))
   ggsave(bmisPath, width = 21, height = 21)

   bmisCaption   = "BMI combined"
   latexFilePath = writeImageLATEX(bmisPath, captionText = bmisCaption,
                                   pageWidth = 1, pageHeight = 1)
   
   logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(latexFilePath), "}" , "\n")
   write( logLine , file   = logTXTFileConnection, append = TRUE )
   
   
}






# Close the TXT connections
close(logTXTFileConnection)

# Update the latex file
source("latex.R", encoding="utf-8")
