# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("indexes.R",      encoding="utf-8")

# Rename the complete table for testing porpuses
automaticTable = completeTable

# Analyse which type of variables you have in each column
# -----------------------------------------------------------------------------
{
  
  # How many columns and rows
  totalColumns = ncol(automaticTable)
  totalRows    = nrow(automaticTable)
  
  # What tipe of data for each column
  myClasses    = sapply(automaticTable, class)
  
  # Simplify types to boolean
  # - FALSE = Numerical
  # - TRUE  = Categorical
  categoricalColumns = rep(FALSE,totalColumns)
  
  for(i in 1:totalColumns){
    
    if(myClasses[[i]] == "character" || myClasses[[i]] == "factor") categoricalColumns[i] = TRUE
    
  }
  
  # Get how many for each
  totalCategorical = sum(categoricalColumns)
  totalNumerical   = length(categoricalColumns) - totalCategorical
  
  # For those who are categorical, check how many categories are in each column
  # Here, a 0 means no categories, which implies numerical
  totalCategoriesList = rep(0,totalColumns)
  
  for(i in 1:totalColumns){
    
    if(categoricalColumns[i] == TRUE){
    
      totalCategoriesList[i] = length(unique(automaticTable[,i]))
      
    }
    
  }
  
}


# How many columns and rows
totalColumns = ncol(completeTable)
totalRows    = nrow(completeTable)

# How many of each variables
totalCategorical = sum(   variablesInfoDF$Type == "categorical"  )
totalNumerical   = sum(   variablesInfoDF$Type == "numerical"    )

# Take away the variables that you don't want to be analyzed automatically
# Examples of these are:
# ---- the ID might be usefull to print in text, so we keep it in the phenotype table, but that will create 1000 categories which is silly, so we don't do any plot with that column as reference
# ---- Variables that you already transform it to categories, you will just get duplicated plots.
# ---- Variables that you already know are not interesting and are in your table for something completely unrelated
# -----------------------------------------------------------------------------
maskColumns          = rep(TRUE,totalColumns)
maskColumns[IDIndex] = FALSE # ID column

# Take away the columns that have way too many categories and add it to the columns to be deleted
maskColumns = maskColumns & (! (variablesInfoDF$TotalCategories > CATEGORY_LIMIT))

# Then, take away everything from the supertable
# And update the rest of lists
# automaticTable = automaticTable[,maskColumns]

# Analyse (again) which type of variables you have in each column
# And update the numbers for all the thigns
# -----------------------------------------------------------------------------
{
  
  # How many columns and rows
  totalColumns = ncol(automaticTable)
  totalRows    = nrow(automaticTable)
  
  # What tipe of data for each column
  myClasses    = sapply(automaticTable, class)
  
  # Simplify types to boolean
  # - FALSE = Numerical
  # - TRUE  = Categorical
  categoricalColumns = rep(FALSE,totalColumns)
  
  for(i in 1:totalColumns){
    
    if(myClasses[[i]] == "character" || myClasses[[i]] == "factor") categoricalColumns[i] = TRUE
    
  }
  
  # Get how many for each
  totalCategorical = sum(categoricalColumns)
  totalNumerical   = length(categoricalColumns) - totalCategorical
  
  # For those who are categorical, check how many categories are in each column
  # Here, a 0 means no categories, which implies numerical
  totalCategoriesList = rep(0,totalColumns)
  
  for(i in 1:totalColumns){
    
    if(categoricalColumns[i] == TRUE){
      
      totalCategoriesList[i] = length(unique(automaticTable[,i]))
      
    }
    
  }
  
}



















# ( Supertable )
# -- Do all the table without filters
# -- For each of the columns with categories
# ----- Do a subset for each category, and run everything again without that column
# ----- Don't do the subset if the number of categories is insane. I define it as more than 16









# Summarize how many plots/analyses of each type we are going to have
# Then, create a summary CSV for each type where we write the results (ie p-values, r2, ...)
#
# We have C categorical variables
# We are in the Ci categorical variable
# We have N numerical variables
# We have K categories inside all categorical variables
# We have Kc categories inside each categorical variable
# We are in the Kcj category of all the Kc possible
#

relevantCategoriesColumns = automaticTable[, totalCategoriesList >= 1]
totalRelevantColumns      = ncol(relevantCategoriesColumns)
totalK                    = sum(totalCategoriesList)
allCategoriesList         = rep("", totalK)
categoriesIndex           = 1
for(i in 1:totalRelevantColumns){
  
  currentKc = as.character(unique(relevantCategoriesColumns[,i]))
  totalKc   = length(currentKc)

  for(j in 1:totalKc){

    currentKcj = as.character(currentKc[j])
    allCategoriesList[categoriesIndex] = currentKcj
    categoriesIndex = categoriesIndex + 1
    
  }  
  
}

# -- Barplots 1D
# ---- We have one for each categorical variable
totalBarplots1D       = totalCategorical
# -- Barplots by groups
# ---- We have one for each categorical variable
totalBarplotsGrouping = totalCategorical * (totalCategorical - 1)
# -- Boxplots by groups with p-values
totalBoxplots         = totalCategorical * totalNumerical
totalBoxplotsPValues  = sum(choose(totalCategoriesList,2))
# -- Regression 2D
# -- Regression by groups with R2






# Create the data frame where we put the results.
# This goes as a matrix of all possible categories x all possible categories, repeated all possible numerical times
#
#                                                                                      \ Category K \ Category K \ Category K \ ....
#                                                                                     ------------------------------------------------
# \ Plot where this is show \ Numerical Variable \ Categorical variable I \ Category K \   p-value  \   p-value  \   p-value  \ ....
#
#
# And for this you have two copies. One with EVERYTHING, and the other one with only the p-values lower than threshold

# Create the dataframe
summaryTotalRows    = totalK * totalNumerical
summaryTotalColumns = totalK + 4
summaryTable        = data.frame(matrix(3, summaryTotalRows, summaryTotalColumns))

# Setup the columns
colnames(summaryTable)[1] = "PlotFilePath"
colnames(summaryTable)[2] = "N_Variable"
colnames(summaryTable)[3] = "C_Variable"
colnames(summaryTable)[4] = "K_Category"
colnames(summaryTable)[5:summaryTotalColumns] = allCategoriesList

# Index to control which row are we filling now
summaryIndexRow    = 1
summaryIndexColumn = 4



# Remove extreme outliers (+4sd)
# -----------------------------------------------------------------------------

# Analyse each data manually and suggest changes
# 
# Some heuristics that I can think of:
#   Likely to be categorical data
# 
# make a summary of the unique value, if it's < some_threshold, there is higher chance to be categorical data.
#     if the data is highly concentrate (low std.)
#     if the unique value are highly sequential, and starts from 1
#     if all the value in column has fixed length (may be ID/Date)
#     if it has a very small p-value at Benford's Law
# if it has a very small p-value at the Chi-square test against the result column
# 
# Likely to be quantitative data
# 
# if the column has floating number
# if the column has sparse value
# if the column has negative value
# 
# Other
# 
# Maybe quantitative data are more likely to be near/next to quantitative data (vice-versa)




# Reduce the number of columns for testing
testing = TRUE
if(testing == TRUE){
  
  maskColumns = rep(FALSE,totalColumns)
  maskColumns[2:5] = TRUE
  maskColumns[53:56] = TRUE
  
}



# Do everything
# FOR EACH VARIABLE...
for(i in 1:totalColumns){

  # Get the variable name
  #variableName       = colnames(automaticTable[i])
  variableName       = variablesInfoDF$VariableID[i]
  
  # --------------------------------------
  # CATEGORICAL VARIABLE
  # --------------------------------------
  if(categoricalColumns[i] == TRUE){

    # --------------------------------------------------
    # ONLY ONE CATEGORICAL VARIABLE with nothing else
    # --------------------------------------------------
    {
      
      # Create automatic filename and filepath
      plotName_automatic = paste("barplot_",i,"_",variableName,".png",sep="")
      filepath_automatic = file.path(paste(automaticFolder, plotName_automatic,      sep = ""))
      
      # Set up the special color vector if any
      usingColorVector = NULL
      if(i == sexIndex) usingColorVector = colorVectorSex
      
      # Barplot
      if(saveBarPlots == TRUE){
        
        print(paste("Doing barplot for ",variableName, sep=""))
        
        automaticTitle = paste("Abs. Frequency for ",variableName, sep="")
        
        doBarPlot(automaticTable, i, filepath_automatic, colorsVector = usingColorVector,
                  plotTitle = automaticTitle, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
        
      }
    
}

    # -------------------------------------------
    # ONE CATEGORY WITH SOMETHING ELSE
    # -------------------------------------------
    {
      
      # For every other column
      for(j in 1:totalColumns){

        # Check that is not the same column we just analyze
        # That wouldn't make any sense for multivariable analysis
        if(i!=j){
          
          # Get the name
          otherVariableName   = colnames(automaticTable[j])
          
          # -----------------------------------------
          # TWO CATEGORIES GROUPED TOGUETHER
          # -----------------------------------------
          {
            
            if(categoricalColumns[j] == TRUE){
              
              # Get the number of categories
              secondCategoryList  = unique(automaticTable[,j])
              secondCategoryTotal = length(secondCategoryList)
              
              # If we are under the limit of categories
              if(secondCategoryTotal < categoryLimit){
                
                # Create automatic filename and filepath
                plotName_automatic1 = paste("barplotDodge_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
                filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
                
                plotName_automatic2 = paste("barplotStack_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
                filepath_automatic2 = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
                
                plotName_automatic3 = paste("barplotRelative_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
                filepath_automatic3 = file.path(paste(automaticFolder, plotName_automatic3,      sep = ""))
                
                # Set up the special color vector if any
                usingColorVector = NULL
                if(j == sexIndex) usingColorVector = colorVectorSex
                
                # Barplots
                if(saveBarPlots == TRUE){
                  
                  print(paste("Doing barplots for ",variableName," and ",otherVariableName, sep=""))
                  
                  automaticTitleDodge    = paste("Abs. Frequency for ",variableName, " and ", otherVariableName, sep="")
                  automaticTitleStack    = paste("Abs. Frequency for ",variableName, " and ", otherVariableName, sep="")
                  automaticTitleRelative = paste("Rel. Frequency for ",variableName, " and ", otherVariableName, sep="")
                  
                  doBarDodgeCombinePlot(automaticTable, i, j, usingColorVector, filepath_automatic1,
                                        plotTitle = automaticTitleDodge, plotSubtitle = paste("Grouped by ", otherVariableName), plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
                  
                  doBarStackCombinePlot(automaticTable, i, j, usingColorVector, filepath_automatic2,
                                        plotTitle = automaticTitleStack, plotSubtitle = paste("Grouped by ", otherVariableName), plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
                  
                  doBarRelativeCombinePlot(automaticTable, i, j, usingColorVector, filepath_automatic2,
                                           plotTitle = automaticTitleRelative, plotSubtitle = paste("Grouped by ", otherVariableName), plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
                  
                }
                
              }
              
            }
            
          }
          
          # -------------------------------------------------
          # ONE CATEGORICAL VARIABLE WITH NUMERICAL VARIABLE
          # -------------------------------------------------
          {
            
            if(categoricalColumns[j] == FALSE){
              
              # Create automatic filename and filepath
              plotName_automatic1 = paste("boxplot_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
              filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
              
              plotName_automatic2 = paste("boxplot_combine_pValues_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
              filepath_automatic2 = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
              
              plotName_automatic3 = paste("densityplot_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
              filepath_automatic3 = file.path(paste(automaticFolder, plotName_automatic3,      sep = ""))
              
              # Set up the special color vector if any
              usingColorVector = NULL
              if(i == sexIndex) usingColorVector = colorVectorSex
              
              # Do the boxplots
              if(saveBoxPlots == TRUE){
                
                print(paste("Doing boxplots for ",variableName," and ",otherVariableName, sep=""))
                
                automaticTitle = paste("Boxplot for ",variableName, " and " , otherVariableName, sep="")
                
                currentPValues = doBoxPlotPValues(automaticTable, i, j, usingColorVector, filepath_automatic2,
                                                  plotTitle = automaticTitle, plotSubtitle = "P-value under boxs", plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
                
                totalCurrentCategories = nrow(currentPValues)

                # Fill the subsquare matrix
                for(k in 1:totalCurrentCategories){
                  for(l in 1:totalCurrentCategories){
                    summaryTable[ (summaryIndexRow + k - 1) , (summaryIndexColumn + l - 1) ] = currentPValues[k,l]    
                  }  
                }
                
                summaryIndexRow    = summaryIndexRow    + totalCurrentCategories
                summaryIndexColumn = summaryIndexColumn + totalCurrentCategories
                
              }
              
              # Do the density plots
              if(saveDensityPlots == TRUE){
                
                print(paste("Doing densityplot for ",variableName," and ",otherVariableName, sep=""))
                
                automaticTitle = paste("Density for ",variableName, " and " , otherVariableName, sep="")
                
                doDensityPlot(automaticTable, i, j, usingColorVector, plotFilePath = filepath_automatic3,
                              plotTitle = automaticTitle, plotSubtitle = "Integers can cause wonky shapes", plotCaption = NULL, plotXLabel = otherVariableName, plotYLabel = "Rel. Frequency")                      
                
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
  else{
    
    # --------------------------------------
    # ONLY NUMERICAL VARIABLE
    # --------------------------------------
    
    # (nothing yet)

    # --------------------------------------
    # NUMERICAL VARIABLE with something else
    # --------------------------------------
    
    for(j in 1:totalColumns){

      print("Local advance: ")
      print( (j/totalColumns) * 100 )
      
      # Don't run analysis againts yourself
      if(i!=j){
        
        # Get the other variable name
        otherVariableName   = colnames(automaticTable[j])
        
        # --------------------------------------
        # NUMERICAL AND CATEGORICAL VARIABLE
        # --------------------------------------
        
        # (this is already done in category with numerical :P )
        
        # --------------------------------------
        # NUMERICAL AND NUMERICAL VARIABLE
        # --------------------------------------
        {
          
          if(categoricalColumns[j] == FALSE){
            
            # Create automatic filename and filepath
            plotName_automatic1 = paste("regression_simple_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
            filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
            
            if(saveRegressionPlots == TRUE){
              
              print(paste("Doing regression for ",variableName," and ",otherVariableName, sep=""))
              
              #doSimpleRegression(phenotypeTable, i, j, NULL, filepath_automatic1)
              
              doSimpleRegression(automaticTable, i, j, filepath_automatic1,
                                 plotTitle = "Regression", plotSubtitle = "The further from 0 the better", plotCaption = NULL, plotXLabel = variableName, plotYLabel = otherVariableName)
              
            }
            
            # -------------------------------------------------------
            # NUMERICAL AND NUMERICAL VARIABLE GROUP BY CATEGORICAL
            # -------------------------------------------------------
            
            for(k in 1:totalColumns){
              
              if(categoricalColumns[k] == TRUE){
                
                thirdVariableName   = colnames(automaticTable[k])
                
                plotName_automatic = paste("regression_combine_",i,"_",j,"_",k,"_",variableName,"_",otherVariableName,"_",thirdVariableName,".png",sep="")
                filepath_automatic = file.path(paste(automaticFolder, plotName_automatic,      sep = ""))
                
                if(saveRegressionPlots == TRUE){
                  
                  print(paste("    Doing combine regression for ",variableName," and ",otherVariableName, "; highlighted by ", thirdVariableName ,sep=""))
                  
                  
                  
                  # Set up the special color vector if any
                  usingColorVector = NULL
                  if(k == sexIndex) usingColorVector = colorVectorSex

                  myResults = doCombineRegression(automaticTable, i, j, k, usingColorVector, filepath_automatic, borders = "density",
                                                  plotTitle = "Combine regression", plotSubtitle = NULL, plotCaption = NULL, plotXLabel = variableName, plotYLabel = otherVariableName)
                  
                  
                }
                
              }
              
            }
            
            
          }                   
          
        }
        
      }

    }
    
  }
  
}


# Save the results for proper analysis on a file
# P-values
write.csv2(summaryTable, pvaluesCSVPath)



# # Plot everything
# for(i in 1:totalColumns){
#   
#   # First check if it is a variable that we need to take into account when ploting
#   if(maskColumns[i] == TRUE){
#     
#     # Get the variable name
#     variableName       = colnames(automaticTable[i])
#     
#     # --------------------------------------
#     # CATEGORICAL VARIABLE
#     # --------------------------------------
#     if(categoricalColumns[i] == TRUE){
# 
#       #  Check how many categories we have, and if we excess the limit
#       firstCategoryList  = unique(automaticTable[,i])
#       firstCategoryTotal = length(firstCategoryList)
#       
#       if(firstCategoryTotal<categoryLimit){
#         
#         # --------------------------------------------------
#         # ONLY ONE CATEGORICAL VARIABLE with nothing else
#         # --------------------------------------------------
#         {
#           
#           # Create automatic filename and filepath
#           plotName_automatic = paste("barplot_",i,"_",variableName,".png",sep="")
#           filepath_automatic = file.path(paste(automaticFolder, plotName_automatic,      sep = ""))
#           
#           # Set up the special color vector if any
#           usingColorVector = NULL
#           if(i == sexIndex) usingColorVector = colorVectorSex
#           
#           # Barplot
#           if(saveBarPlots == TRUE){
#             
#             print(paste("Doing barplot for ",variableName, sep=""))
#             
#             automaticTitle = paste("Abs. Frequency for ",variableName, sep="")
#             
#             doBarPlot(automaticTable, i, filepath_automatic, colorsVector = usingColorVector,
#                       plotTitle = automaticTitle, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
#             
#           }
#           
#         }
#         
#         
#         # -------------------------------------------
#         # ONE CATEGORY WITH SOMETHING ELSE
#         # -------------------------------------------
#         {
#           
#           # For every other column
#           for(j in 1:totalColumns){
#             
#             # Check if it is a variable that we need to take into account when ploting
#             if(maskColumns[j] == TRUE){
#               
#               # Check that is not the same column we just analyze
#               # That wouldn't make any sense for multivariable analysis
#               if(i!=j){
#                 
#                 # Get the name
#                 otherVariableName   = colnames(automaticTable[j])
#                 
#                 # -----------------------------------------
#                 # TWO CATEGORIES GROUPED TOGUETHER
#                 # -----------------------------------------
#                 {
#                   
#                   if(categoricalColumns[j] == TRUE){
#                     
#                     # Get the number of categories
#                     secondCategoryList  = unique(automaticTable[,j])
#                     secondCategoryTotal = length(secondCategoryList)
#                     
#                     # If we are under the limit of categories
#                     if(secondCategoryTotal < categoryLimit){
#                       
#                       # Create automatic filename and filepath
#                       plotName_automatic1 = paste("barplotDodge_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                       filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
#                       
#                       plotName_automatic2 = paste("barplotStack_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                       filepath_automatic2 = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
#                       
#                       plotName_automatic3 = paste("barplotRelative_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                       filepath_automatic3 = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
#                       
#                       # Set up the special color vector if any
#                       usingColorVector = NULL
#                       if(j == sexIndex) usingColorVector = colorVectorSex
#                       
#                       # Barplots
#                       if(saveBarPlots == TRUE){
#                         
#                         print(paste("Doing barplots for ",variableName," and ",otherVariableName, sep=""))
#                         
#                         automaticTitleDodge    = paste("Abs. Frequency for ",variableName, " and ", otherVariableName, sep="")
#                         automaticTitleStack    = paste("Abs. Frequency for ",variableName, " and ", otherVariableName, sep="")
#                         automaticTitleRelative = paste("Rel. Frequency for ",variableName, " and ", otherVariableName, sep="")
#                         
#                         doBarDodgeCombinePlot(automaticTable, i, j, usingColorVector, filepath_automatic1,
#                                               plotTitle = automaticTitleDodge, plotSubtitle = paste("Grouped by ", otherVariableName), plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
# 
#                         doBarStackCombinePlot(automaticTable, i, j, usingColorVector, filepath_automatic2,
#                                               plotTitle = automaticTitleStack, plotSubtitle = paste("Grouped by ", otherVariableName), plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
#                         
#                         doBarRelativeCombinePlot(automaticTable, i, j, usingColorVector, filepath_automatic2,
#                                                  plotTitle = automaticTitleRelative, plotSubtitle = paste("Grouped by ", otherVariableName), plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
#                         
#                       }
#                       
#                     }
#                     
#                   }
#                   
#                 }
#                 
#                 
#                 # -------------------------------------------------
#                 # ONE CATEGORICAL VARIABLE WITH NUMERICAL VARIABLE
#                 # -------------------------------------------------
#                 
#                 {
#                   
#                   if(categoricalColumns[j] == FALSE){
#                     
#                     # Create automatic filename and filepath
#                     plotName_automatic1 = paste("boxplot_combine_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                     filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
#                     
#                     plotName_automatic2 = paste("boxplot_combine_pValues_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                     filepath_automatic2 = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
#                     
#                     plotName_automatic3 = paste("densityplot_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                     filepath_automatic3 = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
#                     
#                     # Set up the special color vector if any
#                     usingColorVector = NULL
#                     if(i == sexIndex) usingColorVector = colorVectorSex
#                     
#                     # Do the boxplots
#                     if(saveBoxPlots == TRUE){
#                       
#                       print(paste("Doing boxplots for ",variableName," and ",otherVariableName, sep=""))
#                       
#                       automaticTitle = paste("Boxplot for ",variableName, " and " , otherVariableName, sep="")
#                       
#                       doBoxPlotPValues(automaticTable, i, j, usingColorVector, filepath_automatic2,
#                                        plotTitle = automaticTitle, plotSubtitle = "P-value under boxs", plotCaption = NULL, plotXLabel = variableName, plotYLabel = "Total")
#                       
#                     }
#                     
#                     # Do the density plots
#                     if(saveDensityPlots == TRUE){
#                       
#                       print(paste("Doing densityplot for ",variableName," and ",otherVariableName, sep=""))
#                       
#                       automaticTitle = paste("Density for ",variableName, " and " , otherVariableName, sep="")
#                       
#                       doDensityPlot(automaticTable, i, j, usingColorVector, plotFilePath = filepath_automatic3,
#                                     plotTitle = automaticTitle, plotSubtitle = "Integers can cause wonky shapes", plotCaption = NULL, plotXLabel = otherVariableName, plotYLabel = "Rel. Frequency")                      
#                       
#                     }
#                     
# 
#                     
#                   }
#                   
#                 }
#                 
#                 
#                 
#               }
#               
#             }
#             
#           }
#           
#           
#           
#         }
#         
#         
#         
#       }
#       
#       
#     }
#     
# 
#     
#     # --------------------------------------
#     # NUMERICAL VARIABLE
#     # --------------------------------------
#     else{
#       
#       # --------------------------------------
#       # ONLY NUMERICAL VARIABLE
#       # --------------------------------------
#       
#       # (nothing yet)
#       
#       # --------------------------------------
#       # NUMERICAL VARIABLE with something else
#       # --------------------------------------
# 
#         for(j in 1:totalColumns){
#            
#             # Check if it is a variable that we need to take into account when ploting
#             if(maskColumns[j] == TRUE){
#              
#                # Don't run analysis againts yourself
#                if(i!=j){
#                
#                  # Get the other variable name
#                  otherVariableName   = colnames(automaticTable[j])
#                  
#                  # --------------------------------------
#                  # NUMERICAL AND CATEGORICAL VARIABLE
#                  # --------------------------------------
#                  
#                  # (this is already done in category with numerical :P )
#                  
#                  # --------------------------------------
#                  # NUMERICAL AND NUMERICAL VARIABLE
#                  # --------------------------------------
#                  {
#                    
#                    if(categoricalColumns[j] == FALSE){
#                      
#                      # Create automatic filename and filepath
#                      plotName_automatic1 = paste("regression_simple_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
#                      filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
#                      
#                      if(saveRegressionPlots == TRUE){
#                        
#                        print(paste("Doing regression for ",variableName," and ",otherVariableName, sep=""))
#                        
#                        #doSimpleRegression(phenotypeTable, i, j, NULL, filepath_automatic1)
#                        
#                        doSimpleRegression(automaticTable, i, j, filepath_automatic1,
#                                           plotTitle = "Regression", plotSubtitle = "The further from 0 the better", plotCaption = NULL, plotXLabel = variableName, plotYLabel = otherVariableName)
#                        
#                      }
#                      
#                      # -------------------------------------------------------
#                      # NUMERICAL AND NUMERICAL VARIABLE GROUP BY CATEGORICAL
#                      # -------------------------------------------------------
#                      
#                      for(k in 1:totalColumns){
#                        
#                        if(categoricalColumns[k] == TRUE){
#                          
#                          thirdVariableName   = colnames(phenotypeTable[k])
#                                           
#                          plotName_automatic = paste("regression_combine_",i,"_",j,"_",k,"_",variableName,"_",otherVariableName,"_",thirdVariableName,".png",sep="")
#                          filepath_automatic = file.path(paste(automaticFolder, plotName_automatic2,      sep = ""))
#                                           
#                          if(saveRegressionPlots == TRUE){
#                                             
#                              print(paste("    Doing combine regression for ",variableName," and ",otherVariableName, "; highlighted by ", thirdVariableName ,sep=""))
#                             
#                            
#                            
#                              # Set up the special color vector if any
#                              usingColorVector = NULL
#                              if(k == sexIndex) usingColorVector = colorVectorSex
#                            
#                              doCombineRegression(automaticTable, i, j, k, colorsVector, filepath_automatic, borders = TRUE,
#                                                  plotTitle = "Combine regression", plotSubtitle = NULL, plotCaption = NULL, plotXLabel = variableName, plotYLabel = otherVariableName)
# 
#                              
#                           }
#                          
#                        }
#                        
#                      }
#                      
#                      
#                    }                   
#                    
#                  }
# 
#                }
#               
#             }
#           
#         }
#       
# 
#       
#     }
    
    
    
    
    
    
    
    
        
    # If we are in a numerical type
    # --------------------------------------
    
    # else{
    #   
    #   # ONLY ONE NUMERICAL VARIABLE
    #   # -------------------------------------------
    #   
    #   # Get the variable name
    #   variableName       = colnames(phenotypeTable[i])
    #   
    #   # Create automatic filename and filepath
    #   plotName_automatic = paste("boxplot_",i,"_",variableName,".png",sep="")
    #   filepath_automatic = file.path(paste(automaticFolder, plotName_automatic,      sep = ""))
    #   
    #   if(saveBoxPlots == TRUE){
    #     print(paste("Doing boxplots for ",variableName, sep=""))
    #     
    #     do1Boxplot(phenotypeTable, i, NULL, filepath_automatic)
    #   }
    #  
    #   # ONE NUMERICAL WITH SOMETHING ELSE
    #   # -------------------------------------------
    #   for(j in 1:totalColumns){
    #     
    #     # Check if it is a variable that we need to take into account when ploting
    #     if(maskColumns[j] == TRUE){
    #       
    #       if(i!=j){
    #         
    #         otherVariableName   = colnames(phenotypeTable[j])
    #         
    #         # ONE CATEGORICAL VARIABLE WITH NUMERICAL VARIABLE (We don't do that here, is already done in the categorical part)
    #         # -----------------------------------------------------------------------------------------------------------------
    #         # (Nope)
    #         
    #         # ONE NUMERICAL VARIABLE WITH NUMERICAL VARIABLE 
    #         # -----------------------------------------------
    #         if(categoricalColumns[j] == FALSE){
    #           
    #           # Create automatic filename and filepath
    #           plotName_automatic1 = paste("regression_simple_",i,"_",j,"_",variableName,"_",otherVariableName,".png",sep="")
    #           filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
    #           
    #           if(saveRegressionPlots == TRUE){
    #           
    #             print(paste("Doing regression for ",variableName," and ",otherVariableName, sep=""))
    #             
    #             #doSimpleRegression(phenotypeTable, i, j, NULL, filepath_automatic1)  
    #             
    #           }
    #           
    #           # TWO NUMERICAL WITH SOMETHING ELSE
    #           # -------------------------------------------
    #           
    #           for(k in 1:totalColumns){
    #             
    #             # Check if it is a variable that we need to take into account when ploting
    #             if(maskColumns[k] == TRUE){
    #             
    #               thirdVariableName   = colnames(phenotypeTable[k])
    #                 
    #               # Check that we have a categorical variable
    #               if(categoricalColumns[k] == TRUE){
    #                 
    #                 plotName_automatic1 = paste("regression_simple_colored_",i,"_",j,"_",k,"_",variableName,"_",otherVariableName,"_",thirdVariableName,".png",sep="")
    #                 filepath_automatic1 = file.path(paste(automaticFolder, plotName_automatic1,      sep = ""))
    #                 
    #                 if(saveRegressionPlots == TRUE){
    #                   
    #                   print(paste("Doing colored regression for ",variableName," and ",otherVariableName, "; highlighted by ", thirdVariableName ,sep=""))
    #                   
    #                   #doSimpleColorRegression(phenotypeTable, i, j, k, NULL, filepath_automatic1)
    #                   doCombineRegression(    phenotypeTable, i, j, k, NULL, filepath_automatic1)
    #                   
    #                 }
    #                 
    #               }
    #               
    #             }
    #             
    #           }
    #           
    #         }
    #         
    #       }
    #       
    #     }
    #     
    #   }
    #   
    # }
    
#   }
#   
# }


