
# This is not true (yet):
#
# This script generate the proper latex file, but it DOESN'T compile it.
# You need to manually go to your preferred latex editor and do that there
#


# This script copy everything in the result folder into the latex folder
# Later on, you compile your latex document and it should be updated with
# the new info


LATEX_SRC_RELATIVE_PATH_TO_ALL = "../img/results/all/"
#LATEX_FOLDER_IMAGES_RESULTS_LOGS
LATEX_SUMMARY_FILE             = paste(LATEX_FOLDER_IMAGES_RESULTS_ALL, "General_MetaVariablesSummary.tex", sep="")

# -----------------------------------------------------------------
#     COPY EVERYTHING
# -----------------------------------------------------------------
{
  
  # Copy the last result folder into the image folder.
  # This will overwrite everything there not matter what it was.
  
  # You need to copy each folder one by one
  # Also, we don't care about the fake data or the automatic (yet)
  
  file.copy( from = GENERAL_FOLDER, to = LATEX_FOLDER_IMAGES_RESULTS_GENERAL, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  file.copy( from = NETWORK_FOLDER, to = LATEX_FOLDER_IMAGES_RESULTS_NETWORK, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  file.copy( from = AUREUS_FOLDER,  to = LATEX_FOLDER_IMAGES_RESULTS_AUREUS,  overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  file.copy( from = CONTROL_FOLDER, to = LATEX_FOLDER_IMAGES_RESULTS_CONTROL, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  file.copy( from = LOGS_FOLDER,    to = LATEX_FOLDER_IMAGES_RESULTS_LOGS,    overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  
  # This is for copying the content of each folder in a one unique folder called "all"
  # ---- Get all the files that you want to copy
  print("Copying files,")
  print("Please wait...")
  
  generalFiles = list.files(GENERAL_FOLDER, full.names = TRUE)
  networkFiles = list.files(NETWORK_FOLDER, full.names = TRUE)
  aureusFiles  = list.files(AUREUS_FOLDER,  full.names = TRUE)
  controlFiles = list.files(CONTROL_FOLDER, full.names = TRUE)
  logFiles     = list.files(LOGS_FOLDER,    full.names = TRUE)
  allFiles     = c(generalFiles, networkFiles, aureusFiles, controlFiles, logFiles)
  
  file.copy( from = allFiles, to = LATEX_FOLDER_IMAGES_RESULTS_ALL, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  
}

# -----------------------------------------------------------------
#     GENERATE METASUMMARIES
# -----------------------------------------------------------------
{
  # Get the proper files
  allFiles          = list.files(GENERAL_FOLDER)
  withSummaryString = grep("_Summary_Table_", allFiles)
  summaryFiles      = allFiles[withSummaryString]
  totalSummaries    = length(summaryFiles)
  
  # If there is at least one, update. Otherwise skip this.
  if(totalSummaries>0){
    
    # The latex string goes here
    latexSummaryString = ""
    
    
    # General summary for all categorical data
    for (i in 1:totalImportantCategoricalIndexes) {
      
      currentVariableName = importantCategoricalNames[i]
      
      # Prepare the subsection
      currentSubsubsection = paste("\\subsubsection{", currentVariableName ,"}",             "\n", sep="")
      
      # Find the correct file for that subsection
      currentFileIndex = grep(paste("_Summary_Table_", currentVariableName, ".tex", sep="") , summaryFiles)
      
      # Add the input latex expression
      currentInput         = paste("\\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, summaryFiles[currentFileIndex], "}" , "\n", sep="")
      
      # Add it to whatever we had before
      latexSummaryString   = paste( latexSummaryString, "\n",  # Add new line
                                    currentSubsubsection,      # Add subsubsection
                                    currentInput , sep="")     # Add the input
    }
    
    
    latexSummaryString   = paste( "\\subsection{Categorical} \n",     # Add the upper section for both
                                  latexSummaryString, "\n",
                                  "\\subsection{Numerical} \n", sep="")
    
    # Same thing for the numerical data
    for (i in 1:totalImportantNumericalIndexes) {
      
      currentVariableName = importantNumericalNames[i]
      
      # Prepare the subsection
      currentSubsubsection = paste("\\subsubsection{", currentVariableName ,"}",             "\n", sep="")
      
      # Find the correct file for that subsection
      currentFileIndex = grep(paste("_Summary_Table_", currentVariableName, ".tex", sep="") , summaryFiles)
      
      # Add the input latex expression
      currentInput         = paste("\\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, summaryFiles[currentFileIndex], "}" , "\n", sep="")
      
      # Add it to whatever we had before
      latexSummaryString   = paste( latexSummaryString, "\n",  # Add new line
                                    currentSubsubsection,      # Add subsubsection
                                    currentInput     , sep="") # Add the input
      
      
    }
    
    # Write the results in disk
    summaryTEXConnection = file(LATEX_SUMMARY_FILE, 'w')
    writeLinesBN(latexSummaryString, summaryTEXConnection)
    
  }
  
}

# -----------------------------------------------------------------
#     TODO: GENERATE ALL THE INPUTS
# -----------------------------------------------------------------
{
  
  # # Get all the files that were copied
  # allFiles          = list.files(LATEX_FOLDER_IMAGES_RESULTS_ALL)
  # 
  # # Divide those files into images files and tables files
  # # -- Tables
  # filesSummaryTables = grep("_Summary_Table_", allFiles)
  # # -- Images
  # filesImages = grep("AbsBarplot", allFiles)
  # filesImages = c(filesImages , grep("Histogram", allFiles)) 
  # 
  # 
  # # Generate all the inputs{} for each variable
  # {
  #   
  #   # Init the latex string
  #   latexSummaryString = ""
  #   
  #   # Categorical
  #   for (i in 1:totalImportantCategoricalIndexes) {
  #     
  #     # Get the variable name
  #     currentVariableName = importantCategoricalNames[i]
  #     
  #     # Prepare the subsection for that variable
  #     currentSubsubsection = paste("\\subsubsection{", currentVariableName ,"}",             "\n", sep="")
  #     
  #     # Find the correct files for that subsection
  #     # -- Summary Table
  #     currentFileIndex = grep(paste("_Summary_Table_", currentVariableName, ".tex", sep="") , summaryFiles)
  #     # -- Each image
  #     
  #     # Add the input latex expression
  #     currentInput         = paste("\\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, filesSummaryTables[currentFileIndex], "}" , "\n", sep="")
  #     
  #     # Add it to whatever we had before
  #     latexSummaryString   = paste( latexSummaryString, "\n",  # Add new line
  #                                   currentSubsubsection,      # Add subsubsection
  #                                   currentInput , sep="")     # Add the input
  #   }
  #   
  #   
  #   latexSummaryString   = paste( "\\subsection{Categorical} \n",     # Add the upper section for both
  #                                 latexSummaryString, "\n",
  #                                 "\\subsection{Numerical} \n", sep="")
  #   
  #   
  #   
  #   
  # }
  # 
  # 

  
}



# TODO Delete this:
# Print the inputs so I don't have to look and copypaste
if(FALSE){
texFiles = grep(".tex" ,  aureusFiles)
for(i in 1:length(texFiles)){
  
  currentIndex = texFiles[i]
  
  # Get the filename with no path
  fileName = paste0(getFileNameNoExtension(aureusFiles[currentIndex]),".tex")
  
  # Add the input latex expression
  currentInput         = paste("\\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, fileName, "}" , sep="")
  
  print(currentInput)
  
  
}
}



# Now, from the ALL FILES folder, move away all the latex tables to the latex table folder
# TODO


# Now you need to find out how many plots we have and the type of each.

# You will always have 3 type of files:
# -- PNG
# -- TXT
# -- TEX

# So you only need to look for one of them, let say PNG and is guarantee that you will find the other 2 as well.








# I have no idea why I made this, save it for later:
if(FALSE){
  
  
  # By type of plot
  absBarplotsCount  = 0
  relBarplotsCount  = 0
  QQPlotsCount      = 0
  graphPlotsCount   = 0
  unknownPlotsCount = 0
  
  # By categorical/numerical
  oneCategoryCount = 0
  twoCategoryCount = 0
  
  
  absBarplotsList  = list()
  relBarplotsList  = list()
  QQPlotsList      = list()
  
  
  # For each folder in the latexFolder (there should be 4 , but find them anyway)
  allPNGFiles = list.files(latexFolderALL ,pattern = ".png$", recursive = FALSE)
  totalFiles  = length(allPNGFiles)
  
  if(totalFiles <= 0){
    
    print("")
    print("WARNING!!")
    print("No files found in the LATEX folder!!!")
    print("Did you run the analysis first?")
    print("")
    
  }else{
    
    # For each file
    for(i in 1:totalFiles){
      
      # Init your variables
      
      myVar1 = ""
      myVar2 = ""
      myVar3 = ""
      myVar4 = ""
      myVar5 = ""
      
      plotFound = FALSE
      
      #Figure it out what type of file you have
      myFileName = allPNGFiles[i]
      auxUnderscore = (strsplit(myFileName, "_"))[[1]]
      
      # The type of plot you have, depending on that, you have the variables in one place or another
      thisManyUnderscores = length(auxUnderscore)
      
      auxFinal = auxUnderscore[thisManyUnderscores]
      myPlotType = (strsplit(auxFinal, "\\."))[[1]][1]
      
      print(auxUnderscore)
      print(thisManyUnderscores)
      print(auxFinal)
      print(myPlotType)
      
      # -- ABSBAR
      if(myPlotType == "AbsBar") {
        
        plotFound = TRUE
        absBarplotsCount = absBarplotsCount + 1
        
        # Each of the variables
        #myVar1 = auxUnderscore[1]
        #myVar2 = auxUnderscore[2]
        #myVar3 = auxUnderscore[3]
        
      }
      
      # -- RELBAR
      
      # -- GRAPH
      if(myPlotType == "AbsBar") {
        
        plotFound = TRUE
        graphPlotsCount = graphPlotsCount + 1  
        
      }
      
      if(plotFound == FALSE){
        
        unknownPlotsCount = unknownPlotsCount + 1
        
      }
      
      
      
      # Each of the possible files types
      myPNGPath = myFilePath
      myTXTPath = paste( gsub('.{3}$', '', myPNGPath)  , "txt" , sep=''  )
      myTEXPath = paste( gsub('.{3}$', '', myPNGPath)  , "tex" , sep=''  )
      
      print(myPlotType)
      print(myPNGPath)
      print(myTXTPath)
      print(myTEXPath)
      
      
      
      
      
    }
    
  }
  
  
  
  # Now we have finish analysing what plots were generated.
  # We are ready to start making our own LATEX document based on this results
  
  
}


