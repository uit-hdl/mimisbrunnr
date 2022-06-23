# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(ISLR) # ??
library(pscl) # ??
library(scales) # ??
library(car)    # ??
#library(caret)
#library(rcompanion)
#library(oddsratio)


library(ergm)
library(statnet)

source("indexes.R",      encoding="utf-8")


YOUNG_LIMIT      = 16
POPULARITY_LIMIT = 3
totalSimulations = 1000
TOTAL_SUBJECTS   = nrow(completeTable)

# -----------------------------------
#     Log for the inputs{} in latex
# -----------------------------------
{
  
  logTXTFileConnection = file(AUREUS_LOG_PATH, 'w')
  logLine              = paste( "AUREUS DATA LOG at: ", RIGHT_NOW, sep = "")
  write( logLine ,
         file = logTXTFileConnection,
         append = FALSE)
}


# ********************************************
#     GENERAL STATISTIC SUMMARY TABLES
#
#     Count how many positives we have for
#     each variables, and find the relative
#     count for men and women.
# ********************************************
print("General statistics")
if(TRUE){
  
  # Prepare the dataframe with the results
  carrierSummaryDF  = data.frame(matrix(NA, nrow = totalConsecuenceIndexes, ncol = 4))
  colnames(carrierSummaryDF) = c("Name", "Positive", "Men", "Women")
  
  # Init each of the rows of the DF
  for (i in 1:totalConsecuenceIndexes){
  
    # Get and set the name
    currentName = consecuenceNames[i]
    carrierSummaryDF$Name[i] = currentName
      
    # Look in the table for the column corresponding to that name
    currentIndex = grep(paste0("^",currentName,"$"), colnames(completeTable))
    
    # Count each of the possibilities
    carrierSummaryDF$Positive[i] = sum(completeTable[,currentIndex]  == "Positive") 
    carrierSummaryDF$Men[i]      = paste0(round((sum(menOnlyTable[,currentIndex]   == "Positive") / totalMenRows)   * 100,2),"%")
    carrierSummaryDF$Women[i]    = paste0(round((sum(womenOnlyTable[,currentIndex] == "Positive") / totalWomenRows) * 100,2),"%") 

  }

  # Prepare a dataframe so we can plot this with long format in ggplot (this step is very stupid)
  {
    
    # Absolute count
    {
      # Create the DF
      totalCounts = sum(carrierSummaryDF[,2])
      ggplotAbleDFCount = data.frame(matrix(NA, nrow = totalCounts, ncol = 1))
      colnames(ggplotAbleDFCount) = c("Variable")
      # Add the data
      currentIndex = 0
      for(i in 1:nrow(carrierSummaryDF)){
        
        currentRowName  = carrierSummaryDF[i,1]
        currentRowCount = carrierSummaryDF[i,2]
        
        for(j in 1:currentRowCount){
          ggplotAbleDFCount[(j+currentIndex),1] = currentRowName
        }
        
        currentIndex = currentIndex + currentRowCount
        
      }
      # Factor the variable name but preserve the original order
      ggplotAbleDFCount$Variable = factor(ggplotAbleDFCount$Variable, levels = c((unique(ggplotAbleDFCount$Variable))))
      
    }
    
    # Relative count (I hate R so much really, there is so much inconsistency, can't use melt() here at all for example to make this easier, some times factors are relevant sometimes not. If it were up to me I would banish it from any university and force everyone back to C++)
    {
      # ggplotAbleDFRelative = data.frame(matrix("Woman", nrow = TOTAL_SUBJECTS, ncol = nrow(carrierSummaryDF)), stringsAsFactors=FALSE)
      # colnames(ggplotAbleDFRelative) = carrierSummaryDF[,1]
      # 
      # for (j in 1:nrow(carrierSummaryDF)){
      #   
      #   # Get the total Men
      #   totalCurrentMen = carrierSummaryDF[j,3]
      #   
      #   # Init those variables
      #   for (i in 1:totalCurrentMen){
      #     
      #     ggplotAbleDFRelative[i,j] = "Man"
      #     
      #   }
      #   
      # }
      # 
      # ggplotAbleDFRelative2 = data.frame(matrix(NA, nrow = (TOTAL_SUBJECTS * nrow(carrierSummaryDF) ), ncol = 2 ))
      # colnames(ggplotAbleDFRelative2) = c("Variable", "Sex")
      # 
      # currentIndex = 1
      # for (j in 1:ncol(ggplotAbleDFRelative)){
      #   
      #   for (i in 1:nrow(ggplotAbleDFRelative)){
      #     
      #     currentColumnName = as.character(colnames(ggplotAbleDFRelative)[j])
      #     currentSex        = as.character(ggplotAbleDFRelative[i,j])
      #     
      #     ggplotAbleDFRelative2[currentIndex,1] = currentColumnName
      #     ggplotAbleDFRelative2[currentIndex,2] = currentSex
      #     
      #     currentIndex = currentIndex + 1
      #     
      #   }
      #   
      # }
      # 
      # ggplotAbleDFRelative2$Variable = factor(ggplotAbleDFRelative2$Variable, levels = c((unique(ggplotAbleDFRelative2$Variable))))
      # ggplotAbleDFRelative2$Sex      = factor(ggplotAbleDFRelative2$Sex,      levels = c("Man", "Woman"))      
    }

    
  }
  
  # Write tables into disk
  
  summaryTable = writeTableLATEX(carrierSummaryDF, AUREUS_FOLDER,
                                 tableCaption = "Summary of total positives for all relevant variables")

  absplotResults = doLongBarPlot(ggplotAbleDFCount, 1, AUREUS_FOLDER,
                                 overrideCaption = "Absolute count for each variable type",
                                 plotTitle = "", plotXLabel = "", plotYLabel = "",
                                 sort = "none")
  
  
  # Prepare the dataframe with prevalence for each category
  {
    # Create the DF where to save the data
    # -- Each of the categorical variables
    importantTotalRows = 0
    for (i in 1:totalImportantCategoricalIndexes) {
      
      importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
      totalImportantModalities = length(importantModalities)
      
      importantTotalRows = importantTotalRows + totalImportantModalities
      
    }
    
    # -- Each of the consequence variables
    importantTotalRows = importantTotalRows

    # -- Create the dataframe
    prevalenceDF           = data.frame(matrix(NA, nrow = importantTotalRows, ncol = 5))
    colnames(prevalenceDF) = c("Variable", "Modality", "Index", "Direct Culture", "Enrichment")
        
    # Initialize the concepts
    # -- Base categories
    importantIndex = 1
    for (i in 1:totalImportantCategoricalIndexes) {
      
      # Get the name of the variable
      currentImportantCategory = importantCategoricalNames[i]
      importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
      totalImportantModalities = length(importantModalities)
      
      for (j in 1:totalImportantModalities) {
        
        prevalenceDF[importantIndex,1] = currentImportantCategory
        prevalenceDF[importantIndex,2] = importantModalities[j]
        prevalenceDF[importantIndex,3] = importantCategoricalIndexes[i]
        
        importantIndex = importantIndex + 1
      }
      
    }
    
  
    # Fill the dataframe
    for(i in 1:importantTotalRows){
      
      # Get the indexes
      currentIndex        = prevalenceDF[i,3]
      currentModalityName = prevalenceDF[i,2]
      
      # Find the people that correspond to this filtering
      currentFiltering    = completeTable[,currentIndex] == currentModalityName
      
      # Count them
      currentTotalPeople  = sum(currentFiltering)
      
      # Add the results to the table
      prevalenceDF[i,4] = sum(completeTable[currentFiltering, nasalCoagulaseCarrierIndex]  == "Positive")/currentTotalPeople
      prevalenceDF[i,5] = sum(completeTable[currentFiltering, nasalEnrichmentCarrierIndex] == "Positive")/currentTotalPeople
      
      # Round it
      prevalenceDF[i,4] = round(prevalenceDF[i,4],3)*100
      prevalenceDF[i,5] = round(prevalenceDF[i,5],3)*100
    }
    
    
    
    
    
    
  }

  

  
  
}

# ********************************************
#     DATES FOR SWAPS AND HIGHSCHOOL + FRIENDS COVERING
# ********************************************
print("Dates processing")
if(TRUE){
  
  myDates  = completeTable[,dateIndexComplete]
  myHS     = sort(as.character(unique(completeTable[,highSchoolIndex])))
  
  # The first date is 2010-09-20, which is Week 38 of 2010
  # The last  date is 2011-04-27, which is Week 17 of 2010
  # 2010 has 52 weeks (last day ending in Saturday)
  totalWeeks = 52-38+17+1
  
  datesDF  = data.frame(matrix(0, nrow = totalWeeks, ncol = 4 + length(myHS)))
  colnames(datesDF) = c("Week", "Year", myHS, "Friends", "IDs")
  
  # Initialize the dataframe
  currentWeek = 38
  currentYear = 2010
  
  for(i in 1:totalWeeks){
  
    datesDF$Week[i] = currentWeek
    datesDF$Year[i] = currentYear
    datesDF$IDs[i]  = ""
    
    currentWeek = currentWeek + 1
    if(currentWeek == 53){
      currentWeek = 1
      currentYear = 2011
    } 
    
  }
  
  # Find out how many people don't have valid or unregistered dates
  totalUnknownDates = sum(is.na(myDates))
  
  # Start filling the dataframe
  for(i in 1:TOTAL_SUBJECTS){
    
    # Check if the date is valid
    currentDate = myDates[i]
    
    # If the date is valid, do stuff
    if(!is.na(currentDate)){
      
      # Get the data for that row
      currentYear = year(ymd(currentDate))
      currentWeek = as.numeric( strftime(currentDate, format = "%V"))
      currentHS   = as.numeric(substring(completeTable[i,highSchoolIndex], 2))
      currentID   = completeTable$ID[i]
      
      # Find the proper row in the dataframe that correspond with this Year and Week
      # Since weeks doesn't overlap, we can find the Week only
      currentIndex = as.numeric(rownames(datesDF[datesDF$Week==currentWeek,]))

      # Add the total to the proper highshool
      currentHSTotal = datesDF[currentIndex, (2+currentHS)]
      datesDF[currentIndex, (2+currentHS)] = currentHSTotal + 1
      
      # Add this person ID to the list of IDs
      datesDF$IDs[currentIndex] = paste0(datesDF$IDs[currentIndex]," ", currentID)
      
    }

  }
  
  # Find out the friend coverage
  for(i in 1:totalWeeks){
    
    # Get the list of people that were this week, week before, or week after doing the test
    currentList = as.numeric(strsplit(datesDF$IDs[i]," ")[[1]])
    # The first one is always NA
    currentList  = currentList[2:length(currentList)]
    currentTotal = length(currentList)
    
    # For each ID, find out how many of his friends attended swabbing that week
    currentAverage = 0
    
    for (j in 1:currentTotal) {
    
      currentID = currentList[j]
      
      nominatedByThisID = overallEdgesDF[overallEdgesDF$from == currentID,]$to
      totalNominations  = length(nominatedByThisID)
      
      # If you nominated more than one person, count how many we have
      if(totalNominations>0){
        
        myAverage = sum(nominatedByThisID %in% currentList)/totalNominations
        currentAverage = myAverage + currentAverage
        
      }
      # If you don't nominated anyone, technically 100% of your friends are there
      else currentAverage = currentAverage + 1
        
    }
    
    currentAverage = currentAverage/currentTotal
    
    # Register the Friends coverage average
    datesDF$Friends[i] = round(currentAverage,4)
    
  }
  
  # Find the average of friendship
  averageFrienship = 0
  for(i in 1:totalWeeks){
    
    averageFrienship = averageFrienship + (sum(datesDF[i,3:10]) * datesDF$Friends[i])
    
  }
  print("Weighted average friendship coverage per week")
  print(averageFrienship / sum(datesDF[,3:10]))
  
  # Write the table into disk
  {
    # Convert Frienship into %
    datesDF$Friends = paste(round(datesDF$Friends * 100,2),"%")
    
    # Drop the ID column
    datesDF$IDs = NULL
    
    # Write to latex table
    summaryTable = writeTableLATEX(datesDF, AUREUS_FOLDER,
                                   tableCaption = "Summary swab attendance date statistics")
    
  }
  
}

# ********************************************
#     SNA-TYPING
# ********************************************
print("SNA-typing")
if(TRUE){
  
  # For the positives in the persistent coagulase nasal carrier
  {
    doLongBarPlot(positiveCoagulaseTableOnly, spaT1IndexComplete, AUREUS_FOLDER, 
                  plotTitle = "Total of SPA Typing for Throat1",
                  plotSubtitle = "For positive nasal direct culture persistent carrier only",
                  plotXLabel = "SPA Throat1", plotYLabel = "Total",
                  overrideCaption = "Barplot with the top 15 SPA types.",
                  top = 15, barsFontSize=5, overrideHeigh = 5)
    
    currentSummary = summarizeCategorical(positiveCoagulaseTableOnly, spaT1IndexComplete, crop=15)
    
    overrideTableName   = "SPAT1_C_Nasal_Summary"
    currentTableCaption = "Overview of all the SPAT1 categories for positive nasal direct culture persistent carrier." 
    
    writeTableLATEX(currentSummary, AUREUS_FOLDER, roundMe = 2,
                    tableCaption = currentTableCaption,
                    overrideTableName = overrideTableName)
    
    # How many empty we have in the other variables
    # ---- (They should all be the same number)
    EmptySPAT2 = sum(positiveCoagulaseTableOnly$SPAThroat2 == "")/nrow(positiveCoagulaseTableOnly)
    EmptySPAN1 = sum(positiveCoagulaseTableOnly$SPANasal1  == "")/nrow(positiveCoagulaseTableOnly)
    EmptySPAN2 = sum(positiveCoagulaseTableOnly$SPANasal2  == "")/nrow(positiveCoagulaseTableOnly)
    # How many different we have in SPAT1
    totalUniquesSPAT1 = length(unique(positiveCoagulaseTableOnly$SPAThroat1))
    print("Total Unique SPAT1:")
    print(totalUniquesSPAT1)
    
    # Vitamim D levels with respect the SPA type
    #     ( I don't even know why I did this, someone
    #       ask for it but we don't know whom )
    {
      # There are 250ish different spa types. Get a table with the top 10 only
      # Otherwise the boxplot doesn't make sense
      
      spaSummary     = summarizeCategorical(positiveCoagulaseTableOnly, spaT1IndexComplete)
      candidatesSPAs = as.character(spaSummary[1:10,1])
      
      restrictedSPADF = positiveCoagulaseTableOnly[positiveCoagulaseTableOnly[,spaT1IndexComplete] %in% candidatesSPAs,]
      
      myBoxplotResults = doCategoricalBoxPlot (restrictedSPADF,
                                               spaT1IndexComplete,
                                               vitamimDIndex,
                                               AUREUS_FOLDER,
                                               showPValues  = FALSE)    
      
      # ANOVA
      if(FALSE){
        # Restrict the dataframe
        anovaDF     = restrictedSPADF[,c(spaT1IndexComplete,vitamimDIndex)]  
        # Delete the NA rows
        anovaDF = anovaDF[is.na(anovaDF[,2]) == FALSE,]
        
        colnames(anovaDF) = c("SPA", "VitamimD")
        
        anovaResult = aov(SPA ~ VitamimD, data = anovaDF)
        anovaResult = aov(VitamimD ~ SPA, data = anovaDF)
        summary(anovaResult)
        
      }
    }
  }
  
  # For the positives in the persistent enrichment nasal carrier
  {
    doLongBarPlot(positiveEnrichmentTableOnly, spaT1IndexComplete, AUREUS_FOLDER, 
                  plotTitle = "Total of SPA Typing for Throat1",
                  plotSubtitle = "For positive nasal enrichment persistent carrier only",
                  plotXLabel = "SPA Throat1", plotYLabel = "Total",
                  overrideCaption = "Barplot with the top 15 SPA types.",
                  top = 15, barsFontSize=5, overrideHeigh = 5)
    
    currentSummary = summarizeCategorical(positiveEnrichmentTableOnly, spaT1IndexComplete, crop=15)
    
    overrideTableName   = "SPAT1_E_Nasal_Summary"
    currentTableCaption = "Overview of all the SPAT1 categories for positive nasal enrichment persistent carrier." 
    
    writeTableLATEX(currentSummary, AUREUS_FOLDER, roundMe = 2,
                    tableCaption = currentTableCaption,
                    overrideTableName = overrideTableName)
    
    # How many empty we have in the other variables
    # ---- (They should all be the same number)
    EmptySPAT2 = sum(positiveEnrichmentTableOnly$SPAThroat2 == "")/nrow(positiveEnrichmentTableOnly)
    EmptySPAN1 = sum(positiveEnrichmentTableOnly$SPANasal1  == "")/nrow(positiveEnrichmentTableOnly)
    EmptySPAN2 = sum(positiveEnrichmentTableOnly$SPANasal2  == "")/nrow(positiveEnrichmentTableOnly)
    # How many different we have in SPAT1
    totalUniquesSPAT1 = length(unique(positiveEnrichmentTableOnly$SPAThroat1))
    print("Total Unique SPAT1:")
    print(totalUniquesSPAT1)
  }

  # Check that all samples of SPA are mostly the same
  {
    
    # Gather some statistics regarding the SPA-type
    totalSameSPAT       = 0         # How many rows (with 2 or more SPA types, has all same SPA-type regardless of nose/throat)
    totalDifferentSPAT  = 0         # How many rows (with 2 or more SPA types, has all not same SPA-types, regardless of nose/throat)
    totalAtLeastTwo     = 0         # How many rows have at least 2 different SPA types anywhere
    totalOneSPA         = 0         # How many rows have 1 SPA
    totalZeroSPA        = 0         # How many rows have 0 SPA
    totalTwoSPA         = 0         # How many rows have 2 SPA
    totalThreeSPA       = 0         # How many rows have 3 SPA
    totalFourSPA        = 0         # How many rows have 4 SPA
    sameNtoNSPA         = 0         # How many rows have the same SPA-type in the nose, if both nose were typed
    sameTtoTSPA         = 0         # How many rows have the same SPA-type in the throat, if both nose were typed
    totalNtoTCandidates = 0         # How many rows have at least one SPA in the nose and one SPA in the throat
    totalNtoTCheck      = 0         # How many rows have at least one SPA-type in the nose that coincide with at least one SPA-type in the throat (or viceversa)
    
    # For each person in the table count the statistics
    for(i in 1:TOTAL_SUBJECTS){

      # Reset variables to check if nose and throat coincide
      NasalAndThroatCheckOut  = FALSE
      NasalAndThroatCandidate = FALSE
            
      # Get each of the variables for this row
      currentT1 = completeTable$SPAThroat1[i]
      currentT2 = completeTable$SPAThroat2[i]
      currentN1 = completeTable$SPANasal1[i]
      currentN2 = completeTable$SPANasal2[i]
      
      # Check that those which are not empty, are the same
      notEmptyList = c(currentT1, currentT2, currentN1, currentN2)
      notEmptyList = notEmptyList[notEmptyList != ""]
      notEmptyLenght = length(notEmptyList)
      
      # -- If we have at least two SPA to compare
      if(notEmptyLenght > 2){
        
        # Add the counts to the statistics
        # -- How many rows has at least 2 SPA-types
        totalAtLeastTwo = totalAtLeastTwo + 1
        # -- How many rows have all same/different SPA-types
        if(length(unique(notEmptyList)) == 1 )
          totalSameSPAT = totalSameSPAT + 1
        else
          totalDifferentSPAT = totalDifferentSPAT + 1
        
        # Check if the Nose coincide with the throat
        {
          # -- If the nose 1 is valid
          if(currentN1 != ""){
            # -- And at least one of the throat is valid
            if(currentT1 != "" || currentT2 != ""){
              NasalAndThroatCandidate = TRUE
              if(currentN1 == currentT1) NasalAndThroatCheckOut = TRUE
              if(currentN1 == currentT2) NasalAndThroatCheckOut = TRUE
            }
          }
          # -- Same for nose 2
          if(currentN2 != ""){
            if(currentT1 != "" || currentT2 != ""){
              NasalAndThroatCandidate = TRUE
              if(currentN2 == currentT1) NasalAndThroatCheckOut = TRUE
              if(currentN2 == currentT2) NasalAndThroatCheckOut = TRUE          
            }
          }
          
          if(NasalAndThroatCandidate == TRUE){
            totalNtoTCandidates = totalNtoTCandidates + 1  
            if(NasalAndThroatCheckOut == TRUE) totalNtoTCheck = totalNtoTCheck + 1
          }
        }

        # Check if the Nose coincide with the Nose
        {
          if(currentN1 != "" && currentN2 != "" && currentN1 == currentN2) sameNtoNSPA = sameNtoNSPA + 1
        }
        
        # Check if the Throat coincide with the Throat
        {
          if(currentT1 != "" && currentT2 != "" && currentT1 == currentT2) sameTtoTSPA = sameTtoTSPA + 1
        }
        
        print(paste0(currentN1," ", currentN2, " ", currentT1, " ", currentT2))
        
      }
      
      # -- If we have one or less, just add the info to the statistics summary
      if(notEmptyLenght == 0) totalZeroSPA  = totalZeroSPA + 1
      if(notEmptyLenght == 1) totalOneSPA   = totalOneSPA  + 1
      if(notEmptyLenght == 2) totalTwoSPA   = totalTwoSPA + 1
      if(notEmptyLenght == 3) totalThreeSPA = totalThreeSPA + 1
      if(notEmptyLenght == 4) totalFourSPA  = totalFourSPA + 1

    }
    
    # Show the results
    {
      print("How many rows with no SPA types")
      print(totalZeroSPA)
      print("How many rows with one SPA type")
      print(totalOneSPA)
      print("How many rows with two SPA type")
      print(totalTwoSPA)
      print("How many rows with three SPA type")
      print(totalThreeSPA)
      print("How many rows with four SPA type")
      print(totalFourSPA)
      print("How many rows with at least 2 SPA types")
      print(totalAtLeastTwo)
      print("How many rows with at leat 2, have ALL SPA the same ")
      print(totalSameSPAT)
      print("How many rows with at leat 2, have NOT ALL SPA the same ")
      print(totalDifferentSPAT)
      print("How many rows with at least 2, have same N1 to N2")
      print(sameNtoNSPA)
      print("How many rows with at least 2, have same T1 to T2")
      print(sameTtoTSPA)
      print("How many rows with at least 2, have at least one N and at least one T")
      print(totalNtoTCandidates)
      print("How many rows with at least 2, that have at least one N and at least one T, coincide at least one N with one T")
      print(totalNtoTCheck)    
    }
    

    
    
  }
  
  # Check which variable drives the SPA sharing
  {
    # Creates a triangular matrix with all the IDs, FALSE means they don't share
    # the same SPA type, TRUE means that they do. We limited the matrix to only
    # people with valid SPA type
    
    # The main diagonal must be TRUE (you are equal to yourself)
    
    # The upper triangular matrix must be NA (we only need one part of the matrix)
    
    listOfIDsWithValidSPA = completeTable$ID
    listOfIDsWithValidSPA = listOfIDsWithValidSPA[completeTable$SPAThroat1 != ""]
    
    totalValidIds = length(listOfIDsWithValidSPA)
    
    matrixSPATypes = matrix(NA, nrow = totalValidIds, ncol = totalValidIds)
    for(i in 1:totalValidIds){
      
      for(j in 1:i){
        
        #Get the two IDs that we are comparing
        ID1 = listOfIDsWithValidSPA[i]
        ID2 = listOfIDsWithValidSPA[j]
       
        # Compare the SPAt1
        currentResults = FALSE
        if(completeTable$SPAThroat1[ID1] == completeTable$SPAThroat1[ID2]){
          currentResults = TRUE
        }
        
        # Write the results
        matrixSPATypes[i,j] = currentResults
          
      }
      
    }
    
    # Count how many people share the same SPA-type (take away the main diagonal)
    totalSameSPAtype = sum(matrixSPATypes, na.rm = TRUE) - totalValidIds
    
    # Make another matrix with people sharing the same highschool
    matrixSharingSchool = matrix(NA, nrow = totalValidIds, ncol = totalValidIds)
    
    for(i in 1:totalValidIds){
      
      for(j in 1:i){
        
        #Get the two IDs that we are comparing
        ID1 = listOfIDsWithValidSPA[i]
        ID2 = listOfIDsWithValidSPA[j]
        
        # Compare the SPAt1
        currentResults = FALSE
        if(completeTable$HighSchoolID[ID1] == completeTable$HighSchoolID[ID2]){
          currentResults = TRUE
        }
        
        # Write the results
        matrixSharingSchool[i,j] = currentResults
        
      }
      
    }
    totalSharingSameHS = sum(matrixSharingSchool, na.rm=TRUE) - totalValidIds
    
    # Count how many SPA-types are from people sharing the same high school VS
    # people not sharing same high school
    totalSPASharingSameHS    = 0
    totalSPANotSharingSameHS = 0
    
    for(i in 1:totalValidIds){
      for(j in 1:i){
        
        # If they share the same SPA type, and we are not talking about the 
        # same person here:
        if(matrixSPATypes[i,j] == TRUE && (i != j) ){
          
          if(matrixSharingSchool[i,j] == TRUE)
              totalSPASharingSameHS = totalSPASharingSameHS + 1
          else
              totalSPANotSharingSameHS = totalSPANotSharingSameHS + 1
          
        }
        
      }
    }
    
    print("  --RAW proportion of SPA types--  ")
    print(totalSPASharingSameHS / totalSameSPAtype)
    print(totalSPANotSharingSameHS / totalSameSPAtype)
    
    
    # Now we are going to do something similar but with edges only
    

    # How many edges are to people of the same highschool (900)
    # How many edges are to people of different highschool (100)
    # How many (%) same SPA types are in the edges of people to same highschool
    # How many (%) same SPA types are in the edges of people to different highschool
    totalSameHighschoolEdges                  = 0
    totalDifferentHighschoolEdges             = 0
    totalSameHighschoolEdgesSameSPA           = 0
    totalSameHighschoolEdgesDifferentSPA      = 0
    totalDifferentHighschoolEdgesSameSPA      = 0
    totalDifferentHighschoolEdgesDifferentSPA = 0
    
    
    # For each of the valid IDs
    for(i in 1:totalValidIds){
      
      # Get each person
      currentID = listOfIDsWithValidSPA[i]
      
      # Get the list of friends (undirected)
      currentFriendshipResults = getFrienshipTypes(currentID, overallNetworkDF)
      currentFriends = unique(currentFriendshipResults[[4]], currentFriendshipResults[[5]])
      currentTotalFriends = length(currentFriends)
      
      # For each friend
      if(currentTotalFriends > 0){
        for(j in 1:currentTotalFriends){
          
          # Get the ID
          currentFriendID = currentFriends[j]
          
          # Check if the ID has a valid SPAtype
          currentFriendIDValid = (currentFriendID %in% listOfIDsWithValidSPA)
          
          # If it is a valid ID continue, otherwise, do nothing and ignore it
          if(currentFriendIDValid == TRUE){
          
            # Are they in the same highschool?
            # -- If YES
            if(completeTable$HighSchoolID[currentID] == completeTable$HighSchoolID[currentFriendID]){
              totalSameHighschoolEdges      = totalSameHighschoolEdges + 1
              # Check the SPA and count
              if(completeTable$SPAThroat1[currentID] == completeTable$SPAThroat1[currentFriendID]){
                totalSameHighschoolEdgesSameSPA = totalSameHighschoolEdgesSameSPA  + 1
              }
              else{
                totalSameHighschoolEdgesDifferentSPA = totalSameHighschoolEdgesDifferentSPA + 1
              }
                
            }
            # -- If NO
            else{
              totalDifferentHighschoolEdges = totalDifferentHighschoolEdges + 1
              # Check the SPA and count
              if(completeTable$SPAThroat1[currentID] == completeTable$SPAThroat1[currentFriendID]){
                totalDifferentHighschoolEdgesSameSPA = totalDifferentHighschoolEdgesSameSPA  + 1
              }
              else{
                totalDifferentHighschoolEdgesDifferentSPA = totalDifferentHighschoolEdgesDifferentSPA + 1
              }

            }
              
          }

        }
        
      }
      
    }
    
    
    print("  -- EDGES proportion of SPA types--  ")
    print("")
    # How many edges goes to people that belongs to the same high school
    # (Almost the same as homophily)
    print("  -- Proportion of same highschool edges")
    print(totalSameHighschoolEdges / (totalSameHighschoolEdges + totalDifferentHighschoolEdges))
    print("      -- Same School Edges")
    print(totalSameHighschoolEdges)
    print("         Proportion of same/different SPA types")
    # How many people, who are friends, and who go to the same school, share the same SPA type
    # VS
    # How many people, who are friends, and who go to the same school, don't share the same SPA type
    print(totalSameHighschoolEdgesSameSPA/totalSameHighschoolEdges)
    print(totalSameHighschoolEdgesDifferentSPA/totalSameHighschoolEdges)
    print("      -- Different School Edges")
    print(totalDifferentHighschoolEdges)
    # How many people, who are friends, and who go to different school, share the same SPA type
    # VS
    # How many people, who are friends, and who go to different school, don't share the same SPA type
    print("         Proportion of same/different SPA types")
    print(totalDifferentHighschoolEdgesSameSPA/totalDifferentHighschoolEdges)
    print(totalDifferentHighschoolEdgesDifferentSPA/totalDifferentHighschoolEdges)
    
    
  }
 
  # Check which SPA-type is more popular in each highschool
  {
    
    # Get the list of highshools
    HSList  = sort(as.character(unique(completeTable$HighSchoolID)))
    totalHS = length(HSList)
    
    # For each highschool
    for(i in 1:totalHS){
      
      currentHS = HSList[i]
      
      # Get the people that bellongs to that HS
      peopleInCurrentHS = completeTable[completeTable$HighSchoolID == currentHS,]
      
      # Get the list count of SPAtypes of 
      currentPlotTitle = paste0("Total of SPA Typing for Throat1 in ",currentHS)
      currentTableName = paste0("LongBarPlot_SPAT1_Highschool_",      currentHS)
      
      doLongBarPlot(peopleInCurrentHS, spaT1IndexComplete, AUREUS_FOLDER, 
                    plotTitle = currentPlotTitle,
                    plotXLabel = "SPA Throat1", plotYLabel = "Total",
                    top = 15, barsFontSize=5, overrideHeigh = 5,
                    overrideTableName = currentTableName)
      
      currentSummary = summarizeCategorical(peopleInCurrentHS, spaT1IndexComplete, crop=15)
      
      print(currentSummary)
      
    }
    
    
  }
   
}

# ********************************************
#     GENERAL STATISTIC PLOTS
# ********************************************
if(FALSE){
  
  # NO STRATIFICATION
  # ------------------------------------
  print("No stratification...")
  {
    
    logLine = paste0("\\subsection{ No stratification }" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # SA Carrier status
    
    barplotResults = doBarPlot(completeTable, carrierIndex, AUREUS_FOLDER,
                               colorsVector = COLOR_VECTOR_CARRIER,
                               countingType = "count")
    
    relplotResults = doBarPlot(completeTable, carrierIndex, AUREUS_FOLDER,
                               colorsVector = COLOR_VECTOR_CARRIER,
                               countingType = "identity",
                               polarCoordinates = TRUE)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(barplotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
    # SA Carrier with respect all categorical variables
    for (i in 1:totalImportantCategoricalIndexes) {
      
      logLine = paste0("\\subsubsection{ ", importantCategoricalNames[i]  ," }" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      relplotResultsA = doBarRelativeCombinePlot(completeTable, carrierIndex, importantCategoricalIndexes[i],
                                                 colorsVector = COLOR_VECTOR_CARRIER,
                                                 AUREUS_FOLDER)
      
      relplotResultsB = doBarRelativeCombinePlot(completeTable, importantCategoricalIndexes[i], carrierIndex,
                                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                                 AUREUS_FOLDER)
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResultsA[[3]]), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResultsB[[3]]), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
    }
    
    # SA Carrier with respect all numerical variables

    # -- Age
    
    logLine = paste0("\\subsubsection{ Age }" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    myBoxplotResults = doCategoricalBoxPlot(completeTable, carrierIndex, ageIndex, AUREUS_FOLDER,
                                        colorsVector=COLOR_VECTOR_CARRIER)
    
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = "P-values for carrier status with respect age.", roundMe = 2)
    
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = "Centralities for carrier status with respect age.", roundMe = 2)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
    # -- BMI
    
    logLine = paste0("\\subsubsection{ BMI }" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    myBoxplotResults = doCategoricalBoxPlot(completeTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                                            colorsVector=COLOR_VECTOR_CARRIER)
    
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = "P-values for carrier status with respect BMI.", roundMe = 2)
    
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = "Centralities for carrier status with respect BMI.", roundMe = 2)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
    doBMIPlot(completeTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
              colorsVector = COLOR_VECTOR_CARRIER)
    
  }
  
  # BY SEX
  # ------------------------------------
  print("Stratification by sex...")
  {
    
    logLine = paste0("\\subsection{ By Sex }" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # SA Carrier status
    
    barplotResults = doBarPlot(menOnlyTable, carrierIndex, AUREUS_FOLDER,
                               colorsVector = COLOR_VECTOR_CARRIER,
                               countingType = "count")
    
    relplotResults = doBarPlot(menOnlyTable, carrierIndex, AUREUS_FOLDER,
                               colorsVector = COLOR_VECTOR_CARRIER,
                               countingType = "identity",
                               polarCoordinates = TRUE)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(barplotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    barplotResults = doBarPlot(womenOnlyTable, carrierIndex, AUREUS_FOLDER,
                               colorsVector = COLOR_VECTOR_CARRIER,
                               countingType = "count")
    
    relplotResults = doBarPlot(womenOnlyTable, carrierIndex, AUREUS_FOLDER,
                               colorsVector = COLOR_VECTOR_CARRIER,
                               countingType = "identity",
                               polarCoordinates = TRUE)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(barplotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
    # SA Carrier with respect all categorical variables
    for (i in 1:totalImportantCategoricalIndexes) {
      
      logLine = paste0("\\subsubsection{ ", importantCategoricalNames[i]  ," }" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      relplotResultsA = doBarRelativeCombinePlot(menOnlyTable, carrierIndex, importantCategoricalIndexes[i],
                                                 colorsVector = COLOR_VECTOR_CARRIER,
                                                 AUREUS_FOLDER)
      
      relplotResultsB = doBarRelativeCombinePlot(menOnlyTable, importantCategoricalIndexes[i], carrierIndex,
                                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                                 AUREUS_FOLDER)
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResultsA[[3]]), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResultsB[[3]]), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      relplotResultsA = doBarRelativeCombinePlot(womenOnlyTable, carrierIndex, importantCategoricalIndexes[i],
                                                 colorsVector = COLOR_VECTOR_CARRIER,
                                                 AUREUS_FOLDER)
      
      relplotResultsB = doBarRelativeCombinePlot(womenOnlyTable, importantCategoricalIndexes[i], carrierIndex,
                                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                                 AUREUS_FOLDER)
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResultsA[[3]]), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relplotResultsB[[3]]), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
    }
    
    # SA Carrier with respect all numerical variables
    
    # -- Age
    
    logLine = paste0("\\subsubsection{ Age }" , "\n")
    
    myBoxplotResults = doCategoricalBoxPlot(menOnlyTable, carrierIndex, ageIndex, AUREUS_FOLDER,
                                            colorsVector=COLOR_VECTOR_CARRIER)
    
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = "P-values for carrier status with respect age (men only) .", roundMe = 2)
    
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = "Centralities for carrier status with respect age (men only).", roundMe = 2)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    myBoxplotResults = doCategoricalBoxPlot(womenOnlyTable, carrierIndex, ageIndex, AUREUS_FOLDER,
                                            colorsVector=COLOR_VECTOR_CARRIER)
    
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = "P-values for carrier status with respect age (women only).", roundMe = 2)
    
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = "Centralities for carrier status with respect age (women only).", roundMe = 2)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
    # -- BMI
    
    logLine = paste0("\\subsubsection{ BMI }" , "\n")
    
    myBoxplotResults = doCategoricalBoxPlot(menOnlyTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                                            colorsVector=COLOR_VECTOR_CARRIER)
    
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = "P-values for carrier status with respect BMI (men only).", roundMe = 2)
    
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = "Centralities for carrier status with respect BMI (men only).", roundMe = 2)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    myBoxplotResults = doCategoricalBoxPlot(womenOnlyTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                                            colorsVector=COLOR_VECTOR_CARRIER)
    
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], GENERAL_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = "P-values for carrier status with respect BMI (women only).", roundMe = 2)
    
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], GENERAL_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = "Centralities for carrier status with respect BMI (women only).", roundMe = 2)
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("    \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
    doBMIPlot(menOnlyTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
              colorsVector = COLOR_VECTOR_CARRIER)
    
    doBMIPlot(womenOnlyTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
              colorsVector = COLOR_VECTOR_CARRIER)
    
  }
 
  # Copy
  {
    
    
    
    
    # STRATIFICATION BY AGE
    # ------------------------------------
    print("Stratification by age...")
    {
      # General Description of the variable (copy from General Analysis)
      doHistogramPlot2(completeTable, ageIndex, AUREUS_FOLDER,
                       binsWidth    = 1,
                       plotTitle    = "Age histogram",
                       plotXLabel   = "Age in years", plotYLabel = "Total")
      
      # SA Carrier status
      
      doBarPlot(youngOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "count",
                plotSubtitle = "Young only")
      
      doBarPlot(youngOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "identity",
                plotSubtitle = "Young only",
                polarCoordinates = TRUE)
      
      doBarPlot(oldOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "count",
                plotSubtitle = "Old only")
      
      doBarPlot(oldOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "identity",
                plotSubtitle = "Old only",
                polarCoordinates = TRUE)
      
      # SA Carrier with respect all categorical variables
      for (i in 1:totalImportantCategoricalIndexes) {
        
        doBarRelativeCombinePlot(youngOnlyTable, carrierIndex, importantCategoricalIndexes[i],
                                 colorsVector = COLOR_VECTOR_CARRIER,
                                 plotSubtitle = "Young only",
                                 AUREUS_FOLDER)
        
        doBarRelativeCombinePlot(youngOnlyTable, importantCategoricalIndexes[i], carrierIndex,
                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                 plotSubtitle = "Young only",
                                 AUREUS_FOLDER)
        
        doBarRelativeCombinePlot(oldOnlyTable, carrierIndex, importantCategoricalIndexes[i],
                                 colorsVector = COLOR_VECTOR_CARRIER,
                                 plotSubtitle = "Old only",
                                 AUREUS_FOLDER)
        
        doBarRelativeCombinePlot(oldOnlyTable, importantCategoricalIndexes[i], carrierIndex,
                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                 plotSubtitle = "Old only",
                                 AUREUS_FOLDER)
        
      }
      
      
      
      # -- BMI
      doBoxPlot(youngOnlyTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                colorsVector=COLOR_VECTOR_CARRIER,
                plotSubtitle="Young only")
      
      doBoxPlot(oldOnlyTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                colorsVector=COLOR_VECTOR_CARRIER,
                plotSubtitle="Old only")
      
      doBMIPlot(youngOnlyTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                plotSubtitle="Young only")
      
      doBMIPlot(oldOnlyTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                plotSubtitle="Old only")
      
      
    }
    
    # STRATIFICATION BY NUMBER OF FRIENDS
    # ------------------------------------
    print("Stratification by popularity...")
    {
      doHistogramPlot2(completeTable, overallPopularityIndex, AUREUS_FOLDER,
                       binsWidth    = 1,
                       plotTitle    = " Popularity histogram ",
                       plotXLabel   = " In-Degree, (people who likes you)", plotYLabel = "Total")    
      
      
      # SA Carrier status
      
      doBarPlot(popularOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "count",
                plotSubtitle = "Popular only")
      
      doBarPlot(popularOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "identity",
                plotSubtitle = "Popular only")
      
      doBarPlot(unpopularOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "count",
                plotSubtitle = "Unpopular only")
      
      doBarPlot(unpopularOnlyTable, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                countingType = "identity",
                plotSubtitle = "Unpopular only")
      
      # SA Carrier with respect all categorical variables
      for (i in 1:totalImportantCategoricalIndexes) {
        
        doBarRelativeCombinePlot(popularOnlyTable, carrierIndex, importantCategoricalIndexes[i],
                                 colorsVector = COLOR_VECTOR_CARRIER,
                                 plotSubtitle = "Popular only",
                                 AUREUS_FOLDER)
        
        doBarRelativeCombinePlot(popularOnlyTable, importantCategoricalIndexes[i], carrierIndex,
                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                 plotSubtitle = "Popular only",
                                 AUREUS_FOLDER)
        
        doBarRelativeCombinePlot(unpopularOnlyTable, carrierIndex, importantCategoricalIndexes[i],
                                 colorsVector = COLOR_VECTOR_CARRIER,
                                 plotSubtitle = "Unpopular only",
                                 AUREUS_FOLDER)
        
        doBarRelativeCombinePlot(unpopularOnlyTable, importantCategoricalIndexes[i], carrierIndex,
                                 colorsVector = myListOfColorVectors[[importantCategoricalIndexes[i]]],
                                 plotSubtitle = "Unpopular only",
                                 AUREUS_FOLDER)
        
      }
      
      # -- Age
      doBoxPlot(popularOnlyTable, carrierIndex, ageIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                plotSubtitle = "Popular only")
      
      doBoxPlot(unpopularOnlyTable, carrierIndex, ageIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                plotSubtitle = "Unpopular only")
      
      # -- BMI
      doBoxPlot(popularOnlyTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                colorsVector=COLOR_VECTOR_CARRIER,
                plotSubtitle = "Popular only")
      
      doBoxPlot(unpopularOnlyTable, carrierIndex, BMIIndex, AUREUS_FOLDER,
                colorsVector=COLOR_VECTOR_CARRIER,
                plotSubtitle = "Unpopular only")
      
      doBMIPlot(popularOnlyTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                plotSubtitle = "Popular only")
      
      doBMIPlot(unpopularOnlyTable, BMIIndex, carrierIndex, AUREUS_FOLDER,
                colorsVector = COLOR_VECTOR_CARRIER,
                plotSubtitle = "Unpopular only")
      
    }
    
  }
  
   
}

# ********************************************
#     CARRIER XI-TABLES 2
#     This was an attemp to do all the possible
#     combinations, but is not really interesting
#     so maybe delete in the future.
# ********************************************
if(FALSE){
  
  # take away the SA index and prepare the target indexes with SA, nasal, and throat
  {
    referenceIndexes = importantCategoricalIndexes[1:(length(importantCategoricalIndexes)-1)]
    targetIndexes    = c(nasalCarrierIndex, throatCarrierIndex, carrierIndex)
  }
  #referenceIndexes = referenceIndexes[1:3]
  
  currentTargetName = variablesInfoDF$VariableID[targetIndexes[3]]
  
  # Prepare the array with the indexes, here also have all the needed info
  # sorted by rows.
  {
    currentVariablesInfoDF     = variablesInfoDF[referenceIndexes,]
    currentVariablesInfoDF[,4] = 0
    currentTotalVariables      = nrow(currentVariablesInfoDF)
    newNames    = colnames(currentVariablesInfoDF)
    newNames[4] = "level"
    colnames(currentVariablesInfoDF) = newNames    
  }

  # Prepare a place from where to read the modalities
  targetTotalModalities  = variablesInfoDF[targetIndexes[3],3]
  targetModalities       = as.character(summarizeCategorical(completeTable, targetIndexes[3])[,1])
  maximumModalities      = max(currentVariablesInfoDF[,3])
  currentModalitiesDF    = data.frame(matrix(NA, nrow = currentTotalVariables, ncol = maximumModalities))
  for (i in 1:currentTotalVariables) {
    
    listOfModalities = as.character(summarizeCategorical(completeTable, referenceIndexes[i], sorted="None")[,1])
    
    for (j in 1:currentVariablesInfoDF[i,3]){
      
      currentModalitiesDF[i,j] = listOfModalities[j]
      
    }
    
  }
  
  # Prepare a place where we are going to save the results
  {
    totalCombos = findWeirdCombo(currentVariablesInfoDF[,3])
    xiComboResultsDF = data.frame(matrix(NA, nrow = totalCombos, ncol = currentTotalVariables + 3 + targetTotalModalities + 1))
    colnames(xiComboResultsDF) = c("nRows", "Variable", currentVariablesInfoDF[,1], "pvalue", targetModalities, "Extreme")    
    
    # This is exactly the same table, but it saves the variables indexes with
    # respect the super table instead of human readable text
    xiComboResultsIndexDF = data.frame(matrix(NA, nrow = totalCombos, ncol = currentTotalVariables + 3))
    colnames(xiComboResultsIndexDF) = c("nRows", "Variable", referenceIndexes, "pvalue")    
    
  }

  # Make the all combinations loop
  tableString   = ""
  filterString  = ""
  finish        = FALSE
  overallIndex  = 1
  
  for (i in 1:currentTotalVariables){
    
    # Reset some variables
    currentVariableName            = currentVariablesInfoDF[i,1]
    currentVariableIndex           = referenceIndexes[i]
    currentVariableTotalModalities = currentVariablesInfoDF[,3]
    currentVariableModalities      = 
    currentTable                   = completeTable
    currentTotalRows               = nrow(currentTable)
    filterString                   = ""
    tableNameString                = ""
    currentVariablesInfoDF[,4]     = 0   # Mark every level to no filter
    currentVariablesInfoDF[i,4]    = -1  # Mark this level for ignoring
    
    # Get how many combos we are going to do for this particular variable
    # This change from variable to variable
    totalLocalCombos = prod(currentVariablesInfoDF[,3][-i] + 1)
    
    # Do all the combinations for this particular root variable
    for(j in 1:totalLocalCombos){
    
      # Reset the table and the filter related variables
      currentTable               = completeTable
      filterString                = ""
      tableNameString             = ""
      
      
      # Apply the filters for each modality
      for(k in 1:currentTotalVariables){
        
        # Don't apply the filter to any variable which level is 0 or ignore
        if(currentVariablesInfoDF[k,4]>0) {
          
          # Get the reference link of this particular variable
          filterIndex = referenceIndexes[k]
          
          # Get the name of the modality which we are going to filter by
          currentModality = currentModalitiesDF[k, currentVariablesInfoDF[k,4]]
          
          # String to print for testing
          filterString    = paste0(filterString, " & ", k,":", currentModality ) # This is human readable
          tableNameString = paste0(tableNameString, "_", filterIndex,"-", k)     # This is for robots
          
          # Update the table
          currentTable = subset(currentTable, currentTable[,filterIndex] == currentModality)
          
          # Register this in the results DF
          xiComboResultsDF[overallIndex, (2 + k)]      = currentModality
          xiComboResultsIndexDF[overallIndex, (2 + k)] = currentVariablesInfoDF[k,4]
          
          
        }
        
      }
      
      # Register how many rows of each TARGET modality we have, and normalize
      # with respect the total rows (if total rows > 0)
      {
        currentTotalRows                  = nrow(currentTable)
        xiComboResultsDF[overallIndex, 1] = currentTotalRows
        xiComboResultsDF[overallIndex, 2] = currentVariableName
        
        # If you have more than one row (so we can actually count something)
        if(currentTotalRows > 0){
          
          # For each of the target modalities
          for(k in 1:targetTotalModalities){
          
            # Find the modality
            currentTargetModality = targetModalities[k]
            
            # Find how much there is (relative)
            currentTotalTargetModality    = sum(currentTable[, targetIndexes[3]] == currentTargetModality, na.rm = TRUE)
            currentRelativeTargetModality = currentTotalTargetModality / currentTotalRows
            
            # Add it to the table
            xiComboResultsDF[overallIndex, currentTotalVariables + 3 + k] = currentRelativeTargetModality
            
          }
            
        }
        
        
      }
      
      # If you have enough rows to try, do the xi
      {
        
        
        if(currentTotalRows > 4){
          
          # Do the Xi
          # If you have no enough data or whatever, that is reported in the result error code
          currentTableName = paste0("Subtable_", currentVariableIndex, "_", targetIndexes[3], "_w_" , tableNameString)   
          
          myResults   = categoricalXi(currentTable, currentVariableIndex, targetIndexes[3],
                                      overrideTableName = currentTableName,
                                      AUREUS_FOLDER, supressWarnings = TRUE)
          
          xiComboResultsDF$pvalue[overallIndex]      = myResults[[7]] # Negative pvalues means that there was an error trying this combo
          xiComboResultsIndexDF$pvalue[overallIndex] = myResults[[7]]
          
          
          # If everything goes right
          # Register how many target modalities for each of the current variable modalities
          # and check if the p-value is relevant
          if(myResults[[7]] == 0){

            # Register, which is the most extreme value with respect its average
            # That IS NOT "Unknown"
            {
              # Get all the results from the matrix
              meltedRelativeResults = melt(myResults[[3]])  
              
              # Take away the Unknowns (if any)
              totalUnknown = sum(meltedRelativeResults[,1] == "Unknown", na.rm=TRUE)
              if(totalUnknown>0) meltedRelativeResults = meltedRelativeResults[meltedRelativeResults[,1] != "Unknown",]
              
              # Count how many left
              totalRelativeResults  = length(meltedRelativeResults)
              
              # Add an extra column for the distance
              meltedRelativeResults$distance = -1

              # Find the distance for each element cell in the matrix              
              # For each of the target modalities
              for(k in 1:targetTotalModalities){
                
                # Find the modality
                currentTargetModality = targetModalities[k]
                
                # Find those in the table that has that modality
                modifyThisIndexes = (meltedRelativeResults[,2] == currentTargetModality)
                
                # Find the reference distance for this modality
                currentReferenceDistance = xiComboResultsDF[overallIndex, currentTotalVariables + 3 + k]
                
                # Apply the distance function with respect this reference
                meltedRelativeResults[modifyThisIndexes, 4] =   abs(meltedRelativeResults[modifyThisIndexes, 3] - currentReferenceDistance)

              }
              
              # Find the row with the maximum distance
              distanceSortedRelativeResults = meltedRelativeResults[ rev(order(meltedRelativeResults[,4])) ,]
              distanceString = paste0(distanceSortedRelativeResults[1,1], " ",
                                      distanceSortedRelativeResults[1,2], " ",
                                      round(distanceSortedRelativeResults[1,3],2))
            
              # Add the summary to that row
              xiComboResultsDF[overallIndex, ncol(xiComboResultsDF)] = distanceString
                
            }

            # Get the p-value and add to the table, wether is relevant or not
            # Then you can sort the table later and get whatever you want
            currentPValue = myResults[[6]]    
            xiComboResultsDF$pvalue[overallIndex] = currentPValue
            xiComboResultsIndexDF$pvalue[overallIndex] = currentPValue
            
            # If it is relevant, show save the absolute matrix and the relative plot
            if(currentPValue < 0.05){
              
              #print(paste0(" | variable: ", currentVariableName, " | filters ", filterString, " | n: ", currentTotalRows, " | pV: ", round(currentPValue, 3))) 


              
              # Do a  relative combine bar plot
              tableNameOveride    = overallIndex
              titleOverride       = paste0("Xi Subtable", currentVariableName, " with ", currentTargetName)
              subTitleOverride    = paste0("Filters: ",  filterString)
              barCaptionOverride  = paste0("Relative barplot for relevant xi combo: ", overallIndex)
              tabCaptionOverride  = paste0("Xi Table for combo: ", overallIndex)
              relCombinePlotResults = doBarRelativeCombinePlot(currentTable,
                                                               currentVariableIndex,
                                                               targetIndexes[3],
                                                               AUREUS_FOLDER,
                                                               colorsVector      = myListOfColorVectors[[currentVariableIndex]],
                                                               plotCaption       = barCaptionOverride,
                                                               overrideCaption   = tableNameOveride,
                                                               plotTitle         = titleOverride,
                                                               plotSubtitle      = subTitleOverride,
                                                               overrideTableName = tableNameOveride)
              
              # Write the relative table too
              xiTable = writeTableLATEX(myResults[[2]], AUREUS_FOLDER,
                                        tableCaption      = tabCaptionOverride,
                                        overrideTableName = tableNameOveride,
                                        warningComment    = FALSE,
                                        roundMe = 4)
              
            } 
            
          }
          
        }
      }

      # Increase the levels to the next combo
      {
        
        # This variable starts from the last level, and goes back to the first
        levelUpTracker  = currentTotalVariables
        
        
        continueModulus = TRUE
        while(continueModulus){
          
          # If this level, is the one tracking the current variable, skip it.
          # ( -1 means ignore )
          if(currentVariablesInfoDF[levelUpTracker,4] == -1){
            
            levelUpTracker = levelUpTracker - 1
            
          }
          else{

            # Increase the level
            currentVariablesInfoDF[levelUpTracker,4] = (currentVariablesInfoDF[levelUpTracker,4] + 1) %% (currentVariablesInfoDF[levelUpTracker,3] + 1)

            # If we modulus 0 the level, go back to the previous variable and repeat
            if(currentVariablesInfoDF[levelUpTracker,4] == 0){
              
              levelUpTracker = levelUpTracker - 1
              
            }
            else{
              continueModulus = FALSE
            }

          }
          
          # If we backtrack too much, the loop is finish
          if(levelUpTracker == 0){
            #print("Supersafety!!!")
            continueModulus = FALSE
          }

          
        }
        
        
      }
      
      # Increase the overall index that keeps track of how many xis we have done
      overallIndex = overallIndex + 1
      
    }
    
  }
  
  
  # Feedback
  print(paste0("I have done ", overallIndex, " combination, phew!"))
  
  # Clean the irrelevant part and write the rest on disk
  xiComboResultsDF$ComboID = rownames(xiComboResultsDF)                             # Add the ID to search for the plots later
  xiComboRelevantDF        = xiComboResultsDF[xiComboResultsDF$pvalue   < 0.05 , ]  # I only wants those with p-value lower than 0.05
  xiComboRelevantDF        = xiComboRelevantDF[xiComboRelevantDF$pvalue > 0    , ]  # Also with p-value greater than 0, the rest didn't have enought categories to run a xi analysis
  xiComboRelevantDF        = xiComboRelevantDF[!is.na(xiComboRelevantDF$pvalue), ]  # No NA p-values, those didn't even had enought rows
  # -- Sort by pvalue
  xiComboRelevantDF        = xiComboRelevantDF[with(xiComboRelevantDF, order(`pvalue`)), ]
  
  # Write the relative table too
  xiTableResults = writeTableLATEX(xiComboRelevantDF, AUREUS_FOLDER,
                                   tableCaption      = "All relevant xi combinations",
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   roundMe = 2)
  
}


# ********************************************
#     CARRIER XI-TABLES
# ********************************************
print("Xi tables")
if(TRUE){
  
  # -- reboot the explanatory indexes (this is only for debugging)
  explanatoryIndexes      = c(sexIndex,  schoolIndex, BMICatIndex,
                              smokeIndex, snuffIndex, alcoholIndex,
                              sportsIndex)
  
  # Create the DF where to save the results
  # -------- Rows --------------------------
  # (All the important categorical indexes)
  # (-3 , each carrier status)
  # -------- Columns -----------------------
  # (Variable names + Nasal, Throat, Carrier for each group)
  xiSquareSummaryTableDF = data.frame(matrix(NA, nrow = totalExplanatoryIndexes, ncol = totalConsecuenceIndexes + 1))
  colnames(xiSquareSummaryTableDF) = c("Variable", consecuenceNames)

  # Initialize the concepts (everything except hormonal)
  # -- For each categorical
  for (i in 1:totalExplanatoryIndexes) {
    
    # Get the variable
    currentImportantCategoryName  = explanatoryNames[i]
    currentImportantCategoryIndex = explanatoryIndexes[i]

    # Init the row in the summary
    xiSquareSummaryTableDF[i,1]       = currentImportantCategoryName

    # For each of the consequence variable, find the xi table
    for(j in 1:totalConsecuenceIndexes){
    
      currentConsecuenceIndex = consecuenceIndexes[j]
      myResultsXi = categoricalXi(completeTable, currentImportantCategoryIndex, currentConsecuenceIndex, AUREUS_FOLDER,
                                       logFile = logTXTFileConnection)

      xiSquareSummaryTableDF[i,(j+1)]  = myResultsXi[[8]]
      
    }
    
  }

  # In order to write the results of the xi properly, we need to run the chi.sq so we get the actual number
  {
    tempTable = table(completeTable$Sex, completeTable[,108])
    tempTable = tempTable[,-3] # Delete the unknown column which are all equal 0    
    print(chisq.test(tempTable))
    
    tempTable = table(completeTable$Sex, completeTable[,111])
    tempTable = tempTable[,-3] # Delete the unknown column which are all equal 0    
    print(chisq.test(tempTable))
  }

  
  
  # Same thing, stratify by sex (Man, Woman), with no hormonal contraceptives, which we will delete later
  menOnlyXiSquareSummaryTableDF   = xiSquareSummaryTableDF
  womenOnlyXiSquareSummaryTableDF = xiSquareSummaryTableDF # (+1 , hormonal data, will add later)

  # MEN ONLY
  # -----------------------------------------------------
  for (i in 1:totalExplanatoryIndexes) {
    
    # Get the variable
    currentImportantCategoryName  = explanatoryNames[i]
    currentImportantCategoryIndex = explanatoryIndexes[i]
    
    # Init the row in the summary
    #menOnlyXiSquareSummaryTableDF[i,1] = currentImportantCategoryName
    
    # For each of the consequence variable, find the xi table
    for(j in 1:totalConsecuenceIndexes){
      
      currentConsecuenceIndex = consecuenceIndexes[j]
      
      myResultsXi = categoricalXi(menOnlyTable, currentImportantCategoryIndex, currentConsecuenceIndex, AUREUS_FOLDER,
                                  logFile = logTXTFileConnection)
      
      menOnlyXiSquareSummaryTableDF[i,(j+1)]  = myResultsXi[[8]]
      
      if(currentImportantCategoryIndex == 1004){
        
        print("------------------------")
        print(" DEBUG: Men Xi isolated")
        print("------------------------")
        print( paste0(" Category: ", currentImportantCategoryIndex , " consecuence: ", currentConsecuenceIndex))
        print(myResultsXi[[8]])
        
      }
      
    }
    
  }
  
  # WOMEN ONLY
  # -----------------------------------------------------
  for (i in 1:totalExplanatoryIndexes) {
    
    # Get the variable
    currentImportantCategoryName  = explanatoryNames[i]
    currentImportantCategoryIndex = explanatoryIndexes[i]
    
    # Init the row in the summary
    womenOnlyXiSquareSummaryTableDF[i,1] = currentImportantCategoryName
    
    # For each of the consequence variable, find the xi table
    for(j in 1:totalConsecuenceIndexes){
      
      currentConsecuenceIndex = consecuenceIndexes[j]
      
      myResultsXi = categoricalXi(womenOnlyTable, currentImportantCategoryIndex, currentConsecuenceIndex, AUREUS_FOLDER,
                                  logFile = logTXTFileConnection)
      
      womenOnlyXiSquareSummaryTableDF[i,(j+1)]  = myResultsXi[[8]]
      
    }
    
  }
  
  # Add the hormonal data to the woman table
  {
    
    # Add the new line
    womenOnlyXiSquareSummaryTableDF[nrow(womenOnlyXiSquareSummaryTableDF)+1,] = NA
    womenOnlyXiSquareSummaryTableDF[nrow(womenOnlyXiSquareSummaryTableDF),1]  = "Hormonal"
    # -- Get the women menstruating only
    womenMenstruatingOnlyTable = subset(womenOnlyTable, womenOnlyTable$Menstruating == "Yes")
    # -- Prepare a new column with the type of hormonal contraceptives that they use
    womenMenstruatingOnlyTable$Hormonal = "None or non-hormonal"
    totalContraceptives = nrow(contraceptivesTable)
    for (i in 1:totalContraceptives) {
      
      # Get the ID
      currentID = contraceptivesTable$ID[i]
      
      # Get the hormonal type
      currentHormonal = contraceptivesTable$Hormonal[i]
      
      # If it has estrogens, divide it between low and high estrogen
      if(currentHormonal=="Progestin-Estradiol"){
        
        currentHormonal = "Low Estrogen"
        
        currentBrand = contraceptivesTable$Brand[i]
        
        if(currentBrand == "Marvelon")   currentHormonal = "High Estrogen"
        if(currentBrand == "Yasmin")     currentHormonal = "High Estrogen"
        if(currentBrand == "Microgynon") currentHormonal = "High Estrogen"
        if(currentBrand == "Oralcon")    currentHormonal = "High Estrogen"
        if(currentBrand == "Diane")      currentHormonal = "High Estrogen"
        if(currentBrand == "Synfase")    currentHormonal = "High Estrogen"
        if(currentBrand == "Evra")       currentHormonal = "High Estrogen"
        
      }
      
      # If the hormonal type is a non-hormonal type, register it on the new column
      if(currentHormonal!="Non-hormonal"){

        womenMenstruatingOnlyTable[womenMenstruatingOnlyTable$ID == currentID,]$Hormonal = currentHormonal

      }
      
      
    }
    
    # Factor the results based on the amount of Estrogen
    womenMenstruatingOnlyTable$Hormonal = factor(womenMenstruatingOnlyTable$Hormonal ,
                                                 levels = c("None or non-hormonal", "Progestin", "Low Estrogen", "High Estrogen", "Unknown"))
    
    
    # Perform the X^2 test
    {
      # Find the index for the hormonal column
      hormonalIndex = grep("^Hormonal$", colnames(womenMenstruatingOnlyTable))      
      
      # Take away those which are unknown for the xi-square test
      womenNotUknownTable = womenMenstruatingOnlyTable
      womenNotUknownTable = womenNotUknownTable[ womenNotUknownTable$Hormonal != "Unknown", ]
      
      # Save the xi-sq for nasal, throat, and carrier of each variable
      for(j in 1:totalConsecuenceIndexes){
        
        currentConsecuenceIndex = consecuenceIndexes[j]
        
        myResultsXi = categoricalXi(womenNotUknownTable, hormonalIndex, currentConsecuenceIndex, AUREUS_FOLDER,
                                    logFile = logTXTFileConnection)
        
        womenOnlyXiSquareSummaryTableDF[nrow(womenOnlyXiSquareSummaryTableDF),(j+1)]  = myResultsXi[[8]]
        
      }
      
    }

    


    
    
    
    
    # myResultsNasal   = categoricalXi(womenNotUknownTable, hormonalIndex, nasalCarrierIndex,  AUREUS_FOLDER)
    # myResultsThroat  = categoricalXi(womenNotUknownTable, hormonalIndex, throatCarrierIndex, AUREUS_FOLDER)
    # myResultsCarrier = categoricalXi(womenNotUknownTable, hormonalIndex, carrierIndex,       AUREUS_FOLDER)
    # 
    # womenOnlyXiSquareSummaryTableDF[nrow(womenOnlyXiSquareSummaryTableDF),2]  = myResultsNasal[[8]]
    # womenOnlyXiSquareSummaryTableDF[nrow(womenOnlyXiSquareSummaryTableDF),3]  = myResultsThroat[[8]]
    # womenOnlyXiSquareSummaryTableDF[nrow(womenOnlyXiSquareSummaryTableDF),4]  = myResultsCarrier[[8]]
    
  }

  # Delete the SEX row from the women and men table
  menOnlyXiSquareSummaryTableDF   = menOnlyXiSquareSummaryTableDF[-1,]
  womenOnlyXiSquareSummaryTableDF = womenOnlyXiSquareSummaryTableDF[-1,]
  
  print("R is horrible")
  
  # R is a horrible language, and because some reason now numbers are characters
  # in this particular dataframe only, so you need to convert them back to numbers
  # again. I HATE YOUR LACK OF VARIABLE TYPE DECLARATION!!!
  {
    
    for(i in 1:nrow(womenOnlyXiSquareSummaryTableDF)){
      for (j in 2:ncol(womenOnlyXiSquareSummaryTableDF)) {
    
        womenOnlyXiSquareSummaryTableDF[i,j] = as.numeric(womenOnlyXiSquareSummaryTableDF[i,j])
            
        #print(womenOnlyXiSquareSummaryTableDF[i,j])
        
      }
    }
    
  }
  
  # Write the latex tables
  {
    xiTable = writeTableLATEX(xiSquareSummaryTableDF, AUREUS_FOLDER,
                              tableCaption  = paste0("Xi Square test results with respect carrier status."),
                              intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                              roundMe = 4, rotateColumnHeaders = TRUE)
    
    xiTable = writeTableLATEX(menOnlyXiSquareSummaryTableDF, AUREUS_FOLDER,
                              tableCaption  = paste0("Men only, xi Square test results with respect carrier status."),
                              intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                              roundMe = 4, rotateColumnHeaders = TRUE)
    
    xiTable = writeTableLATEX(womenOnlyXiSquareSummaryTableDF, AUREUS_FOLDER,
                              tableCaption  = paste0("Women only, xi Square test results with respect carrier status."),
                              intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                              roundMe = 4, rotateColumnHeaders = TRUE)
  }

  
  # Do the metaanalysis for several variables at the same time
  # Since the metatable is so big, we do it in batches of 3 for the consecuence indexes.
  # (Deprecated, we only have two variables now)
  # TotalIndexGroups is always 1 now
  {
    totalIndexGroups = ceiling(length(consecuenceIndexes)/3)
    
    indexesGroupingDF = data.frame(matrix(NA, nrow = totalIndexGroups, ncol = 3))
    colnames(indexesGroupingDF) = c("Nasal", "Throat", "Either")
    
    currentIndex = 1
    for (i in 1:totalIndexGroups) {
      for (j in 1:3) {
        indexesGroupingDF[i,j] = consecuenceIndexes[currentIndex]
        currentIndex = currentIndex + 1
      }
    }
  }

  # For both sexes
  # -- reboot the explanatory indexes (this is only for debugging)
  print("Both")
  explanatoryIndexes      = c(sexIndex,  schoolIndex, BMICatIndex,
                              smokeIndex, snuffIndex, alcoholIndex,
                              sportsIndex)
  for (i in 1:totalIndexGroups) {
    
    # Get the indexes you want    
    currentIndexes = as.numeric(indexesGroupingDF[i,])

    # It might happens that you have less than 3 indexes
    # Remove the NAs
    currentIndexes = currentIndexes[!is.na(currentIndexes)]
    
    # Give a proper talbe name (meta + first index)
    overrideNameString    = paste0("Metasummary_both_", as.character(indexesGroupingDF[i,1]))
    overrideCaptionString = paste0("Metasummary both ", as.character(indexesGroupingDF[i,1]))
    
    myMetaResult = metasummaryXi(completeTable, explanatoryIndexes, currentIndexes, AUREUS_FOLDER,
                                 overrideTableName = overrideNameString,
                                 overrideCaption   = overrideCaptionString)
    
  }
  
  # Men only
  print("Men")
  explanatoryIndexes = explanatoryIndexes[2:length(explanatoryIndexes)]
  for (i in 1:totalIndexGroups) {
    # Get the indexes you want    
    currentIndexes = as.numeric(indexesGroupingDF[i,])
    
    # It might happens that you have less than 3 indexes
    # Remove the NAs
    currentIndexes = currentIndexes[!is.na(currentIndexes)]
    
    
    # Give a proper talbe name (meta + first index)
    overrideNameString = paste0("Metasummary_men_", as.character(indexesGroupingDF[i,1]))
    overrideCaptionString = paste0("Metasummary MEN ", as.character(indexesGroupingDF[i,1]))
    
    myMetaResult = metasummaryXi(menOnlyTable, explanatoryIndexes, currentIndexes, AUREUS_FOLDER,
                                 overrideTableName = overrideNameString,
                                 overrideCaption   = overrideCaptionString)
    
  }
  
  # Women only
  print("Women")
  explanatoryIndexes = c(explanatoryIndexes,hormonalIndex)
  for (i in 1:totalIndexGroups) {
    # Get the indexes you want    
    currentIndexes = as.numeric(indexesGroupingDF[i,])
    
    # It might happens that you have less than 3 indexes
    # Remove the NAs
    currentIndexes = currentIndexes[!is.na(currentIndexes)]
    
    # Give a proper talbe name (meta + first index)
    overrideNameString = paste0("Metasummary_women_", as.character(indexesGroupingDF[i,1]))
    overrideCaptionString = paste0("Metasummary WOMEN ", as.character(indexesGroupingDF[i,1]))
    
    myMetaResult = metasummaryXi(womenMenstruatingOnlyTable, explanatoryIndexes, currentIndexes, AUREUS_FOLDER,
                                 overrideTableName = overrideNameString,
                                 overrideCaption   = overrideCaptionString)
    
  }
  

  
  
}

source("latex.R", encoding="utf-8")

# ********************************************
#     GRAPHS
# ********************************************
print("Graphs") 
if(TRUE){

  # Make a constant layout for consistence between plots
  myGraph = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
  myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
  #myConstantLayoutB = subset(myConstantLayoutA, name %in% myConstantLayoutA$name, x:y)

  # All based on nasal enrichment carrier
  
  # Complete overall network
    
  # ---- For Carrier (enrichment)
  {
    currentPlotTitle          = "Overall Network for positive enricment status"
    currentOverridedPlotName  = "OverallDFCarrierOnly"
    currentOverrideCaption    = "Graph with the network for carrier status."
    
    doGraphPlot(overallEdgesDF,  completeTable, AUREUS_FOLDER,
                highlightVariable = nasalEnrichmentCarrierIndex,
                colorVectorHighlight = COLOR_VECTOR_CARRIER,
                sizeVariableIndex = overallConnectionsIndex,
                manualLayout = myConstantLayoutA,
                plotTitle    = currentPlotTitle,
                plotSubtitle = "Size based on number of undirected relationships",
                overrideTableName = currentOverridedPlotName,
                overrideCaption   = currentOverrideCaption) 
    
    
    # Modified for Dina
    {
        
        currentPlotTitle          = "Overall Network for enriched nasal carriage status"
        currentOverridedPlotName  = "OverallDFCarrierOnly"
        currentOverrideCaption    = "Graph with the network for carrier status."
    
        # Change the size of the nodes, later change it back to the original value
        originalValueOverallConnections =  completeTable[,overallConnectionsIndex]
        completeTable[,overallConnectionsIndex] = (originalValueOverallConnections + 3) * 1.25
        
        completeTableModified = completeTable
        
        colnames(completeTableModified)[nasalEnrichmentCarrierIndex] = "Enriched nasal carriage status"
        
        doGraphPlot(overallEdgesDF,  completeTableModified, AUREUS_FOLDER,
                    highlightVariable = nasalEnrichmentCarrierIndex,
                    colorVectorHighlight = COLOR_VECTOR_CARRIER,
                    sizeVariableIndex = overallConnectionsIndex,
                    manualLayout = myConstantLayoutA,
                    plotTitle    = currentPlotTitle,
                    plotSubtitle = "Size based on number of undirected relationships",
                    overrideTableName = currentOverridedPlotName,
                    overrideCaption   = currentOverrideCaption) 
    
        completeTable[,overallConnectionsIndex] = originalValueOverallConnections
        
        
    }
    
    
    
    
    
            # Change the size of the nodes, later change it back to the original value
        originalValueOverallConnections =  completeTable[,overallConnectionsIndex]
        completeTable[,overallConnectionsIndex] = (originalValueOverallConnections + 3) * 1.25
    
    
  }

  # ---- For Highschool
  {
    currentPlotTitle          = "Overall Network for highschool ID"
    currentOverridedPlotName  = "OverallDFHighschool"
    currentOverrideCaption    = "Graph with the network for Highschool ID."
    
    doGraphPlot(overallEdgesDF,  completeTable, AUREUS_FOLDER,
                highlightVariable = highSchoolIndex,
                sizeVariableIndex = overallConnectionsIndex,
                manualLayout = myConstantLayoutA,
                plotTitle = currentPlotTitle, plotSubtitle = "Size based on number of undirected relationships",
                overrideTableName = currentOverridedPlotName,
                overrideCaption   = currentOverrideCaption)   
  }
    
  # Positive Only by High-school
  {
    
    # Create the edges of people who has valid spa-types
    spaTableOnly    = subset(completeTable, completeTable[,spaT1IndexComplete] != "")
    nonSPATableOnly = subset(completeTable, completeTable[,spaT1IndexComplete] == "")
    
    # Overall people with typable SPA
    nonSPAIDs              = nonSPATableOnly$ID
    deleteThisLists        = deleteConnections(overallEdgesDF, nonSPAIDs, nonSPAIDs)[[1]]
    spaOnlyOverallEdgesDF  = overallEdgesDF[!deleteThisLists,]
    
    spaOnlyOverallEdgesDF$SameSPAT1 = addEdgeRelationship(spaOnlyOverallEdgesDF, completeTable, spaT1IndexComplete)
    
    sameSPAT1Index             = grep("^SameSPAT1$",   colnames(spaOnlyOverallEdgesDF))
        
    # - Add a new column that is TRUE if they have the same SPA Type.
    # - Add a new column that is TRUE if they are both the same carrier status    
    # - Add a new column that is TRUE if they are both the same carrier status AND the type of carrier status
    overallEdgesDF$SameSPAT1   = addEdgeRelationship(overallEdgesDF, completeTable, spaT1IndexComplete)
    overallEdgesDF$SameCarrier = addEdgeRelationship(overallEdgesDF, completeTable, nasalEnrichmentCarrierIndex)
    overallEdgesDF$SameCarrierCategorical = addEdgeRelationship(overallEdgesDF, completeTable, nasalEnrichmentCarrierIndex, keepName = TRUE)
    sameSPAT1Index             = grep("^SameSPAT1$",   colnames(overallEdgesDF))
    sameCarrierIndex           = grep("^SameCarrier$", colnames(overallEdgesDF))
    sameCarrierCatIndex        = grep("^SameCarrierCategorical$", colnames(overallEdgesDF))

    # Delete all the non carrier nodes and edges involving non carrier IDs
    # This also make a list with all the non-carrier edges list for each network
    # for later.
    {
      nonCarriersIDs              = negativeEnrichmentTableOnly$ID
      nonCarriersIDs              = negativeCoagulaseTableOnly$ID
      # ---- Overall carriers only (we are using only this one now, but later we use the rest)
      deleteThisLists             = deleteConnections(overallEdgesDF, nonCarriersIDs, nonCarriersIDs)[[1]]
      carriersOnlyOverallEdgesDF  = overallEdgesDF[!deleteThisLists,]
      # ---- Physical carriers only
      deleteThisLists             = deleteConnections(physicalEdgesDF , nonCarriersIDs, nonCarriersIDs)[[1]]
      carriersOnlyPhysicalEdgesDF = physicalEdgesDF[!deleteThisLists,]
      # ---- School carriers only
      deleteThisLists             = deleteConnections(schoolEdgesDF, nonCarriersIDs, nonCarriersIDs)[[1]]
      carriersOnlySchoolEdgesDF   = schoolEdgesDF[!deleteThisLists,]
      # ---- Sports carriers only
      deleteThisLists             = deleteConnections(sportsEdgesDF, nonCarriersIDs, nonCarriersIDs)[[1]]
      carriersOnlySportsEdgesDF   = sportsEdgesDF[!deleteThisLists,]
      # ---- Home carriers only
      deleteThisLists             = deleteConnections(homeEdgesDF, nonCarriersIDs, nonCarriersIDs)[[1]]
      carriersOnlyHomeEdgesDF     = homeEdgesDF[!deleteThisLists,]
      # ---- Others carriers only
      deleteThisLists             = deleteConnections(otherEdgesDF, nonCarriersIDs, nonCarriersIDs)[[1]]
      carriersOnlyOthersEdgesDF   = otherEdgesDF[!deleteThisLists,]
      # ---- All of them into a list
      carriersOnlyEdgesList = c(carriersOnlyOverallEdgesDF, carriersOnlyPhysicalEdgesDF,
                                carriersOnlySchoolEdgesDF, carriersOnlySportsEdgesDF,
                                carriersOnlyHomeEdgesDF, carriersOnlyOthersEdgesDF)
    }

    # Make the MDS graph (old)
    if(TRUE){
      # Prepare the plot titles
      
        # Old version, TRUE FALSE and SameSPAT1
      currentPlotTitle          = "School relathionships highlighting highchool in nodes and same SPAT1 in edges."
      currentPlotSubtitle       = "Isolated nodes are hidden (21)"
      currentOverridedPlotName  = "OverallDF_E_NasalCarrierOnly_SPArel"
      currentOverrideCaption    = "Graph with the network, with positives only, for Highschool ID. Isolated nodes are hidden."
      
      schoolEdgesDF$SameSPAT1   = addEdgeRelationship(schoolEdgesDF, completeTable, spaT1IndexComplete)
      
      doGraphPlot(schoolEdgesDF,  completeTable, AUREUS_FOLDER,
                  sizeVariableIndex = overallConnectionsIndex,
                  highlightVariable = highSchoolIndex,
                  edgesHighlight    = sameSPAT1Index,
                  colorVectorEdge   = rev(COLOR_VECTOR_SAME_RELATIONSHIP),
                  plotTitle         = currentPlotTitle,
                  plotSubtitle      = currentPlotSubtitle,
                  overrideTableName = currentOverridedPlotName,
                  overrideCaption   = currentOverrideCaption,
                  suppressAloneNode = TRUE)
      
      
        # New plot with modfied labels 
        {
        
            schoolEdgesDFModfied = schoolEdgesDF
            schoolEdgesDFModfied[schoolEdgesDFModfied$SameSPAT1 == TRUE,]$SameSPAT1   = "Yes"
            schoolEdgesDFModfied[schoolEdgesDFModfied$SameSPAT1 == FALSE,]$SameSPAT1  = "No"
            colnames(schoolEdgesDFModfied)[4] = "Same spa-type"
        
            currentPlotTitle          = "School relathionships highlighting highchool in nodes and same SPAT1 in edges."
            currentPlotSubtitle       = "Isolated nodes are hidden (21)"
            currentOverridedPlotName  = "OverallDF_E_NasalCarrierOnly_SPArel"
            currentOverrideCaption    = "Graph with the network, with positives only, for Highschool ID. Isolated nodes are hidden."
      
            # Redo the color palette to qualitative instead of diverging
            myHSPalette    = colorRampPalette(brewer.pal(8, "Set1"))
            myHSColors     = myHSPalette(8)
        
            # Change the size of the nodes, later change it back to the original value
            #originalValueOverallConnections =  completeTable[,overallConnectionsIndex]
            #completeTable[,overallConnectionsIndex] = (originalValueOverallConnections + 3) * 1.25
            
            doGraphPlot(schoolEdgesDFModfied,  completeTable, AUREUS_FOLDER,
                        sizeVariableIndex = overallConnectionsIndex,
                        highlightVariable = highSchoolIndex,
                        edgesHighlight    = sameSPAT1Index,
                        colorVectorHighlight = myHSColors,
                        colorVectorEdge   = rev(COLOR_VECTOR_SAME_RELATIONSHIP),
                        plotTitle         = currentPlotTitle,
                        plotSubtitle      = currentPlotSubtitle,
                        overrideTableName = currentOverridedPlotName,
                        overrideCaption   = currentOverrideCaption,
                        overrideLegendSize = 5,
                        suppressAloneNode = TRUE)
          
            #completeTable[,overallConnectionsIndex] = originalValueOverallConnections
      
            
              
        }
      
        
    }
    
    # Make the MDS graph (new complete spat1)
    #        (this looks like crap, but you can see the "path" of 
    #         infection around the network if you follow the same
    #         SPAT1 trail from student to student)
    if(FALSE){
      # Prepare the plot titles
      
        # Old plot with TRUE / FALSE labels
        currentPlotTitle          = "Relationships of students with same SPAT1."
        currentPlotSubtitle       = "Only nodes with valid SPAT1 are shown."
        currentOverridedPlotName  = "OverallDF_E_NasalCarrierOnly_SPArel"
        currentOverrideCaption    = "Graph with the network, with positives only, for Highschool ID. Isolated nodes are hidden."
      
        doGraphPlot(spaOnlyOverallEdgesDF,  spaTableOnly, AUREUS_FOLDER,
                    selectedLayout    = "dh",
                    sizeVariableIndex = overallConnectionsIndex,
                    highlightVariable = highSchoolIndex,
                    edgesHighlight    = sameSPAT1Index,
                    colorVectorEdge   = rev(COLOR_VECTOR_SAME_RELATIONSHIP),
                    plotTitle         = currentPlotTitle,
                    plotSubtitle      = currentPlotSubtitle,
                    overrideTableName = currentOverridedPlotName,
                    overrideCaption   = currentOverrideCaption,
                    suppressAloneNode = TRUE)
        
       
        }
    
    # Make the SOCIETAL Highschool graph for sharing same SPAT1 (RED and NULL)
    # (The categorical inverse of this doesn't make sense, believe me I tried)
    if(TRUE){
      highSchoolLayout = createCategoricalLayout(spaOnlyOverallEdgesDF, spaTableOnly, highSchoolIndex)
      
      myGraph = graph_from_data_frame(spaOnlyOverallEdgesDF, vertices = spaTableOnly, directed = FALSE)
      myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
      
      myConstantLayoutA$x = highSchoolLayout[[1]]$x
      myConstantLayoutA$y = highSchoolLayout[[1]]$y
      
      # Space out the nodes so the maximum amount of edges are shown
      #carriersOnlyOverallEdgesDF = carriersOnlyOverallEdgesDF
      
      # Prepare the plot titles
      currentPlotTitle          = "Relationships of students with same SPAT1."
      currentPlotSubtitle       = "Only nodes with valid SPAT1 are shown."
      currentOverridedPlotName  = "OverallDF_CarrierOnly_HSrel"
      currentOverrideCaption    = "Graph with the network sorted by Highshool ID, highligthing the SPA-type relationship."
      
      doGraphPlot(spaOnlyOverallEdgesDF,  spaTableOnly, AUREUS_FOLDER,
                  sizeVariableIndex = overallConnectionsIndex,
                  highlightVariable = highSchoolIndex,
                  edgesHighlight    = sameSPAT1Index,
                  edgesThickness    = 1,
                  edgesAlpha        = 0.3,
                  colorVectorEdge   = c(NA, "red"),
                  #colorVectorEdge   = c("white", "red"),
                  manualLayout      = myConstantLayoutA,
                  plotTitle         = currentPlotTitle,
                  plotSubtitle      = currentPlotSubtitle,
                  overrideTableName = currentOverridedPlotName,
                  overrideCaption   = currentOverrideCaption)
    }
    
    # Make the SOCIETAL Highschool graph for sharing same SPAT1 (RED , BLUE and NULL)
    if(TRUE){
      highSchoolLayout = createCategoricalLayout(spaOnlyOverallEdgesDF, spaTableOnly, highSchoolIndex)
      
      myGraph = graph_from_data_frame(spaOnlyOverallEdgesDF, vertices = spaTableOnly, directed = FALSE)
      myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
      
      myConstantLayoutA$x = highSchoolLayout[[1]]$x
      myConstantLayoutA$y = highSchoolLayout[[1]]$y
      
      # Add the color code data
      spaOnlyOverallEdgesDF$HS_and_SPAT1 = spaOnlyOverallEdgesDF$SameSPAT1
      spaOnlyOverallEdgesDF[spaOnlyOverallEdgesDF$HS_and_SPAT1 == FALSE,]$HS_and_SPAT1 = "Different SPAT1"
      spaOnlyOverallEdgesDF[spaOnlyOverallEdgesDF$HS_and_SPAT1 == TRUE,]$HS_and_SPAT1  = "SPAT1, same HS"
      for(i in 1:nrow(spaOnlyOverallEdgesDF)){
        
        # Get the IDs
        ID1 = as.integer(spaOnlyOverallEdgesDF$from[i])
        ID2 = as.integer(spaOnlyOverallEdgesDF$to[i])

        # If they are in different highschool, but the same SPAT1 market it down
        if(   (completeTable$HighSchoolID[ID1] != completeTable$HighSchoolID[ID2]) && spaOnlyOverallEdgesDF$SameSPAT1[i] == TRUE  ){
          
          spaOnlyOverallEdgesDF$HS_and_SPAT1[i] = "SPAT1, different HS"
          
        }
        
      }
      
      # Get the index of the new variable for later
      sameHSandSPAT1Index        = grep("^HS_and_SPAT1$", colnames(spaOnlyOverallEdgesDF))
      
      # Prepare the plot titles
      currentPlotTitle          = "Relationships of students with same SPAT1."
      currentPlotSubtitle       = "Only nodes with valid SPAT1 are shown."
      currentOverridedPlotName  = "OverallDF_CarrierOnly_HSrel_and_SPAT1"
      currentOverrideCaption    = "Graph with the network sorted by Highshool ID, highligthing the SPA-type relationship. Edges also highlight highschool"
      
      # Do the plot
      doGraphPlot(spaOnlyOverallEdgesDF,  spaTableOnly, AUREUS_FOLDER,
                  sizeVariableIndex = overallConnectionsIndex,
                  highlightVariable = highSchoolIndex,
                  edgesHighlight    = sameHSandSPAT1Index,
                  edgesThickness    = 1,
                  edgesAlpha        = 0.3,
                  colorVectorEdge   = c(NA, "blue", "red"),
                  manualLayout      = myConstantLayoutA,
                  plotTitle         = currentPlotTitle,
                  plotSubtitle      = currentPlotSubtitle,
                  overrideTableName = currentOverridedPlotName,
                  overrideCaption   = currentOverrideCaption)
    }
    

    # MODIFIED VERSION REQUESTED BY DINA
    # Make the SOCIETAL Highschool graph for sharing same SPAT1 (RED and NULL)
    # (The categorical inverse of this doesn't make sense, believe me I tried)
    if(TRUE){
      highSchoolLayout = createCategoricalLayout(spaOnlyOverallEdgesDF, spaTableOnly, highSchoolIndex)
      
      myGraph = graph_from_data_frame(spaOnlyOverallEdgesDF, vertices = spaTableOnly, directed = FALSE)
      myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
      
      myConstantLayoutA$x = highSchoolLayout[[1]]$x
      myConstantLayoutA$y = highSchoolLayout[[1]]$y

      # Prepare the plot titles
      currentPlotTitle          = "Relationships of students with same SPAT1."
      currentPlotSubtitle       = "Only nodes with valid SPAT1 are shown."
      currentOverridedPlotName  = "OverallDF_CarrierOnly_HSrel"
      currentOverrideCaption    = "Graph with the network sorted by Highshool ID, highligthing the SPA-type relationship."
      
      
        spaOnlyOverallEdgesDFModified = spaOnlyOverallEdgesDF
        spaOnlyOverallEdgesDFModified[spaOnlyOverallEdgesDFModified$SameSPAT1 == TRUE,]$SameSPAT1   = "Yes"
        spaOnlyOverallEdgesDFModified[spaOnlyOverallEdgesDFModified$SameSPAT1 == FALSE,]$SameSPAT1  = "No"
        colnames(spaOnlyOverallEdgesDFModified)[4] = "Same spa-type"

        # Change the size of the nodes, later change it back to the original value
        #originalValueOverallConnections =  spaTableOnly[,overallConnectionsIndex]
        #spaTableOnly[,overallConnectionsIndex] = (originalValueOverallConnections + 3) * 1.25
      
        # Redo the color palette to qualitative instead of diverging
        myHSPalette    = colorRampPalette(brewer.pal(8, "Set1"))
        myHSColors     = myHSPalette(8)
        
      
      doGraphPlot(spaOnlyOverallEdgesDFModified,  spaTableOnly, AUREUS_FOLDER,
                  sizeVariableIndex = overallConnectionsIndex,
                  highlightVariable = highSchoolIndex,
                  edgesHighlight    = sameSPAT1Index,
                  edgesThickness    = 1,
                  edgesAlpha        = 0.3,
                  colorVectorHighlight = myHSColors,
                  colorVectorEdge   = c(NA, "red"),
                  #colorVectorEdge   = c("white", "red"),
                  manualLayout      = myConstantLayoutA,
                  plotTitle         = currentPlotTitle,
                  plotSubtitle      = currentPlotSubtitle,
                  overrideTableName = currentOverridedPlotName,
                  overrideCaption   = currentOverrideCaption)
      
      #spaTableOnly[,overallConnectionsIndex] = originalValueOverallConnections
      
    }    
    

  }

}

source("latex.R", encoding="utf-8")

# ********************************************
#     NETWORK SIMULATIONS
# ********************************************  
print("Simulations") 
if(FALSE){
 
  # SPA Bias analysis by simulations
  {
    
    # Create the edges of people who has valid spa-types
    spaTableOnly    = subset(completeTable, completeTable[,spaT1IndexComplete] != "")
    nonSPATableOnly = subset(completeTable, completeTable[,spaT1IndexComplete] == "")
    
    # Overall people with typable SPA
    nonSPAIDs              = nonSPATableOnly$ID
    deleteThisLists        = deleteConnections(overallEdgesDF, nonSPAIDs, nonSPAIDs)[[1]]
    spaOnlyOverallEdgesDF  = overallEdgesDF[!deleteThisLists,]
    
    # Count how many nodes you have
    # totalNodes         = nrow(positiveTableOnly)
    # totalRelationships = nrow(carriersOnlyOverallEdgesDF)
    totalNodes         = nrow(spaTableOnly)
    totalRelationships = nrow(spaOnlyOverallEdgesDF)
    
    # For the positive only edges, add if they have the same SPAT1
    #carriersOnlyOverallEdgesDF$SameSPAT1 = addEdgeRelationship(carriersOnlyOverallEdgesDF, positiveTableOnly, spaT1IndexComplete)
    spaOnlyOverallEdgesDF$SameSPAT1 = addEdgeRelationship(spaOnlyOverallEdgesDF, spaTableOnly, spaT1IndexComplete)
    
    # Get the indexes for these new variables
    #sameSPAT1Index = grep("^SameSPAT1$", colnames(carriersOnlyOverallEdgesDF))
    sameSPAT1Index = grep("^SameSPAT1$", colnames(spaOnlyOverallEdgesDF))
    
    # How many relationships we have with the same SPAT1
    #sameSPAT1Relationships   = sum(carriersOnlyOverallEdgesDF$SameSPAT1)
    sameSPAT1Relationships   = sum(spaOnlyOverallEdgesDF$SameSPAT1)
    
    # Check if SPATypes have bias friendship towards people with same SPA Types. 
    {
      #spaBiasResult = doBiasAnalysis(positiveTableOnly, carriersOnlyOverallEdgesDF, spaT1IndexComplete, totalSimulations)
      spaBiasResult = doBiasAnalysis(spaTableOnly, spaOnlyOverallEdgesDF, spaT1IndexComplete, totalSimulations)
      spaAverage    = mean(spaBiasResult)
      spaSD         = sd(spaBiasResult)
      
      summaryDF     = summary(spaBiasResult)
      
      summaryDF           = as.data.frame(t(summaryDF))[,2:3]
      colnames(summaryDF) = c("Concept", "Value")
      
      writeTableLATEX(summaryDF, AUREUS_FOLDER, roundMe = 2,
                      tableCaption = "Summary values of the SPAT1 simulations. Values are number of relationships with same SPA Type.",
                      overrideTableName = "spaBiasSimulation")  
      
      pValueSPA     = pnorm(sameSPAT1Relationships, mean=spaAverage, sd=spaSD, lower.tail=FALSE)
      
    }
    
    # Summary of p-values with respect each carrier
    print("SPA Pvalue:")
    print(pValueSPA)
    
  }
  
  # Everything else bias analysis by simulations
  if(TRUE){
    
    # Prepare the dataframes with all the results
    biasSimulationsDF           =  data.frame(matrix(NA, nrow = TOTAL_NETWORKS, ncol = 9 + 1 ))
    colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Q3", "MAX", "SD",  "ConsequenceIndex"  )
    
    for(i in 1:TOTAL_NETWORKS){
      biasSimulationsDF[i,1]  = NETWORK_NAMES[i]
    }

    # We are going to have a list for each of the consequence indexes. Each
    # element of the list is a bias resulst DF, all of them with exactly the
    # same columns. In here, we init the DF to empty for each consequence index.
    biasResultsList = vector("list", length = totalConsecuenceIndexes)
    for( i in 1:totalConsecuenceIndexes){
      
      # Init the DF to empty
      biasResultsList[[i]] = biasSimulationsDF

      # Change the name of the variable we are interested in for this DF
      colnames(biasResultsList[[i]])[10] = consecuenceNames[i]

    }
    
    
    # For each of the consequence indexes, we are going to do the same
    # bias analysis. There is 1000 simulation for each, so about 18000 simulations
    # Plus each network simulation, about 80.000 simulation in total.
    for (i in 1:totalConsecuenceIndexes){
      
      # Get the DF where we save the results for this variable
      
      # R is stupid, why can't I pass a reference? why do I need to use an index
      # here when then an alias to the variable would make everything more
      # readable and efficient??? >:[
      # currentDF = biasResultsList[[i]]
      
      # Get index and variable name
      {
        currentIndex = consecuenceIndexes[i]
        currentName  = consecuenceNames[i]        
      }

      # Init the edges dataframes
      {
        currentOverallEdgesDF  = overallEdgesDF
        currentPhysicalEdgesDF = physicalEdgesDF
        currentSchoolEdgesDF   = schoolEdgesDF
        currentSportsEdgesDF   = sportsEdgesDF
        currentHomeEdgesDF     = homeEdgesDF
        currentOtherEdgesDF    = otherEdgesDF        
      }

      # Add the relationships
      {
        currentOverallEdgesDF$SameCarrier  = addEdgeRelationship(currentOverallEdgesDF,  completeTable, currentIndex)
        currentPhysicalEdgesDF$SameCarrier = addEdgeRelationship(currentPhysicalEdgesDF, completeTable, currentIndex)
        currentSchoolEdgesDF$SameCarrier   = addEdgeRelationship(currentSchoolEdgesDF,   completeTable, currentIndex)
        currentSportsEdgesDF$SameCarrier   = addEdgeRelationship(currentSportsEdgesDF,   completeTable, currentIndex)
        currentHomeEdgesDF$SameCarrier     = addEdgeRelationship(currentHomeEdgesDF,     completeTable, currentIndex)
        currentOtherEdgesDF$SameCarrier    = addEdgeRelationship(currentOtherEdgesDF,    completeTable, currentIndex)
        
        # currentNetworksEdgesList = c(currentOverallEdgesDF, currentPhysicalEdgesDF,
        #                              currentSchoolEdgesDF,  currentSportsEdgesDF,
        #                              currentHomeEdgesDF,    currentOtherEdgesDF)
        
        myListOfEdgesDF = vector("list", length = TOTAL_NETWORKS)
        myListOfEdgesDF[[1]] = currentOverallEdgesDF
        myListOfEdgesDF[[2]] = currentPhysicalEdgesDF
        myListOfEdgesDF[[3]] = currentSchoolEdgesDF
        myListOfEdgesDF[[4]] = currentSportsEdgesDF
        myListOfEdgesDF[[5]] = currentHomeEdgesDF
        myListOfEdgesDF[[6]] = currentOtherEdgesDF
      }

      # Get the indexes for these new variables
      sameCarrierIndex = grep("^SameCarrier$", colnames(currentOverallEdgesDF))

      # Vectors with each of the same carrier relationship for each network
      sameCarrierVector = c(sum(currentOverallEdgesDF$SameCarrier),
                            sum(currentPhysicalEdgesDF$SameCarrier),
                            sum(currentSchoolEdgesDF$SameCarrier),
                            sum(currentSportsEdgesDF$SameCarrier),
                            sum(currentHomeEdgesDF$SameCarrier),
                            sum(currentOtherEdgesDF$SameCarrier))
      
      # Add the total relationships and total equal
      # We don't need this, but is ok to have it
      {
        
        biasResultsList[[i]][1,2] = nrow(currentOverallEdgesDF)
        biasResultsList[[i]][2,2] = nrow(currentPhysicalEdgesDF)
        biasResultsList[[i]][3,2] = nrow(currentSchoolEdgesDF)
        biasResultsList[[i]][4,2] = nrow(currentSportsEdgesDF)
        biasResultsList[[i]][5,2] = nrow(currentHomeEdgesDF)
        biasResultsList[[i]][6,2] = nrow(currentOtherEdgesDF)
        
        biasResultsList[[i]][1,3] = sum(currentOverallEdgesDF$SameCarrier  == TRUE)
        biasResultsList[[i]][2,3] = sum(currentPhysicalEdgesDF$SameCarrier == TRUE)
        biasResultsList[[i]][3,3] = sum(currentSchoolEdgesDF$SameCarrier   == TRUE)
        biasResultsList[[i]][4,3] = sum(currentSportsEdgesDF$SameCarrier   == TRUE)
        biasResultsList[[i]][5,3] = sum(currentHomeEdgesDF$SameCarrier     == TRUE)
        biasResultsList[[i]][6,3] = sum(currentOtherEdgesDF$SameCarrier    == TRUE)
        
        # biasSimulationsDF[1,2] = nrow(currentOverallEdgesDF)
        # biasSimulationsDF[2,2] = nrow(currentPhysicalEdgesDF)
        # biasSimulationsDF[3,2] = nrow(currentSchoolEdgesDF)
        # biasSimulationsDF[4,2] = nrow(currentSportsEdgesDF)
        # biasSimulationsDF[5,2] = nrow(currentHomeEdgesDF)
        # biasSimulationsDF[6,2] = nrow(currentOtherEdgesDF)
        # 
        # biasSimulationsDF[1,3] = sum(currentOverallEdgesDF$SameCarrier  == TRUE)
        # biasSimulationsDF[2,3] = sum(currentPhysicalEdgesDF$SameCarrier == TRUE)
        # biasSimulationsDF[3,3] = sum(currentSchoolEdgesDF$SameCarrier   == TRUE)
        # biasSimulationsDF[4,3] = sum(currentSportsEdgesDF$SameCarrier   == TRUE)
        # biasSimulationsDF[5,3] = sum(currentHomeEdgesDF$SameCarrier     == TRUE)
        # biasSimulationsDF[6,3] = sum(currentOtherEdgesDF$SameCarrier    == TRUE)
      }

      
      # For each of the networks, check the bias
      for(j in 1:TOTAL_NETWORKS){
      #for(j in 4:4){
       
        # Get the proper network
        #myNetworkEdgeDF = currentNetworksEdgesList[[j]]
        myNetworkEdgeDF = myListOfEdgesDF[[j]]

        # Check if carrier have bias friendship towards people with the same carrier status
        carrierBiasResult = doBiasAnalysis(completeTable, myNetworkEdgeDF, currentIndex, totalSimulations)
        carrierAverage    = mean(carrierBiasResult)
        carrierSD         = sd(carrierBiasResult)
        pValueCarrier     = pnorm(sameCarrierVector[[j]],
                                  mean=carrierAverage,
                                  sd=carrierSD, lower.tail=FALSE)
        
        # Save the results into the DF
        biasResultsList[[i]][j,   4 ] = min(carrierBiasResult)
        biasResultsList[[i]][j,   5 ] = as.integer(summary(carrierBiasResult)[2])
        biasResultsList[[i]][j,   6 ] = carrierAverage
        biasResultsList[[i]][j,   7 ] = as.integer(summary(carrierBiasResult)[5])
        biasResultsList[[i]][j,   8 ] = max(carrierBiasResult)
        biasResultsList[[i]][j,   9 ] = carrierSD
        biasResultsList[[i]][j,  10]  = pValueCarrier        
        
        # biasSimulationsDF[j,   4 ] = min(carrierBiasResult)
        # biasSimulationsDF[j,   5 ] = as.integer(summary(carrierBiasResult)[2])
        # biasSimulationsDF[j,   6 ] = carrierAverage
        # biasSimulationsDF[j,   7 ] = as.integer(summary(carrierBiasResult)[5])
        # biasSimulationsDF[j,   8 ] = max(carrierBiasResult)
        # biasSimulationsDF[j,   9 ] = carrierSD
        # biasSimulationsDF[j,(i+9)] = pValueCarrier
         
        #colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Q3", "MAX", "SD",  consecuenceNames  )
        
      }

    }
   
    # Manual change to column names for the two interesting variables
    #colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "Direct Culture", "Enrichment" )
     
    # Show the results
    for (i in 1:totalConsecuenceIndexes){

      print(biasResultsList[[i]])
      
    }
    

    
    
    
    #simulationResultsDF2$VIndex = NULL # Drop the VIndex as it doesn't have any relevant information for the analysis
    # print(biasSimulationsDF)
    # pValuesTable = writeTableLATEX(biasSimulationsDF, AUREUS_FOLDER,
    #                                overrideTableName = "carrierByNetwork",
    #                                tableCaption = "Summary values of the carrier simulations divided by network.",
    #                                intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
    #                                roundMe = 2, rotateColumnHeaders = TRUE)
    
  }
 
  # Check which variables have higher risk of carrier based on the network distribution
  if(TRUE){
    
    # We need to create a list of DFs, each DF has the result for each of the consequence indexes
    {
      # Prepare the list
      simulationByVariablesDFList      = vector("list", length = length(consecuenceNames))  
      
      # Create an empty DF that we are going to copy inside each element of the list
      # -- Count how many rows do we have
      importantTotalRows = 0
      for (i in 1:totalImportantCategoricalIndexes) {
        
        importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
        totalImportantModalities = length(importantModalities)
        
        importantTotalRows = importantTotalRows + totalImportantModalities
        
      }
      # -- Create the dataframe
      blankSimulationResultsDF       = data.frame(matrix(NA, nrow = importantTotalRows, ncol = 13))
      colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Real Relationships", "Real Same to Same", "Simulated Unbias Average Same", "Simulated Unbias Minimum Same", "Simulated Bias Average Same", "Simulated Bias SD", "Target Variable", "Base Risk", "Low 95CI", "High 95CI")
      # -- Init the proper columns
      importantIndex = 1
      for (i in 1:totalImportantCategoricalIndexes) {
        
        # Get the name of the variable
        currentImportantCategory = importantCategoricalNames[i]
        importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
        totalImportantModalities = length(importantModalities)
        
        # For each modality, add it to the dataframe
        for (j in 1:totalImportantModalities) {
          
          blankSimulationResultsDF[importantIndex,1] = currentImportantCategory
          blankSimulationResultsDF[importantIndex,2] = importantModalities[j]
          blankSimulationResultsDF[importantIndex,3] = importantCategoricalIndexes[i]
          
          importantIndex = importantIndex + 1
        }
        
      }
      
      # Copy inside each element of the list
      for(i in 1:totalConsecuenceIndexes){
        
        # Get the index and name
        currentConsecuenceIndex = consecuenceIndexes[i]
        currentConsecuenceName  = consecuenceNames[i]
        
        # Copy inside the list
        simulationByVariablesDFList[[i]] = blankSimulationResultsDF
        
        # Change the name of the variable
        colnames(simulationByVariablesDFList[[i]])[10] = currentConsecuenceName
      }
      
    }
    
    # Do the simulations for each of the elements on that list
    # so each consequence variable
    for (j in 1:totalConsecuenceIndexes) {
      
      # Get the index and name
      currentConsecuenceIndex = consecuenceIndexes[j]
      currentConsecuenceName  = consecuenceNames[j]
      
      print("-------")
      print("Doing the simulations for ")
      print(currentConsecuenceName)
      print("-------")
      
      # Count how many same to same relationships
      # (This is the same for all the variables and all modalities)
      currentOverallEdgesDF              = overallEdgesDF
      currentOverallEdgesDF$SameCarrier  = addEdgeRelationship(currentOverallEdgesDF,  completeTable, currentConsecuenceIndex)

      # Count how many real relationships and same to same
      # (This is the same for all the variables and all modalities)
      totalRelationships     = nrow(currentOverallEdgesDF)
      totalSameRelationships = sum(currentOverallEdgesDF$SameCarrier == TRUE)
      
      # Add that info to the dataframe
      simulationByVariablesDFList[[j]][,4] = totalRelationships
      simulationByVariablesDFList[[j]][,5] = totalSameRelationships

      # Find out the Same to Same simulation without any bias (this is the target to compare to)
      # (This is the same for all the variables and all modalities)
      print("Simulating base comparison")
      myUnbiasSimulationResult = doBiasAnalysis(completeTable, overallEdgesDF, consecuenceIndexes[j], totalSimulations,
                                                showProgressBar = FALSE)
      meanUnbiasSameRelationships    = summary(myUnbiasSimulationResult)[[4]]
      minimumUnbiasSameRelationships = summary(myUnbiasSimulationResult)[[1]]
      simulationByVariablesDFList[[j]][,6] = meanUnbiasSameRelationships
      simulationByVariablesDFList[[j]][,7] = minimumUnbiasSameRelationships
      
      # For each row in the table
      for(i in 1:importantTotalRows){
        
        # Get the variable index and modality name
        currentVariableIndex = simulationByVariablesDFList[[j]][i,3]
        currentModalityName  = simulationByVariablesDFList[[j]][i,2]
        
        # Give some feedback to the user
        print(  "-------")
        print(  "DOING INDEX: ")
        print(  paste(round(j/totalConsecuenceIndexes,2)*100 , "%"),       sep='')
        print(  "-------")
        print(  paste(round(i/importantTotalRows,2)*100 , "%"),       sep='')
        print(  paste("For variable:         ", currentVariableIndex, sep='') )
        print(  paste("Doing simulation for: ", currentModalityName,  sep='') )
        
        mySimulationResult = doBiasAnalysis(completeTable, overallEdgesDF, consecuenceIndexes[j], totalSimulations,
                                            overrideFrequenciesIndex = currentVariableIndex,
                                            overrideBaseOnValue      = currentModalityName,
                                            showProgressBar = FALSE)

        # Get how much is the average and sd of the simulations
        meanSameRelationships = summary(mySimulationResult)[[4]]
        sdSameRelationships   = sd(mySimulationResult)
        
        simulationByVariablesDFList[[j]][i,8] = meanSameRelationships
        simulationByVariablesDFList[[j]][i,9] = sdSameRelationships
        

        # Find the p-value of that
        simulationByVariablesDFList[[j]][i,10] = pnorm(meanSameRelationships, mean= meanUnbiasSameRelationships, sd=sdSameRelationships, lower.tail=FALSE)

      }
      
    }
    
    
    # Crap code, delete
    if(FALSE){
      originalNumbersDF$S1_C_Colonize   = pnorm(originalNumbersDF$`Simulated Bias Average Same`,   originalNumbersDF$`Simulated Unbias Average Same`,   originalNumbersDF$`Simulated Bias SD`,   lower.tail=FALSE)
      enrichmentNumbersDF$S1_C_Colonize = pnorm(enrichmentNumbersDF$`Simulated Bias Average Same`, enrichmentNumbersDF$`Simulated Unbias Average Same`, enrichmentNumbersDF$`Simulated Bias SD`, lower.tail=FALSE)
      directNumbersDF$S1_C_Colonize     = pnorm(directNumbersDF$`Simulated Bias Average Same`,     directNumbersDF$`Simulated Unbias Average Same`,     directNumbersDF$`Simulated Bias SD`,     lower.tail=FALSE)
      
      enrichmentNumbersDF2 = enrichmentNumbersDF
      enrichmentNumbersDF2$Index = NULL
      enrichmentNumbersDF2$`Real Relationships` = NULL
      enrichmentNumbersDF2$`Real Same to Same` = NULL
      enrichmentNumbersDF2$`Simulated Unbias Minimum Same` = NULL
      
      colnames(enrichmentNumbersDF2) = c("Variable", "Category", "UnbiasAv", "BiasAv", "BiasSD", "C_NasalCarrier", "Base", "Low", "High", "p")
      
      enrichmentNumbersDF2$pr = round(enrichmentNumbersDF2$p,3)
      
      enrichmentNumbersDF2$Base = round(enrichmentNumbersDF2$Base,3)
      enrichmentNumbersDF2$Low  = round(enrichmentNumbersDF2$Low, 3)
      enrichmentNumbersDF2$High = round(enrichmentNumbersDF2$High,3)
      
    }
    
    # Find the relative risk with respect each variable group
    for (j in 1:totalConsecuenceIndexes){
      
      currentReferenceIndex = simulationByVariablesDFList[[j]][1,3]
      currentBaseValue      = simulationByVariablesDFList[[j]][1,8]
      currentOrderValue     = 1
      for(i in 1:importantTotalRows){
        
        # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
        simulationByVariablesDFList[[j]][i,11] = simulationByVariablesDFList[[j]][i,8] / currentBaseValue
        
        # Get the lower and upper intervals for a CI of 95%
        if(currentOrderValue == 1){
          simulationByVariablesDFList[[j]][i,12] = NA
          simulationByVariablesDFList[[j]][i,13] = NA
        }
        else{
          simulationByVariablesDFList[[j]][i,12] = (-1.96 * simulationByVariablesDFList[[j]][i,9] + simulationByVariablesDFList[[j]][i,8]) / currentBaseValue
          simulationByVariablesDFList[[j]][i,13] = (+1.96 * simulationByVariablesDFList[[j]][i,9] + simulationByVariablesDFList[[j]][i,8]) / currentBaseValue
        }
          
        
        # Is there a next index?
        # -- If yes...
        if(i<importantTotalRows){
          # Which one?
          nextReferenceIndex = simulationByVariablesDFList[[j]][(i+1),3]
          # Is the same as the current?
          # -- If yes...
          #    Take the next order
          if(currentReferenceIndex == nextReferenceIndex){
            
            currentOrderValue = currentOrderValue + 1
            
          }
          # -- If no...
          #    Change the base value and reference index, reset order to 1
          else{
            currentOrderValue = 1
            currentReferenceIndex = nextReferenceIndex
            currentBaseValue      = simulationByVariablesDFList[[j]][(i+1),8]
            
            
          }
          
        }
        # -- If no... we are finish
         
      }
      
    }
    
    # For each of the resulting DF, write that on disk
    {
      
      for (j in 1:totalConsecuenceIndexes){
        
        # Get the index and name
        currentConsecuenceIndex = consecuenceIndexes[j]
        currentConsecuenceName  = consecuenceNames[j]
        
        # Prepare the latex special fields
        currentOverrideTableName = paste0("carrierByVariable_",currentConsecuenceName)
        currentTableCaption      = paste0("Summary values of simulations for each modality for variable ",currentConsecuenceName)
        
        
        pValuesTable = writeTableLATEX(simulationByVariablesDFList[[j]], AUREUS_FOLDER,
                                       overrideTableName = currentOverrideTableName,
                                       tableCaption = currentTableCaption,
                                       intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                       roundMe = 2, rotateColumnHeaders = TRUE)
        
      }
      
      # Show and write the results
      # simulationResultsDF2Significant = simulationResultsDF2
      # for(i in 1:totalConsecuenceIndexes){
      #   
      #   simulationResultsDF2Significant[,(i+3)] = signif(simulationResultsDF2Significant[,(i+3)],2)
      #   
      # }
      

      
      
    }

    
    source("latex.R", encoding="utf-8")
    
  }
  
  
  
  # Finally, we are going to add the hormonal contraceptives to the table
  # but we do half-random network since the men don't take any contraceptives.
  if(FALSE){
    
    # We need to create a list of DFs, each DF has the result for each of the consequence indexes
    {
      
      # Count how many modalities we have (4, we know already)
      importantTotalRows = 4
      
      # Prepare the list
      simulationByVariablesDFList      = vector("list", length = length(consecuenceNames))  
      
      # Prepare the empty DF
      hormornalSimulationResultsDF = data.frame(matrix(NA, nrow = importantTotalRows, ncol = 13))
      colnames(hormornalSimulationResultsDF) = c("Variable", "Modality", "Index", "Real Relationships", "Real Same to Same", "Simulated Unbias Average Same", "Simulated Unbias Minimum Same", "Simulated Bias Average Same", "Simulated Bias SD", "Target Variable", "Base Risk", "Low 95CI", "High 95CI")
      
      hormornalSimulationResultsDF[1,1] = "Hormonal Contraceptives"
      hormornalSimulationResultsDF[2,1] = "Hormonal Contraceptives"
      hormornalSimulationResultsDF[3,1] = "Hormonal Contraceptives"
      hormornalSimulationResultsDF[4,1] = "Hormonal Contraceptives"
      
      hormornalSimulationResultsDF[1,2] = "None or non-hormonal"
      hormornalSimulationResultsDF[2,2] = "Progestin"
      hormornalSimulationResultsDF[3,2] = "Low Estrogen"
      hormornalSimulationResultsDF[4,2] = "High Estrogen"

      hormornalSimulationResultsDF[1,3] = hormonalIndex
      hormornalSimulationResultsDF[2,3] = hormonalIndex
      hormornalSimulationResultsDF[3,3] = hormonalIndex
      hormornalSimulationResultsDF[4,3] = hormonalIndex
      
      # Copy inside each element of the list
      for(i in 1:totalConsecuenceIndexes){
        
        # Get the index and name
        currentConsecuenceIndex = consecuenceIndexes[i]
        currentConsecuenceName  = consecuenceNames[i]
        
        # Copy inside the list
        simulationByVariablesDFList[[i]] = hormornalSimulationResultsDF
        
        # Change the name of the variable
        colnames(simulationByVariablesDFList[[i]])[10] = currentConsecuenceName
      }
     
      
       
    }
   
    # Do the simulations for each of the elements on that list
    # so each consequence variable
    for (j in 1:totalConsecuenceIndexes) {
      
      # Get the index and name
      currentConsecuenceIndex = consecuenceIndexes[j]
      currentConsecuenceName  = consecuenceNames[j]
      
      print("-------")
      print("Doing the simulations for ")
      print(currentConsecuenceName)
      print("-------")
      
      # Find out the Same to Same simulation without any bias (this is the target to compare to)
      # (This is the same for all the variables and all modalities, but changes for each consequence index)
      print("Simulating base comparison")
      myUnbiasSimulationResult = doBiasAnalysis(completeTable, overallEdgesDF, consecuenceIndexes[j], totalSimulations,
                                                showProgressBar = FALSE)
      meanUnbiasSameRelationships    = summary(myUnbiasSimulationResult)[[4]]
      minimumUnbiasSameRelationships = summary(myUnbiasSimulationResult)[[1]]
      simulationByVariablesDFList[[j]][,6] = meanUnbiasSameRelationships
      simulationByVariablesDFList[[j]][,7] = minimumUnbiasSameRelationships
      
      # Count how many same to same relationships
      # (This is the same for all the variables and all modalities)
      currentOverallEdgesDF              = overallEdgesDF
      currentOverallEdgesDF$SameCarrier  = addEdgeRelationship(currentOverallEdgesDF,  completeTable, currentConsecuenceIndex)
      totalRelationships                 = nrow(currentOverallEdgesDF)
      totalSameRelationships             = sum(currentOverallEdgesDF$SameCarrier == TRUE)
      
      # Add that info to the dataframe
      simulationByVariablesDFList[[j]][,4] = totalRelationships
      simulationByVariablesDFList[[j]][,5] = totalSameRelationships
      
      # Here we are going to do a pseudo-copypaste of the doBiasAnalysis() function
      # Because what we are doing is a bit bizarre and we are in a bit of a rush
      # to change the function properly. But this neeeds to be change into the
      # function properly so the code is more readable and makes more sense.
      # This part of the code is common for each simulation
      {
        
        # Get the frequency of positive/negative for contraceptives
        # -- For men is always constant
        frequencyTableMen   = summarizeCategorical(menOnlyTable, currentConsecuenceIndex)
        # -- For women we have 4 possibilities
        NoneFilter = womenMenstruatingOnlyTable[womenMenstruatingOnlyTable$Hormonal == "None or non-hormonal",]
        frequencyTableWomenNone          = summarizeCategorical(NoneFilter, currentConsecuenceIndex)
        ProgestinFilter = womenMenstruatingOnlyTable[womenMenstruatingOnlyTable$Hormonal == "Progestin",]
        frequencyTableWomenProgestin     = summarizeCategorical(ProgestinFilter, currentConsecuenceIndex)
        LowEstrogenFilter                = womenMenstruatingOnlyTable[womenMenstruatingOnlyTable$Hormonal == "Low Estrogen",]
        frequencyTableWomenLowEstrogen   = summarizeCategorical(LowEstrogenFilter, currentConsecuenceIndex)
        HighEstrogenFilter               = womenMenstruatingOnlyTable[womenMenstruatingOnlyTable$Hormonal == "High Estrogen",]
        frequencyTableWomenHighEstrogen  = summarizeCategorical(HighEstrogenFilter, currentConsecuenceIndex)
        
        # Make the frequencies into a list that we will call later with the proper index
        frequencyTableWomenList = vector("list", length = 4)  
        frequencyTableWomenList[[1]] = frequencyTableWomenNone
        frequencyTableWomenList[[2]] = frequencyTableWomenProgestin 
        frequencyTableWomenList[[3]] = frequencyTableWomenLowEstrogen
        frequencyTableWomenList[[4]] = frequencyTableWomenHighEstrogen
        
        # Generate the patient table
        basePatientTable = data.frame(matrix(NA, nrow = totalPeople, ncol = 2))
        colnames(basePatientTable) = c("ID", "TargetVariable")
        basePatientTable[,1] = completeTable[,1]
        # The special part here is that men maintain the frequency of contraceptives
        # While women are randomized
        # (This change for every simulation actually, here we just init to something
        #  and we change it later again)
        currentMenSample      = sample(frequencyTableMen$Modality, size = totalMenRows, replace = TRUE, prob = frequencyTableMen$Relative)
        currentMenSampleIndex = 1

        currentWomenSampleNone           = sample(frequencyTableWomenNone$Modality,         size = totalWomenRows, replace = TRUE, prob = frequencyTableWomenNone$Relative)
        currentWomenNoneSampleIndex      = 1
        currentWomenSampleProgestin      = sample(frequencyTableWomenProgestin$Modality,    size = totalWomenRows, replace = TRUE, prob = frequencyTableWomenProgestin$Relative)
        currentWomenProgestinSampleIndex = 1
        currentWomenSampleLowEstrogen    = sample(frequencyTableWomenLowEstrogen$Modality,  size = totalWomenRows, replace = TRUE, prob = frequencyTableWomenLowEstrogen$Relative)
        currentWomenLowSampleIndex       = 1
        currentWomenSampleHighEstrogen   = sample(frequencyTableWomenHighEstrogen$Modality, size = totalWomenRows, replace = TRUE, prob = frequencyTableWomenHighEstrogen$Relative)
        currentWomenHighSampleIndex      = 1

        # Gives the positive/negative value to each person
        for(z in 1:totalPeople){
          
          currentSex = completeTable[z,sexIndex]
          
          # If you are a man, you take no contraceptives, or non hormonal contraceptives
          if(currentSex == "Man"){
            
            basePatientTable[z,2] = as.character(currentMenSample[currentMenSampleIndex])
            currentMenSampleIndex = currentMenSampleIndex + 1
            
          }
            
          # If you are a woman, you get your variable randomized from the hormonal table
          # This change for each case (None, Progestin, Low and High), since this is just
          # a silly initialization, we use None only
          else{
            
            basePatientTable[z,2] = as.character(currentWomenSampleNone[currentWomenNoneSampleIndex])
            currentWomenNoneSampleIndex = currentWomenNoneSampleIndex + 1
            
          } 
            
        }
        
        # -- Relationships
        #    In this case, relationships are never random, so we skip all the
        #    randomizing part of the function and keep the from, to and same
        #    column forever.
        baseFrienshipDF           = data.frame(matrix(NA, nrow = totalRelationships, ncol = 4))
        colnames(baseFrienshipDF) = c("from","to","value","SameRelationship")
        baseFrienshipDF[,1] = currentOverallEdgesDF[i,1]
        baseFrienshipDF[,2] = currentOverallEdgesDF[i,2]
        baseFrienshipDF[,3] = currentOverallEdgesDF[i,3] # value is useless here, but whatever
        baseFrienshipDF[,4] = currentOverallEdgesDF[i,4] # This will change in the simulations, just init to whatever

      }
      
      # For each row in the table (4 for HC)
      for(i in 1:importantTotalRows){

        # Get the variable index and modality name
        currentVariableIndex = simulationByVariablesDFList[[j]][i,3]
        currentModalityName  = simulationByVariablesDFList[[j]][i,2]        
      
        # Give some feedback to the user
        print(  "-------")
        print(  "DOING INDEX: ")
        print(  paste(round(j/totalConsecuenceIndexes,2)*100 , "%"),       sep='')
        print(  "-------")
        print(  paste(round(i/importantTotalRows,2)*100 , "%"),       sep='')
        print(  paste("For variable:         ", currentVariableIndex, sep='') )
        print(  paste("Doing simulation for: ", currentModalityName,  sep='') )
        
        # Prepare the vector with the bootsraps results
        bootstrapVector      = rep(0,totalSimulations)
        
        # Do the simulations in here
        for(y in 1:totalSimulations){
          
          # Here we repeat the pseudo-code from before
          {

            # Generate the patient table
            basePatientTable = data.frame(matrix(NA, nrow = totalPeople, ncol = 2))
            colnames(basePatientTable) = c("ID", "TargetVariable")
            basePatientTable[,1] = completeTable[,1]
            # The special part here is that men maintain the frequency of contraceptives
            # While women are randomized
            # (This change for every simulation actually, here we just init to something
            #  and we change it later again)
            currentMenSample      = sample(frequencyTableMen$Modality, size = totalMenRows, replace = TRUE, prob = frequencyTableMen$Relative)
            currentMenSampleIndex = 1
            
            currentWomenHormonalSample = sample(frequencyTableWomenList[[i]]$Modality, size = totalWomenRows, replace = TRUE, prob = frequencyTableWomenList[[i]]$Relative)
            currentWomenHormonalIndex = 1

            # Gives the positive/negative value to each person
            for(z in 1:totalPeople){
              
              currentSex = completeTable[z,sexIndex]
              
              # If you are a man, you take no contraceptives, or non hormonal contraceptives
              if(currentSex == "Man"){
                
                basePatientTable[z,2] = as.character(currentMenSample[currentMenSampleIndex])
                currentMenSampleIndex = currentMenSampleIndex + 1
                
              }
              
              # If you are a woman, you get your variable randomized from the hormonal table
              else{
                
                basePatientTable[z,2] = as.character(currentWomenHormonalSample[currentWomenHormonalIndex])
                currentWomenHormonalIndex = currentWomenHormonalIndex + 1
                
              } 
              
            }
            
            # For each relationship, check if they have same to same relationship and add it to the list
            for(z in 1:totalRelationships){

              # Get the two persons that have that relationship
              targetFrom = baseFrienshipDF[z,1]
              targetTo   = baseFrienshipDF[z,2]

              # Search for those patients in the patient table and get their variable
              # (This can be done more efficiently, but right now the function asume
              #  that IDs are not sequential)
              targetTypeFrom = basePatientTable[basePatientTable[,1] == targetFrom,2]
              targetTypeTo   = basePatientTable[basePatientTable[,1] == targetTo,  2]             
              
              # Add the relationship same-to-same status to the list of relationships
              baseFrienshipDF$SameRelationship[z] = (targetTypeFrom == targetTypeTo)
              
            }
            
          }
          
          # The current simulation is finish, just add the result to the vector
          bootstrapVector[y] = sum(baseFrienshipDF$SameRelationship)
          
        }
        
        # Add the simulation results to the results dataframe
        mySimulationResult = bootstrapVector
        # -- Get how much is the average and sd of the simulations
        meanSameRelationships = summary(mySimulationResult)[[4]]
        sdSameRelationships   = sd(mySimulationResult)
        
        simulationByVariablesDFList[[j]][i,8] = meanSameRelationships
        simulationByVariablesDFList[[j]][i,9] = sdSameRelationships
        
        # Find the p-value of that
        simulationByVariablesDFList[[j]][i,10] = pnorm(meanSameRelationships, mean= meanUnbiasSameRelationships, sd=sdSameRelationships, lower.tail=FALSE)
        
        
      }
      
      

      

      
      
      
      
      
    }
    
    # Find the relative risk with respect each variable group
    for (j in 1:totalConsecuenceIndexes){
      
      currentReferenceIndex = simulationByVariablesDFList[[j]][1,3]
      currentBaseValue      = simulationByVariablesDFList[[j]][1,8]
      currentOrderValue     = 1
      for(i in 1:importantTotalRows){
        
        # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
        simulationByVariablesDFList[[j]][i,11] = simulationByVariablesDFList[[j]][i,8] / currentBaseValue
        
        # Get the lower and upper intervals for a CI of 95%
        if(currentOrderValue == 1){
          simulationByVariablesDFList[[j]][i,12] = NA
          simulationByVariablesDFList[[j]][i,13] = NA
        }
        else{
          simulationByVariablesDFList[[j]][i,12] = (-1.96 * simulationByVariablesDFList[[j]][i,9] + simulationByVariablesDFList[[j]][i,8]) / currentBaseValue
          simulationByVariablesDFList[[j]][i,13] = (+1.96 * simulationByVariablesDFList[[j]][i,9] + simulationByVariablesDFList[[j]][i,8]) / currentBaseValue
        }
        
        
        # Is there a next index?
        # -- If yes...
        if(i<importantTotalRows){
          # Which one?
          nextReferenceIndex = simulationByVariablesDFList[[j]][(i+1),3]
          # Is the same as the current?
          # -- If yes...
          #    Take the next order
          if(currentReferenceIndex == nextReferenceIndex){
            
            currentOrderValue = currentOrderValue + 1
            
          }
          # -- If no...
          #    Change the base value and reference index, reset order to 1
          else{
            currentOrderValue = 1
            currentReferenceIndex = nextReferenceIndex
            currentBaseValue      = simulationByVariablesDFList[[j]][(i+1),8]
            
            
          }
          
        }
        # -- If no... we are finish
        
      }
      
    }
    
    # For each of the resulting DF, show results
    {
     
      for (j in 1:totalConsecuenceIndexes){
      
        simulationByVariablesDFList[[j]]  
        
      }
       
      
      
    }
    
    
    
    
  }
  
  
  
  # Check which variables have higher risk of carrier based on the network distribution
  # Old code? gee, this need a complete clean up
  if(FALSE){
    
    # Create the DF where to save the data
    importantTotalRows = 0
    for (i in 1:totalImportantCategoricalIndexes) {
      
      
      importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
      totalImportantModalities = length(importantModalities)
      
      importantTotalRows = importantTotalRows + totalImportantModalities
      
    }
    simulationResultsDF2          = data.frame(matrix(NA, nrow = importantTotalRows, ncol = 11))
    colnames(simulationResultsDF2) = c("Variable", "Modality", "VIndex", "Min", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max.", "sd", "p-value")
    
    # Initialize the concepts
    importantIndex = 1
    for (i in 1:totalImportantCategoricalIndexes) {
      
      currentImportantCategory = importantCategoricalNames[i]
      importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
      totalImportantModalities = length(importantModalities)
      
      for (j in 1:totalImportantModalities) {
        
        simulationResultsDF2[importantIndex,1] = currentImportantCategory
        simulationResultsDF2[importantIndex,2] = importantModalities[j]
        simulationResultsDF2[importantIndex,3] = importantCategoricalIndexes[i]
        
        importantIndex = importantIndex + 1
      }
      
    }
    
    # Do the simulations
    for(i in 1:importantTotalRows){
      
      print(  paste(round(i/importantTotalRows,2)*100 , "%"),            sep='')
      print(  paste("Doing simulation for: ", simulationResultsDF2[i,2], sep='') )
      
      mySimulationResult = doBiasAnalysis(completeTable, overallEdgesDF, carrierIndex, totalSimulations,
                                          overrideFrequenciesIndex = simulationResultsDF2[i,3],
                                          overrideBaseOnValue      = simulationResultsDF2[i,2])
      
      simulationResultsDF2[i,4]  = summary(mySimulationResult)[[1]]
      simulationResultsDF2[i,5]  = summary(mySimulationResult)[[2]]
      simulationResultsDF2[i,6]  = summary(mySimulationResult)[[3]]
      simulationResultsDF2[i,7]  = summary(mySimulationResult)[[4]]
      simulationResultsDF2[i,8]  = summary(mySimulationResult)[[5]]
      simulationResultsDF2[i,9]  = summary(mySimulationResult)[[6]]
      simulationResultsDF2[i,10] = sd(mySimulationResult)
      simulationResultsDF2[i,11] = pnorm(sameCarrierRelationships, mean=summary(mySimulationResult)[[4]], sd=sd(mySimulationResult), lower.tail=FALSE)
      
      
    }
    
  }
  
  # Old code, do nothing, just saved
  if(FALSE){
    # Prepare some variables
    {
      
      # Count how many nodes you have
      totalNodes         = nrow(positiveTableOnly)
      totalRelationships = nrow(carriersOnlyOverallEdgesDF)
      
      # For the positive only edges, add if they have the same SPAT1
      carriersOnlyOverallEdgesDF$SameSPAT1 = addEdgeRelationship(carriersOnlyOverallEdgesDF, positiveTableOnly, spaT1IndexComplete)
      
      # For the whole edge list, add if they have the same:
      # -- Carrier status
      overallEdgesDF$SameCarrier  = addEdgeRelationship(overallEdgesDF,  completeTable, carrierIndex)
      physicalEdgesDF$SameCarrier = addEdgeRelationship(physicalEdgesDF, completeTable, carrierIndex)
      schoolEdgesDF$SameCarrier   = addEdgeRelationship(schoolEdgesDF,   completeTable, carrierIndex)
      sportsEdgesDF$SameCarrier   = addEdgeRelationship(sportsEdgesDF,   completeTable, carrierIndex)
      homeEdgesDF$SameCarrier     = addEdgeRelationship(homeEdgesDF,     completeTable, carrierIndex)
      otherEdgesDF$SameCarrier    = addEdgeRelationship(otherEdgesDF,    completeTable, carrierIndex)
      
      # -- Nasal carrier status
      overallEdgesDF$SameNasalCarrier  = addEdgeRelationship(overallEdgesDF,  completeTable, nasalCarrierIndex)
      physicalEdgesDF$SameNasalCarrier = addEdgeRelationship(physicalEdgesDF, completeTable, nasalCarrierIndex)
      schoolEdgesDF$SameNasalCarrier   = addEdgeRelationship(schoolEdgesDF,   completeTable, nasalCarrierIndex)
      sportsEdgesDF$SameNasalCarrier   = addEdgeRelationship(sportsEdgesDF,   completeTable, nasalCarrierIndex)
      homeEdgesDF$SameNasalCarrier     = addEdgeRelationship(homeEdgesDF,     completeTable, nasalCarrierIndex)
      otherEdgesDF$SameNasalCarrier    = addEdgeRelationship(otherEdgesDF,    completeTable, nasalCarrierIndex)
      # -- Throat carrier status
      overallEdgesDF$SameThroatCarrier  = addEdgeRelationship(overallEdgesDF,  completeTable, throatCarrierIndex)
      physicalEdgesDF$SameThroatCarrier = addEdgeRelationship(physicalEdgesDF, completeTable, throatCarrierIndex)
      schoolEdgesDF$SameThroatCarrier   = addEdgeRelationship(schoolEdgesDF,   completeTable, throatCarrierIndex)
      sportsEdgesDF$SameThroatCarrier   = addEdgeRelationship(sportsEdgesDF,   completeTable, throatCarrierIndex)
      homeEdgesDF$SameThroatCarrier     = addEdgeRelationship(homeEdgesDF,     completeTable, throatCarrierIndex)
      otherEdgesDF$SameThroatCarrier    = addEdgeRelationship(otherEdgesDF,    completeTable, throatCarrierIndex)
      
      
      # Get the indexes for these new variables
      sameSPAT1Index             = grep("^SameSPAT1$",         colnames(carriersOnlyOverallEdgesDF))
      sameCarrierIndex           = grep("^SameCarrier$",       colnames(overallEdgesDF))
      sameNasalCarrierIndex      = grep("^SameNasalCarrier$",  colnames(overallEdgesDF))
      sameThroatCarrierIndex     = grep("^SameThroatCarrier$", colnames(overallEdgesDF))
      
      
      # How many relationships we have with the same SPAT1
      sameSPAT1Relationships   = sum(carriersOnlyOverallEdgesDF$SameSPAT1)
      
      # How many relationships we have with the same carrier
      sameCarrierRelationships       = sum(overallEdgesDF$SameCarrier)
      sameNasalCarrierRelationships  = sum(overallEdgesDF$SameNasalCarrier)
      sameThroatCarrierRelationships = sum(overallEdgesDF$SameThroatCarrier)
      
      # Vectors with a lot of information
      sameCarrierVector = c(sum(overallEdgesDF$SameCarrier),
                            sum(physicalEdgesDF$SameCarrier),
                            sum(schoolEdgesDF$SameCarrier),
                            sum(sportsEdgesDF$SameCarrier),
                            sum(homeEdgesDF$SameCarrier),
                            sum(otherEdgesDF$SameCarrier))
      
      sameNasalCarrierVector = c(sum(overallEdgesDF$SameNasalCarrier),
                                 sum(physicalEdgesDF$SameNasalCarrier),
                                 sum(schoolEdgesDF$SameNasalCarrier),
                                 sum(sportsEdgesDF$SameNasalCarrier),
                                 sum(homeEdgesDF$SameNasalCarrier),
                                 sum(otherEdgesDF$SameNasalCarrier))
      
      sameThroatCarrierVector = c(sum(overallEdgesDF$SameThroatCarrier),
                                  sum(physicalEdgesDF$SameThroatCarrier),
                                  sum(schoolEdgesDF$SameThroatCarrier),
                                  sum(sportsEdgesDF$SameThroatCarrier),
                                  sum(homeEdgesDF$SameThroatCarrier),
                                  sum(otherEdgesDF$SameThroatCarrier))
      
      
    }
    
    # Check if SPATypes have bias friendship towards people with same SPA Types. 
    {
      spaBiasResult = doBiasAnalysis(positiveTableOnly, carriersOnlyOverallEdgesDF, spaT1IndexComplete, totalSimulations)
      spaAverage    = mean(spaBiasResult)
      spaSD         = sd(spaBiasResult)
      
      summaryDF     = summary(spaBiasResult)
      
      summaryDF           = as.data.frame(t(summaryDF))[,2:3]
      colnames(summaryDF) = c("Concept", "Value")
      
      writeTableLATEX(summaryDF, AUREUS_FOLDER, roundMe = 2,
                      tableCaption = "Summary values of the SPAT1 simulations. Values are number of relationships with same SPA Type.",
                      overrideTableName = "spaBiasSimulation")  
      
      
      pValueSPA     = pnorm(sameSPAT1Relationships, mean=spaAverage, sd=spaSD, lower.tail=FALSE)
      
    }
    
    # Check if carrier have bias friendship towards people with the same carrier status
    {
      
      carrierBiasResult = doBiasAnalysis(completeTable, overallEdgesDF, carrierIndex, totalSimulations)
      
      carrierAverage    = mean(carrierBiasResult)
      carrierSD         = sd(carrierBiasResult)
      
      summaryDF     = summary(carrierBiasResult)
      
      summaryDF           = as.data.frame(t(summaryDF))[,2:3]
      colnames(summaryDF) = c("Concept", "Value")
      
      writeTableLATEX(summaryDF, AUREUS_FOLDER, roundMe = 2,
                      tableCaption = "Summary values of the carrier simulations. Values are number of relationships with same carrier status.",
                      overrideTableName = "carrierBiasSimulation")  
      
      
      pValueCarrier = pnorm(sameCarrierRelationships, mean=carrierAverage, sd=carrierSD, lower.tail=FALSE)
      
    }
    
    # Check if carrier have bias friendship towards people with the same nasal carrier status
    {
      
      nasalCarrierBiasResult = doBiasAnalysis(completeTable, overallEdgesDF, nasalCarrierIndex, totalSimulations)
      
      carrierAverage    = mean(nasalCarrierBiasResult)
      carrierSD         = sd(nasalCarrierBiasResult)
      
      summaryDF     = summary(nasalCarrierBiasResult)
      
      summaryDF           = as.data.frame(t(summaryDF))[,2:3]
      colnames(summaryDF) = c("Concept", "Value")
      
      writeTableLATEX(summaryDF, AUREUS_FOLDER, roundMe = 2,
                      tableCaption = "Summary values of the nasal carrier simulations. Values are number of relationships with same nasal carrier status.",
                      overrideTableName = "nasalCarrierBiasSimulation")  
      
      
      pValueNasalCarrier = pnorm(sameNasalCarrierRelationships, mean=carrierAverage, sd=carrierSD, lower.tail=FALSE)
      
      print(pValueNasalCarrier)
      print(summaryDF)
      
    }
    
    # Check if carrier have bias friendship towards people with the same throat carrier status
    {
      
      throatCarrierBiasResult = doBiasAnalysis(completeTable, overallEdgesDF, throatCarrierIndex, totalSimulations)
      
      carrierAverage    = mean(throatCarrierBiasResult)
      carrierSD         = sd(throatCarrierBiasResult)
      
      summaryDF     = summary(throatCarrierBiasResult)
      
      summaryDF           = as.data.frame(t(summaryDF))[,2:3]
      colnames(summaryDF) = c("Concept", "Value")
      
      writeTableLATEX(summaryDF, AUREUS_FOLDER, roundMe = 2,
                      tableCaption = "Summary values of the throat carrier simulations. Values are number of relationships with same throat carrier status.",
                      overrideTableName = "throatCarrierBiasSimulation")  
      
      
      pValueThroatCarrier = pnorm(sameThroatCarrierRelationships, mean=carrierAverage, sd=carrierSD, lower.tail=FALSE)
      
    }
    
    # print(pValueSPA)
    # print(pValueCarrier)
    # print(pValueNasalCarrier)
    # print(pValueThroatCarrier)
  }

  # Old code? Check if the bias of carrier change with respect the network
  if(FALSE){
    
    carrierNetworkDF           =  data.frame(matrix(NA, nrow = TOTAL_NETWORKS, ncol = 6))
    colnames(carrierNetworkDF) = c("Network", "Sim Average", "Real", "Carrier pv", "Nasal pv", "Throat pv")
    
    for(i in 1:TOTAL_NETWORKS){
      
      # Name of the network
      carrierNetworkDF[i,1] = NETWORK_NAMES[i]
    
      # Analysis for the carrier
      carrierBiasResult = doBiasAnalysis(completeTable, allEdges[[i]], carrierIndex, totalSimulations)
      carrierAverage    = mean(carrierBiasResult)
      carrierSD         = sd(carrierBiasResult)
      pValueCarrier = pnorm(sameCarrierVector[i], mean=carrierAverage, sd=carrierSD, lower.tail=FALSE)

      # Analysis for the nasal carrier
      carrierBiasResult = doBiasAnalysis(completeTable, allEdges[[i]], nasalCarrierIndex, totalSimulations)
      carrierAverage    = mean(carrierBiasResult)
      carrierSD         = sd(carrierBiasResult)
      nasalPValueCarrier = pnorm(sameCarrierVector[i], mean=carrierAverage, sd=carrierSD, lower.tail=FALSE)
      
      # Analysis for the throat carrier
      carrierBiasResult = doBiasAnalysis(completeTable, allEdges[[i]], throatCarrierIndex, totalSimulations)
      carrierAverage    = mean(carrierBiasResult)
      carrierSD         = sd(carrierBiasResult)
      throatPValueCarrier = pnorm(sameCarrierVector[i], mean=carrierAverage, sd=carrierSD, lower.tail=FALSE)
      
      # Write results in the table
      carrierNetworkDF[i,2] = carrierAverage
      carrierNetworkDF[i,3] = sameCarrierVector[i]
      carrierNetworkDF[i,4] = pValueCarrier
      carrierNetworkDF[i,5] = nasalPValueCarrier
      carrierNetworkDF[i,6] = throatPValueCarrier
      
    }
  }
  
  # Show final results (deprecated, shown in above code)
  {



    
    
    # Take away the unknown and keep the relevant p-values only. Sort by P-value
    # Delete all the descriptive statistics
    # simulationSimplified = simulationResultsDF2
    # simulationSimplified[,3:9] = NULL
    # simulationSimplified = simulationSimplified[simulationSimplified$Modality != "Unknown",]
    # simulationSimplified = simulationSimplified[simulationSimplified$`p-value` < 0.05,]
    # simulationSimplified = simulationSimplified[with(simulationSimplified, order(`p-value`)), ]
    # pValuesTable = writeTableLATEX(simulationSimplified, AUREUS_FOLDER,
    #                                overrideTableName = "carrierByModalitySimplified",
    #                                tableCaption = "Simplified summary  divided by categories and modalities.",
    #                                intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
    #                                roundMe = 2)
    
    # The network with respect SA carrier only
    # print(carrierNetworkDF)
    # pValuesTable = writeTableLATEX(carrierNetworkDF, AUREUS_FOLDER,
    #                                overrideTableName = "carrierByTypeOfNetwork",
    #                                tableCaption = "Simulating the bias of SA carrier with respect each network",
    #                                intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
    #                                roundMe = 2)
    
    
  }
  
}

# ********************************************
#     ISOLATION AND FRIENSHIP RESPECT SA
# ********************************************  
print("Isolation") 
if(TRUE){
  
  # Create the table with the people in physical isolation
  physicalIsolatedTable = completeTable[completeTable$PhysicalPopularity == 0,]
  totalPhysicalIsolationRows = nrow(physicalIsolatedTable)
  
  # Create the DF where to save the data
  # -- Each of the categorical variables
  importantTotalRows = 0
  for (i in 1:totalImportantCategoricalIndexes) {
    
    importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
    totalImportantModalities = length(importantModalities)
    
    importantTotalRows = importantTotalRows + totalImportantModalities
    
  }
  # -- Each of the consequence variables
  importantTotalRows = importantTotalRows + (totalConsecuenceIndexes * 2)
  
  isolationDF           = data.frame(matrix(NA, nrow = importantTotalRows, ncol = 6))
  colnames(isolationDF) = c("Variable", "Modality", "Index", "AveragePopularity", "RelativePhysicalIsolation", "RelativeAll")
  
  # Initialize the concepts
  # -- Base categories
  importantIndex = 1
  for (i in 1:totalImportantCategoricalIndexes) {
    
    # Get the name of the variable
    currentImportantCategory = importantCategoricalNames[i]
    importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
    totalImportantModalities = length(importantModalities)
    
    for (j in 1:totalImportantModalities) {
      
      isolationDF[importantIndex,1] = currentImportantCategory
      isolationDF[importantIndex,2] = importantModalities[j]
      isolationDF[importantIndex,3] = importantCategoricalIndexes[i]
      
      importantIndex = importantIndex + 1
    }
    
  }
  # -- Consequence indexes
  for(i in 1:totalConsecuenceIndexes){
    
    # Get the name of the variable
    currentImportantCategory = consecuenceNames[i]
    importantModalities      = as.character(unique(completeTable[,consecuenceIndexes[i]]))
    totalImportantModalities = length(importantModalities)
    
    for (j in 1:totalImportantModalities) {
      
      isolationDF[importantIndex,1] = currentImportantCategory
      isolationDF[importantIndex,2] = importantModalities[j]
      isolationDF[importantIndex,3] = consecuenceIndexes[i]
      
      importantIndex = importantIndex + 1
    }
    
  }
  
  
  # Do the proper things for each column
  # -- Average Popularity
  {
    for(i in 1:importantTotalRows){
      
      # Get the index of the variable and name of the modality
      currentModalityName  = isolationDF[i,2]
      currentVariableIndex = isolationDF[i,3]

      # Get the subtable for this variable
      currentSubTable  = completeTable[ completeTable[,currentVariableIndex] == currentModalityName, ]
      currentTotalRows = nrow(currentSubTable)
      
      # Find the average popularity
      currentPopAverage = mean(currentSubTable[,overallPopularityIndex], na.rm = TRUE) 
      isolationDF[i,4]  = round(currentPopAverage,2)
      
      # Find the relative physical isolation
      # Get the subtable of isolates for this variable
      currentIsolationSubTable  = physicalIsolatedTable[ physicalIsolatedTable[,currentVariableIndex] == currentModalityName, ]
      currentIsolationTotalRows = nrow(currentIsolationSubTable)
      
      currentRelativePhyIsolation = round(100 * currentIsolationTotalRows / 
                                                totalPhysicalIsolationRows , 2)
      isolationDF[i,5]            = currentRelativePhyIsolation
      
      # Find how many of this variable exist in the general population
      currentGeneralPopulation   = round(100 * currentTotalRows / 
                                               TOTAL_SUBJECTS   , 2)
      isolationDF[i,6]           = currentGeneralPopulation
      
    }
    
    
    
    
  }
  # -- Average Popularity (for hormonal contraceptives)
  {
    
    # Create the physical isolation table for women only
    womenPhysicalIsolatedTable =  womenMenstruatingOnlyTable[womenMenstruatingOnlyTable$PhysicalPopularity == 0,]
    totalWomenIsolationRows    =  nrow(womenPhysicalIsolatedTable)
    
    
    hormonalVariables = as.character(unique(womenMenstruatingOnlyTable$Hormonal))
    totalHormonalVariables = length(hormonalVariables)
    for(j in 1:totalHormonalVariables){

      # Get the proper variable
      currentModalityName = hormonalVariables[j]

      
      # Get the subtable for this variable
      currentSubTable  = womenMenstruatingOnlyTable[ womenMenstruatingOnlyTable[,hormonalIndex] == currentModalityName, ]
      currentTotalRows = nrow(currentSubTable)

      # Find the average popularity
      currentPopAverage = mean(currentSubTable[,overallPopularityIndex], na.rm = TRUE) 
      #isolationDF[i,4]  = round(currentPopAverage,2)
         
      # Find the relative physical isolation
      # Get the subtable of isolates for this variable
      currentIsolationSubTable  = womenPhysicalIsolatedTable[ womenPhysicalIsolatedTable[,hormonalIndex] == currentModalityName, ]
      currentIsolationTotalRows = nrow(currentIsolationSubTable)

      
      currentRelativePhyIsolation = round(100 * currentIsolationTotalRows / 
                                            totalWomenIsolationRows , 2)
      
      # Find how many of this variable exist in the general population
      currentGeneralPopulation   = round(100 * totalWomenIsolationRows / 
                                           totalWomenRows   , 2)
      
      # Print everything, so we don't have to create a df just for this
      print("----------------")
      print(currentModalityName)
      print("----------------")
      print("Popularity average:")
      print(currentPopAverage)
      #print(currentIsolationTotalRows)
      print("Relative Physical Isolation:")
      print(currentRelativePhyIsolation)
      #print("Isolation of general population:")
      #print(currentGeneralPopulation)
      print("Relative numbers:")
      print(currentTotalRows/totalWomenRows)
      
    }
    

  }
  
  # Do the boxplot for each network for each carrier variable with respect
  # the number of friends
  {
    for (j in 1:totalConsecuenceIndexes){
      
      currentConsecuenceIndex = consecuenceIndexes[j]
      
      myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                               currentConsecuenceIndex,
                                               overallPopularityIndex,
                                               AUREUS_FOLDER,
                                               colorsVector = COLOR_VECTOR_CARRIER,
                                               showPValues = TRUE)
      
      print(myBoxplotResults[[3]])
      
    }
    
    
  }
   
  # Do the boxplot/anova for categorical variable based on the popularity
  {
    for (i in 1:totalImportantCategoricalIndexes){
      
      currentImportantCategory = importantCategoricalIndexes[i]
      currentCategoricalName   = importantCategoricalNames[i]
      
      print(currentCategoricalName)
      
      # Check the number of modalities, if more than 2, do anova
      importantModalities      = as.character(unique(completeTable[,importantCategoricalIndexes[i]]))
      totalImportantModalities = length(importantModalities)
      
      if(totalImportantModalities > 2){
        # Restrict the dataframe
        anovaDF     = completeTable[,c(currentImportantCategory,overallPopularityIndex)]  
        # Delete the NA rows
        anovaDF = anovaDF[is.na(anovaDF[,2]) == FALSE,]
        anovaDF = anovaDF[is.na(anovaDF[,1]) == FALSE,]
        
        colnames(anovaDF) = c("Categorical", "Popularity")
        
        #anovaResult = aov(SPA ~ VitamimD, data = anovaDF)
        #anovaResult = aov(Categorical ~ Popularity, data = anovaDF)
        anovaResult = aov(Popularity ~ Categorical, data = anovaDF)
        print(summary(anovaResult))
        
      }
      else{
        myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                                 currentImportantCategory,
                                                 overallPopularityIndex,
                                                 AUREUS_FOLDER,
                                                 showPValues = TRUE)
        
        print(myBoxplotResults[[3]])
        print(myBoxplotResults[[2]])
      }
      
      
      

      
    }
    
    
  }
  
  # Do the ANOVA for the hormonal
  {
    
    # Restrict the dataframe
    anovaDF     = womenMenstruatingOnlyTable[,c(hormonalIndex,overallPopularityIndex)]  
    # Delete the NA rows
    anovaDF = anovaDF[is.na(anovaDF[,2]) == FALSE,]
    anovaDF = anovaDF[is.na(anovaDF[,1]) == FALSE,]
    
    colnames(anovaDF) = c("Categorical", "Popularity")
    
    #anovaResult = aov(SPA ~ VitamimD, data = anovaDF)
    #anovaResult = aov(Categorical ~ Popularity, data = anovaDF)
    anovaResult = aov(Popularity ~ Categorical, data = anovaDF)
    print(summary(anovaResult))
    
  }
  
}

# ********************************************
#     BLOOD
# ********************************************  
print("Blood tests") 
if(TRUE){

  # Both
  {
    # Create the DF where we put the results
    bloodNames         = colnames(completeTable)[bloodIndexes]
    bloodBothResultsDF = data.frame(matrix(NA, nrow = totalBloodIndex, ncol = totalConsecuenceIndexes + 1))
    colnames(bloodBothResultsDF) = c("Variable", consecuenceNames)
    for(i in 1:totalBloodIndex){
      
      bloodBothResultsDF[i,1] = bloodNames[i]
      
    }
    
    # Do all the combinations of blood and carrier
    # For each blood variable
    for(i in 1:totalBloodIndex){
      
      currentBloodIndex = bloodIndexes[i]
      
      # For each carrier variable
      for(j in 1:totalConsecuenceIndexes){
        
        currentConsecuenceIndex = consecuenceIndexes[j]  
        
        myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                                 currentConsecuenceIndex,
                                                 currentBloodIndex,
                                                 GENERAL_FOLDER,
                                                 showPValues  = TRUE,
                                                 colorsVector = COLOR_VECTOR_CARRIER)
        
        bloodBothResultsDF[i,(j+1)] = myBoxplotResults[[2]][2,2]
        
        
      }
      
      
      
      
    }
    
    # Write the results in disk
    pTable = writeTableLATEX(bloodBothResultsDF, AUREUS_FOLDER,
                             tableCaption  = paste0("Blood with respect carrier for BOTH."),
                             intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                             roundMe = 4, rotateColumnHeaders = TRUE)
    
  }
  
  # Men
  {
    # Create the DF where we put the results
    bloodMenResultsDF = bloodBothResultsDF

    # Do all the combinations of blood and carrier
    # For each blood variable
    for(i in 1:totalBloodIndex){
      
      currentBloodIndex = bloodIndexes[i]
      
      # For each carrier variable
      for(j in 1:totalConsecuenceIndexes){
        
        currentConsecuenceIndex = consecuenceIndexes[j]  
        
        myBoxplotResults = doCategoricalBoxPlot (menOnlyTable,
                                                 currentConsecuenceIndex,
                                                 currentBloodIndex,
                                                 GENERAL_FOLDER,
                                                 showPValues  = TRUE,
                                                 colorsVector = COLOR_VECTOR_CARRIER)
        
        bloodMenResultsDF[i,(j+1)] = myBoxplotResults[[2]][2,2]
        
      }
      
    }
    
    # Write the results in disk
    pTable = writeTableLATEX(bloodMenResultsDF, AUREUS_FOLDER,
                             tableCaption  = paste0("Blood with respect carrier for MEN."),
                             intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                             roundMe = 4, rotateColumnHeaders = TRUE)
    
  }

  # Women
  {
    # Create the DF where we put the results
    bloodWomenResultsDF = bloodBothResultsDF
    
    # Do all the combinations of blood and carrier
    # For each blood variable
    for(i in 1:totalBloodIndex){
      
      currentBloodIndex = bloodIndexes[i]
      
      # For each carrier variable
      for(j in 1:totalConsecuenceIndexes){
        
        currentConsecuenceIndex = consecuenceIndexes[j]  
        
        myBoxplotResults = doCategoricalBoxPlot (womenOnlyTable,
                                                 currentConsecuenceIndex,
                                                 currentBloodIndex,
                                                 GENERAL_FOLDER,
                                                 showPValues  = TRUE,
                                                 colorsVector = COLOR_VECTOR_CARRIER)
        
        bloodWomenResultsDF[i,(j+1)] = myBoxplotResults[[2]][2,2]
        
      }
      
    }
    
    # Write the results in disk
    pTable = writeTableLATEX(bloodWomenResultsDF, AUREUS_FOLDER,
                             tableCaption  = paste0("Blood with respect carrier for WOMEN."),
                             intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                             roundMe = 4, rotateColumnHeaders = TRUE)
    
  }
  
  
}

# ********************************************
#     LOGISTIC REGRESSION
# ********************************************  
print("Logistic regression") 
if(FALSE){
 
  # We save here the best score of each model
  absoluteBestModel = c(0,0,0)
  
  # Each of the DF we are going to study
  listOfDFs         = list(completeTable, menOnlyTable, womenMenstruatingOnlyTable) 
  
  # Each of the explanatory variables to see
  bothExplanatoryIndexes  = c(sexIndex, BMIIndex,
                              smokeIndex, snuffIndex, alcoholIndex,
                              sportsIndex)
  menExplanatoryIndexes   = c(BMIIndex,
                              smokeIndex, snuffIndex, alcoholIndex,
                              sportsIndex)
  womenExplanatoryIndexes = c(BMIIndex,
                              smokeIndex, snuffIndex, alcoholIndex,
                              sportsIndex, hormonalIndex)
  
  listOfExplanatory = list(bothExplanatoryIndexes, menExplanatoryIndexes, womenExplanatoryIndexes)
  
  # The results are to be saved here
  listOfResultsORR  = list(NA, NA, NA)
  listOfResultsCIL  = list(NA, NA, NA)
  listOfResultsCIH  = list(NA, NA, NA)
  
  # For each of the dataframes colections
  for(z in 1:length(absoluteBestModel)){
    
    # Prepare all the data to do the odd-ratio tables
    # -- Which DF are you using
    logisticReadyDF = listOfDFs[[z]]
    
    # -- Which explicatory variables do you want to use
    currentExplanatoryIndexes = listOfExplanatory[[z]]
    
    # -- The increment is always the same, BMI is the only numerical variable
    currentIncrementVector         = list(BMI = 5)
    
    # We need to keep track of how many modalities are in each variable
    # We use this array for that. 1 = variable is numerical
    totalExplanatoryModalities = rep(1,length(currentExplanatoryIndexes))
    
    # Clean the DF from Unknowns and NAs
    # Clean the final DF so it doesn't have any Unknowns or NAs
    {
      keepTheseRows = rep(TRUE,nrow(logisticReadyDF))
      for (i in 1:length(currentExplanatoryIndexes)){
        currentIndex = currentExplanatoryIndexes[i]
        keepTheseRows = keepTheseRows & ((logisticReadyDF[,currentIndex] != "Unknown") &
                                           (!is.na(logisticReadyDF[,currentIndex]))) 
        
      }
      logisticReadyDF  = logisticReadyDF[keepTheseRows,]
      logisticReadyDF  = logisticReadyDF[,c(currentExplanatoryIndexes)]
      currentTotalRows = nrow(logisticReadyDF)     
    }
    
    # Prepare the targets information for later. We need to conserve the same
    # rows from the original DF from which the logistic is based upon.
    {
      
      targetsDF = listOfDFs[[z]]
      targetsDF = targetsDF[,c(consecuenceIndexes)]
      targetsDF = targetsDF[keepTheseRows,]
      
    }
    
    # Prepare the dataframe where we are going to put everything
    {
      # Count how much information we have
      {
        
        aVector = currentExplanatoryIndexes
        tableBase = logisticReadyDF
        
        # How many grouping variables we have and names
        totalAs = length(aVector)
        ANames  = colnames(tableBase)
        
        # Get how many categories we have in total in all the grouping variables
        totalAModalities = 0
        for(i in 1:totalAs){
          # If it is not numeric, then is categorical
          if(!is.numeric(tableBase[1,i])){
            currentSummaryA       = summarizeCategorical(tableBase, i , sorted="none")  
            totalAModalities      = totalAModalities + nrow(currentSummaryA)        
            totalExplanatoryModalities[i] = nrow(currentSummaryA)
          }
          
        }
        
      }
      
      
      # -- It has:
      #
      #         COLUMNS:
      #         - Variable Name or Modalities 
      #         - Each of the carrier definition)
      #
      #         ROWS:
      #
      #         - One row for carrier variables name (colnames)
      #         - One row per variable, plus one for each modality if categorical
      myTotalRows    = totalAs + totalAModalities
      myTotalColumns = totalConsecuenceIndexes + 1
      
      # Generate the dataframe with empty columns names
      oddResultsDF           =  data.frame(matrix(NA, nrow = myTotalRows, ncol = myTotalColumns))
      colnames(oddResultsDF) = c("Variable", consecuenceNames)
      
      # Init the DF
      currentRowIndex = 1
      for(i in 1:totalAs){
        
        # Write the name of the variable in the DF
        oddResultsDF[currentRowIndex,1] = ANames[i]
        
        # Continue to the next row
        currentRowIndex = currentRowIndex + 1  
        
        # If the variable is numerical, do nothing else
        # If it is categorical, add the modalities
        if(!is.numeric(tableBase[1,i])){
          currentSummaryA       = summarizeCategorical(tableBase, i , sorted="none")  
          
          # For each modality, write it in the dataframe
          for(k in 1:nrow(currentSummaryA)){
            # Write the modality
            oddResultsDF[currentRowIndex,1] = as.character(currentSummaryA[k,1])
            currentRowIndex = currentRowIndex + 1
          }
          
        }
        
      }
      
      cilResultsDF = oddResultsDF
      cihResultsDF = oddResultsDF
      
    }
    
    # Add the target column to the base DF. This is just to reserve
    # memory before the loop so we don't have to delete it each time
    logisticReadyDF$Target = 0
    
    # For each of the carrier definitions
    for(y in 1:totalConsecuenceIndexes){
      print("---S---")
      print(z)
      print(y)
      
      # -- Which target variable do you want
      targetCarrier   = consecuenceIndexes[y]
      
      # -- Reset the base DF target
      #    and change the final target to 0 (negative) 1 (positive)
      {
        
        logisticReadyDF$Target = 0
        for(i in 1:nrow(logisticReadyDF)){
          if(targetsDF[i,y] == "Positive")
            logisticReadyDF$Target[i] = 1 
        }
        
      }
      
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
  
      # Try all possible combination of models with the given variables
      # Evaluate the model with pseudo R2
      {
        totalBruteRows = (2^length(currentExplanatoryIndexes)) - 1
        totalBruteCols = length(currentExplanatoryIndexes) + 3
        bestModelDF    = data.frame(matrix(FALSE, nrow = totalBruteRows, ncol = totalBruteCols))
        colnames(bestModelDF) = c(valid.names, "McFadden", "Cox", "Nagelkerke")
        for(i in 1:totalBruteRows){
          
          myCurrentBinary = number2binary(i, length(currentExplanatoryIndexes))
          
          # Mark the columns that you are using
          bestModelDF[i,] = c(myCurrentBinary, -99, -99, -99)
          
          # Make the formula and evaluate the model
          currentFormulaString = "Target ~ "
          for(j in 1:length(currentExplanatoryIndexes)){
            # If it is a valid variable
            if(bestModelDF[i,j] == 1){
              currentFormulaString = paste0(currentFormulaString, " ", valid.names[j], " +")   
            }
          }
          # Delete the last  "+"
          currentFormulaString = substr(currentFormulaString,1,nchar(currentFormulaString)-1)
          myFinalFormula = as.formula(currentFormulaString)
          
          # Make the model
          model.current = glm(formula = myFinalFormula,
                              data=logisticReadyDF,
                              family = binomial(link="logit"))
          
          modelEvaluation = nagelkerke(model.current)
          
          bestModelDF[i,(totalBruteCols - 2)] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[1]
          bestModelDF[i,(totalBruteCols - 1)] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[2]
          bestModelDF[i, totalBruteCols     ] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[3]
          
        }
        
        # Update the score of the best model
        absoluteBestModel[z] = max(  max(bestModelDF[ ,(totalBruteCols - 2)])  , absoluteBestModel[z] )
      }
      
      # Define full and null models and do step procedure (if you want to refine the model)
      {
          model.null = glm(Target ~ 1,
                           data=logisticReadyDF,
                           family = binomial(link="logit"))
          
          model.full = glm(formula = myFormula,
                           data=logisticReadyDF,
                           family = binomial(link="logit"))
        }
       
        # # Show the model results
        # summary(model.full)
        # 
        # # Show the variables score
        # Anova(model.full, type="II", test="Wald")
        # 
        # # Show the model score
        # nagelkerke(model.full)
      
      # Do the odd ratio table results
      oddResults = or_glm(data = logisticReadyDF, model = model.full, incr = currentIncrementVector)
        
      # Put the results into the proper place in the dataframe
      {
        oddResultsORR = oddResults[,2]
        oddResultsCIL = oddResults[,3]
        oddResultsCIH = oddResults[,4]
        
        oddIndex          = 1
        resultsRowIndex   = 1
        currentVariable   = 1
        
        while(oddIndex <= length(oddResultsORR)){
          
          # Find how many modalities we have in the current variable
          currentModalities = totalExplanatoryModalities[currentVariable]
          
          # If the total is 1, we just copypaste the results
          if(currentModalities == 1){
            oddResultsDF[resultsRowIndex,(y+1)] = oddResultsORR[oddIndex]
            cilResultsDF[resultsRowIndex,(y+1)] = oddResultsCIL[oddIndex]
            cihResultsDF[resultsRowIndex,(y+1)] = oddResultsCIH[oddIndex]
            oddIndex                            = oddIndex        + 1
            resultsRowIndex                     = resultsRowIndex + 1
          }
          # If the total is other number we have several categories
          else{
            
            # Skip one line, that line is just the variable name
            resultsRowIndex                  = resultsRowIndex + 1
            
            # The first modality is always one
            oddResultsDF[resultsRowIndex,(y+1)] = 1
            cilResultsDF[resultsRowIndex,(y+1)] = 0.999
            cihResultsDF[resultsRowIndex,(y+1)] = 1.001
            resultsRowIndex                  = resultsRowIndex + 1
            # The rest, we copy as they come
            for (j in 2:currentModalities) {
              
              oddResultsDF[resultsRowIndex,(y+1)] = oddResultsORR[oddIndex]
              cilResultsDF[resultsRowIndex,(y+1)] = oddResultsCIL[oddIndex]
              cihResultsDF[resultsRowIndex,(y+1)] = oddResultsCIH[oddIndex]
              oddIndex                            = oddIndex + 1
              resultsRowIndex                     = resultsRowIndex + 1
              
            }
            
          }
          
          # Change to the next variable
          currentVariable   = currentVariable + 1
          
        }
        
      }
          
    }
    
    # Save the results into the appropiate DF
    listOfResultsORR[[z]] = oddResultsDF
    listOfResultsCIL[[z]] = cilResultsDF
    listOfResultsCIH[[z]] = cihResultsDF
    
  }  
  
 
  # Trash and examples code, ignore
  if(FALSE){
    
    oddResultsDF
    
    # # Do the odd ratio table results
    # oddResults = or_glm(data = logisticReadyDF, model = model.full, incr = list(BMI = 5, X25.OH.D_.nmol.L. = 10))
    # 
    # 
    # model.manual = glm(Target ~ 
    #                      logisticReadyDF[,1] + 
    #                      logisticReadyDF[,2] +
    #                      logisticReadyDF[,3] +
    #                      logisticReadyDF[,4] +
    #                      logisticReadyDF[,5] +
    #                      logisticReadyDF[,6] +
    #                      logisticReadyDF[,7],
    #                  data=logisticReadyDF,
    #                  family = binomial(link="logit"))
    
    
    # Do the odd ratio table results
    oddResults = or_glm(data = logisticReadyDF, model = model.manual)
    
    
    
    # Calculate OR for specific increment step of continuous variable
    or_glm(data = data_glm, model = fit_glm, 
           incr = list(gre = 380, gpa = 5))
    
    
    
    
    # Create the proper explanatory variables
    # For both
    explanatoryIndexes      = c(sexIndex,  BMIIndex,
                                smokeIndex, snuffIndex, alcoholIndex,
                                sportsIndex)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    step(model.null,
         scope = list(upper=model.full),
         direction="both",
         test="Chisq",
         data=logisticReadyDF)
    
    # Prepare the final model
    model.final = glm(Target ~ 
                        logisticReadyDF[,6] + 
                        logisticReadyDF[,1],
                      data=logisticReadyDF,
                      family = binomial(link="logit"),
                      na.action(na.omit))
    
    summary(model.final)
    
    Anova(model.final, type="II", test="Wald")
    
    nagelkerke(model.final)
    
    
    
    
    
    
    
    
    
    
    # Prepare the model
    #Use 70% of dataset as training set and remaining 30% as testing set
    sampleModel = sample(c(TRUE, FALSE), nrow(logisticReadyDF), replace=TRUE, prob=c(0.7,0.3))
    trainModel  = logisticReadyDF[sampleModel, ]
    testModel   = logisticReadyDF[!sampleModel, ] 
    
    # 
    #   # # Fit logistic regression model
    logisticModel <- glm(trainModel[,7]~
                           trainModel[,1] + trainModel[,2] +
                           trainModel[,3] + trainModel[,4] +
                           trainModel[,5] + trainModel[,6],
                         family="binomial", data=trainModel)
    # 
    #   #Fit logistic regression model
    #   logisticModel <- glm(logisticReadyDF[,5]~
    #                          logisticReadyDF[,1] + logisticReadyDF[,2] +
    #                          logisticReadyDF[,3] + logisticReadyDF[,4],
    #                        family="binomial", data=trainModel)
    
    
    # Disable scientific notation for model summary
    options(scipen=999)
    
    # View model summary
    summary(logisticModel)
    
    # McFadden evaluation, above 0.4 is good
    pscl::pR2(logisticModel)["McFadden"]
    
    # Higher number, more important variable
    caret::varImp(logisticModel)
    
    #calculate VIF values for each predictor variable in our model
    # Above 5 indicates multicollinearity
    car::vif(logisticModel)
    
    # Test code
    if(FALSE){
      data <- ISLR::Default
      nrow(data)
      
      #make this example reproducible
      set.seed(1)
      
      #Use 70% of dataset as training set and remaining 30% as testing set
      sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
      train <- data[sample, ]
      test <- data[!sample, ]
      
      #fit logistic regression model
      model <- glm(default~student+balance+income, family="binomial", data=train)
      
      
      #disable scientific notation for model summary
      options(scipen=999)
      
      #view model summary
      summary(model)
      
      
      # McFadden evaluation, above 0.4 is good
      pscl::pR2(model)["McFadden"]
      
      # Higher number, more important variable
      caret::varImp(model)
      
    }
    
    
    
    
    
    
    
  }

}


# Funny, the BMI doesn't correlate with sports
# model = lm(formula = BMI ~ Sex + Sports, data = completeTable)
# summary(model)
# confint(model)



source("latex.R", encoding="utf-8")

# ********************************************
#     AUTOCORRELATION
# ********************************************  
{
  
  # Prepare the frienship matrix with numbers
  {
    # The friend matrix, as 1 (friend) and 0 (non friend)
    # -- Friends if the original code is defined by rows (each row a maximum of 5)
    tempOverall = overallNetworkDF
    tempOverall$ID = NULL
    friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
  }

  # Prepare the explicative variables DF with numbers rather than categorical
  {
    categoricalExplicativeDF = completeTable[, importantCategoricalIndexes]  
    myExplicativeVariablesDF = data.matrix(categoricalExplicativeDF)
    
    # For whatever reason that I don't understand, this doesn't work with School
    # So we do the school part manually
    {
      categoricalSchoolData = completeTable$School
      numericalSchoolData   = rep(0, totalRows)
      
      numericalSchoolData[categoricalSchoolData == "General"] = 1
      numericalSchoolData[categoricalSchoolData == "Vocational"] = 2
      numericalSchoolData[categoricalSchoolData == "Sports"] = 3
      
      myExplicativeVariablesDF[,2] = numericalSchoolData      
    }

    # Change Sex into Sex_Women and Sex_Men
    # Same for School, School_General, School_Vocational and School_Sports
    # When trying this, we get a non-inversible matrix so the LM can't be solve :(
    if(FALSE){
      sex_women         = rep(1, nrow(myExplicativeVariablesDF))
      sex_men           = rep(1, nrow(myExplicativeVariablesDF))
      school_general    = rep(1, nrow(myExplicativeVariablesDF))
      school_vocational = rep(1, nrow(myExplicativeVariablesDF))
      school_sports     = rep(1, nrow(myExplicativeVariablesDF))
     
      for(i in 1:nrow(myExplicativeVariablesDF)){
        # Sex
        if(completeTable$Sex[i] == "Woman") sex_women[i] = 2
        else sex_men[i] = 2
        # School
        if(completeTable$School[i] == "General") school_general[i] = 2
        else{
          if(completeTable$School[i] == "Vocational") school_vocational[i] = 2  
          else school_sports[i] = 2
        }
        
      }
       
      myExplicativeVariablesDF = cbind(school_sports,     myExplicativeVariablesDF)
      myExplicativeVariablesDF = cbind(school_vocational, myExplicativeVariablesDF)
      myExplicativeVariablesDF = cbind(school_general,    myExplicativeVariablesDF)
      myExplicativeVariablesDF = cbind(sex_men,           myExplicativeVariablesDF)
      myExplicativeVariablesDF = cbind(sex_women,         myExplicativeVariablesDF)
      
      # Delete the old variables
      myExplicativeVariablesDF = myExplicativeVariablesDF[,-6:-7]
      
    }
    
    # Transform back into dataframe
    myExplicativeVariablesDF = as.data.frame(myExplicativeVariablesDF)
    
    # Substract -1 so the baseline is 0 in everything
    myExplicativeVariablesDF = myExplicativeVariablesDF - 1
  }
  
  # Prepare the dependent variables DF with numbers rather than categorical
  {
  
    numericalDirectCultureData   = rep(0, totalRows)
    numericalEnrichmentData      = rep(0, totalRows)
    
    numericalDirectCultureData[completeTable$C_NasalCarrier == "Positive"] = 1
    numericalDirectCultureData[completeTable$C_NasalCarrier == "Negative"] = 0
    
    numericalEnrichmentData[completeTable$E_NasalCarrier == "Positive"] = 1
    numericalEnrichmentData[completeTable$E_NasalCarrier == "Negative"] = 0
    
  }
  
  # Direct culture data
  {
    dependentVariablesVector = numericalDirectCultureData
    
    
    builtInResults = builtInLNAM(dependentVariablesVector, myExplicativeVariablesDF,
                                 friendshipMatrix = friendshipMatrix, epsilon = EPSILON)

    summary(builtInResults)
    plot(builtInResults)
  }

  
  # Enrichment data
  {
    dependentVariablesVector = numericalEnrichmentData
    
    builtInResults = builtInLNAM(dependentVariablesVector, myExplicativeVariablesDF,
                                 friendshipMatrix = friendshipMatrix, epsilon = EPSILON)
    
    summary(builtInResults)
    plot(builtInResults)
  }


  # Prepare the explicative variables DF with dummy variables
  {
    # Subset what you need
    categoricalExplicativeDF    = completeTable[, importantCategoricalIndexes]  
    dummyExplicativeVariablesDF = fastDummies::dummy_cols(categoricalExplicativeDF, remove_most_frequent_dummy = TRUE)
    # Delete the columns that you don't want (I)
    dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(1:totalImportantCategoricalIndexes)]
    # Delete the columns that you don't want (II) Everything that has an Unknown
    # unknownColumns = grep("Unknown",             colnames(dummyExplicativeVariablesDF))
    # dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(unknownColumns)]
    
    # # check that we have no 0 columns or rows
    # for(i in 1:totalRows){
    #   rowTotal = sum(dummyExplicativeVariablesDF[i,])  
    #   if(rowTotal == 0) print(i)
    # }
    
    # do weird DF combinations
    
    # # Take Away Sex, and add the dummy variable
    # myExplicativeVariablesDF2           = myExplicativeVariablesDF[,-1]
    # myExplicativeVariablesDF2$Sex_Man   = dummyExplicativeVariablesDF[,1]
    # myExplicativeVariablesDF2$Sex_Woman = dummyExplicativeVariablesDF[,2]
    # # Take Away School, and add the dummy variable
    # myExplicativeVariablesDF2           = myExplicativeVariablesDF2[,-1]
    # myExplicativeVariablesDF2$School_General    = dummyExplicativeVariablesDF[,3]
    # myExplicativeVariablesDF2$School_Sports     = dummyExplicativeVariablesDF[,4]
    # myExplicativeVariablesDF2$School_Vocational = dummyExplicativeVariablesDF[,5]
    # 
    # myExplicativeVariablesDF2 = myExplicativeVariablesDF2[,c(-1)]
    # 
    # myExplicativeVariablesDF2 = myExplicativeVariablesDF2[,c(-3,-4,-5)]
    
    # Run the model
    
    # Direct culture data
    {
      dependentVariablesVector = numericalDirectCultureData

      # myResults = networkAutoCorrelation(dependentVariablesVector, dummyExplicativeVariablesDF, friendshipMatrix,
      #                                    maximumIterations = 10, epsilon = 0.01, silent = FALSE)

      builtInResults = builtInLNAM(dependentVariablesVector, dummyExplicativeVariablesDF,
                                   friendshipMatrix = friendshipMatrix, epsilon = EPSILON)
            
      builtInResults = builtInLNAM(dependentVariablesVector, myExplicativeVariablesDF,
                                   friendshipMatrix = friendshipMatrix, epsilon = EPSILON)
      
      summary(builtInResults)
      plot(builtInResults)
    }
    
    
    
   
     
  }
  
}

# Test autoresults
{
  # Count how many people surrounding you  are positive
  totalCPositiveFriends = rep(0, totalRows)
  totalEPositiveFriends = rep(0, totalRows)
  totalFriends          = rep(0,  totalRows)
  
  # For each person
  for(i in 1:totalRows){
    
    # Current ID
    currentID = i
    
    # Find the friends surrounding you (you nominate or nominate you)
    currentFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                              getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
    
    currentTotalFriends = length(currentFriends) 
  
    totalFriends[i] = currentTotalFriends
      
    # Find how many of those are positive if you have more than 0 friends
    if(currentTotalFriends > 0){
      for(j in 1:currentTotalFriends){
        
        # Friend ID
        currentFriendID = currentFriends[j]
        
        # Status of the friend
        friendCStatus = as.character(completeTable[currentFriendID,]$C_NasalCarrier)
        friendEStatus = as.character(completeTable[currentFriendID,]$E_NasalCarrier)
        
        # Add it to the list whatever the result
        if(friendCStatus == "Positive") totalCPositiveFriends[i] = totalCPositiveFriends[i] + 1
        if(friendEStatus == "Positive") totalEPositiveFriends[i] = totalEPositiveFriends[i] + 1
        
      }
    }

    
  }
  
  # Grab only people who has friends
  onlyPeopleWithFriendsTotalCPositives = totalCPositiveFriends[totalFriends > 0]
  onlyPeopleWithFriendsTotalEPositives = totalEPositiveFriends[totalFriends > 0]
  onlyPeopleWithFriendsCarrierCStatus  = completeTable[totalFriends > 0,]$C_NasalCarrier
  onlyPeopleWithFriendsCarrierEStatus  = completeTable[totalFriends > 0,]$E_NasalCarrier
  
  # Make the numbers for the model
  onlyPeopleWithFriendsCarrierCStatusNumerical = rep(0, length(onlyPeopleWithFriendsCarrierCStatus))
  onlyPeopleWithFriendsCarrierCStatusNumerical[onlyPeopleWithFriendsCarrierCStatus == "Positive"] = 1
  onlyPeopleWithFriendsCarrierEStatusNumerical = rep(0, length(onlyPeopleWithFriendsCarrierEStatus))
  onlyPeopleWithFriendsCarrierEStatusNumerical[onlyPeopleWithFriendsCarrierEStatus == "Positive"] = 1
  
  CModel <- glm(onlyPeopleWithFriendsCarrierCStatus~onlyPeopleWithFriendsTotalCPositives, family=binomial(link="logit"))
  EModel <- glm(onlyPeopleWithFriendsCarrierEStatus~onlyPeopleWithFriendsTotalEPositives, family=binomial(link="logit"))
  
  CModel2 = glm(formula = onlyPeopleWithFriendsCarrierCStatusNumerical ~ onlyPeopleWithFriendsTotalCPositives, family = binomial)
  EModel2 = glm(formula = onlyPeopleWithFriendsCarrierEStatusNumerical ~ onlyPeopleWithFriendsTotalEPositives, family = binomial)
  
  # BMI doesn't correlate with carrier status
  CModel3 <- glm(formula = E_NasalCarrier ~ BMI,
                 family=binomial(link="logit"),
                 data = completeTable)
  
  # Change the numbers so they looks nicer in the plot
  onlyPeopleWithFriendsCarrierCStatusNumerical = rep(-0.02, length(onlyPeopleWithFriendsCarrierCStatus))
  onlyPeopleWithFriendsCarrierCStatusNumerical[onlyPeopleWithFriendsCarrierCStatus == "Positive"] = 0.98
  onlyPeopleWithFriendsCarrierEStatusNumerical = rep(+0.02, length(onlyPeopleWithFriendsCarrierEStatus))
  onlyPeopleWithFriendsCarrierEStatusNumerical[onlyPeopleWithFriendsCarrierEStatus == "Positive"] = 1.02
  
  # Prepare the dataframes for the plots
  logisticsPlotsDF           = data.frame(matrix(NA, nrow = length(onlyPeopleWithFriendsTotalCPositives), ncol = 6))
  colnames(logisticsPlotsDF) = c("C_Status", "E_Status", "TotalFriendsCPositives", "TotalFriendsEPositives", "C_Numerical", "E_Numerical")
  logisticsPlotsDF[,1]       = onlyPeopleWithFriendsCarrierCStatus
  logisticsPlotsDF[,2]       = onlyPeopleWithFriendsCarrierEStatus
  logisticsPlotsDF[,3]       = onlyPeopleWithFriendsTotalCPositives
  logisticsPlotsDF[,4]       = onlyPeopleWithFriendsTotalEPositives
  logisticsPlotsDF[,5]       = onlyPeopleWithFriendsCarrierCStatusNumerical
  logisticsPlotsDF[,6]       = onlyPeopleWithFriendsCarrierEStatusNumerical
  
  
  # C boxplots
  {
    myBoxplotResults = doCategoricalBoxPlot (logisticsPlotsDF,
                                             1,
                                             3,
                                             AUREUS_FOLDER,
                                             colorsVector = COLOR_VECTOR_CARRIER,
                                             showPValues = TRUE)
    myBoxplotResults[[1]]
    myBoxplotResults[[2]]
    myBoxplotResults[[3]]
  }

  # E boxplots
  {
    myBoxplotResults = doCategoricalBoxPlot (logisticsPlotsDF,
                                             2,
                                             4,
                                             AUREUS_FOLDER,
                                             colorsVector = COLOR_VECTOR_CARRIER,
                                             showPValues = TRUE)
    myBoxplotResults[[1]]
    myBoxplotResults[[2]]
    myBoxplotResults[[3]]
  }

  # Do the function plot
  {
   
    # Prepare the exponential functions
    myCfunction = function(x){
      return(1/(1 + exp(-(0.16*x-1.09) ) ) )
    }
    
    myEfunction = function(x){
      return(1/(1 + exp(-(0.14*x-0.63) ) ) )
    }
     
    # Prepare the points for the boxplots
    {
      CNegatives = summary(logisticsPlotsDF[logisticsPlotsDF$C_Status == "Negative",]$TotalFriendsCPositives)
      CPositives = summary(logisticsPlotsDF[logisticsPlotsDF$C_Status == "Positive",]$TotalFriendsCPositives)
      ENegatives = summary(logisticsPlotsDF[logisticsPlotsDF$E_Status == "Negative",]$TotalFriendsEPositives)
      EPositives = summary(logisticsPlotsDF[logisticsPlotsDF$E_Status == "Positive",]$TotalFriendsEPositives)
  
      CNegativeMin = as.numeric(CNegatives[1])
      CNegativeMax = as.numeric(CNegatives[6])
      CNegative1   = as.numeric(CNegatives[2])
      CNegative2   = as.numeric(CNegatives[3])
      CNegative3   = as.numeric(CNegatives[5])
      CNegativeA   = as.numeric(CNegatives[4])
      
      ENegativeMin = as.numeric(ENegatives[1])
      ENegativeMax = as.numeric(ENegatives[6])
      ENegative1   = as.numeric(ENegatives[2])
      ENegative2   = as.numeric(ENegatives[3])
      ENegative3   = as.numeric(ENegatives[5])
      ENegativeA   = as.numeric(ENegatives[4])
      
      CPositiveMin = as.numeric(CPositives[1])
      CPositiveMax = as.numeric(CPositives[6])
      CPositive1   = as.numeric(CPositives[2])
      CPositive2   = as.numeric(CPositives[3])
      CPositive3   = as.numeric(CPositives[5])
      CPositiveA   = as.numeric(CPositives[4])
      
      EPositiveMin = as.numeric(EPositives[1])
      EPositiveMax = as.numeric(EPositives[6])
      EPositive1   = as.numeric(EPositives[2])
      EPositive2   = as.numeric(EPositives[3])
      EPositive3   = as.numeric(EPositives[5])
      EPositiveA   = as.numeric(EPositives[4])
      
    }

    # Prepare the dataframes for the density plots
    {
      # -- The negatives
      {
        totalC_Negatives  = sum(logisticsPlotsDF$C_Status == "Negative")
        totalE_Negatives  = sum(logisticsPlotsDF$E_Status == "Negative")
        totalLogisticRows = totalC_Negatives + totalE_Negatives
        meltedLogisticsNegativesDF           = data.frame(matrix(NA, nrow = totalLogisticRows, ncol = 2))
        colnames(meltedLogisticsNegativesDF) = c("Status", "Total_Friends")
        
        currentMeltedIndex = 1
        for(z in 1:length(onlyPeopleWithFriendsTotalCPositives)){
          
          # For the direct culture
          if(logisticsPlotsDF$C_Status[z] == "Negative"){
            
            meltedLogisticsNegativesDF[currentMeltedIndex,1] = "C_Negative"
            meltedLogisticsNegativesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsCPositives[z]
            currentMeltedIndex = currentMeltedIndex + 1
            
          }
          
          # For the enrichment
          if(logisticsPlotsDF$E_Status[z] == "Negative"){
            
            meltedLogisticsNegativesDF[currentMeltedIndex,1] = "E_Negative"
            meltedLogisticsNegativesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsEPositives[z]
            currentMeltedIndex = currentMeltedIndex + 1
            
          }
        }        
      }

      # -- The Positives
      {
        totalC_Positives  = sum(logisticsPlotsDF$C_Status == "Positive")
        totalE_Positives  = sum(logisticsPlotsDF$E_Status == "Positive")
        totalLogisticRows = totalC_Positives + totalE_Positives
        meltedLogisticsPositivesDF           = data.frame(matrix(NA, nrow = totalLogisticRows, ncol = 2))
        colnames(meltedLogisticsPositivesDF) = c("Status", "Total_Friends")
        
        currentMeltedIndex = 1
        for(z in 1:length(onlyPeopleWithFriendsTotalCPositives)){ # The 1:X range is correct, is the total length of the summary DF
          
          # For the direct culture
          if(logisticsPlotsDF$C_Status[z] == "Positive"){
            
            meltedLogisticsPositivesDF[currentMeltedIndex,1] = "C_Positive"
            meltedLogisticsPositivesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsCPositives[z]
            currentMeltedIndex = currentMeltedIndex + 1
            
          }
          
          # For the enrichment
          if(logisticsPlotsDF$E_Status[z] == "Positive"){
            
            meltedLogisticsPositivesDF[currentMeltedIndex,1] = "E_Positive"
            meltedLogisticsPositivesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsEPositives[z]
            currentMeltedIndex = currentMeltedIndex + 1
            
          }
        }        
      }
      
      
    }
    
    # Do the actual plot (both variables)
    if(TRUE){
      ggplot() +
        # Jitter points
        geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsCPositives,y=C_Numerical),  width = 0.25, height = 0.025, color = "red",   alpha = 0.2) +
        geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsCPositives,y=E_Numerical),  width = 0.25, height = 0.025, color = "black", alpha = 0.2) +
        # Horizontal lines
        geom_hline(yintercept=0) +
        geom_hline(yintercept=1) +
        # Formulas
        geom_function(fun = myCfunction, colour = "red",   lwd = 1, linetype = 1) +
        geom_function(fun = myEfunction, colour = "black", lwd = 1, linetype = 1) +
        # Density
        # geom_density(data = meltedLogisticsNegativesDF, aes(x = Total_Friends, fill = Status, y = -..density..), adjust = 3, alpha = 0.4, position = position_nudge(y = -0.05)) +
        # geom_density(data = meltedLogisticsPositivesDF, aes(x = Total_Friends, fill = Status, y =  ..density..), adjust = 2, alpha = 0.4, position = position_nudge(y = +1.05)) +
        # Manual boxplots
        # -- C Negatives
        geom_segment(aes(x = CNegativeMin, y = -0.15, xend = CNegative1,   yend = -0.15), colour = "black") + 
        geom_segment(aes(x = CNegative3,   y = -0.15, xend = 6, yend = -0.15), colour = "black") + 
        geom_rect(aes(xmin=CNegative1, xmax=CNegative3, ymin=-0.13, ymax=-0.17), fill="red", color="black", alpha=0.5) +
        geom_segment(aes(x = CNegativeA,   y = -0.13, xend = CNegativeA,   yend = -0.17), colour = "black") + 
        # -- E Negatives
        geom_segment(aes(x = ENegativeMin, y = -0.09, xend = ENegative1,   yend = -0.09), colour = "black") + 
        geom_segment(aes(x = ENegative3,   y = -0.09, xend = 6, yend = -0.09), colour = "black") + 
        geom_rect(aes(xmin=ENegative1, xmax=ENegative3, ymin=-0.07, ymax=-0.11), fill="black", color="black", alpha=0.5) +
        geom_segment(aes(x = ENegativeA,   y = -0.07, xend = ENegativeA,   yend = -0.11), colour = "black") + 
        # -- C Positives
        geom_segment(aes(x = CPositiveMin, y = +1.09, xend = CPositive1,   yend = +1.09), colour = "black") + 
        geom_segment(aes(x = CPositive3,   y = +1.09, xend = 6, yend = +1.09), colour = "black") + 
        geom_rect(aes(xmin=CPositive1, xmax=CPositive3, ymin=+1.07, ymax=+1.11), fill="red", color="black", alpha=0.5) +
        geom_segment(aes(x = CPositiveA,   y = +1.07, xend = CPositiveA,   yend = +1.11), colour = "black") + 
        # -- E Positives
        geom_segment(aes(x = EPositiveMin, y = +1.15, xend = EPositive1,   yend = +1.15), colour = "black") + 
        geom_segment(aes(x = EPositive3,   y = +1.15, xend = 6, yend = +1.15), colour = "black") + 
        geom_rect(aes(xmin=EPositive1, xmax=EPositive3, ymin=+1.13, ymax=+1.17), fill="black", color="black", alpha=0.5) +
        geom_segment(aes(x = EPositiveA,   y = +1.13, xend = EPositiveA,   yend = +1.17), colour = "black") + 
        
        
        # Break in integers numbers only
        scale_x_continuous( breaks = seq(0,6,1),   limits=c(-0.5, 6.5))    +
        scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
        # scale_y_continuous( breaks = seq(0,1,0.1)) +
        # Legends and axys
        scale_colour_manual(name="Legend",
                            labels=c("Direct Culture", "Enrichment"),
                            values=c("Red",            "Black")) +
        
        # scale_fill_manual(name="Legend",
        #                   labels=c("Direct Culture", NULL, "Enrichment",         NULL),
        #                   values=c("Red",            "Red",            "Black",   "Black")) +
        
        labs(x = "Number of friends defined as persistent carriers",
             y = "Probability of S. aureus persistent carriage",
             title = "Logistic Regression between total positive friends and probability of positive status",
             color = "Legend") +
        
        theme_linedraw()
    }
    
    # Do the actual plot (Enrichment only for Dina's presentation)
    if(FALSE){
      ggplot() +
        # Jitter points
        
        geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsCPositives,y=E_Numerical-0.02),  width = 0.25, height = 0.025, color = "#e88e2e", alpha = 0.2) +
        # Horizontal lines
        geom_hline(yintercept=0) +
        geom_hline(yintercept=1) +
        # Formulas
        
        geom_function(fun = myEfunction, colour = "#e88e2e", lwd = 1, linetype = 1) +


        # -- E Negatives
        geom_segment(aes(x = ENegativeMin, y = -0.09, xend = ENegative1,   yend = -0.09), colour = "black") + 
        geom_segment(aes(x = ENegative3,   y = -0.09, xend = 6, yend = -0.09), colour = "black") + 
        geom_rect(aes(xmin=ENegative1, xmax=ENegative3, ymin=-0.07, ymax=-0.11), fill="#e88e2e", color="black", alpha=0.5) +
        geom_segment(aes(x = ENegativeA,   y = -0.07, xend = ENegativeA,   yend = -0.11), colour = "black") + 

        # -- E Positives
        geom_segment(aes(x = EPositiveMin, y = +1.15, xend = EPositive1,   yend = +1.15), colour = "black") + 
        geom_segment(aes(x = EPositive3,   y = +1.15, xend = 6, yend = +1.15), colour = "black") + 
        geom_rect(aes(xmin=EPositive1, xmax=EPositive3, ymin=+1.13, ymax=+1.17), fill="#e88e2e", color="black", alpha=0.5) +
        geom_segment(aes(x = EPositiveA,   y = +1.13, xend = EPositiveA,   yend = +1.17), colour = "black") + 
        
        
        # Break in integers numbers only
        scale_x_continuous( breaks = seq(0,6,1),   limits=c(-0.5, 6.5))    +
        scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
        # scale_y_continuous( breaks = seq(0,1,0.1)) +
        # Legends and axys
        scale_colour_manual(name="Legend",
                            labels=c("Enrichment"),
                            values=c("Black")) +
        
        # scale_fill_manual(name="Legend",
        #                   labels=c("Direct Culture", NULL, "Enrichment",         NULL),
        #                   values=c("Red",            "Red",            "Black",   "Black")) +
        
        labs(x = "Total Positive Friends (people with friends only)",
             y = "Probability of Positive Status",
             title = "Logistic Regression between total positive friends and probability of positive status",
             color = "Legend") +
        
        theme_linedraw()
    }
   

  }

  # Find the average of increase probability
  ccc = myCfunction(seq(10,0,-1)) 
  eee = myEfunction(seq(10,0,-1)) 
  
  averageC = 0
  averageE = 0
  for(i in 1:(length(ccc)-1)){
    
    difference = ccc[i] - ccc[i+1] 
    averageC = averageC + difference
    
    difference = eee[i] - eee[i+1] 
    averageE = averageE + difference
    
  }
  averageC = averageC / (length(ccc)-1)
  averageE = averageE / (length(ccc)-1)
  
    # Find CI for increase probability by numbers of friends
    {
    
        ccc2 = ccc
        for(i in 1:(length(ccc)-1)){

            ccc2[i] = ccc[i] - ccc[i+1] 
            
        }
        ccc2 = ccc2[1:length(ccc2)-1]
        
        getCI(ccc2)
        
        eee2 = eee
        for(i in 1:(length(eee)-1)){

            eee2[i] = eee[i] - eee[i+1] 
            
        }
        eee2 = eee2[1:length(eee2)-1]
        
        getCI(eee2)
        
        
}

}

# ********************************************
#     EGRM
# ********************************************  
{

  # Do the homophily analysis first and see what going on, there is some obvious
  # bias for almost everybody in the network.
  partialHomophilyDF  = partialHomophily(overallGraph, importantCategoricalIndexes)
  completeHomophilyDF = completeHomophily(overallGraph, importantCategoricalIndexes)
  
  # In this variable is already a summary of the complete table that we need
  # head(categoricalExplicativeDF)
  
  # In this variable (from the autocorrelation part) there is already a
  # friendship matrix turn into numbers from the overall Network
  # friendshipMatrix
  
  # Prepare the frienship matrix with numbers
  {
    # The friend matrix, as 1 (friend) and 0 (non friend)
    # -- Friends if the original code is defined by rows (each row a maximum of 5)
    tempOverall = overallNetworkDF
    tempOverall$ID = NULL
    friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
  }
  
  
  # Create the network model
  myOverallNetwork = as.network(x = friendshipMatrix, 
                                    directed = FALSE, 
                                    loops   = FALSE, 
                                    matrix.type = "adjacency" 
  )

  # Set the IDs  
  network.vertex.names(myOverallNetwork) = completeTable$ID
  # set.vertex.attribute(myOverallNetwork, "Sex",     as.vector(completeTable$Sex))
  # set.vertex.attribute(myOverallNetwork, "Alcohol", as.vector(completeTable$Alcohol))
  # set.vertex.attribute(myOverallNetwork, "School",  as.vector(completeTable$School))
  # set.vertex.attribute(myOverallNetwork, "Smoke",   as.vector(completeTable$Smoke))
  # set.vertex.attribute(myOverallNetwork, "Snuff",   as.vector(completeTable$Snuff))
  
  
  # Set the categorical attributes (there will be no numerical values)
  for(i in 1:totalImportantCategoricalIndexes){
    
    currentIndex = explanatoryIndexes[i]
    currentName  = explanatoryNames[i]
  
    set.vertex.attribute(myOverallNetwork, currentName, as.vector(completeTable[,currentIndex]))
      
  }
  
  # Check that is working
  # plot(myOverallNetwork,vertex.col="Sex")
  # plot(myOverallNetwork,vertex.col="Alcohol")
  # plot(myOverallNetwork,vertex.col="School")
  
  # Check the Density (igraph do this too, and better)
  gden(myOverallNetwork)
  
  # ERGM
  
  # By edges only
  myRandomModel = ergm(myOverallNetwork ~ edges)  
  summary(myRandomModel)
  # Check that the edges coincide with density (it is)
  logit2prob(-5.26) # I don't know how to get the value :(
  

    
  # By some variables (everybody get together with similar peers as shown in the homophily table)
  myHomoModel = ergm(myOverallNetwork ~ edges + 
                                        nodematch("Sex") + nodematch("School") +
                                        nodematch("BMICategorical") + nodematch("Sports") +                       
                                        nodematch("Smoke") + nodematch("Snuff") +                                              
                                        nodematch('Alcohol'))
  summary(myHomoModel)

  
  x=gof(myHomoModel ~ model) #here use a gof function to test for
  plot(x)
  
    

  # Adding reciprocity to the model
  myHomoMutualModel = ergm( myOverallNetwork ~ edges + 
                                               nodematch("Sex") + nodematch("School") +
                                               nodematch("BMICategorical") + nodematch("Sports") +                       
                                               nodematch("Smoke") + nodematch("Snuff") +                                              
                                               nodematch('Alcohol') +
                                               mutual)
  summary(myHomoMutualModel)                         
  

  
  
  
  # By some variables (everybody get together with similar peers as shown in the homophily table)
  myHomoModel = ergm(myOverallNetwork ~ edges + 
                       nodematch("School") +
                       nodematch("BMICategorical") + nodematch("Snuff") +                                              
                       nodematch('Alcohol'))
  summary(myHomoModel)
  
  
  # Adding reciprocity to the model
  myHomoMutualModel = ergm( myOverallNetwork ~ edges + 
                              nodematch("School") +
                              nodematch("BMICategorical") + nodematch("Snuff") +                                              
                              nodematch('Alcohol') +
                              mutual)
  
  summary(myHomoMutualModel)    
  
  
  
  
  
  mcmc.diagnostics(myHomoMutualModel); #looking at the model diagnostics
  #-the MCMC sample of network statistics
  
  plot(myHomoMutualModel$sample)
  
  
  x=gof(myHomoModel ~ model) #here use a gof function to test for
  plot(x)
  
  
  x=gof(myHomoMutualModel ~ model) #here use a gof function to test for
  #goodness of fit
  plot(x)
  
  

  # Run homophily for highshool and class too
  completeHomophily(overallGraph, highSchoolIndex)
  completeHomophily(overallGraph, classIndex)
  
  

  
  
  
}


# ********************************************
#     ODDs and Weird plots to be converted into functions
# ********************************************  
{
  
  # Copy the results by hand into a new DF
  # This is a bit shitty, it should be corrected when we actually know what
  # we want to do -_-
  oddsDF = DF(22,5, defaultValue = NA)
  colnames(oddsDF) = c("Concept", "Base", "Low", "High", "Row")
  
  # oddsDF[1,1]  = "Sex: Woman"          ; oddsDF[1,2]  = 1      ; oddsDF[1,3]   = NA      ; oddsDF[1,4]    = NA    ; oddsDF[1,5]    = 1
  # oddsDF[2,1]  = "Sex: Man"            ; oddsDF[2,2]  = 0.845  ; oddsDF[2,3]   = 0.805  ; oddsDF[2,4]   = 0.884 ; oddsDF[2,5]    = 2
  # oddsDF[3,1]  = "School: Vocational"  ; oddsDF[3,2]  = 1      ; oddsDF[3,3]   = 1      ; oddsDF[3,4]    = 1    ; oddsDF[3,5]    = 3
  # oddsDF[4,1]  = "School: General"     ; oddsDF[4,2]  = 1.002  ; oddsDF[4,3]   = 0.950  ; oddsDF[4,4]   = 1.054 ; oddsDF[4,5]    = 4
  # oddsDF[5,1]  = "School: Sport"       ; oddsDF[5,2]  = 1.009  ; oddsDF[5,3]   = 0.960  ; oddsDF[5,4]   = 1.059 ; oddsDF[5,5]    = 5
  # oddsDF[6,1]  = "BMI: Healthy"        ; oddsDF[6,2]  = 1      ; oddsDF[6,3]   = NA      ; oddsDF[6,4]   = NA    ; oddsDF[6,5]    = 6
  # oddsDF[7,1]  = "BMI: Underweight"    ; oddsDF[7,2]  = 0.953  ; oddsDF[7,3]   = 0.905  ; oddsDF[7,4]   = 1.001 ; oddsDF[7,5]    = 7
  # oddsDF[8,1]  = "BMI: Obese"          ; oddsDF[8,2]  = 0.941  ; oddsDF[8,3]   = 0.896  ; oddsDF[8,4]   = 0.987 ; oddsDF[8,5]    = 8
  # oddsDF[9,1]  = "BMI: Overweight"     ; oddsDF[9,2]  = 0.901  ; oddsDF[9,3]   = 0.860  ; oddsDF[9,4]   = 0.943 ; oddsDF[9,5]    = 9
  # oddsDF[10,1] = "Smoke: Daily"        ; oddsDF[10,2] = 1      ; oddsDF[10,3]  = NA     ; oddsDF[10,4]    = NA    ; oddsDF[10,5]   = 10
  # oddsDF[11,1] = "Smoke: Never"        ; oddsDF[11,2] = 0.986  ; oddsDF[11,3]  = 0.937  ; oddsDF[11,4]   = 1.034 ; oddsDF[11,5]   = 11
  # oddsDF[12,1] = "Smoke: Sometimes"    ; oddsDF[12,2] = 0.971  ; oddsDF[12,3]  = 0.922  ; oddsDF[12,4]   = 1.020 ; oddsDF[12,5]   = 12
  # oddsDF[13,1] = "Snuff: Daily"        ; oddsDF[13,2] = 1      ; oddsDF[13,3]  = NA     ; oddsDF[13,4]    = NA    ; oddsDF[13,5]   = 13
  # oddsDF[14,1] = "Snuff: Never"        ; oddsDF[14,2] = 0.999  ; oddsDF[14,3]  = 0.949  ; oddsDF[14,4]   = 1.049 ; oddsDF[14,5]   = 14
  # oddsDF[15,1] = "Snuff: Sometimes"    ; oddsDF[15,2] = 0.962  ; oddsDF[15,3]  = 0.915  ; oddsDF[15,4]   = 1.010 ; oddsDF[15,5]   = 15
  # oddsDF[16,1] = "Alcohol: > 2 month"  ; oddsDF[16,2] = 1      ; oddsDF[16,3]  = NA     ; oddsDF[16,4]    = NA    ; oddsDF[16,5]   = 16
  # oddsDF[17,1] = "Alcohol: <= 1 month" ; oddsDF[17,2] = 0.933  ; oddsDF[17,3]  = 0.885  ; oddsDF[17,4]   = 0.980 ; oddsDF[17,5]   = 17
  # oddsDF[18,1] = "Alcohol: Never"      ; oddsDF[18,2] = 0.938  ; oddsDF[18,3]  = 0.890  ; oddsDF[18,4]   = 0.986 ; oddsDF[18,5]   = 18
  # oddsDF[19,1] = "Physical: Light"     ; oddsDF[19,2] = 1      ; oddsDF[19,3]  = NA     ; oddsDF[19,4]    = NA    ; oddsDF[19,5]   = 19
  # oddsDF[20,1] = "Physical: None"      ; oddsDF[20,2] = 0.930  ; oddsDF[20,3]  = 0.886  ; oddsDF[20,4]   = 0.975 ; oddsDF[20,5]   = 20
  # oddsDF[21,1] = "Physical: Medium"    ; oddsDF[21,2] = 1.053  ; oddsDF[21,3]  = 0.999  ; oddsDF[21,4]   = 1.107 ; oddsDF[21,5]   = 21
  # oddsDF[22,1] = "Physical: Hard"      ; oddsDF[22,2] = 0.959  ; oddsDF[22,3]  = 0.913  ; oddsDF[22,4]   = 1.005 ; oddsDF[22,5]   = 22
  
  oddsDF = DF(13,5, defaultValue = NA)
  colnames(oddsDF) = c("Concept", "Base", "Low", "High", "Row")
  
  oddsDF[1,1]  = "Sex: Woman"          ; oddsDF[1,2]  = 1      ; oddsDF[1,3]   = NA     ; oddsDF[1,4]   = NA    ; oddsDF[1,5]    = 13
  oddsDF[2,1]  = "Sex: Man"            ; oddsDF[2,2]  = 0.845  ; oddsDF[2,3]   = 0.805  ; oddsDF[2,4]   = 0.884 ; oddsDF[2,5]    = 12
  oddsDF[3,1]  = "BMI: Healthy"        ; oddsDF[3,2]  = 1      ; oddsDF[3,3]   = NA     ; oddsDF[3,4]   = NA    ; oddsDF[3,5]    = 11
  oddsDF[4,1]  = "BMI: Underweight"    ; oddsDF[4,2]  = 0.953  ; oddsDF[4,3]   = 0.905  ; oddsDF[4,4]   = 1.001 ; oddsDF[4,5]    = 10
  oddsDF[5,1]  = "BMI: Obese"          ; oddsDF[5,2]  = 0.941  ; oddsDF[5,3]   = 0.896  ; oddsDF[5,4]   = 0.987 ; oddsDF[5,5]    = 9
  oddsDF[6,1]  = "BMI: Overweight"     ; oddsDF[6,2]  = 0.901  ; oddsDF[6,3]   = 0.860  ; oddsDF[6,4]   = 0.943 ; oddsDF[6,5]    = 8
  oddsDF[7,1] = "Alcohol: > 2 month"   ; oddsDF[7,2]  = 1      ; oddsDF[7,3]   = NA     ; oddsDF[7,4]   = NA    ; oddsDF[7,5]    = 7
  oddsDF[8,1] = "Alcohol: <= 1 month"  ; oddsDF[8,2]  = 0.933  ; oddsDF[8,3]   = 0.885  ; oddsDF[8,4]   = 0.980 ; oddsDF[8,5]    = 6
  oddsDF[9,1] = "Alcohol: Never"       ; oddsDF[9,2]  = 0.938  ; oddsDF[9,3]   = 0.890  ; oddsDF[9,4]   = 0.986 ; oddsDF[9,5]    = 5
  oddsDF[10,1] = "Physical: Light"     ; oddsDF[10,2] = 1      ; oddsDF[10,3]  = NA     ; oddsDF[10,4]  = NA    ; oddsDF[10,5]   = 4
  oddsDF[11,1] = "Physical: None"      ; oddsDF[11,2] = 0.930  ; oddsDF[11,3]  = 0.886  ; oddsDF[11,4]  = 0.975 ; oddsDF[11,5]   = 3
  oddsDF[12,1] = "Physical: Medium"    ; oddsDF[12,2] = 1.053  ; oddsDF[12,3]  = 0.999  ; oddsDF[12,4]  = 1.107 ; oddsDF[12,5]   = 2
  oddsDF[13,1] = "Physical: Hard"      ; oddsDF[13,2] = 0.959  ; oddsDF[13,3]  = 0.913  ; oddsDF[13,4]  = 1.005 ; oddsDF[13,5]   = 1
  
  # oddsDF[1,1]  = "Sex: Woman"          ; oddsDF[1,2]  = 1      ; oddsDF[1,3]   = NA     ; oddsDF[1,4]   = NA    ; oddsDF[1,5]    = 1
  # oddsDF[2,1]  = "Sex: Man"            ; oddsDF[2,2]  = 0.937  ; oddsDF[2,3]   = 0.900  ; oddsDF[2,4]   = 0.974 ; oddsDF[2,5]    = 2
  # oddsDF[3,1]  = "School: Vocational"  ; oddsDF[3,2]  = 1      ; oddsDF[3,3]   = 1      ; oddsDF[3,4]   = 1     ; oddsDF[3,5]    = 3
  # oddsDF[4,1]  = "School: General"     ; oddsDF[4,2]  = 0.996  ; oddsDF[4,3]   = 0.954  ; oddsDF[4,4]   = 1.038 ; oddsDF[4,5]    = 4
  # oddsDF[5,1]  = "School: Sport"       ; oddsDF[5,2]  = 0.974  ; oddsDF[5,3]   = 0.935  ; oddsDF[5,4]   = 1.013 ; oddsDF[5,5]    = 5
  # oddsDF[6,1]  = "BMI: Healthy"        ; oddsDF[6,2]  = 1      ; oddsDF[6,3]   = NA     ; oddsDF[6,4]   = NA    ; oddsDF[6,5]    = 6
  # oddsDF[7,1]  = "BMI: Underweight"    ; oddsDF[7,2]  = 0.968  ; oddsDF[7,3]   = 0.928  ; oddsDF[7,4]   = 1.008 ; oddsDF[7,5]    = 7
  # oddsDF[8,1]  = "BMI: Obese"          ; oddsDF[8,2]  = 1.003  ; oddsDF[8,3]   = 0.959  ; oddsDF[8,4]   = 1.047 ; oddsDF[8,5]    = 8
  # oddsDF[9,1]  = "BMI: Overweight"     ; oddsDF[9,2]  = 0.974  ; oddsDF[9,3]   = 0.935  ; oddsDF[9,4]   = 1.012 ; oddsDF[9,5]    = 9
  # oddsDF[10,1] = "Smoke: Daily"        ; oddsDF[10,2] = 1      ; oddsDF[10,3]  = NA     ; oddsDF[10,4]  = NA    ; oddsDF[10,5]   = 10
  # oddsDF[11,1] = "Smoke: Never"        ; oddsDF[11,2] = 1.022  ; oddsDF[11,3]  = 0.978  ; oddsDF[11,4]  = 1.066 ; oddsDF[11,5]   = 11
  # oddsDF[12,1] = "Smoke: Sometimes"    ; oddsDF[12,2] = 1.036  ; oddsDF[12,3]  = 0.991  ; oddsDF[12,4]  = 1.081 ; oddsDF[12,5]   = 12
  # oddsDF[13,1] = "Snuff: Daily"        ; oddsDF[13,2] = 1      ; oddsDF[13,3]  = NA     ; oddsDF[13,4]  = NA    ; oddsDF[13,5]   = 13
  # oddsDF[14,1] = "Snuff: Never"        ; oddsDF[14,2] = 1.017  ; oddsDF[14,3]  = 0.973  ; oddsDF[14,4]  = 1.060 ; oddsDF[14,5]   = 14
  # oddsDF[15,1] = "Snuff: Sometimes"    ; oddsDF[15,2] = 0.986  ; oddsDF[15,3]  = 0.947  ; oddsDF[15,4]  = 1.025 ; oddsDF[15,5]   = 15
  # oddsDF[16,1] = "Alcohol: > 2 month"  ; oddsDF[16,2] = 1      ; oddsDF[16,3]  = NA     ; oddsDF[16,4]  = NA    ; oddsDF[16,5]   = 16
  # oddsDF[17,1] = "Alcohol: <= 1 month" ; oddsDF[17,2] = 0.991  ; oddsDF[17,3]  = 0.948  ; oddsDF[17,4]  = 1.034 ; oddsDF[17,5]   = 17
  # oddsDF[18,1] = "Alcohol: Never"      ; oddsDF[18,2] = 1.006  ; oddsDF[18,3]  = 0.964  ; oddsDF[18,4]  = 1.049 ; oddsDF[18,5]   = 18
  # oddsDF[19,1] = "Physical: Light"     ; oddsDF[19,2] = 1      ; oddsDF[19,3]  = NA     ; oddsDF[19,4]  = NA    ; oddsDF[19,5]   = 19
  # oddsDF[20,1] = "Physical: None"      ; oddsDF[20,2] = 0.952  ; oddsDF[20,3]  = 0.814  ; oddsDF[20,4]  = 0.990 ; oddsDF[20,5]   = 20
  # oddsDF[21,1] = "Physical: Medium"    ; oddsDF[21,2] = 0.982  ; oddsDF[21,3]  = 0.941  ; oddsDF[21,4]  = 1.023 ; oddsDF[21,5]   = 21
  # oddsDF[22,1] = "Physical: Hard"      ; oddsDF[22,2] = 0.951  ; oddsDF[22,3]  = 0.913  ; oddsDF[22,4]  = 0.989 ; oddsDF[22,5]   = 22
  
  # oddsDF = DF(13,5, defaultValue = NA)
  # colnames(oddsDF) = c("Concept", "Base", "Low", "High", "Row")
  # 
  # oddsDF[1,1]  = "Sex: Woman"          ; oddsDF[1,2]  = 1      ; oddsDF[1,3]   = NA     ; oddsDF[1,4]   = NA    ; oddsDF[1,5]    = 1
  # oddsDF[2,1]  = "Sex: Man"            ; oddsDF[2,2]  = 0.937  ; oddsDF[2,3]   = 0.900  ; oddsDF[2,4]   = 0.974 ; oddsDF[2,5]    = 2
  # 
  # oddsDF[3,1]  = "BMI: Healthy"        ; oddsDF[3,2]  = 1      ; oddsDF[3,3]   = NA     ; oddsDF[3,4]   = NA    ; oddsDF[3,5]    = 3
  # oddsDF[4,1]  = "BMI: Underweight"    ; oddsDF[4,2]  = 0.968  ; oddsDF[4,3]   = 0.928  ; oddsDF[4,4]   = 1.008 ; oddsDF[4,5]    = 4
  # oddsDF[5,1]  = "BMI: Obese"          ; oddsDF[5,2]  = 1.003  ; oddsDF[5,3]   = 0.959  ; oddsDF[5,4]   = 1.047 ; oddsDF[5,5]    = 5
  # oddsDF[6,1]  = "BMI: Overweight"     ; oddsDF[6,2]  = 0.974  ; oddsDF[6,3]   = 0.935  ; oddsDF[6,4]   = 1.012 ; oddsDF[6,5]    = 6
  # 
  # oddsDF[7,1] = "Alcohol: > 2 month"  ; oddsDF[7,2] = 1        ; oddsDF[7,3]  = NA     ; oddsDF[7,4]  = NA     ; oddsDF[7,5]   = 7
  # oddsDF[8,1] = "Alcohol: <= 1 month" ; oddsDF[8,2] = 0.991    ; oddsDF[8,3]  = 0.948  ; oddsDF[8,4]  = 1.034  ; oddsDF[8,5]   = 8
  # oddsDF[9,1] = "Alcohol: Never"      ; oddsDF[9,2] = 1.006    ; oddsDF[9,3]  = 0.964  ; oddsDF[9,4]  = 1.049  ; oddsDF[9,5]   = 9
  # oddsDF[10,1] = "Physical: Light"     ; oddsDF[10,2] = 1      ; oddsDF[10,3]  = NA     ; oddsDF[10,4]  = NA    ; oddsDF[10,5]   = 10
  # oddsDF[11,1] = "Physical: None"      ; oddsDF[11,2] = 0.952  ; oddsDF[11,3]  = 0.814  ; oddsDF[11,4]  = 0.990 ; oddsDF[11,5]   = 11
  # oddsDF[12,1] = "Physical: Medium"    ; oddsDF[12,2] = 0.982  ; oddsDF[12,3]  = 0.941  ; oddsDF[12,4]  = 1.023 ; oddsDF[12,5]   = 12
  # oddsDF[13,1] = "Physical: Hard"      ; oddsDF[13,2] = 0.951  ; oddsDF[13,3]  = 0.913  ; oddsDF[13,4]  = 0.989 ; oddsDF[13,5]   = 13
  
  oddsDF[,1] = factor(oddsDF[,1], levels=rev(oddsDF[,1]))
  
  oddsDF$LittleSegmentCoefficient = nrow(oddsDF)*0.01
  
  # Make the scatterplot with the random data highlighted by HS
  aa = ggplot(oddsDF, aes(x = Base, y = Concept)) +
    
    geom_point( fill = "yellow", color = "black", pch = 21, size = 2) +
    
    geom_vline(xintercept=1, color = "red", linetype = "dashed") +
    
    geom_segment(aes(x = Low,  y = Row, xend = High, yend = Row), color = "black") +
    geom_segment(aes(x = Low,  y = (Row + LittleSegmentCoefficient) , xend = Low, yend  = (Row + (-LittleSegmentCoefficient))), color = "black") +
    geom_segment(aes(x = High, y = (Row + LittleSegmentCoefficient) , xend = High, yend = (Row + (-LittleSegmentCoefficient))), color = "black") +
    
    geom_point( fill = "yellow", color = "black", pch = 21, size = 2) +
    
    labs(title = "CI Comparison for Direct Culture ") +
    
    #xlim(0, +1.2) +
    theme_bw()
  
  ggsave("blah_direct", device="png")
  
  
  # Popularity as boxplot
  popularityDF = DF(2,2)
  colnames(popularityDF) = c("Concept", "Total")
  
  popularityDF[1,1]  = "Sex: Male"             ; popularityDF[1,2]  = 3.46
  popularityDF[2,1]  = "Sex: Female"           ; popularityDF[2,2]  = 3.81
  popularityDF[3,1]  = "BMI: Underweight"      ; popularityDF[3,2]  = 3.61    
  popularityDF[4,1]  = "BMI: Normal"           ; popularityDF[4,2]  = 3.72
  popularityDF[5,1]  = "BMI: Overweight"       ; popularityDF[5,2]  = 3.63
  popularityDF[6,1]  = "BMI: Obese"            ; popularityDF[6,2]  = 2.64
  
  popularityDF[7,1]  = "Alcohol: Never"        ; popularityDF[7,2]  = 3.05     
  popularityDF[8,1]  = "Alcohol: Once a month" ; popularityDF[8,2]  = 3.76 
  popularityDF[9,1]  = "Alcohol: > 2 a month"  ; popularityDF[9,2]  = 3.93 
  popularityDF[10,1] = "Physical: None"        ; popularityDF[10,2] = 3.45    
  popularityDF[11,1] = "Physical: Light"       ; popularityDF[11,2] = 3.56
  popularityDF[12,1] = "Physical: Medium"      ; popularityDF[12,2] = 3.73
  popularityDF[13,1] = "Physical: Hard"        ; popularityDF[13,2] = 3.80
  
  popularityDF[,1] = factor(popularityDF[,1], levels=rev(popularityDF[,1]))
  
  ggplot(popularityDF, aes(Concept, Total)) +
    
      geom_bar(stat="identity", fill = "#e88e2e", color = "black") +
      geom_hline(yintercept=3.6, color = "blue", linetype = "dashed") +
      geom_shadowtext( aes(label=Total), position = position_dodge(0.9), vjust = 0.5, hjust = -0.5, color = "white", fontface = "bold") + 
      ylim(0, +4.4) +
      coord_flip() +
      labs(title = "Popularity barplot",
           subtitle = "Average popularity across all population = 3.6") +
    
      theme_bw()
}


# Trash code for testing things

# # Popularity with carrier
# doCategoricalBoxPlot(completeTable,
#                                            nasalCarrierIndex,
#                                            physicalConnectionsIndex,
#                                            AUREUS_FOLDER,
#                                            showPValues  = TRUE,
#                                            colorsVector = COLOR_VECTOR_CARRIER)
# 
# doCategoricalBoxPlot(womenOnlyTable,
#                      carrierIndex,
#                      overallConnectionsIndex,
#                      AUREUS_FOLDER,
#                      showPValues  = TRUE,
#                      colorsVector = COLOR_VECTOR_CARRIER)  
# 
# doCategoricalBoxPlot(womenVocational,
#                      carrierIndex,
#                      overallConnectionsIndex,
#                      AUREUS_FOLDER,
#                      showPValues  = TRUE,
#                      colorsVector = COLOR_VECTOR_CARRIER)  
# 
# completeTable$PhysicalContact = "Yes"
# 
# IDsWithPhysical = as.numeric(unique(c(physicalEdgesDF$from, physicalEdgesDF$to)))
# 
# for(i in 1:totalRows){
#   
#   if(!(completeTable$ID[i] %in% IDsWithPhysical)) completeTable$PhysicalContact[i] = "No"
#   
# }
# 
# 
# physicalContactPeople = completeTable[completeTable$ID %in% IDsWithPhysical,]
# 
# nonPhysicalContactPeople = completeTable[ !(completeTable$ID %in% IDsWithPhysical),]
# 
# myResults = categoricalXi(completeTable, smokeIndex, 131, AUREUS_FOLDER)
# myResults
# 
# myResults = categoricalXi(nonPhysicalContactPeople, schoolIndex, carrierIndex, AUREUS_FOLDER)
# myResults
# 
# doCategoricalBoxPlot(physicalContactPeople,
#                      carrierIndex,
#                      sexIndex,
#                      AUREUS_FOLDER,
#                      showPValues  = TRUE,
#                      colorsVector = COLOR_VECTOR_CARRIER) 
# 
# myResults = categoricalXi(physicalContactPeople, sexIndex, carrierIndex, AUREUS_FOLDER)



# Close the TXT connections
close(logTXTFileConnection)

# Update the latex file
source("latex.R", encoding="utf-8")

