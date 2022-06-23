# Save the results of the summaries in here
variablesInfoList = newList(totalBasicTables)

# We are going to add the Diabetes info into the basicTable
basicTable$DiabetesT1 = "No"
diabeticusIDs = diseasesDBDF[diseasesDBDF$Diagnostic == "Diabetes Type 1",]$ID
basicTable[basicTable$ID %in% diabeticusIDs,]$DiabetesT1 = "Yes"
sum(basicTable$DiabetesT1 == "Yes")

menList   = basicTable$Sex == "Man"
womenList = basicTable$Sex == "Woman"

# Reload the tables so it includes diabetes
# Serious R, why??? This is just a pointer solution, they were invented 50 years ago!!!
allBasicTablesList = list(basicTable, antropometricTable, aureusTable,
                          highSchoolTable, bloodTable, frienshipTable,
                          drugsTable, sportsTable, biomarkersTable,
                          dietTable, sleepTable)


# For each of the tables that we have    
for(i in 1:totalBasicTables){
    
    # Get the table and table name
    currentTable     = allBasicTablesList[[i]]
    currentTableName = allBasicTablesNames[i]
    
    # FIlter by whatever
    currentTable = currentTable[womenList,]
    
    print(" ---- Doing...")
    print(currentTableName)
    
    # Count how many columns you have
    currentTotalColumns = ncol(currentTable)
    
    # Prepare the variables infor DF
    variablesInfoDF           =  DF(currentTotalColumns,4)
    colnames(variablesInfoDF) = c("VariableID", "Type", "TotalCategories", "TotalNAValues")
    
    # Check how many variables are:
    # -- Dates,
    # -- Categorical,
    # -- Numerical,
    datesVariables       = rep(FALSE, currentTotalColumns)
    categoricalVariables = rep(FALSE, currentTotalColumns)
    numericalVariables   = rep(FALSE, currentTotalColumns)
    
    for(j in 1:currentTotalColumns){
    
        # Get the name
        currentVariableName = colnames(currentTable)[j]

        print(" -------- Variable Summary...")
        print(currentVariableName)
        myCurrentSummary = NA
                
        # Write the name in table
        variablesInfoDF$VariableID[j] = currentVariableName
        
        # Init some values, this is true for numerical and dates
        # if categorical, we change it later.
        variablesInfoDF$TotalCategories[j] = 0
        variablesInfoDF$TotalNAValues[j]   = sum(is.na(currentTable[,j]))
        
        # What type of data for each column
        myCurrentClass = class(currentTable[,j])

        # -- Date
        if(myCurrentClass == "POSIXct" || myCurrentClass == "POSIXt" || myCurrentClass == "Date"){

            # Mark variable as categorical
            datesVariables[j]       = TRUE
            variablesInfoDF$Type[j] = "date"
            
            # Get the variables date statistics
            myCurrentSummary = c(min(currentTable[,j]) , max(currentTable[,j]))
            
        }
        
        # -- Categorical
        if(myCurrentClass == "character" || myCurrentClass == "factor"){

            # Write the name
            variablesInfoDF$VariableID[j] = colnames(currentTable)[j]
            
            # Mark variable as categorical
            categoricalVariables[j] = TRUE
            variablesInfoDF$Type[j] = "categorical"
            
            # Get the variables categorical statistics
            myCurrentSummary = summarizeCategorical(currentTable, j, sorted = "none", crop = 0)
            
            # Add the info to the summary DF
            variablesInfoDF$TotalCategories[j] = nrow(myCurrentSummary)
            variablesInfoDF$TotalNAValues[j]   = sum(is.na(currentTable[,j]))
            
        }
        # -- Numerical
        if(myCurrentClass == "integer" || myCurrentClass == "numeric"){

            # Write the name
            variablesInfoDF$VariableID[j] = colnames(currentTable)[j]
            
            # Mark variable as numerical
            numericalVariables[j]   = TRUE
            variablesInfoDF$Type[j] = "numerical"
            
            # Get the variables numerical statistics
            myCurrentSummary = summarizeNumerical(currentTable, j)
            
        }
        
        print(myCurrentSummary)
        
        
    }
    
    
    # Save the infoDF
    variablesInfoList[[i]] = variablesInfoDF
    
    # Wait for confirmation for the next variables
    invisible(readline(prompt="Press [enter] to continue"))
    
}

# Show everything
for(i in 1:totalBasicTables){
    
    print(variablesInfoList[[i]])
    
}
    

# Check if mental ilnesses correlate with number of friends
{

    # Get all the ICD10 that are related to mental diseases
    # (Everything that starts with F, there a couple of other candidates
    #  cutting by self harm X78 and R53.82 CFS, but Christina said to not include these as mental)
    icd10Mental = unique(c(diseasesDBDF[diseasesDBDF$Title == "Mental and behavioural",]$ICD10))
    
    # Summary of all diseases
    diseaseSummary = summarizeCategorical(diseasesDBDF, 4, sorted = "none", crop = 0, roundMe = 2)
    write.csv2(diseaseSummary, "DiseasesSummary.csv")
    
    # Summary of all mental diseases
    allMentalDiseasesDBDF = diseasesDBDF[diseasesDBDF$ICD10 %in% icd10Mental,]
    mentalSummary = summarizeCategorical(allMentalDiseasesDBDF, 2, sorted = "none", crop = 0, roundMe = 2)
    write.csv2(mentalSummary, "MentalDiseasesSummary.csv")
    
    # Create a dataframe for all mental diseases
    mentalIDs   = allMentalDiseasesDBDF$ID
    noMentalIDs = basicTable[!(basicTable$ID %in% mentalIDs),]$ID
    
    mentalDF               = basicTable
    mentalDF$Connections   = frienshipTable$OverallConnections
    mentalDF$MentalDisease = "No mental disease"
    
    for(i in 1:totalOriginalRows){
        
        currentID = mentalDF$ID[i]
        
        if(currentID %in% mentalIDs) mentalDF$MentalDisease[i] = "Mental disease"
        
    }
    
    # Do the barplot and get p-values
    myBoxplotResults = doCategoricalBoxPlot (mentalDF, 9, 8, PAPER_FOLDER, showPValues  = FALSE)
    
    myBoxplotResults[[1]]
    myBoxplotResults[[2]]
    
    # Get the actual means
    meanNoMental =  mean(mentalDF[mentalDF$MentalDisease == "No mental disease",]$Connections)
    meanMental   =  mean(mentalDF[mentalDF$MentalDisease == "Mental disease",]$Connections)
    
    # Check amount and alcohol and whatnot
    summarizeCategorical(drugsTable[mentalIDs,],   9)
    summarizeCategorical(drugsTable[noMentalIDs,], 9)
    
}

# Create the date plot with the date references
{

    # Find the minimum and maximum date from all the dates
    
    # R is horrible and here is another reason to hate it. Turn out that
    # the friendship creation date starts at 2010-09-20. This date is correct,
    # however it lacks the timezone information (ie UTC). The rest of the dates
    # do have the timezone information. When you do the min() of a no timezone
    # date, and a timezone date, R say "fuck-you" and gives you back POSIX at
    # 1970-01-01. :/ , so I don't include the friendship creation minimum
    # since it is the same at the other minimum anyway.
    minimumDate = min(basicTable$AttendanceDateFF1, basicTable$AttendanceDateFF12, basicTable$MedicationDateFF1, aureusTable$S1_AttendanceDate, aureusTable$S2_AttendanceDate,
        aureusTable$S1_CultureDate, aureusTable$S1_Nasal_FreezeDate, aureusTable$S1_Throat_FreezeDate, bloodTable$BloodAnalysisDate, na.rm=TRUE,
        bloodTable$PlasmaAnalysisDate, na.rm=TRUE)
    
    maximumDate = max(basicTable$AttendanceDateFF1, basicTable$AttendanceDateFF12, basicTable$MedicationDateFF1, aureusTable$S1_AttendanceDate, aureusTable$S2_AttendanceDate,
        aureusTable$S1_CultureDate, aureusTable$S1_Nasal_FreezeDate, aureusTable$S1_Throat_FreezeDate, frienshipTable$Created, bloodTable$BloodAnalysisDate,
        bloodTable$PlasmaAnalysisDate, na.rm=TRUE)
    

    totalWeeks = week(maximumDate) + week(as.Date("2010-12-31")) - week(minimumDate) + 1
    

    # This dataframe put all the dates in the same place so it easier to analyze in a loop
    datesDF = basicTable
    datesDF$Sex           = NULL
    datesDF$Age           = NULL
    datesDF$GeneralHealth = NULL
    datesDF$S1_AttendanceDate    = aureusTable$S1_AttendanceDate
    datesDF$S2_AttendanceDate    = aureusTable$S2_AttendanceDate
    datesDF$S1_CultureDate       = aureusTable$S1_CultureDate
    datesDF$S1_Nasal_FreezeDate  = aureusTable$S1_Nasal_FreezeDate
    datesDF$S1_Throat_FreezeDate = aureusTable$S1_Throat_FreezeDate
    datesDF$Created              = frienshipTable$Created
    datesDF$BloodAnalysisDate    = bloodTable$BloodAnalysisDate
    datesDF$PlasmaAnalysisDate   = bloodTable$PlasmaAnalysisDate
    
    totalConcepts = ncol(datesDF) - 1 # -1 because we don't include the ID
    
    # This dataframe represent each of the weeks from 2010 and 2011
    timelineDF           = data.frame(matrix(0, nrow = totalWeeks * totalConcepts, ncol = 6))
    colnames(timelineDF) = c("Week", "Year", "Total", "Concept", "y", "x")
    
    # Init the week , year and concept in the timelineDF
    currentWeek    = week(minimumDate)
    currentYear    = year(minimumDate)
    targetWeek     = week(as.Date("2010-12-31"))
    
    currentIndex   = 1
    currentXWeek   = 1
    
    for(i in 1:totalWeeks){
    
        # Init the concept
        currentConcept = colnames(datesDF)[1+1] # 2 to skip ID
            
        for(j in 1:totalConcepts){
        
            # Write the current indexes
            timelineDF$Week[currentIndex]    = currentWeek
            timelineDF$Year[currentIndex]    = currentYear
            timelineDF$Concept[currentIndex] = currentConcept
            
            # Update the high and wide x,y variable for plotting
            timelineDF$x[currentIndex] = currentXWeek
            timelineDF$y[currentIndex] = j
            
            # Update the current concept and repeat
            currentConcept = colnames(datesDF)[j+1+1]
    
            currentIndex   = currentIndex + 1
                    
        }
        
        # Update week and year
        currentXWeek = currentXWeek + 1
        currentWeek  = currentWeek  + 1
        
        # Reset the week if we go too far
        if(currentWeek == 54){
            currentWeek = 1
            currentYear = currentYear + 1
        }
        
    }
        
    # Now you just need to go throw each date and increase the proper counter
    for(i in 1:nrow(datesDF)){
        
        for(j in 2:ncol(datesDF)){    
        
            # Get the date
            currentDate = datesDF[i,j]    
            
            # If it is not an NA date
            if(!is.na(currentDate)){
                
                # Get the week and year
                currentWeek = week(currentDate)
                currentYear = year(currentDate)            
            
                # Get the concept
                currentConcept = colnames(datesDF)[j]
            
                # Search for this combination and increase the total+1
                relevantIndex = as.numeric(rownames(timelineDF[timelineDF$Week == currentWeek & timelineDF$Year == currentYear & timelineDF$Concept == currentConcept,]))
                timelineDF$Total[relevantIndex] = timelineDF$Total[relevantIndex] + 1

            }
        

            
        }
            
    }
     
    # Everything is initialize now
    
    # Create a variable with a date string so it get sorted properly
    timelineDF$Date_String = paste0(timelineDF$Year,"_",timelineDF$Week)
       
    # Plot the thing (heatmap is kinda horrible)
    if(FALSE){
        ggplot(timelineDF, aes(x, Concept)) +
            geom_tile(aes(fill = Total)) +
            scale_fill_gradient(low = "white", high = "red")        
    }

    # Basic line plot with points (crap)
    if(FALSE){
         ggplot(data=timelineDF, aes(x, Concept)) +
            geom_line()+
            geom_point()   
    }
    
    # Actual lines (ok, but doesn't look good the leyend)
    if(FALSE){
         ggplot( timelineDF, aes(x=x, y=Total, fill=Concept)) + 
                geom_line(aes(color=Concept))+
                facet_grid(Concept~.)   
    }
    
    # Actual lines rotated (this one is fine)
    # This is for all highschools.
    myHPlot = ggplot( timelineDF, aes(x=x, y=Total, fill=Concept)) + 
                      geom_line(aes(color=Concept))+
                      scale_y_continuous(sec.axis=  sec_axis(~ . + 10, name = derive()) ) +
                      facet_grid(Concept~., switch = "y")+
                      theme(strip.text.y.left = element_text(angle = 0),
                            legend.position="none") +
                      labs(title = "All highschools")
 
    currentFilename = paste0(PAPER_FOLDER,"allHighschools_dates.png")
        
    ggsave(plot = myHPlot, filename = currentFilename)
    
    
    # Now we are going to plot the same thing but divided for each highschool
    allHighschools = sort(unique(highSchoolTable$HighSchool))
    totalHighschools = length(allHighschools)
    
    for(k in 1:totalHighschools){
        
        targetHighschool = as.character(allHighschools[k])
    
        # Now we do the same thing, but for highschool 1 only
        timelineDF$Total = 0
    
        # Init the DF to H1
        for(i in 1:nrow(datesDF)){
            
            # Get the ID and check that it is from H1
            currentID = datesDF$ID[i]
            currentHS = highSchoolTable$HighSchool[currentID]
            # If it is, count things
            if(currentHS == targetHighschool){
            
                for(j in 2:ncol(datesDF)){    
            
                    # Get the date
                    currentDate = datesDF[i,j]    
                
                    # If it is not an NA date
                    if(!is.na(currentDate)){
                    
                    # Get the week and year
                        currentWeek = week(currentDate)
                        currentYear = year(currentDate)            
                    
                        # Get the concept
                        currentConcept = colnames(datesDF)[j]
                    
                        # Search for this combination and increase the total+1
                        relevantIndex = as.numeric(rownames(timelineDF[timelineDF$Week == currentWeek & timelineDF$Year == currentYear & timelineDF$Concept == currentConcept,]))
                        timelineDF$Total[relevantIndex] = timelineDF$Total[relevantIndex] + 1
    
                    }
            
                }
                    
            }
                
        }
            
        # Repeat the same plot
        myHPlot = ggplot( timelineDF, aes(x=x, y=Total, fill=Concept)) + 
                          geom_line(aes(color=Concept))+
                          scale_y_continuous(sec.axis=  sec_axis(~ . + 10, name = derive()) ) +
                          facet_grid(Concept~., switch = "y")+
                          theme(strip.text.y.left = element_text(angle = 0),
                                legend.position="none") +
                          labs(title = paste0(targetHighschool))
        
        
        currentFilename = paste0(PAPER_FOLDER,targetHighschool,"_dates.png")
        
        ggsave(plot = myHPlot, filename = currentFilename)
        
    }
    
    
    

    
}

