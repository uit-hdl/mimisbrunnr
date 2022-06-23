# This script generates the biomarkers results
#
# -- Included in the paper
#
# -- Not included in the paper
# 
#        Vitamim D results
# 


library(ergm)
library(statnet)
library(ggpubr)
library(broom) # How completely stupid R is, that you can't extract the p-value
               # from a model, without using an external library T_T

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

#source("loadDataV2.R", encoding="utf-8")

#++---------------------------------
# Init constants and indexes
#---------------------------------
{
 
    
    TOTAL_SIMULATIONS = 100
    
    # Local filepaths
    # -- LOD vs NDL
    LOD_STATISTICS_FILEPATH          = paste0(BIOMARKERS_FOLDER_TABLES,"LODStats.csv")
    # -- Pvelances
    PREVALENCES_TABLE_FILEPATH       = paste0(BIOMARKERS_FOLDER_TABLES,"prevalences.csv")
    # -- Averages
    AVERAGES_NDL_TABLE_FILEPATH      = paste0(BIOMARKERS_FOLDER_TABLES,"averagesNDL.csv")
    AVERAGES_BMI_NDL_TABLE_FILEPATH  = paste0(BIOMARKERS_FOLDER_TABLES,"averagesBMINDL.csv")
    # -- Scatter plots summaries
    SCATTER_SIMPLE_ROUNDED_FILEPATH  = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"simple_myVSfriendBiomarkersRounded.csv")
    SCATTER_STRATOS_FILEPATH         = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"complex_myVSfriendBiomarkers.csv")
    SCATTER_STRATOS_ROUNDED_FILEPATH = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"complex_myVSfriendBiomarkersRounded.csv")
    SCATTER_BMI_FILEPATH             = paste0(BIOMARKERS_FOLDER,"BMIResults.csv")
    
    
    
    
    # -- Boxplots summaries
    BOXPLOT_BMI_FILEPATH             = paste0(BIOMARKERS_FOLDER,"BMICatResults.csv")
    # -- Distances summary
    DISTANCES_FILEPATH               = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"distancesBioSex.csv")
    
    # -- Categorical summary
    MEN_P_SIMPLE                     = paste0(BIOMARKERS_FOLDER_TABLES,"menPSimple.csv")
    MEN_P_BONFERRONI                 = paste0(BIOMARKERS_FOLDER_TABLES,"menPBonferroni.csv")
    MEN_P_BENJAMINI                  = paste0(BIOMARKERS_FOLDER_TABLES,"menPBenjamini.csv")
    WOMEN_P_SIMPLE                   = paste0(BIOMARKERS_FOLDER_TABLES,"womenPSimple.csv")
    WOMEN_P_BONFERRONI               = paste0(BIOMARKERS_FOLDER_TABLES,"womenPBonferroni.csv")
    WOMEN_P_BENJAMINI                = paste0(BIOMARKERS_FOLDER_TABLES,"womenPBenjamini.csv")
    BOTH_P_SIMPLE                    = paste0(BIOMARKERS_FOLDER_TABLES,"bothPSimple.csv")
    BOTH_P_BONFERRONI                = paste0(BIOMARKERS_FOLDER_TABLES,"bothPBonferroni.csv")
    BOTH_P_BENJAMINI                 = paste0(BIOMARKERS_FOLDER_TABLES,"bothPBenjamini.csv")
    BOTH_EXPAND_P_SIMPLE             = paste0(BIOMARKERS_FOLDER_TABLES,"bothExpandedPSimple.csv")
    BOTH_EXPAND_P_BONFERRONI         = paste0(BIOMARKERS_FOLDER_TABLES,"bothExpandedPBonferroni.csv")
    BOTH_EXPAND_P_BENJAMINI          = paste0(BIOMARKERS_FOLDER_TABLES,"bothExpandedPBenjamini.csv")    
    
    # -- Drugs summary
    # ---- Hormonal summary
    HORMONAL_FILEPATH                = paste0(BIOMARKERS_FOLDER,"HormonalResults.csv")
    
    # -- Diseases summary
    
    # Get how many patients we have
    totalPeople = nrow(completeTable)
  
    # Get how many biomarkers you have.
    totalBiomarkersColumns = ncol(biomarkersTable) - 3
    totalBiomarkers        = totalBiomarkersColumns/2  # LOD and NDL are the same
  
    # Get the appropiate indexes
    LODIndex = firstBiomarkerIndex
    NDLIndex = totalBiomarkers + firstBiomarkerIndex  
  
    # Collection of all LOD and NDL
    allNDLIndex = c(NDLIndex:(NDLIndex+totalBiomarkers-1))
    
}

#++-----------------------------------------------------------------------------
# Prepare the proper datasets and what variables are we going to analyze automatically
#
#     - In here we are just adding the hormonal info from the database to the
#       complete table so R can works properly.
# -----------------------------------------------------------------------------
{
    
    # Count how many people we have in total
    TOTAL_SUBJECTS = nrow(completeTable)
    
    # Add hormonal information to each menstruating woman
    {

        # Add the hormonal contraceptives information to this table
        completeTable$HormonalContraceptives = "None or non-hormonal"
    
        # For each person from which we have any info about hormonal contraceptives
        # add it to the table.
        for(i in 1:nrow(contraceptivesDBDF)){

            currentID            = contraceptivesDBDF$ID[i]
            currentContraceptive = contraceptivesDBDF$Hormonal[i]
            
            # I HATE R SO MUCH! Look at this!
            # You need to convert a string to string, because otherwise, this get
            # equal to a number that you need to convert later into a proper string
            # again.
            #
            # And on top of that, you lose the levels() information while doing this
            # There is a proper way of doing this, which is with enum(), which has
            # existed in C since the beggining of time. WHY YOU DON'T JUST COPY 
            # WHAT IT ACTUALLY WORK AND HAS BEEN DONE BEFORE!!!??? I hereby curse
            # the inventor of R to 1000 year of pain.
            currentContraceptive = as.character(currentContraceptive)
            
            # If is not the default option, added to the DF
            if(currentContraceptive!="Non-hormonal"){
                completeTable$HormonalContraceptives[currentID] = as.character(currentContraceptive)    
            }
            
        }
        
        # Set the proper levels of hormonal contraceptives AGAIN as stated before
        # because R sucks. (Also, why can't we have objects T_T is very annoying
        # to do X = X whatever all the time, it needs to be X.setFactors()
        completeTable$HormonalContraceptives = factor(completeTable$HormonalContraceptives , 
                                                      levels = c("None or non-hormonal", "Progestin",
                                                                 "Low Estradiol", "High Estradiol", "Unknown"))
    
        # Create the special table for women and contraceptives only
        # -- Filter by women
        womenOnlyCopyTable = completeTable[ completeTable$Sex == "Woman",]
        # -- Filter by menstruating women only
        womenMenstruatingTable = womenOnlyCopyTable[womenOnlyCopyTable$MenstruationStart == "Yes",]
        
    }

    # Get the hormonal index
    hormonalIndex = grep("^HormonalContraceptives$" ,   colnames(womenMenstruatingTable))
    
}


#++--------------------------------------------------------------------------------
# Convert supplementary tables that we are going to use in Latex or GITHUB
#--------------------------------------------------------------------------------
{

    writeTableLATEX(biomarkersMetadataDF, BIOMARKERS_FOLDER_TABLES, tableCaption = "Summary of all biomarkers. From left to right, short acronym with the protein ID, protein name, UniProt ID, LOD value for each of the two run batches, UniProt web with the protein, Wikipedia link with the protein.",
                    overrideTableName = "SuplementaryAllBiomarkers")
    
}


# ++------------------------------------------------------------------------------
# Do all the network plots
# ------------------------------------------------------------------------------
{

    # Make a constant layout for consistence between plots
    myGraph           = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
    myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
    
    # Figure in annex where it shows all the graph at the same time with
    # no highlights, all using MDS layout.
    {

        # In this list, we keep the image file path for each of the plots
        myListOfPlots        = rep(NA, TOTAL_NETWORKS)
        myListOfPlotsObjects = newList(TOTAL_NETWORKS)
        
        # Each plot for each network.
        for(i in 1:TOTAL_NETWORKS){

            currentOverridedPlotName = paste(NETWORK_NAMES[i],"_with_no_highlight", sep='')
            currentPlotTitle         = paste(NETWORK_NAMES[i], sep='')
            currentOverrideCaption   = "Size based on number of undirected relationships"

            plotResults = doGraphPlot(allEdges[[i]],  completeTable, PAPER_FOLDER,
                                      sizeVariableIndex = overallConnectionsIndex,
                                      selectedLayouts   = DO_THIS_LAYOUTS,
                                      plotTitle         = currentPlotTitle,
                                      overrideTableName = currentOverridedPlotName) 
    
            # Save the current plot for the grid image
            myListOfPlots[i]          = plotResults[[2]]
            myListOfPlotsObjects[[i]] = plotResults[[1]]

        }
  
        # Make the grid image for all the networks
        # ---- By default, the grid is divided into two columns
        #      you can change this into sqrt of images so it more squarish
        #
        # (remember that in the network.R script you have the same code with
        #  the option to export to latex)
        totalGridColumns = 2
        totalGridRows    = ceiling(TOTAL_NETWORKS/totalGridColumns)

        # ---- PNG
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows)
        ggsave(BIOMARKERS_GRAPH_GRID, width = 11, height = 16)
        
        writeImageLATEX2(BIOMARKERS_GRAPH_GRID, "../../../../results/biomarkers/images/network/", 
                         captionText   = "Overview of all friendship networks",
                         overrideLabel = "fig:allGraphsOverview", 
                         pageHeight    = 0.7, overrideFloat = TRUE)
        
    }
    
    # Figure for the societal highschool
    # -- Red friend with close biomarkers (what does close mean?)
    # -- blue friend with far away biomarkers
    # Figure for the same to same BMI (it looks very ugly with no relevant info)
    if(FALSE){
        
        # Create a DF that we can modify freely without destroying the original one
        myCustomEdgesDF = overallEdgesDF
        
        # Create another DF where we are going to count the same to same relationships
        sameToSameBMI = DF(length(BMILevels),length(BMILevels), defaultValue = 0)
        colnames(sameToSameBMI) = BMILevels
        
        
        # Add the same to same relationship with respect the BMI
        myCustomEdgesDF$SameBMI = "Yes"
        myCustomEdgesDF$FromBMI = ".."
        myCustomEdgesDF$ToBMI   = "..."
        for(i in 1:nrow(myCustomEdgesDF)){
        
            # Get the from and to edge IDs
            fromID = as.numeric(myCustomEdgesDF$from[i])
            toID   = as.numeric(myCustomEdgesDF$to[i])
                
            # Check if they have different BMIs
            fromBMI = as.character(completeTable[fromID, BMICatIndex])
            toBMI   = as.character(completeTable[toID,   BMICatIndex])
        
            myCustomEdgesDF$FromBMI[i] = fromBMI
            myCustomEdgesDF$ToBMI[i]   = toBMI
                
            if(fromBMI != toBMI) myCustomEdgesDF$SameBMI[i] = "No"
            
            # Add the info to our relationship table
            fromRow  = grep(fromBMI, BMILevels)
            toColumn = grep(toBMI,   BMILevels)
            
            currentValue = sameToSameBMI[fromRow, toColumn]
            sameToSameBMI[fromRow, toColumn] = currentValue + 1
        }
        
        
        
        # Get the numbers of same to same relationship compared to what we would expect randomly
        if(FALSE){

            totalSameAAA = sum(myCustomEdgesDF$SameBMI == "Yes")
            totalRelAAA  = nrow(myCustomEdgesDF)
        
            totalSameAAA / totalRelAAA
        
            # Do the homophily analysis first and see what going on, there is some obvious
            # bias for almost everybody in the network.
            partialHomophilyDF  = partialHomophily(overallGraph,  BMICatIndex)
            completeHomophilyDF = completeHomophily(overallGraph, BMICatIndex)    
            
            
            
            
            
            
            
            # Fill the table and check if there is bias in the relationships
            
            # Copy the table and delete the unknowns
            sameToSameBMI2 = sameToSameBMI
            sameToSameBMI2$Unknown = NULL
            sameToSameBMI2 = sameToSameBMI2[-5,]
            # Find the probabilities and delete unknowns
            totalBMI = summarizeCategorical(completeTable, BMICatIndex, sorted = "none")[,2]
            totalBMI = totalBMI[1:4]
            probabilityBMI = summarizeCategorical(completeTable, BMICatIndex, sorted = "none")[,3]
            probabilityBMI = probabilityBMI[1:4]
            # Perform the xi2
            aaa = data.frame(myCustomEdgesDF$FromBMI, myCustomEdgesDF$ToBMI)
            bbb = table(myCustomEdgesDF$FromBMI,      myCustomEdgesDF$ToBMI)
            bbb = bbb[-5,-5]
            bbb = bbb[BMILevels[1:4], BMILevels[1:4]]
            ccc = bbb
            ddd = bbb
            
            chisq.test(bbb)
            
            # Check the individual combinations
            for(i in 1:nrow(ccc)){
            
                for(j in 1:ncol(ccc)){

                    #ccc[i,j] = binom.test(bbb[i,j], totalRelAAA, probabilityBMI[i]* probabilityBMI[j], alternative = "two.sided")$p.value
                    ccc[i,j] = binom.test(bbb[i,j], sum(bbb[i,]), probabilityBMI[i], alternative = "two.sided")$p.value
                    ccc[i,j] = getAsterkisPValue(ccc[i,j])
                    ddd[i,j] = probabilityBMI[i] * probabilityBMI[j] * totalRelAAA
                }
                    
            }
            
            
        }


        # Get the societal layout from our self-made function
        highSchoolLayout = createCategoricalLayout(myCustomEdgesDF, completeTable, highSchoolIndex)
        myGraph = graph_from_data_frame(myCustomEdgesDF, vertices = completeTable, directed = FALSE)
        myConstantLayoutB = create_layout(graph = myGraph, layout = 'mds')
        myConstantLayoutB$x = highSchoolLayout[[1]]$x
        myConstantLayoutB$y = highSchoolLayout[[1]]$y
        
        # Prepare the plot titles
        currentPlotTitle          = "School network, highchools in nodes and same spa-type in edges."
        currentPlotSubtitle       = "Only nodes with a valid spa-type are shown (n = 746)."
        
        doGraphPlot(myCustomEdgesDF,  completeTable, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                    sizeVariableIndex = overallConnectionsIndex,
                    highlightVariable = highSchoolIndex,
                    edgesHighlight    = 4,
                    edgesAlpha        = 0.2,
                    colorVectorEdge   = c("blue", "red"),
                    manualLayout      = myConstantLayoutB,
                    plotTitle         = currentPlotTitle,
                    plotSubtitle      = currentPlotSubtitle,
                    overrideLegendSize = 5)
        
    }
   
}

# ++------------------------------------------------------------------------------
# Check if the friendship is good enough
# ------------------------------------------------------------------------------
{
    
    # -- Histogram with How well is this friendship a representation of real life
    {
    
        plotObject = doHistogramPlot2(completeTable, overviewIndex, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                      colorsVector   = COLOR_FRIENDSHIP,
                                      binsWidth      = 1,
                                      binBorderColor = "#333333",
                                      plotTitle      = " Does these friends give a good overview of your social network? ",
                                      plotSubtitle   = " 0 = Low, 10 = High",
                                      plotXLabel     = "Points", plotYLabel = "Total")       
        
        
        
        writeImageLATEX2(plotObject[[2]], "../../../../results/biomarkers/images/network/", 
                         captionText   = "Histogram indicating how well the social network represent students' social network",
                         overrideLabel = "fig:histogramFriendship", 
                         pageHeight    = 0.4, overrideFloat = TRUE)        
        
    }
    
}


#++---------------------------------------------------------------------------
# Check how many proteins are bellow LOD and make the proper plot and table
#---------------------------------------------------------------------------
{
  
    # Before doing anything, there is something really confusing, that is that
    # not everything was done in the same batch. And each batch has different
    # LODs.
    #
    # So, step zero, figure it out which batch the people have and fill the LOD
    # accordingly
    {
    
      batchIDs      = c("20160383", "20160977")
      LOD386Values  = biomarkersMetadataDF$LOD_Batch_20160383
      LOD977Values  = biomarkersMetadataDF$LOD_Batch_20160977
      batchIDsColor = c("#ed0000", "#00ad0b")
    
      batchInfoList = biomarkersTable$BatchNumber    
    
    }
  
    # -- First we do a general pass to see how many Proteins are under LOD
    generalLODStatisticsDF           = DF(totalBiomarkers, 6)
    colnames(generalLODStatisticsDF) = c("Protein", "N_Under_LOD", "Prt_Under_LOD", "Prt_Above_LOD","N_NA", "Prt_NA")
    for(i in 1:totalBiomarkers){
    
        # First count the missing NA and 99999999 values in the batch file.
        # Since the 999 value is especial and common for each person then do nothing
        # and count NA only
        totalMissing = 0
        totalMissing = totalMissing + sum(is.na(completeTable[,NDLIndex+i-1]))
    
        # Prepare the LOD value vector
        LODValueVector = rep(9999, totalPeople)
        for(j in 1:totalPeople){
      
          # Which valid batch are you?
          myBatch = batchInfoList[j]
          if(!is.na(myBatch)){
        
              if(myBatch == "20160977") LODValueVector[j] = as.numeric(LOD977Values[i])
              if(myBatch == "20160383") LODValueVector[j] = as.numeric(LOD386Values[i])
              # This batch means that we perform both batches for this person
              # So we are going to take the minimum of both LOD
              if(myBatch == "99999999"){
                  LODValueVector[j] = min(as.numeric(LOD386Values[i]), as.numeric(LOD977Values[i]))
              }
          }
      }
      
        # Count how many are bellow LOD
        totalUnderLOD = sum(completeTable[,NDLIndex+i-1] < LODValueVector , na.rm=TRUE)
    
        # Write the LOD statistics
        generalLODStatisticsDF[i,1] = biomarkersMetadataDF$Protein[i]
        generalLODStatisticsDF[i,2] = totalUnderLOD 
        generalLODStatisticsDF[i,3] = totalUnderLOD/totalPeople
        generalLODStatisticsDF[i,4] = 1 - (totalUnderLOD + totalMissing)/totalPeople
        generalLODStatisticsDF[i,5] = totalMissing
        generalLODStatisticsDF[i,6] = totalMissing/totalPeople
    }
    
    write.csv2(generalLODStatisticsDF,  LOD_STATISTICS_FILEPATH)
    
    # Now, we are going to make the plot to take a quick look at each biomarker status
    {
    
      # Prepare the data into long format so ggplot can do it things  
      longLODStats = DF(totalBiomarkers*10000,2)
      colnames(longLODStats) = c("Protein","Type")
      
      currentIndex = 1
      
      # For each protein in the summary
      print("Filling LOD Data")
      for(i in 1:totalBiomarkers){
        
        # Get the current protein and fill a bunch of rows with its name
        currentProtein = generalLODStatisticsDF$Protein[i]
        longLODStats[currentIndex:(currentIndex+10000),1] = currentProtein
        
        # Add as many "Under", "Above", or "Missing" as necessary
        totalUnder =  round(10000 * generalLODStatisticsDF$Prt_Under_LOD[i],0)
        totalAbove =  round(10000 * generalLODStatisticsDF$Prt_Above_LOD[i],0)
        totalNA    =  round(10000 * generalLODStatisticsDF$Prt_NA[i],0)
        
        if(totalUnder > 0){
            
            longLODStats[currentIndex:(currentIndex+totalUnder),2] = "Under LOD"
            currentIndex = currentIndex + totalUnder
          
        }
        
        if(totalAbove > 0){
          
            longLODStats[currentIndex:(currentIndex+totalAbove),2] = "Above LOD"
            currentIndex = currentIndex + totalAbove
          
        }
        
        if(totalNA>0){
          
            longLODStats[currentIndex:(currentIndex+totalNA),2] = "Missing Data"
            currentIndex = currentIndex + totalNA
          
        }
        
        # Give the factor order
        longLODStats$Type = factor(longLODStats$Type, levels = c("Under LOD", "Missing Data",  "Above LOD"))  
        
        
      }
      
      filePath = doLongBarRelativeCombinePlot(longLODStats, 2, 1, BIOMARKERS_FOLDER_IMAGES_GENERAL,
                                              barsFontSize = 2,
                                              colorsVector = c("red","grey","blue"),
                                              plotTitle    = "Proportion of under LOD and missing values",
                                              plotXLabel   = "Biomarkers", plotYLabel = "Percentage",
                                              sort = "none",
                                              imageWidth = 10,
                                              imageHeight = 30)
    
       writeImageLATEX2(filePath[[2]], "../../../../results/biomarkers/images/general/", 
                        captionText   = "Overview of all subject (n=1038) biomarkers values with respect LOD levels. Most of the collected values are well above the LOD (blue).",
                        overrideLabel = "fig:LODLevelsOverview", 
                        pageHeight = 0.3, overrideFloat = TRUE)
      
      
    }
      
}

#++---------------------------------------------------------------------------
# Check how much RAW data we have
# -- This is use mostly to write the summary in the ABSTRACT
#---------------------------------------------------------------------------
{
    
    # BIOMARKERS
    {
        # Get some general statistics about biomarkers
        biomarkersTotal = DF(totalBiomarkers, 3)
        colnames(biomarkersTotal) = c("Variable", "LOD_NonNA", "NDL_NonNA")
        
        # -- Init the names and count
        for(i in 1:totalBiomarkers){
            biomarkersTotal[i,1] = biomarkersMetadataDF$Protein[i]
            biomarkersTotal[i,2] =  sum(!is.na(completeTable[,LODIndex + i - 1]))
            biomarkersTotal[i,3] =  sum(!is.na(completeTable[,NDLIndex + i - 1]))
        }
        
        # Percentage wise
        percentageTotal = biomarkersTotal[2,2]/totalPeople        
    }
    
    # BLOOD
    {
        bloodTotal                = DF(totalBloodColumns, 3)
        colnames(bloodTotal) = c("Variable", "NonNA", "Prc")
        
        # -- Init the names and count
        offset = 17
        for(i in 1:totalBloodColumns){
            
            bloodTotal[i,1] = bloodMetadataDF$Short[i + offset]
            bloodTotal[i,2] = sum(!is.na(completeTable[,firstBloodIndex + i - 1]))
            bloodTotal[i,3] = sum(!is.na(completeTable[,firstBloodIndex + i - 1]))/totalPeople
            
        }
        
        
    }
    
    # ANTROPOMETRY
    {
        
        antropometryTotal           = DF(ncol(antropometricTable), 3)
        colnames(antropometryTotal) = c("Variable", "NonNA", "Prc")
        
        # -- Init the names and count
        for(i in 1:ncol(antropometricTable)){
            
            antropometryTotal[i,1] = colnames(antropometricTable)[i]
            antropometryTotal[i,2] = sum(!is.na(antropometricTable[i]))
            antropometryTotal[i,3] = sum(!is.na(antropometricTable[i]))/totalPeople
            
        }
        
    }
    
}

#++---------------------------------------------------------------------------
# Check differences between men and women
#---------------------------------------------------------------------------
{
    
    # We are going to analyze each biomarker for men and women, the summary goes here
    sexInflamatorySummmaryDF = DF(totalBiomarkers,7)
    colnames(sexInflamatorySummmaryDF) = c("Protein", "PValue", "Significance","DeltaSigma","AbsSigma","Type", "AvgPro")
    
    # Get some general statistics about biomarkers
    biomarkersSummary = DF(totalBiomarkers, 5)
    colnames(biomarkersSummary) = c("Variable", "LOD Average", "NDL Average", "LOD SD", "NDL SD")
  
    # -- Init the names
    for(i in 1:totalBiomarkers){
        biomarkersSummary[i,1]        = biomarkersMetadataDF$Protein[i]
        sexInflamatorySummmaryDF[i,1] = biomarkersMetadataDF$Protein[i]
    }
   
    # We are going to stratify by sex, so divide in men and women
    biomarkersSummaryMen   = biomarkersSummary
    biomarkersSummaryWomen = biomarkersSummary
  
    # For each biomarker and for men and women, fill the table with the averages
    # and SDs, also do the significant difference between and boxplots
    print("Analyzing sex differnces") 
    for(i in 1:totalBiomarkers){
    
        print(   round(100*i/totalBiomarkers,2)   )  
          
        # Men
        # LOD
        biomarkersSummaryMen[i,2] = mean(menOnlyTable[,LODIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryMen[i,4] = sd(menOnlyTable[,LODIndex + i - 1] ,   na.rm = TRUE )
        # NDL
        biomarkersSummaryMen[i,3] = mean(menOnlyTable[,NDLIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryMen[i,5] = sd(menOnlyTable[,NDLIndex + i - 1] ,   na.rm = TRUE )
        
        # Women
        # LOD
        biomarkersSummaryWomen[i,2] = mean(womenOnlyTable[,LODIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryWomen[i,4] = sd(womenOnlyTable[,LODIndex + i - 1] ,   na.rm = TRUE )
        # NDL
        biomarkersSummaryWomen[i,3] = mean(womenOnlyTable[,NDLIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryWomen[i,5] = sd(womenOnlyTable[,NDLIndex + i - 1] ,   na.rm = TRUE )
        
        # Get the LOD cutoff values
        cutOffList      = newList(2)
        cutOffList[[1]] = as.numeric(c(LOD386Values[i], LOD977Values[i]))
        cutOffList[[2]] = batchIDs
        cutOffList[[3]] = batchIDsColor
        
        # Make a nice title
        currentBoxplotTitle    = paste0(biomarkersMetadataDF$Protein[i], " NDL")
        currentBoxplotSubtitle = paste0("Different batches have different LOD (horizontal lines)")
        
        # Do the plot
        myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                                 sexIndex,
                                                 (NDLIndex+i-1),
                                                 BIOMARKERS_FOLDER,
                                                 cutOffLine   = cutOffList,
                                                 colorsVector = COLOR_VECTOR_SEX,
                                                 plotTitle    = currentBoxplotTitle,
                                                 plotSubtitle = currentBoxplotSubtitle,
                                                 plotYLabel   = currentBoxplotTitle,
                                                 overrideImageWidth = 7,
                                                 showPValues=TRUE)
        
        # Get the p-value
        currentPValue = myBoxplotResults[[2]][2,2]
        
        # Write everything into the sex summary result table
        sexInflamatorySummmaryDF[i,2] = currentPValue
        sexInflamatorySummmaryDF[i,3] = getAsterkisPValue(currentPValue)
        sexInflamatorySummmaryDF[i,4] = (biomarkersSummaryMen[i,3] - biomarkersSummaryWomen[i,3]) / biomarkersSummaryWomen[i,5]
        sexInflamatorySummmaryDF[i,5] = abs(sexInflamatorySummmaryDF[i,4])
        
        # Finally, check out the pvalue, and the sex average difference
        # Fill the type accordingly
        
        # If we have significant values
        if(currentPValue < 0.05){
            
            # If men average is greater than women average
            if( biomarkersSummaryMen[i,3] > biomarkersSummaryWomen[i,3]){
                
                sexInflamatorySummmaryDF[i,6] = "Men low p-value"
                sexInflamatorySummmaryDF[i,7] = (-100) * ((biomarkersSummaryMen[i,3] / biomarkersSummaryWomen[i,3]) -  1) 
                
            }
            # If women average is greater than men average
            else{
                
                sexInflamatorySummmaryDF[i,6] = "Women low p-value"
                sexInflamatorySummmaryDF[i,7] = 100 * ((biomarkersSummaryWomen[i,3] / biomarkersSummaryMen[i,3]) - 1)
            }
            
        }
        else{
            
            # If men average is greater than women average
            if(biomarkersSummaryMen[i,3] > biomarkersSummaryWomen[i,3]){
                
                sexInflamatorySummmaryDF[i,6] = "Men"
                
                # Correction for Artemin that has negative values
                if(biomarkersSummaryMen[i,3] / biomarkersSummaryWomen[i,3] < 1)
                    sexInflamatorySummmaryDF[i,7] = 0
                else
                    sexInflamatorySummmaryDF[i,7] = (-100) * ((biomarkersSummaryMen[i,3] / biomarkersSummaryWomen[i,3]) -  1) 
            }
            # If women average is greater than men average
            else{
                
                sexInflamatorySummmaryDF[i,6] = "Women"
                sexInflamatorySummmaryDF[i,7] = 100 * ((biomarkersSummaryWomen[i,3] / biomarkersSummaryMen[i,3]) - 1)
            }
            
        }
        
    }

    # Give the factor order
    sexInflamatorySummmaryDF$Type = factor(sexInflamatorySummmaryDF$Type, levels = c("Men",  "Men low p-value",
                                                                                     "Women","Women low p-value"))  
    
    
    # Do the plot
    filePath = doLongBarAbsoluteCombinePlot(sexInflamatorySummmaryDF, 1, 7, 6, BIOMARKERS_FOLDER_IMAGES_GENERAL,
                                            colorsVector = rev(c(COLOR_MAN_LOW,  COLOR_MAN,
                                                                 COLOR_WOMAN_LOW,COLOR_WOMAN)),
                                            plotTitle    = "Average NDL levels differences with respect sex",
                                            plotYLabel   = "% increase", 
                                            sort = "none", imageHeight = 20, imageWidth = 10)
        
    writeImageLATEX2(filePath[[2]], "../../../../results/biomarkers/images/general/", 
                        captionText   = "Overview of all biomarkers diferences with respect sex. In many cases there is a significant difference between men and women (p<0.05). 
                                         Due biological reasons.",
                        overrideLabel = "fig:BiomarkersBySexDifference",
                        pageHeight = 0.3, overrideFloat = TRUE)
    
    # Prepare a table with the significance differences for the article
    paperSexDifferencesSummmaryDF              = sexInflamatorySummmaryDF
    paperSexDifferencesSummmaryDF$Acronym      = biomarkersMetadataDF$Acronym
    paperSexDifferencesSummmaryDF$Men          = biomarkersSummaryMen$`NDL Average`
    paperSexDifferencesSummmaryDF$Women        = biomarkersSummaryWomen$`NDL Average`
    
    paperSexDifferencesSummmaryDF$PValue  = NULL
    paperSexDifferencesSummmaryDF$DeltaSigma = NULL
    paperSexDifferencesSummmaryDF$AbsSigma   = NULL
    paperSexDifferencesSummmaryDF$Type = NULL
    paperSexDifferencesSummmaryDF$AvgPro = NULL
    
    paperSexDifferencesSummmaryDF$Men   = round(paperSexDifferencesSummmaryDF$Men,2)
    paperSexDifferencesSummmaryDF$Women = round(paperSexDifferencesSummmaryDF$Women,2)
    
    paperSexDifferencesSummmaryDF = paperSexDifferencesSummmaryDF[, c("Acronym", "Protein", "Significance","Men","Women")]
    colnames(paperSexDifferencesSummmaryDF) = c("Acronym", "Protein", "Significance","$\\overline{x}_{men}$", "$\\overline{x}_{women}$")
    
    writeTableLATEX(paperSexDifferencesSummmaryDF, BIOMARKERS_FOLDER_TABLES, tableCaption = "Sex differences for each biomarker",
                    overrideTableName = "SexDifferencesBiomakers", widthProportion = 0, heightProportion = 0.5)
    
    
    
}

#++---------------------------------------------------------------------------
# Separate tables into men and women due high differences levels
#---------------------------------------------------------------------------
{
    
    # we need to do the stratification for both sexes, and for the LOD and NDL
    menBiomarkersLODTable   = menOnlyTable[,LODIndex:(LODIndex+totalBiomarkers-1)]
    menBiomarkersNDLTable   = menOnlyTable[,NDLIndex:(NDLIndex+totalBiomarkers-1)]
    womenBiomarkersLODTable = womenOnlyTable[,LODIndex:(LODIndex+totalBiomarkers-1)]
    womenBiomarkersNDLTable = womenOnlyTable[,NDLIndex:(NDLIndex+totalBiomarkers-1)]
    
    # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
    # We just need to check column 1 as the rest of the columns just follow along
    keepTheseMen   = !is.na(menBiomarkersLODTable[,1])
    keepTheseWomen = !is.na(womenBiomarkersLODTable[,1])
    
    menBiomarkersLODTable   = menBiomarkersLODTable[keepTheseMen,]
    menBiomarkersNDLTable   = menBiomarkersNDLTable[keepTheseMen,]
    womenBiomarkersLODTable = womenBiomarkersLODTable[keepTheseWomen,]
    womenBiomarkersNDLTable = womenBiomarkersNDLTable[keepTheseWomen,]
    
    # Later on, we need to check who are friend with people with valid LOD/NDL
    # values, so keep track of the IDs
    deletedMenIDs   = menOnlyTable[!keepTheseMen,]$ID
    deletedWomenIDs = womenOnlyTable[!keepTheseWomen,]$ID
    keepMenIDs      = menOnlyTable[keepTheseMen,]$ID
    keepWomenIDs    = womenOnlyTable[keepTheseWomen,]$ID
    
    # Finally, we need a table that has every other variable, but only valid
    # men and women
    biomenOnlyTable   = menOnlyTable[keepTheseMen,]
    biowomenOnlyTable = womenOnlyTable[keepTheseWomen,]
    
    # From here on, we have tables clean of NAs. So check how big is each table
    totalMen   = nrow(menBiomarkersLODTable)
    totalWomen = nrow(womenBiomarkersLODTable)
    
}


#++---------------------------------------------------------------------------
# Do the basic blood analysis
#
# -- This generate the blood summary table, as well as the differences
#    for each blood level between men and women
#
# -- Figure that is very similar to the biomarker one, but for blood
#    instead, and show the absolute differences between men and women
#---------------------------------------------------------------------------
{

    # Copy the original metadata
    bloodMetadataLatexDF = bloodMetadataDF
    # Delete the ID and all event data from the table, not interested
    bloodMetadataLatexDF = bloodMetadataLatexDF[-c(1:17),]
    # Delete the original name, not interesting in the article
    bloodMetadataLatexDF$Original = NULL
    # Rename the final columns
    colnames(bloodMetadataLatexDF)[4] = "Men Lower Limit"
    colnames(bloodMetadataLatexDF)[5] = "Men Upper Limit"
    colnames(bloodMetadataLatexDF)[6] = "Women Lower Limit"
    colnames(bloodMetadataLatexDF)[7] = "Women Upper Limit"
    # Add men, women and significance
    bloodMetadataLatexDF$Men_Average     = 0
    bloodMetadataLatexDF$Women_Average   = 0
    bloodMetadataLatexDF$Significance    = 0
    bloodMetadataLatexDF$Freq_Men_Out    = 0
    bloodMetadataLatexDF$Freq_Women_Out  = 0
    
    # Prepare the DF for the blood differences by sex
    sexBloodSummmaryDF            = DF(totalBloodColumns,7)
    colnames(sexBloodSummmaryDF)  = c("BloodLevel", "PValue", "Significance","DeltaSigma","AbsSigma","Type", "AvgPro")
    sexBloodSummmaryDF$BloodLevel = bloodMetadataLatexDF$Short
    
    # Trick the upper and lower limit
    # We don't have the information for this yet, so just put whatever there
    # base in the SD. Everybody +-2SD away are bad for example
    {
    
        for(i in 1:nrow(bloodMetadataLatexDF)){

            # R, and RStudio in particular, are a a POS eviroments for debugging
            # It doesn't tell you almost never the line where the error is located,
            # and in RStudio 4.x now it has an option by default that send you
            # to a weird browse() function, in the lowest level functions, every
            # time that there is an error. I completely, and utterly, hate this
            # language and I hope one day it dies in a fire.
            
            if(bloodMetadataLatexDF$`Men Upper Limit`[i] == 9999){
                
                currentAverage     = mean(completeTable[, (firstBloodIndex + i - 1)] , na.rm = TRUE)
                currentSTdeviation = sd(completeTable[, (firstBloodIndex + i - 1)] , na.rm = TRUE)
                
                newUpperLimit = currentAverage + 2*currentSTdeviation
                newLowerLimit = currentAverage - 2*currentSTdeviation
                
                bloodMetadataLatexDF$`Men Upper Limit`[i]   = newUpperLimit
                bloodMetadataLatexDF$`Men Lower Limit`[i]   = newLowerLimit
                bloodMetadataLatexDF$`Women Upper Limit`[i] = newUpperLimit
                bloodMetadataLatexDF$`Women Lower Limit`[i] = newLowerLimit                
                
            }
            
    
                
        }
            
    }
    
    # Find the sex differences within blood for each blood parameter
    for(i in firstBloodIndex:lastBloodIndex){
        
        currentTableIndex = i-firstBloodIndex+1
        
        currentPlotTitle = paste0(bloodMetadataLatexDF$Short[currentTableIndex]," (",bloodMetadataLatexDF$Unit[currentTableIndex],")" )
        
        # Average for men and women
        menMean   =  mean(menOnlyTable[,i], na.rm = TRUE)
        womenMean = mean(womenOnlyTable[,i], na.rm = TRUE)
        menSD     = sd(menOnlyTable[,i], na.rm = TRUE)
        womenSD   = sd(womenOnlyTable[,i], na.rm = TRUE)
        
        bloodMetadataLatexDF$Men_Average[currentTableIndex]   = menMean
        bloodMetadataLatexDF$Women_Average[currentTableIndex] = womenMean
        
        # Get the cutoff values for that blood variable
        #cutOffList      = newList(3)
        #cutOffList[[1]] = as.numeric( c( bloodMetadataLatexDF$`Men Lower Limit`[currentTableIndex] , bloodMetadataLatexDF$`Men Upper Limit`[currentTableIndex]))
        #cutOffList[[2]] = c("Lower", "Upper")
        #cutOffList[[3]] = batchIDsColor        
        
        
        # Do the plot
        # This only compares the different between men and women RAW levels.
        # It doesn't make sense to compare the limit between both at this point and
        # that is done in the next series of plots.
        currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, i,
                                              BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                              colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
                                              plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        #currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, i,
        #                                      BIOMARKERS_FOLDER_IMAGES_BLOOD,
        #                                      colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
        #                                      cutOffLine   = cutOffList,
        #                                      plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        
                
        # Get the p-value
        currentPValue  = currentBoxPlot[[2]][2,2]
        asteriskPValue = getAsterkisPValue(currentPValue)
        
        # Find the significance
        bloodMetadataLatexDF$Significance[currentTableIndex] = asteriskPValue
        
        # Count how many men and women are not eating properly
        # -- Lower and Upper Limits
        currentMenLowerLimit   = bloodMetadataLatexDF$`Men Lower Limit`[currentTableIndex]
        currentMenUpperLimit   = bloodMetadataLatexDF$`Men Upper Limit`[currentTableIndex]
        currentWomenLowerLimit = bloodMetadataLatexDF$`Women Lower Limit`[currentTableIndex]
        currentWomenUpperLimit = bloodMetadataLatexDF$`WomenUpper Limit`[currentTableIndex]
        # -- Do the counting
        totalMenData     = sum(!is.na(menOnlyTable[,i]))
        totalWomenData   = sum(!is.na(womenOnlyTable[,i]))
        menEatingUnder   = sum(currentMenLowerLimit > menOnlyTable[,i],   na.rm = TRUE)
        menEatingAbove   = sum(currentMenUpperLimit < menOnlyTable[,i],   na.rm = TRUE)
        womenEatingUnder = sum(currentWomenLowerLimit > womenOnlyTable[,i], na.rm = TRUE)
        womenEatingAbove = sum(currentWomenUpperLimit < womenOnlyTable[,i], na.rm = TRUE)
        # -- Write in table
        bloodMetadataLatexDF$Freq_Men_Out[currentTableIndex]   = paste0(round(((menEatingUnder   + menEatingAbove)/totalMenData)    *100,1),"%")
        bloodMetadataLatexDF$Freq_Women_Out[currentTableIndex] = paste0(round(((womenEatingUnder + womenEatingAbove)/totalWomenData)*100,1),"%")
        
        
        # Write everything into the sex summary result table
        sexBloodSummmaryDF[currentTableIndex,2] = currentPValue
        sexBloodSummmaryDF[currentTableIndex,3] = asteriskPValue
        sexBloodSummmaryDF[currentTableIndex,4] = (bloodMetadataLatexDF$Men_Average[currentTableIndex] - bloodMetadataLatexDF$Women_Average[currentTableIndex]) / womenSD
        sexBloodSummmaryDF[currentTableIndex,5] = abs(sexBloodSummmaryDF[currentTableIndex,4])
        
        # Finally, check out the pvalue, and the sex average difference
        # Fill the type accordingly
        
        # If we have significant values
        if(currentPValue < 0.05){
            
            # If men average is greater than women average
            if( menMean > womenMean){
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Men low p-value"
                sexBloodSummmaryDF[currentTableIndex,7] = (-1) * ((menMean / womenMean) -  0) 
                
            }
            # If women average is greater than men average
            else{
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Women low p-value"
                sexBloodSummmaryDF[currentTableIndex,7] = 1 * ((womenMean / menMean) - 0)
            }
            
        }
        else{
            
            # If men average is greater than women average
            if(menMean > womenMean){
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Men"
                sexBloodSummmaryDF[currentTableIndex,7] = (-1) * ((menMean / womenMean) -  0) 
                
                    
            }
            # If women average is greater than men average
            else{
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Women"
                sexBloodSummmaryDF[currentTableIndex,7] = 1 * ((womenMean / menMean) - 0)
            }
            
        }
        
        # What the fuck
        if((menEatingUnder + menEatingAbove) > totalMenData){
            
            print("Variable")
            print(currentPlotTitle)
            print("In blood table")
            print(currentTableIndex)
            print("In real table")
            print(i)
            print("Data")
            print(menOnlyTable[,i])
            print("Limits")
            print(currentLowerLimit)
            print(currentUpperLimit)
         
            mean(completeTable[, 140] , na.rm = TRUE)
            sd(completeTable[, 140] , na.rm = TRUE)
               
        }
    }
    
    # Round the numbers a bit
    bloodMetadataLatexDF$`Men Lower Limit`   = round(bloodMetadataLatexDF$`Men Lower Limit`, 2)
    bloodMetadataLatexDF$`Men Upper Limit`   = round(bloodMetadataLatexDF$`Men Upper Limit`, 2)
    bloodMetadataLatexDF$`Women Lower Limit` = round(bloodMetadataLatexDF$`Women Lower Limit`, 2)
    bloodMetadataLatexDF$`Women Upper Limit` = round(bloodMetadataLatexDF$`Women Upper Limit`, 2)
    bloodMetadataLatexDF$Men_Average         = round(bloodMetadataLatexDF$Men_Average, 2)
    bloodMetadataLatexDF$Women_Average       = round(bloodMetadataLatexDF$Women_Average, 2)
    
    # Change the column names into latex format
    colnames(bloodMetadataLatexDF)[8] = "$\\overline{x}_{men}$"
    colnames(bloodMetadataLatexDF)[9] = "$\\overline{x}_{women}$"
    colnames(bloodMetadataLatexDF)[11] = "${Men}_{out}$"
    colnames(bloodMetadataLatexDF)[12] = "${Women}_{out}$"
    
    # Write final result
    writeTableLATEX(bloodMetadataLatexDF, BIOMARKERS_FOLDER_TABLES_BLOOD, tableCaption = "Summary of all blood variables",
                    overrideTableName = "SummaryBloodVariablesTable", widthProportion = 1)
    
    
    # Finish the plot for the blood levels as well
    
    # Give the factor order
    sexBloodSummmaryDF$Type = factor(sexBloodSummmaryDF$Type, levels = c("Men",  "Men low p-value",
                                                                         "Women","Women low p-value"))  
    
    
    # Do the plot
    filePath = doLongBarAbsoluteCombinePlot(sexBloodSummmaryDF, 1, 7, 6, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                            colorsVector = rev(c(COLOR_MAN_LOW,  COLOR_MAN,
                                                                 COLOR_WOMAN_LOW,COLOR_WOMAN)),
                                            plotTitle    = "Ratio of averages for each blood levels with respect sex",
                                            plotYLabel   = "Greater average / lower average", 
                                            sort = "none", imageHeight = 20, imageWidth = 10)
        
    writeImageLATEX2(filePath[[2]], "../../../../results/biomarkers/images/blood/", 
                        captionText   = "Overview of all blood diferences with respect sex. In many cases there is a significant difference between men and women (p<0.05), 
                                         due biological reasons. Ratio is calculated by dividing the greater average between the lowest average. Negative and positive values
                                         are arbitrary and merelly to separate men to the left and women to the right.",
                        overrideLabel = "fig:BloodBySexDifference",
                        pageHeight = 0.3, overrideFloat = TRUE)
    
    
    
}

#++---------------------------------------------------------------------------
# Do figures for differences between men and women for blood
#
# -- Relative levels between 0 to 1 for both men and women
#---------------------------------------------------------------------------
{
    
    # We are going to prepare the table with the relative values.
    relativeBloodValues           = DF((totalPeople*totalBloodColumns), 3)
    colnames(relativeBloodValues) = c("RelativeValue", "Sex", "BloodConcept")
    
    currentCounter = 1
    
    # For each person, and for each blood concept, start filling the table
    # with the appropiate relative value
    for(i in 1:totalPeople){
        
        for(j in 1:totalBloodColumns){

            # Get the sex
            currentSex       = "Man"
            if(as.character(completeTable[i, sexIndex]) == "Woman") currentSex = "Woman"  # R is a stupid language and I HATE the levels class. GET ME A STRING ALWAYS FOR FUCK SAKE >:(

            # Get the blood concept
            currentBloodConcent = bloodMetadataLatexDF$Short[j]
            
            # Get the real value
            currentRealValue     = completeTable[i,j+firstBloodIndex-1]
            currentRelativeValue = NA
            currentLowerValue    = 0
            currentUpperValue    = 9999
            
            # If we have an actual value, find the relative of it
            if(!is.na(currentRealValue)){
                
                # Transform it to relative levels
                # -- Get the values for men and women
                if(currentSex == "Man"){
                
                    currentUpperValue = bloodMetadataLatexDF$`Men Upper Limit`[j]
                    currentLowerValue = bloodMetadataLatexDF$`Men Lower Limit`[j]
                
                }
                else{
                
                    currentUpperValue = bloodMetadataLatexDF$`Women Upper Limit`[j]
                    currentLowerValue = bloodMetadataLatexDF$`Women Lower Limit`[j]
                
                }
            
                interValueRange = (currentUpperValue - currentLowerValue)
            
                # The real value is in between and the person is healthy
                if(currentRealValue>currentLowerValue && currentRealValue<currentUpperValue){
                    
                    currentRelativeValue = (currentRealValue - currentLowerValue) / interValueRange
                    
                }
                # The real value is lower, and the person should eat more
                if(currentRealValue<currentLowerValue){
                
                    realDistance         = currentLowerValue - currentRealValue
                    relativeDistance     = realDistance / interValueRange
                    currentRelativeValue = 0 - relativeDistance
                
                }
                # The real value is greater, and the person should eat less
                if(currentRealValue>currentUpperValue){
                
                    realDistance         = currentRealValue - currentUpperValue
                    relativeDistance     = realDistance / interValueRange
                    currentRelativeValue = 1 + relativeDistance    
                
                }
                
            }
            
            # Otherwise, keep the NA and go to the next one
            relativeBloodValues[currentCounter,1] = currentRelativeValue             
            relativeBloodValues[currentCounter,2] = currentSex
            relativeBloodValues[currentCounter,3] = currentBloodConcent
            
            currentCounter = currentCounter + 1 
            
        }
        
    }
    
    # Give levels so we keep consistency in the plots
    relativeBloodValues$BloodConcept = factor(relativeBloodValues$BloodConcept, levels = c(bloodMetadataLatexDF$Short))
    
    # From here, the table is finish, now we need to make the plot for both
    
    # However, we are going to break the image into three parts.
    # -- FA
    # -- wFA
    # -- Everything else
    relativeBloodValues_A = relativeBloodValues
    relativeBloodValues_B = relativeBloodValues
    relativeBloodValues_C = relativeBloodValues
    
    A_names = bloodMetadataLatexDF$Short[1:28]
    B_names = bloodMetadataLatexDF$Short[29:52]
    C_names = bloodMetadataLatexDF$Short[53:nrow(bloodMetadataLatexDF)]
    
    relativeBloodValues_A              = relativeBloodValues_A[relativeBloodValues_A$BloodConcept %in% A_names,]
    relativeBloodValues_A$BloodConcept = factor(relativeBloodValues_A$BloodConcept, levels = c(A_names))
    relativeBloodValues_B              = relativeBloodValues_B[relativeBloodValues_B$BloodConcept %in% B_names,]
    relativeBloodValues_B$BloodConcept = factor(relativeBloodValues_B$BloodConcept, levels = c(B_names))    
    relativeBloodValues_C              = relativeBloodValues_C[relativeBloodValues_C$BloodConcept %in% C_names,]
    relativeBloodValues_C$BloodConcept = factor(relativeBloodValues_C$BloodConcept, levels = c(C_names))    

    # men and women
    # -- Make the titles
    currentPlotTitle    = "Relative Blood levels for men and women"
    currentPlotTitleA   = "Relative Blood levels for men and women A"
    currentPlotTitleB   = "Relative Blood levels for men and women B"
    currentPlotTitleC   = "Relative Blood levels for men and women C"
    currentPlotSubtitle = "Outliers are hidden to avoid visual cluttering"
    # -- Add the 0/1 lines
    cutOffList      = newList(3)
    cutOffList[[1]] = as.numeric(c(0,1))
    cutOffList[[2]] = c("Lower", "Upper")
    cutOffList[[3]] = batchIDsColor
    
    plotA = doBiCategoricalBoxPlot(relativeBloodValues_A, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                   colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                                   plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleA,
                                   plotSubtitle = currentPlotSubtitle,
                                   cutOffLine = cutOffList, outlierShape = NA,
                                   overrideImageWidth = 16, longPlot = TRUE,
                                   ymin = -0.2, ymax = 1.2 )
    
    plotB = doBiCategoricalBoxPlot(relativeBloodValues_B, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                           colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                           plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleB,
                           plotSubtitle = currentPlotSubtitle,
                           cutOffLine = cutOffList, outlierShape = NA,
                           overrideImageWidth = 16, longPlot = TRUE,
                            ymin = -0.2, ymax = 1.2 )    
    
    plotC = doBiCategoricalBoxPlot(relativeBloodValues_C, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                           colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                           plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleC,
                           plotSubtitle = currentPlotSubtitle,
                           cutOffLine = cutOffList, outlierShape = NA,
                           overrideImageWidth = 16, longPlot = TRUE,
                            ymin = -0.2, ymax = 1.2 )    
    

    writeImageLATEX2(plotA[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including variables that are not Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsA",
                     pageHeight = 0.7, overrideFloat = TRUE)
    
    writeImageLATEX2(plotB[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including absolute levels of Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsB",
                     pageHeight = 0.7, overrideFloat = TRUE)
    
    writeImageLATEX2(plotC[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including relative levels of Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsC",
                     pageHeight = 0.7, overrideFloat = TRUE)    
    
    
}

#++---------------------------------------------------------------------------
# Create the antropometry table and figures for men and women
#---------------------------------------------------------------------------
{

    # How many antropomotric variables do you have
    totalAntropometries = lastAntropometryIndex - fistAntropometryIndex
    
    # Save the boxplots in here
    myListOfPlotsObjects = newList(totalAntropometries)
    
    # Save the antropometric values here so we print this table
    antropometricSummaryDF = DF(totalAntropometries, 6)
    colnames(antropometricSummaryDF) = c("Concept","$\\overline{x}_{men}$", "$\\overline{x}_{women}$", "$SD_{men}$", "$SD_{women}$", "Significance")
    
    # For each antropometric variable do your thing
    for(i in 1:totalAntropometries){
        
        currentPlotTitle  = ""
        
        # Adjust the labels for each variable
        if(i == 1) currentPlotTitle  = "Waist circurference (cm)"
        if(i == 2) currentPlotTitle  = "Hip circurference (cm)"
        if(i == 3) currentPlotTitle  = "Height (cm)"
        if(i == 4) currentPlotTitle  = "Weight (kg)"
        if(i == 5) currentPlotTitle  = "BMI (kg/m²)"
        if(i == 6) currentPlotTitle  = "Heart Rate (bpm)"
        if(i == 7) currentPlotTitle  = "Systolic BP (mmHg)"
        if(i == 8) currentPlotTitle  = "Diastolic BP (mmHg)"
        
        # (density plot doesn't look good, boxplots simplest but looks much more nicer)
        #doCategoricalDensityPlot(completeTable, (fistBloodIndex + i - 1), sexIndex, plotFilePath = BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        #                        colorsVector = COLOR_VECTOR_SEX,
        #                       imageWidth = 8, imageHeight = 8)    
                
        
        currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, (fistAntropometryIndex + i - 1),
                                              BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
                                              colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
                                              plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        myListOfPlotsObjects[[i]] = currentBoxPlot[[1]]
        
        # Fill the table
        antropometricSummaryDF[i,1] = colnames(completeTable)[(fistAntropometryIndex + i - 1)]
        antropometricSummaryDF[i,2] = round(mean(menOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,3] = round(mean(womenOnlyTable[,(fistAntropometryIndex + i - 1)], na.rm = TRUE),1)
        antropometricSummaryDF[i,4] = round(sd(menOnlyTable[,(fistAntropometryIndex + i - 1)],     na.rm = TRUE),1)
        antropometricSummaryDF[i,5] = round(sd(womenOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,6] = getAsterkisPValue(currentBoxPlot[[2]][2,2])

    }

    # Make the grid image
    totalGridColumns = 2
    totalGridRows    = ceiling(totalAntropometries/totalGridColumns)

    # Save as PNG
    ALL_ANTROPOMETRY_FILEPATH = file.path(paste(BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, "BloodGrid.png", sep = ""))
    ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows,
              common.legend = TRUE, legend = "bottom")
    ggsave(ALL_ANTROPOMETRY_FILEPATH, width = 10, height = 16)
 
    writeImageLATEX2(ALL_ANTROPOMETRY_FILEPATH, "../../../../results/biomarkers/images/antropometry/", 
                     captionText   = "Overview of all antropometric variables diferences with respect sex.",
                     overrideLabel = "fig:BloodBySexDifference",
                     pageWidth = 1, overrideFloat = TRUE)
     
    
    # Save the table
    writeTableLATEX(antropometricSummaryDF, BIOMARKERS_FOLDER_TABLES, tableCaption = "Sex differences for antropometry variables",
                    overrideTableName = "SexDifferencesAntropometryTable", widthProportion = 0, heightProportion = 0.1)
    
}

#++---------------------------------------------------------------------------
# Create a mega table with all the modalities averages for men and women
# the next part of the analysis is to check if there is any significance
# differences in here
#---------------------------------------------------------------------------
{

    prevalencesDF = summarizePrevalences(completeTable, allCategoricalIndexes,
                                         sexIndex, prevalenceTarget = "Man")
        
    averagesDF    = summarizeAverages(completeTable, allCategoricalIndexes,
                                      allNDLIndex, sexIndex)
    
    
    # Write into disk
    write.csv2(prevalencesDF, PREVALENCES_TABLE_FILEPATH)
    write.csv2(averagesDF,    AVERAGES_NDL_TABLE_FILEPATH)
    
}

#++---------------------------------------------------------------------------
# Try the Xi² for all categories
#---------------------------------------------------------------------------
{

    # First, generate the big table with all the results
    # -- Generate the xi² results summary
    xi2SummaryDF  = simpleXi(completeTable, sexIndex, allCategoricalIndexes)
    # -- Make the table looks better
    xi2SummaryDFT = transposeDF(xi2SummaryDF) # The base code R function for transpose sucks for DFs with row names in columns rather than in row names
    xi2SummaryDFT = xi2SummaryDFT[-1,]
    colnames(xi2SummaryDFT)[2] = "p-value"
    xi2SummaryDFT$Significance = getAsterkisPValue(xi2SummaryDFT[,2])
    # -- Round the p-values with proper sci-notation
    xi2SummaryDFT$`p-value` = formatC(xi2SummaryDFT$`p-value`, format = "e", digits = 2)
        
    # Now, generate each individual sub-table and the proper latex input code
    # This goes into the supplementary
    currentTableCaption = ""
    currentTableName    = ""
    currentVariableName = ""
    for(i in 1:totalCategoricalIndexes){
    
        currentVariableName = colnames(completeTable)[allCategoricalIndexes[i]]
        
        currentSummaryTable              = categoricalXi(completeTable, sexIndex, allCategoricalIndexes[i])[[9]]
        colnames(currentSummaryTable)[1] = currentVariableName
        
        currentTableCaption = paste0("Sex differences for ",      currentVariableName)
        currentTableName    = paste0("CategoricalSexDifferences", currentVariableName)
        
        
        tableInfo = writeTableLATEX(currentSummaryTable, BIOMARKERS_FOLDER_TABLES, tableCaption = currentTableCaption,
                                    overrideTableName = currentTableName, widthProportion = 0, heightProportion = 0.05)
        
        #print(paste0("\\input{../../../../results/biomarkers/tables/",tableInfo[[2]],".tex}"))
        
        	
        
    }
    
    
    # Write the latex table
    writeTableLATEX(xi2SummaryDFT, BIOMARKERS_FOLDER_TABLES, tableCaption = "Sex differences for all categorical host factor",
                    overrideTableName = "SexDifferencesHostFactorsTable", widthProportion = 0.7)
        
    
}


#++---------------------------------------------------------------------------
# Do biomarkers levels for every categorical variable (men and women)
#                           MEN                   |       WOMEN
# Variable  | n = 1023 |          | 0.03 | **     | 
# Mod 1     |      829 |   2.3    |               |
# Mod 2     |      123 |   5.6    |               |
# ...
# Variable  |                                     |

#---------------------------------------------------------------------------
{
    
    # Create the tables for men and women where we are going to fill the results
    menCategoricalTotal             = readyDFModalities(completeTable, allCategoricalIndexes, allNDLIndex)
    menCategoricalAverages          = menCategoricalTotal
    menCategoricalPValues           = readyDFVariables(completeTable,  allCategoricalIndexes, allNDLIndex)
    
    colnames(menCategoricalPValues) = c("Variable",biomarkersMetadataDF$Protein) # Change the names from Protein NDL ..293848.. to just Protein
    
    menCategoricalPAsterisk   = menCategoricalPValues
    
    womenCategoricalTotal     = menCategoricalTotal
    womenCategoricalAverages  = menCategoricalTotal
    womenCategoricalPValues   = menCategoricalPValues
    womenCategoricalPAsterisk = menCategoricalPValues
    
    # We will need to addjust because we have way too many p.values, so here are two methods
    menCategoricalPValuesBonferroni     = menCategoricalPValues
    menCategoricalPValuesBenjamini      = menCategoricalPValues
    menCategoricalPAsteriskBonferroni   = menCategoricalPValues
    menCategoricalPAsteriskBenjamini    = menCategoricalPValues
    
    womenCategoricalPValuesBonferroni   = menCategoricalPValues
    womenCategoricalPValuesBenjamini    = menCategoricalPValues
    womenCategoricalPAsteriskBonferroni = menCategoricalPValues
    womenCategoricalPAsteriskBenjamini  = menCategoricalPValues
    
    # For each biomarker and for each categorical variable, check if there is
    # a significance with the NDL values, using only non NA values.
    # whether this makes sense or not, is up to the reader to check with
    # the table of LOD levels
    for(i in 1:totalBiomarkers){
    
        for(j in 1:length(allCategoricalIndexes)){
            
            # Get the indexes for the biomarker and the variable
            currentBiomarkerIndex = allNDLIndex[i]
            currentVariableIndex  = allCategoricalIndexes[j]
            
            # We skip the j=1 for the pvalues because that is the sex variable
            # and is only men and women
            if(j>1){
            
                # Find the p-values, and skip the unknown categories
                # later on, we will show how many unknown categories are there
                # but they are not use in the calculation
                menCategoricalPValues[j,(i+1)]     = simpleCategoricalPValue(biomenOnlyTable,   currentVariableIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
                menCategoricalPAsterisk[j,(i+1)]   = getAsterkisPValue(menCategoricalPValues[j,(i+1)])
                
                womenCategoricalPValues[j,(i+1)]   = simpleCategoricalPValue(biowomenOnlyTable, currentVariableIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
                womenCategoricalPAsterisk[j,(i+1)] = getAsterkisPValue(womenCategoricalPValues[j,(i+1)])
                
            }
            else{
            
                menCategoricalPValues[j,(i+1)]     = 1
                menCategoricalPAsterisk[j,(i+1)]   = 'ns'
                
                womenCategoricalPValues[j,(i+1)]   = 1
                womenCategoricalPAsterisk[j,(i+1)] = 'ns'
                
                
            }
        }
            
    }
  
    # We need to count how many significances we have in each table and see
    # if we get too many positives just by random chance. So we adjust the
    # previous results by Bonferroni and Benjamini methods
    {
    
        # First we addjust the p-values
        for(i in 1:totalBiomarkers){
        
            menCategoricalPValuesBonferroni[,i+1]   = p.adjust(menCategoricalPValues[,i+1],   method="bonferroni") 
            menCategoricalPValuesBenjamini[,i+1]    = p.adjust(menCategoricalPValues[,i+1],   method="fdr") 
            
            womenCategoricalPValuesBonferroni[,i+1] = p.adjust(womenCategoricalPValues[,i+1], method="bonferroni") 
            womenCategoricalPValuesBenjamini[,i+1]  = p.adjust(womenCategoricalPValues[,i+1], method="fdr") 
            
        }
        
        # Now we converted to asterisks for easy reading
        for(i in 1:totalBiomarkers){
            
            for(j in 1:length(allCategoricalIndexes)){
                
                # Get the indexes for the biomarker and the variable
                currentBiomarkerIndex = allNDLIndex[i]
                currentVariableIndex  = allCategoricalIndexes[j]
                
                # We skip the j=1 for the pvalues because that is the sex variable
                # and is only men and women
                if(j>1){
                    
                    menCategoricalPAsteriskBonferroni[j,(i+1)]   = getAsterkisPValue(menCategoricalPValuesBonferroni[j,(i+1)])
                    menCategoricalPAsteriskBenjamini[j,(i+1)]    = getAsterkisPValue(menCategoricalPValuesBenjamini[j,(i+1)])
                    
                    womenCategoricalPAsteriskBonferroni[j,(i+1)] = getAsterkisPValue(womenCategoricalPValuesBonferroni[j,(i+1)])
                    womenCategoricalPAsteriskBenjamini[j,(i+1)]  = getAsterkisPValue(womenCategoricalPValuesBenjamini[j,(i+1)])
                    
                    
                }
                
            }
            
        }
          
    }
    
    # All the analysis is finish now, we are going to try to make the tables
    # a bit more comprehensible for here on.
    {
    
        # First we are going to check for each cell, if there is a significance
        # in either in men or women, and label it TRUE or FALSE accordingly
        anySignificancePvalues           = menCategoricalPValues
        anySignificancePvaluesBonferroni = menCategoricalPValues
        anySignificancePvaluesBenjamini  = menCategoricalPValues
        
        for(i in 1:totalBiomarkers){
            
            for(j in 1:length(allCategoricalIndexes)){
                
                # Skip the sex line that is set to FALSE
                if(j>1){
                
                    if(   menCategoricalPAsteriskBenjamini[j,(i+1)] != 'ns' ||
                          womenCategoricalPAsteriskBenjamini[j,(i+1)] != 'ns' ){
                        
                        anySignificancePvaluesBenjamini[j,(i+1)] = TRUE    
                        
                    }
                    else{
                        
                        anySignificancePvaluesBenjamini[j,(i+1)] = FALSE
                        
                    }
                        
                }
                else{
                
                    anySignificancePvaluesBenjamini[j,(i+1)] = FALSE    
                    
                }
                
                
            }
            
        }
        
        # Now we mark which columns and rows need to be deleted to simplify the
        # table:
        deleteTheseBonferroniRows    = rep(FALSE, length(allCategoricalIndexes))
        deleteTheseBonferroniColumns = rep(FALSE, totalBiomarkers+1)
        
        deleteTheseBenjaminiRows     = rep(FALSE, length(allCategoricalIndexes))
        deleteTheseBenjaminiColumns  = rep(FALSE, totalBiomarkers+1)
        
        # Delete rows
        for(i in 1:length(deleteTheseBenjaminiRows)){
            
            if(sum(anySignificancePvaluesBenjamini[i,2:totalBiomarkers]) == 0 ){
            
                deleteTheseBenjaminiRows[i] = TRUE
                    
            }

        }
        
        # Delete columns
        for(i in 1:totalBiomarkers){
            
            if(sum(anySignificancePvaluesBenjamini[,i+1]) == 0 ){
                
                deleteTheseBenjaminiColumns[i+1] = TRUE
                
            }
            
        }
        
        # Now we have everything that need to be deleted. So we are going to do
        # that, and create a mixed table with men and women for easy looking.
        {
        
            menBenjaminiSimplified = menCategoricalPAsteriskBenjamini    
            menBenjaminiSimplified = menBenjaminiSimplified[!deleteTheseBenjaminiRows,
                                                            !deleteTheseBenjaminiColumns]
            
            womenBenjaminiSimplified = womenCategoricalPAsteriskBenjamini    
            womenBenjaminiSimplified = womenBenjaminiSimplified[!deleteTheseBenjaminiRows,
                                                                !deleteTheseBenjaminiColumns]
            
        }
        
        # Finally, we can also offer the complete list of significances
        # which is just a melting of the previous tables deleting all the ns
        {

            # All tables have the same structure, for both men and women
            # and whether you do simple, bonferroni or Benjamini addjustment
                        
            menSimpleMelted       = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            menBonferroniMelted   = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            menBenjaminiMelted    = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            womenSimpleMelted     = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            womenBonferroniMelted = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            womenBenjaminiMelted  = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            
            colnames(menSimpleMelted)        = c("Explicative", "Biomarker", "Significance")
            colnames(menBonferroniMelted)    = c("Explicative", "Biomarker", "Significance")
            colnames(menBenjaminiMelted)     = c("Explicative", "Biomarker", "Significance")
            colnames(womenSimpleMelted)      = c("Explicative", "Biomarker", "Significance")
            colnames(womenBonferroniMelted)  = c("Explicative", "Biomarker", "Significance")
            colnames(womenBenjaminiMelted)   = c("Explicative", "Biomarker", "Significance")
            
            # For every table, do the exactly same thing
            counter = 1
            for(i in 1:nrow(menCategoricalPAsteriskBonferroni)){
            
                for(j in 1:(ncol(menCategoricalPAsteriskBonferroni)-1)){
                    
                    currentExplicative = menCategoricalPAsteriskBonferroni[i,1]
                    currentBiomarker   = colnames(menCategoricalPAsteriskBonferroni)[j+1]
                    
                    # Men
                    menSimpleMelted[counter,1]     = currentExplicative
                    menSimpleMelted[counter,2]     = currentBiomarker
                    menSimpleMelted[counter,3]     = menCategoricalPAsterisk[i,j+1]
                    
                    menBonferroniMelted[counter,1] = currentExplicative
                    menBonferroniMelted[counter,2] = currentBiomarker
                    menBonferroniMelted[counter,3] = menCategoricalPAsteriskBonferroni[i,j+1]
                    
                    menBenjaminiMelted[counter,1]  = currentExplicative
                    menBenjaminiMelted[counter,2]  = currentBiomarker
                    menBenjaminiMelted[counter,3]  = menCategoricalPAsteriskBenjamini[i,j+1]
                    
                    
                    # Women
                    womenSimpleMelted[counter,1]     = currentExplicative
                    womenSimpleMelted[counter,2]     = currentBiomarker
                    womenSimpleMelted[counter,3]     = womenCategoricalPAsterisk[i,j+1]
                    
                    womenBonferroniMelted[counter,1] = currentExplicative
                    womenBonferroniMelted[counter,2] = currentBiomarker
                    womenBonferroniMelted[counter,3] = womenCategoricalPAsteriskBonferroni[i,j+1]
                    
                    womenBenjaminiMelted[counter,1]  = currentExplicative
                    womenBenjaminiMelted[counter,2]  = currentBiomarker
                    womenBenjaminiMelted[counter,3]  = womenCategoricalPAsteriskBenjamini[i,j+1]
                    
                    # Next
                    counter = counter + 1
                }
                    
            }
           
            # Now that we have the melted results, delete the NS from the melted tables
            menSimpleMelted       = menSimpleMelted[       menSimpleMelted$Significance       != 'ns' & !is.na(menSimpleMelted$Significance),       ]
            menBonferroniMelted   = menBonferroniMelted[   menBonferroniMelted$Significance   != 'ns' & !is.na(menBonferroniMelted$Significance),   ]
            menBenjaminiMelted    = menBenjaminiMelted[    menBenjaminiMelted$Significance    != 'ns' & !is.na(menBenjaminiMelted$Significance),    ]
            womenSimpleMelted     = womenSimpleMelted[     womenSimpleMelted$Significance     != 'ns' & !is.na(womenSimpleMelted$Significance),     ]
            womenBonferroniMelted = womenBonferroniMelted[ womenBonferroniMelted$Significance != 'ns' & !is.na(womenBonferroniMelted$Significance), ]
            womenBenjaminiMelted  = womenBenjaminiMelted[  womenBenjaminiMelted$Significance  != 'ns' & !is.na(womenBenjaminiMelted$Significance),  ]
            
            # Reset the row names off all tables because R is a stupid language
            # and I hate the lack of pointers and references. So instead of
            # making an easy and simple code in C++ we need to do this hackerish
            # code tricks in this third tier language which problem has been
            # solved since the 70's
            #
            # This doesn't even have a operation of myDataframe.resetRows() :(
            row.names(menSimpleMelted)       = c(1:nrow(menSimpleMelted))
            row.names(menBonferroniMelted)   = c(1:nrow(menBonferroniMelted))
            row.names(menBenjaminiMelted)    = c(1:nrow(menBenjaminiMelted))
            row.names(womenSimpleMelted)     = c(1:nrow(womenSimpleMelted))
            row.names(womenBonferroniMelted) = c(1:nrow(womenBonferroniMelted))
            row.names(womenBenjaminiMelted)  = c(1:nrow(womenBenjaminiMelted))            
            
            # Finally, what we are going to do is to expand the list of modalities
            # of each variable under the biomarker protein, and add the averages value
            # This is already done in the megatable with the averages. The only
            # thing that is a pain is that R is very annoying adding rows to DF, so
            # this takes a while in term of memory allocation
            
            # We are going to merge both tables into one
            {
             
                bothSimpleMelted       = menSimpleMelted
                bothBonferroniMelted   = menBonferroniMelted
                bothBenjaminiMelted    = menBenjaminiMelted
                
                bothSimpleMelted$X     = NA
                bothBonferroniMelted$X = NA
                bothBenjaminiMelted$X  = NA

                colnames(bothSimpleMelted)     = c("Variable","Biomarker","Men","Women")
                colnames(bothBonferroniMelted) = c("Variable","Biomarker","Men","Women")
                colnames(bothBenjaminiMelted)  = c("Variable","Biomarker","Men","Women")
                
                # In here we have initialize the table for men, and we need to add
                # the information for women.
                #
                # So from all the column in the women table, we are going to mark
                # each row and check if the row is already here. If it is, we just
                # add the result, if it is not, we mark it to added later, and set
                # men as 'ns'
                processedWomenRowsPSimple     = rep(FALSE, nrow(womenSimpleMelted))
                processedWomenRowsPBonferroni = rep(FALSE, nrow(womenBonferroniMelted))
                processedWomenRowsPBenjamini  = rep(FALSE, nrow(womenBenjaminiMelted))
                
    
                # SIMPLE
                
                # For each row in the men table (here)
                for(i in 1:nrow(bothSimpleMelted)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = bothSimpleMelted[i,1]
                    currentBiomarkerName = bothSimpleMelted[i,2]
                    
                    # Is this in the woman table too?
                    womenMatch = womenSimpleMelted[(womenSimpleMelted[,1] == currentVariableName &
                                                    womenSimpleMelted[,2] == currentBiomarkerName),]
                    
                    # YES: Copy the significant of women here, and mark the row as copied
                    if(nrow(womenMatch) != 0){
                        
                        processedWomenRowsPSimple[as.numeric(row.names(womenMatch)[1])] = TRUE
                        bothSimpleMelted[i,4] = womenMatch[1,3]
                        
                        if(nrow(womenMatch) > 1){
                            
                            print("ERROR ERROR ERROR")
                            print(womenMatch)
                            
                        }
                        
                    }
                    # NO:  Set woman to NS
                    else{
                        
                        bothSimpleMelted[i,4] = 'ns'
                        
                    }
                    
                }
                #
                # For each row in the women table that is not copied yet
                # Dump everything into the men table (here) and set all men to NS
                remainingWomen = womenSimpleMelted[!processedWomenRowsPSimple,]
                for(i in 1:nrow(remainingWomen)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = remainingWomen[i,1]
                    currentBiomarkerName = remainingWomen[i,2]
                    currentSignificance  = remainingWomen[i,3]
                    newRow  = c(currentVariableName,currentBiomarkerName,'ns',currentSignificance)
                    
                    bothSimpleMelted = rbind(bothSimpleMelted, newRow)  
                    
                }
                
                
                # BONFERRONI
                
                # For each row in the men table (here)
                for(i in 1:nrow(bothBonferroniMelted)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = bothBonferroniMelted[i,1]
                    currentBiomarkerName = bothBonferroniMelted[i,2]
                    
                    # Is this in the woman table too?
                    womenMatch = womenBonferroniMelted[(womenBonferroniMelted[,1] == currentVariableName &
                                                        womenBonferroniMelted[,2] == currentBiomarkerName),]
                    
                    # YES: Copy the significant of women here, and mark the row as copied
                    if(nrow(womenMatch) != 0){
                        
                        processedWomenRowsPBonferroni[as.numeric(row.names(womenMatch)[1])] = TRUE
                        bothBonferroniMelted[i,4] = womenMatch[1,3]
                        
                        if(nrow(womenMatch) > 1){
                            
                            print("ERROR ERROR ERROR")
                            print(womenMatch)
                            
                        }
                        
                    }
                    # NO:  Set woman to NS
                    else{
                        
                        bothBonferroniMelted[i,4] = 'ns'
                        
                    }
                    
                }
                #
                # For each row in the women table that is not copied yet
                # Dump everything into the men table (here) and set all men to NS
                remainingWomen = womenBonferroniMelted[!processedWomenRowsPBonferroni,]
                for(i in 1:nrow(remainingWomen)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = remainingWomen[i,1]
                    currentBiomarkerName = remainingWomen[i,2]
                    currentSignificance  = remainingWomen[i,3]
                    newRow  = c(currentVariableName,currentBiomarkerName,'ns',currentSignificance)
                    
                    bothBonferroniMelted = rbind(bothBonferroniMelted, newRow)  
                     
                }
                    
                # BENJAMINI
                
                # For each row in the men table (here)
                for(i in 1:nrow(bothBenjaminiMelted)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = bothBenjaminiMelted[i,1]
                    currentBiomarkerName = bothBenjaminiMelted[i,2]
                    
                    # Is this in the woman table too?
                    womenMatch = womenBenjaminiMelted[(womenBenjaminiMelted[,1] == currentVariableName &
                                                       womenBenjaminiMelted[,2] == currentBiomarkerName),]
                    
                    # YES: Copy the significant of women here, and mark the row as copied
                    if(nrow(womenMatch) != 0){
                        
                        processedWomenRowsPBenjamini[as.numeric(row.names(womenMatch)[1])] = TRUE
                        bothBenjaminiMelted[i,4] = womenMatch[1,3]
                        
                        if(nrow(womenMatch) > 1){
                            
                            print("ERROR ERROR ERROR")
                            print(womenMatch)
                            
                        }
                        
                    }
                    # NO:  Set woman to NS
                    else{
                        
                        bothBenjaminiMelted[i,4] = 'ns'
                        
                    }
                    
                }
                #
                # For each row in the women table that is not copied yet
                # Dump everything into the men table (here) and set all men to NS
                remainingWomen = womenBenjaminiMelted[!processedWomenRowsPBenjamini,]
                for(i in 1:nrow(remainingWomen)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = remainingWomen[i,1]
                    currentBiomarkerName = remainingWomen[i,2]
                    currentSignificance  = remainingWomen[i,3]
                    newRow  = c(currentVariableName,currentBiomarkerName,'ns',currentSignificance)
                    
                    bothBenjaminiMelted = rbind(bothBenjaminiMelted, newRow)  
                    
                }
                
                
            }
            
            # Everything is merged, and these results are a good report on its
            # own, so save it to disk
            write.csv2(bothSimpleMelted,     BOTH_P_SIMPLE)
            write.csv2(bothBenjaminiMelted,  BOTH_P_BENJAMINI)
            write.csv2(bothBonferroniMelted, BOTH_P_BONFERRONI)
            
            
            # However, now I don't want to have to look back and forth from
            # the big super average table. So we are going to copy the relevant
            # results into here.
            
            
            
            # For each row in each table
            # Add an extra column for the opposite sex significance
            currentIndex = 1
            for(i in 1:nrow(bothBonferroniMelted)){
            
                # Grab the variables where the pointer is
                currentVariableName   = bothBonferroniMelted[currentIndex,1]
                currentBiomarkerName  = bothBonferroniMelted[currentIndex,2]
                
                # Find these variable index in the big table
                currentVariableIndex  = getIndex(currentVariableName,  completeTable)    
                currentBiomarkerIndex = getIndex(currentBiomarkerName, biomarkersMetadataDF$Protein)    
                currentBiomarkerIndex = NDLIndex + currentBiomarkerIndex - 1
                
                # Find how many modalities this variable have
                currentModalities      = getModalities(completeTable, currentVariableIndex, skipUnknowns = TRUE)
                totalCurrentModalities = length(currentModalities)
                
                # To continue...
            }
            
            
            # Write the tables into disk
            write.csv2(menSimpleMelted,       MEN_P_SIMPLE)
            write.csv2(menBonferroniMelted,   MEN_P_BONFERRONI)
            write.csv2(menBenjaminiMelted,    MEN_P_BENJAMINI)
            write.csv2(womenSimpleMelted,     WOMEN_P_SIMPLE)
            write.csv2(womenBonferroniMelted, WOMEN_P_BONFERRONI)
            write.csv2(womenBenjaminiMelted,  WOMEN_P_BENJAMINI)
             
        }
        
    } 
    
    # Write tables into Latex form
    {
     
        # Sort by variable first
        bothSimpleMelted2 = bothSimpleMelted[order(bothSimpleMelted$Variable), ]
        rownames(bothSimpleMelted2) = c(1:nrow(bothSimpleMelted2))
        
        # Break the superlong table into 5 pieces
        # There is manual addjustment so it doesn't break in the middle of
        # a variable series
        FirstBothSimpleMelted   = bothSimpleMelted2[1:64,]
        SecondBothSimpleMelted  = bothSimpleMelted2[65:109,]
        ThirdBothSimpleMelted   = bothSimpleMelted2[110:149,]
        ForthBothSimpleMelted   = bothSimpleMelted2[150:205,]
        FivthBothSimpleMelted   = bothSimpleMelted2[206:253,]
        SixthBothSimpleMelted   = bothSimpleMelted2[254:297,]
        SeventhBothSimpleMelted = bothSimpleMelted2[298:343,]
        EigthBothSimpleMelted   = bothSimpleMelted2[344:386,]
        #NinthBothSimpleMelted   = bothSimpleMelted2[351:386,]

        writeTableLATEX(FirstBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (1 of 8)",
                        overrideTableName = "BiomarkersBothPSimple1", heightProportion = 0.4)        
        
        writeTableLATEX(SecondBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (2 of 8)",
                        overrideTableName = "BiomarkersBothPSimple2", heightProportion = 0.4)        
        
        writeTableLATEX(ThirdBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (3 of 8)",
                        overrideTableName = "BiomarkersBothPSimple3", heightProportion = 0.4)        
        
        writeTableLATEX(ForthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (4 of 8)",
                        overrideTableName = "BiomarkersBothPSimple4", heightProportion = 0.4)        
                
        writeTableLATEX(FivthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (5 of 8)",
                        overrideTableName = "BiomarkersBothPSimple5", heightProportion = 0.4)
        
        writeTableLATEX(SixthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (6 of 8)",
                        overrideTableName = "BiomarkersBothPSimple6", heightProportion = 0.4)
        
        writeTableLATEX(SeventhBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (7 of 8)",
                        overrideTableName = "BiomarkersBothPSimple7", heightProportion = 0.4)
        
        writeTableLATEX(EigthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (8 of 8)",
                        overrideTableName = "BiomarkersBothPSimple8", heightProportion = 0.4)        
        
        # Benjamini and Bonferroni
        
        writeTableLATEX(bothBenjaminiMelted, BIOMARKERS_FOLDER_TABLES,  tableCaption = "Biomarkers that are statistically significant for either men or women, after Benjamini correction",
                        overrideTableName = "BiomarkersBothPBenjamini", heightProportion = 0.4)
        
        writeTableLATEX(bothBonferroniMelted, BIOMARKERS_FOLDER_TABLES, tableCaption = "Biomarkers that are statistically significant for either men or women, after Bonferroni correction",
                        overrideTableName = "BiomarkersBothPBonferroni", heightProportion = 0.4)

    }
    
}

#++---------------------------------------------------------------------------
# Do biomarkers levels for every numerical (Antropometrics and Blood)
#---------------------------------------------------------------------------
{
    
    # Let start with the antropometry table
    menResultsBloodDF           = DF(totalBiomarkers, (totalAntropometries+1))
    colnames(menResultsBloodDF) = c("Protein",antropometricSummaryDF$Concept)
    menResultsBloodDF$Protein   = biomarkersMetadataDF$Protein
    womenResultsBloodDF         = menResultsBloodDF
    
    # For men and women
    for(i in 1:2){
        
        # Select which table are we using
        {
            currentSubtable = menOnlyTable
            currentSex      = "Man"
            if(i == 2){
                currentSex  = "Woman"
                currentSubtable = womenOnlyTable
            }
        }
        
        # For each of the biomarkers
        for(k in 1:totalBiomarkers){
            
            currentBiomarkerIndex = allNDLIndex[k]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
     
            # For each of the antropometric variables
            for(j in 1:totalAntropometries){
                
                currentAntropometricIndex = firstBloodIndex + j - 1
                currentAntropometricName  = antropometricSummaryDF$Concept[j]
                
                # Clean the data from NA data
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentAntropometricIndex]),]
                
                # Run the model
                # Plot the graph
                currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and ", currentAntropometricName)
                currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_",     currentAntropometricName)
                
                myAntropomode        = doSimpleRegression(currentSubtable, currentAntropometricIndex, currentBiomarkerIndex, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
                                                          plotTitle = currentPlotTitle,
                                                          overrideTableName = currentTableOverride)

                if(i == 1) menResultsBloodDF[k,(j+1)]   = glance(myAntropomode)$p.value[[1]]
                else       womenResultsBloodDF[k,(j+1)] = glance(myAntropomode)$p.value[[1]]
                
                
            }
                   
        }
        
    }
            
    # All results are finish, addjust by Benjamini and Bonferroni
    # And create the easy to read asterisk matrix for men and women
    {
     
        menResultsBloodBonferroniDF         = menResultsBloodDF
        menResultsBloodBenjaminiDF          = menResultsBloodDF
        menAsteriskResultsBloodDF           = menResultsBloodDF
        menAsteriskResultsBloodBonferroniDF = menResultsBloodDF
        menAsteriskResultsBloodBenjaminiDF  = menResultsBloodDF
        
        womenResultsBloodBonferroniDF         = womenResultsBloodDF
        womenResultsBloodBenjaminiDF          = womenResultsBloodDF
        womenAsteriskResultsBloodDF           = womenResultsBloodDF
        womenAsteriskResultsBloodBonferroniDF = womenResultsBloodDF
        womenAsteriskResultsBloodBenjaminiDF  = womenResultsBloodDF    
    
        for(j in 1:totalAntropometries){
    
            menResultsBloodBenjaminiDF[,j+1]  = p.adjust(menResultsBloodBenjaminiDF[,j+1],  method = "fdr")  
            menResultsBloodBonferroniDF[,j+1] = p.adjust(menResultsBloodBonferroniDF[,j+1], method = "bonferroni") 
        
            menAsteriskResultsBloodDF[,j+1]           = getAsterkisPValue(menResultsBloodDF[,j+1])
            menAsteriskResultsBloodBenjaminiDF[,j+1]  = getAsterkisPValue(menResultsBloodBenjaminiDF[,j+1])
            menAsteriskResultsBloodBonferroniDF[,j+1] = getAsterkisPValue(menResultsBloodBonferroniDF[,j+1])
                    
            womenResultsBloodBenjaminiDF[,j+1]  = p.adjust(womenResultsBloodBenjaminiDF[,j+1],  method = "fdr")  
            womenResultsBloodBonferroniDF[,j+1] = p.adjust(womenResultsBloodBonferroniDF[,j+1], method = "bonferroni") 
        
            womenAsteriskResultsBloodDF[,j+1]           = getAsterkisPValue(womenResultsBloodDF[,j+1])
            womenAsteriskResultsBloodBenjaminiDF[,j+1]  = getAsterkisPValue(womenResultsBloodBenjaminiDF[,j+1])
            womenAsteriskResultsBloodBonferroniDF[,j+1] = getAsterkisPValue(womenResultsBloodBonferroniDF[,j+1])
            
        }

        # Clean the matrixes from ns for all rows (no column
        
        # For each row
        deleteTheseSimpleRows     = rep(FALSE,totalBiomarkers)
        deleteTheseBenjaminiRows  = rep(FALSE,totalBiomarkers)
        deleteTheseBonferroniRows = rep(FALSE,totalBiomarkers)
        for(i in 1:totalBiomarkers){
            
            if(sum(menAsteriskResultsBloodDF[i,] == 'ns')           == 8) deleteTheseSimpleRows[i]     = TRUE
            if(sum(menAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == 8) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(menAsteriskResultsBloodBonferroniDF[i,] == 'ns') == 8) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        menAsteriskResultsBloodDF           = menAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        menAsteriskResultsBloodBenjaminiDF  = menAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        menAsteriskResultsBloodBonferroniDF = menAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
        
        deleteTheseSimpleRows     = rep(FALSE,totalBiomarkers)
        deleteTheseBenjaminiRows  = rep(FALSE,totalBiomarkers)
        deleteTheseBonferroniRows = rep(FALSE,totalBiomarkers)
        for(i in 1:totalBiomarkers){
            
            if(sum(womenAsteriskResultsBloodDF[i,] == 'ns')           == 8) deleteTheseSimpleRows[i]     = TRUE
            if(sum(womenAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == 8) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(womenAsteriskResultsBloodBonferroniDF[i,] == 'ns') == 8) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        womenAsteriskResultsBloodDF           = womenAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        womenAsteriskResultsBloodBenjaminiDF  = womenAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        womenAsteriskResultsBloodBonferroniDF = womenAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
           
    }
    
    # Write to latex
    writeTableLATEX(menAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in men, after applying Bonferroni correction",
                    overrideTableName = "BiomarkersBloodBonferroniMen", heightProportion = 0.2)
    
    writeTableLATEX(womenAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in women, after applying Bonferroni correction",
                    overrideTableName = "BiomarkersBloodBonferroniWomen", heightProportion = 0.2)
            
  
    
    # Now let do the blood, which is the same but with more variables
    menResultsBloodDF           = DF(totalBiomarkers, (totalBloodColumns+1))
    colnames(menResultsBloodDF) = c("Protein",bloodMetadataLatexDF$Short)
    menResultsBloodDF$Protein   = biomarkersMetadataDF$Protein
    womenResultsBloodDF         = menResultsBloodDF 
    
    # For men and women
    for(i in 1:2){
        
        # Select which table are we using
        {
            currentSubtable = menOnlyTable
            currentSex      = "Man"
            if(i == 2){
                currentSex  = "Woman"
                currentSubtable = womenOnlyTable
            }
        }
        
        # For each of the biomarkers
        for(k in 1:totalBiomarkers){
            
            currentBiomarkerIndex = allNDLIndex[k]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
     
            # For each of the antropometric variables
            for(j in 1:totalBloodColumns){
                
                currentAntropometricIndex = firstBloodIndex + j - 1
                currentAntropometricName  = bloodMetadataLatexDF$Short[j]
                
                # Clean the data from NA data
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentAntropometricIndex]),]
                
                # Run the model
                # Plot the graph
                currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and ", currentAntropometricName)
                currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_",     currentAntropometricName)
                
                myAntropomode        = doSimpleRegression(currentSubtable, currentAntropometricIndex, currentBiomarkerIndex, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                                          plotTitle = currentPlotTitle,
                                                          overrideTableName = currentTableOverride)

                if(i == 1) menResultsBloodDF[k,(j+1)]   = glance(myAntropomode)$p.value[[1]]
                else       womenResultsBloodDF[k,(j+1)] = glance(myAntropomode)$p.value[[1]]
                
                
            }
                   
        }
        
    }
    
    
    
    
    # All results are finish, addjust by Benjamini and Bonferroni
    # And create the easy to read asterisk matrix for men and women
    {
     
        menResultsBloodBonferroniDF         = menResultsBloodDF
        menResultsBloodBenjaminiDF          = menResultsBloodDF
        menAsteriskResultsBloodDF           = menResultsBloodDF
        menAsteriskResultsBloodBonferroniDF = menResultsBloodDF
        menAsteriskResultsBloodBenjaminiDF  = menResultsBloodDF
        
        womenResultsBloodBonferroniDF         = womenResultsBloodDF
        womenResultsBloodBenjaminiDF          = womenResultsBloodDF
        womenAsteriskResultsBloodDF           = womenResultsBloodDF
        womenAsteriskResultsBloodBonferroniDF = womenResultsBloodDF
        womenAsteriskResultsBloodBenjaminiDF  = womenResultsBloodDF    
    
        for(j in 1:totalBloodColumns){
    
            menResultsBloodBenjaminiDF[,j+1]  = p.adjust(menResultsBloodBenjaminiDF[,j+1],  method = "fdr")  
            menResultsBloodBonferroniDF[,j+1] = p.adjust(menResultsBloodBonferroniDF[,j+1], method = "bonferroni") 
        
            menAsteriskResultsBloodDF[,j+1]           = getAsterkisPValue(menResultsBloodDF[,j+1])
            menAsteriskResultsBloodBenjaminiDF[,j+1]  = getAsterkisPValue(menResultsBloodBenjaminiDF[,j+1])
            menAsteriskResultsBloodBonferroniDF[,j+1] = getAsterkisPValue(menResultsBloodBonferroniDF[,j+1])
                    
            womenResultsBloodBenjaminiDF[,j+1]  = p.adjust(womenResultsBloodBenjaminiDF[,j+1],  method = "fdr")  
            womenResultsBloodBonferroniDF[,j+1] = p.adjust(womenResultsBloodBonferroniDF[,j+1], method = "bonferroni") 
        
            womenAsteriskResultsBloodDF[,j+1]           = getAsterkisPValue(womenResultsBloodDF[,j+1])
            womenAsteriskResultsBloodBenjaminiDF[,j+1]  = getAsterkisPValue(womenResultsBloodBenjaminiDF[,j+1])
            womenAsteriskResultsBloodBonferroniDF[,j+1] = getAsterkisPValue(womenResultsBloodBonferroniDF[,j+1])
            
        }

        # Clean the matrixes from ns for all rows (no column
        
        # For each row
        deleteTheseSimpleRows     = rep(FALSE,totalBiomarkers)
        deleteTheseBenjaminiRows  = rep(FALSE,totalBiomarkers)
        deleteTheseBonferroniRows = rep(FALSE,totalBiomarkers)
        for(i in 1:totalBiomarkers){
            
            if(sum(menAsteriskResultsBloodDF[i,] == 'ns')           == totalBloodColumns) deleteTheseSimpleRows[i]     = TRUE
            if(sum(menAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == totalBloodColumns) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(menAsteriskResultsBloodBonferroniDF[i,] == 'ns') == totalBloodColumns) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        menAsteriskResultsBloodDF           = menAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        menAsteriskResultsBloodBenjaminiDF  = menAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        menAsteriskResultsBloodBonferroniDF = menAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
        
        deleteTheseSimpleRows     = rep(FALSE,totalBiomarkers)
        deleteTheseBenjaminiRows  = rep(FALSE,totalBiomarkers)
        deleteTheseBonferroniRows = rep(FALSE,totalBiomarkers)
        for(i in 1:totalBiomarkers){
            
            if(sum(womenAsteriskResultsBloodDF[i,] == 'ns')           == totalBloodColumns) deleteTheseSimpleRows[i]     = TRUE
            if(sum(womenAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == totalBloodColumns) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(womenAsteriskResultsBloodBonferroniDF[i,] == 'ns') == totalBloodColumns) deleteTheseBonferroniRows[i] = TRUE
            
        }
        
        womenAsteriskResultsBloodDF           = womenAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        womenAsteriskResultsBloodBenjaminiDF  = womenAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        womenAsteriskResultsBloodBonferroniDF = womenAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
        
        
        # For each column
        deleteTheseSimpleColumns     = rep(FALSE,totalBloodColumns+1)
        deleteTheseBenjaminiColumns  = rep(FALSE,totalBloodColumns+1)
        deleteTheseBonferroniColumns = rep(FALSE,totalBloodColumns+1)
        for(i in 1:totalBloodColumns){
            
            if(sum(menAsteriskResultsBloodDF[,(i+1)] == 'ns')           == nrow(menAsteriskResultsBloodDF))           deleteTheseSimpleColumns[i+1]     = TRUE
            if(sum(menAsteriskResultsBloodBenjaminiDF[,(i+1)]  == 'ns') == nrow(menAsteriskResultsBloodBenjaminiDF))  deleteTheseBenjaminiColumns[i+1]  = TRUE
            if(sum(menAsteriskResultsBloodBonferroniDF[,(i+1)] == 'ns') == nrow(menAsteriskResultsBloodBonferroniDF)) deleteTheseBonferroniColumns[i+1] = TRUE
            
        }
                
        menAsteriskResultsBloodDF           = menAsteriskResultsBloodDF[,!deleteTheseSimpleColumns]
        menAsteriskResultsBloodBenjaminiDF  = menAsteriskResultsBloodBenjaminiDF[,!deleteTheseBenjaminiColumns]
        menAsteriskResultsBloodBonferroniDF = menAsteriskResultsBloodBonferroniDF[,!deleteTheseBonferroniColumns]
        
        deleteTheseSimpleColumns     = rep(FALSE,totalBloodColumns+1)
        deleteTheseBenjaminiColumns  = rep(FALSE,totalBloodColumns+1)
        deleteTheseBonferroniColumns = rep(FALSE,totalBloodColumns+1)
        for(i in 1:totalBloodColumns){
            
            if(sum(womenAsteriskResultsBloodDF[,(i+1)] == 'ns')           == nrow(womenAsteriskResultsBloodDF))           deleteTheseSimpleColumns[i+1]     = TRUE
            if(sum(womenAsteriskResultsBloodBenjaminiDF[,(i+1)]  == 'ns') == nrow(womenAsteriskResultsBloodBenjaminiDF))  deleteTheseBenjaminiColumns[i+1]  = TRUE
            if(sum(womenAsteriskResultsBloodBonferroniDF[,(i+1)] == 'ns') == nrow(womenAsteriskResultsBloodBonferroniDF)) deleteTheseBonferroniColumns[i+1] = TRUE
            
        }
                
        womenAsteriskResultsBloodDF           = womenAsteriskResultsBloodDF[,!deleteTheseSimpleColumns]
        womenAsteriskResultsBloodBenjaminiDF  = womenAsteriskResultsBloodBenjaminiDF[,!deleteTheseBenjaminiColumns]
        womenAsteriskResultsBloodBonferroniDF = womenAsteriskResultsBloodBonferroniDF[,!deleteTheseBonferroniColumns]
                

           
    }
    
    
    # Change all ns to nothing for easy reading
    menAsteriskResultsBloodBonferroniDF[menAsteriskResultsBloodBonferroniDF     == "ns"] = ""
    womenAsteriskResultsBloodBonferroniDF[womenAsteriskResultsBloodBonferroniDF == "ns"] = ""
    
    # Write to latex
    writeTableLATEX(menAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the blood variables in men, after applying Bonferroni correction. Non-significant values appears as a white space for easy reading.",
                    overrideTableName = "BiomarkersBloodBonferroniMen", heightProportion = 0.1, rotateColumnHeaders = TRUE)
    
    writeTableLATEX(womenAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the blood variables in women, after applying Bonferroni correction. Non-significant values appears as a white space for easy reading.",
                    overrideTableName = "BiomarkersBloodBonferroniWomen", heightProportion = 0.1, rotateColumnHeaders = TRUE)  
    
    
}

#++---------------------------------------------------------------------------
# Do biomarkers levels for every disease (men and women if they exist)
#---------------------------------------------------------------------------
{

    # Specify the cutoff limit for analizing diseases.
    # For example, if we have only two people with a diseases, that is too little
    # for a proper statistical analysis. If we have 100, that is a lot and enough.
    # Somewhere in between is the magic spot, so you set that threshold here:
    minimumDiseasesRequiered = 4
    
    # Divide the disease DB into men and women
    {
    
        totalDiseases   = nrow(diseasesMenDF)
        
        diseasesMenDF   = diseasesDBDF
        diseasesWomenDF = diseasesDBDF
        
        thisRowIsMan    = rep(FALSE, totalDiseases)
        
        # For each row
        for(i in 1:totalDiseases){
        
            # Is this ID a man or a woman?
            currentID  = diseasesMenDF$ID[i]
            currentSex = completeTable$Sex[currentID]
            
            if(currentSex == "Man") thisRowIsMan[i] = TRUE
            
        }
            
        diseasesMenDF   = diseasesMenDF[thisRowIsMan,]
        diseasesWomenDF = diseasesWomenDF[!thisRowIsMan,]
        
        # Put both into a list that we will use later to iterate
        diseaseDFs = newList(2)
        diseaseDFs[[1]] = diseasesMenDF
        diseaseDFs[[2]] = diseasesWomenDF
    }
    
    # Get the general statistics for every disease
    {

        
        # Summarizing tables for general diseases and specific diseases
        
        currentGeneralSummaryMen          = summarizeCategorical(diseasesMenDF, 4,   roundMe = 2)
        currentSpecializationSummaryMen   = summarizeCategorical(diseasesMenDF, 2,   roundMe = 2)
        currentGeneralSummaryWomen        = summarizeCategorical(diseasesWomenDF, 4, roundMe = 2)
        currentSpecializationSummaryWomen = summarizeCategorical(diseasesWomenDF, 2, roundMe = 2)            
        
        writeTableLATEX(currentGeneralSummaryMen,   BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for MEN by ICD10 group",
                        overrideTableName = "GeneralDiseasesMen", heightProportion = 0.2)
        
        writeTableLATEX(currentGeneralSummaryWomen, BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for WOMEN by ICD10 group",
                        overrideTableName = "GeneralDiseasesWomen", heightProportion = 0.2)
        
        writeTableLATEX(currentSpecializationSummaryMen,   BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for MEN by ICD10",
                        overrideTableName = "SpecificDiseasesMen", heightProportion = 0.5)
        
        writeTableLATEX(currentSpecializationSummaryWomen, BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for WOMEN by ICD10",
                        overrideTableName = "SpecificDiseasesWomen", heightProportion = 0.5)        
        
        
        # Histograms
        
        # Create a dataframe where we are going to write how many diseases each person has
        totalDiseasesDF = DF(totalPeople, 2, 0)
        colnames(totalDiseasesDF) = c("ID", "Diseases")
        totalDiseasesDF[,1] = completeTable$ID
        
        for(i in 1:nrow(diseasesDBDF)){
        
            # Get the id
            currentID = diseasesDBDF$ID[i]
            totalDiseasesDF[currentID,2] = totalDiseasesDF[currentID,2] + 1
            
        }
        
        
        
        
    }
    
    # Make a table of healthy men and healthy women for reference
    {
        sickMenIDs      = unique(diseasesMenDF$ID)
        sickWomenIDs    = unique(diseasesWomenDF$ID)
        healthyMenIDs   = menOnlyTable$ID[!(menOnlyTable$ID     %in% sickMenIDs)]
        healthyWomenIDs = womenOnlyTable$ID[!(womenOnlyTable$ID %in% sickWomenIDs)]
        
        healthyMenDF    = completeTable[completeTable$ID %in% healthyMenIDs,]
        healthyWomenDF  = completeTable[completeTable$ID %in% healthyWomenIDs,]
        
        menOnlyTable$CurrentHealth   = "Healthy"
        womenOnlyTable$CurrentHealth = "Healthy"
        
        for(i in 1:nrow(menOnlyTable)){
            
            currentID = menOnlyTable$ID[i]
            
            if(currentID %in% sickMenIDs) menOnlyTable$CurrentHealth[i] = "Sick"
            
        }
        
        for(i in 1:nrow(womenOnlyTable)){
            
            currentID = womenOnlyTable$ID[i]
            
            if(currentID %in% sickWomenIDs) womenOnlyTable$CurrentHealth[i] = "Sick"
            
        }
        
    }
    
    # Find out how many valid diseases we have for each sex
    # Prepare a summary DF accordingly
    {
    
        validDiseasesMenDF      = currentSpecializationSummaryMen[currentSpecializationSummaryMen$Count >= minimumDiseasesRequiered,]
        menDiseasesList         = as.character(validDiseasesMenDF[,1])
        totalDiseasesMen        = length(menDiseasesList)
            
        validDiseasesWomenDF    = currentSpecializationSummaryWomen[currentSpecializationSummaryWomen$Count >= minimumDiseasesRequiered,]
        womenDiseasesList       = as.character(validDiseasesWomenDF[,1])
        totalDiseasesWomen      = length(womenDiseasesList)
        
        summaryDiseasesMenDF           = DF(totalBiomarkers, totalDiseasesMen+1)
        colnames(summaryDiseasesMenDF) = c("Protein", menDiseasesList)
        summaryDiseasesMenDF[,1]       = biomarkersMetadataDF$Protein
        
        summaryDiseasesWomenDF           = DF(totalBiomarkers, totalDiseasesWomen+1)
        colnames(summaryDiseasesWomenDF) = c("Protein", womenDiseasesList)        
        summaryDiseasesWomenDF[,1]       = biomarkersMetadataDF$Protein
    }
    
    
    # For those we have enough people, compare biomarkers with reference
    {
    
        # Make a DF that contain the template for the results.
        # This has, the biomarker name, the significance value with the 3 options
        # (no correction, benjamini, bonferroni), the average for the disease
        # group, and the average for the control group.
        #
        # The template for men and women is the same, and only the averages
        # and significance changes, so let create one for each.
        templateMenDF           = DF(totalBiomarkers, 7)
        colnames(templateMenDF) = c("Protein", "No correction", "Benjamini", "Bonferroni", "Avg Disease", "Avg Healthy", "Image")
        templateMenDF[,1]       = biomarkersMetadataDF$Protein
        templateWomenDF         = templateMenDF
        for(i in 1:totalBiomarkers){
        
            templateMenDF[i,6]   =  mean(healthyMenDF[,   (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)
            templateWomenDF[i,6] =  mean(healthyWomenDF[, (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)    
            
        }
        
        menResultsList   = newList(totalDiseasesMen)
        womenResultsList = newList(totalDiseasesWomen)
        
        
        
        # From here onward we do the analysis
        currentSex                 = "Men"
        currentDiseasesBySex       = diseasesMenDF
        currentSpecializationTable = currentSpecializationSummaryMen
        currentHealthyTable        = healthyMenDF
        currentSexTable            = menOnlyTable
        currentResultsList         = menResultsList
        currentTemplate            = templateMenDF
        currentMetaSummary         = summaryDiseasesMenDF
        
        # For each sex (men and women)
        for(s in 1:2){
            
            # Set the proper tables
            if (s == 2){
                
                currentSex                 = "Women" 
                currentDiseasesBySex       = diseasesWomenDF
                currentSpecializationTable = currentSpecializationSummaryWomen        
                currentHealthyTable        = healthyWomenDF
                currentSexTable            = womenOnlyTable
                currentResultsList         = womenResultsList
                currentTemplate            = templateWomenDF
                currentMetaSummary         = summaryDiseasesWomenDF
            } 

            # Get all the diseases that are above the given disease threshold
            validDiseasesDF      = currentSpecializationTable[currentSpecializationTable$Count >= minimumDiseasesRequiered,]
            currentDiseaseList   = as.character(validDiseasesDF[,1])
            totalCurrentDiseases = length(currentDiseaseList)
            
            # For each disease
            for(d in 1:totalCurrentDiseases){
                
                # Get the disease
                currentDisease = currentDiseaseList[d]
                
                # Get all the IDs for people that coincide with the current disease
                peopleWithThisDisease  = currentDiseasesBySex[currentDiseasesBySex$Diagnostic == currentDisease,]$ID
                totalPeopleWithDisease = length(peopleWithThisDisease)
                diseasePeopleDF        = completeTable[completeTable$ID %in% peopleWithThisDisease,]
                
                # Prepare the template
                diseaseSpecificTemplate = currentTemplate
                
                # For each biomarker    
                for(b in 1:totalBiomarkers){
                    
                    # Init the biomarker
                    currentBiomarkerName = biomarkersMetadataDF$Protein[b]
                    
                    # Prepare the subtable for this case
                    currentDataDF           = DF(nrow(currentHealthyTable) + totalPeopleWithDisease , 2)
                    colnames(currentDataDF) = c(currentBiomarkerName, "CurrentHealth")
                    currentDataDF[,2]       = c(rep("Healthy", nrow(currentHealthyTable)) , rep("Sick", totalPeopleWithDisease))
                    currentDataDF[,1]       = c(currentHealthyTable[, (firstBiomarkerIndex + b - 1)] , diseasePeopleDF[, (firstBiomarkerIndex + b - 1)])
                    
                    #print(head(currentDataDF))
                    #print(tail(currentDataDF))
                    
                    # Do the analysis
                    currentPValue                = simpleCategoricalPValue(currentDataDF, 2, 1,    skipUnknowns = TRUE)
                    diseaseSpecificTemplate[b,2] = currentPValue
                    diseaseSpecificTemplate[b,5] = mean(diseasePeopleDF[, (firstBiomarkerIndex + b - 1)], na.rm = TRUE)
                    
                }
                
                # The analysis is finish, do the benjamini and bonferroni corrections
                diseaseSpecificTemplate[,3] = p.adjust(diseaseSpecificTemplate[,2], method="fdr") 
                diseaseSpecificTemplate[,4] = p.adjust(diseaseSpecificTemplate[,2], method="bonferroni") 
                # Transform into asterisk
                diseaseSpecificTemplate[,2] = getAsterkisPValue(diseaseSpecificTemplate[,2], nsEmpty = TRUE)
                diseaseSpecificTemplate[,3] = getAsterkisPValue(diseaseSpecificTemplate[,3], nsEmpty = TRUE)
                diseaseSpecificTemplate[,4] = getAsterkisPValue(diseaseSpecificTemplate[,4], nsEmpty = TRUE)
                # Round the averages
                diseaseSpecificTemplate[,5] = round(diseaseSpecificTemplate[,5],2)
                diseaseSpecificTemplate[,6] = round(diseaseSpecificTemplate[,6],2)
                
                # Add the results to the metasummary
                currentMetaSummary[,(d+1)]  = diseaseSpecificTemplate[,4]
                
                
                # Write the table into disk
                
                currentDiseaseClean = cleanWeirdCharacters(currentDisease) #Make the disease name Latex compatible for the filename
                  
                currentCaption   = paste0( currentSex," table for biomarkers significance, disease ",currentDisease)
                currentTableName = paste0("DiseasesSummaryii",s,"ii",d) 
                
                latexString = writeTableLATEX(diseaseSpecificTemplate, BIOMARKERS_FOLDER_TABLES_DISEASES, tableCaption = currentCaption,
                                              overrideTableName = currentTableName, heightProportion = 0.5)
                
                
                
                latexString = paste0("input{../../../../results/biomarkers/tables/diseases/", latexString[[2]], ".tex}")
                
                print(latexString)
                
                # Write the table into the list
                currentResultsList[[d]] = diseaseSpecificTemplate
                
            }
            
            # Save the metasummary, because R is a stupid language and
            # passing variable as reference is so difficult these days -_-
            if (s == 1) summaryDiseasesMenDF   = currentMetaSummary
            if (s == 2) summaryDiseasesWomenDF = currentMetaSummary
            
            
        }
        
        

            
    }
        
    
    # Write the summaries into disk
    writeTableLATEX(summaryDiseasesMenDF, BIOMARKERS_FOLDER_TABLES_DISEASES, tableCaption = "Significant biomarkers for diseases and men, addjusted for Bonferroni",
                    overrideTableName = "MenDiseasesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    # Write the summaries into disk
    writeTableLATEX(summaryDiseasesWomenDF, BIOMARKERS_FOLDER_TABLES_DISEASES, tableCaption = "Significant biomarkers for diseases and women, addjusted for Bonferroni",
                    overrideTableName = "WomenDiseasesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    
}

#++---------------------------------------------------------------------------
# Do biomarkers levels for every medicine, with focus on hormonal
#---------------------------------------------------------------------------
{
    
    # Specify the cutoff limit for minimum amount of medicine taken
    # (same as with disease)
    minimumMedicineRequiered = 4
    
    # Divide the disease DB into men and women
    {
    
        totalMedicines  = nrow(medicinesDBDF)
        
        medicinesMenDF   = medicinesDBDF
        medicinesWomenDF = medicinesDBDF
        
        thisRowIsMan    = rep(FALSE, totalMedicines)
        
        # For each row
        for(i in 1:totalMedicines){
        
            # Is this ID a man or a woman?
            currentID  = medicinesMenDF$ID[i]
            currentSex = completeTable$Sex[currentID]
            
            if(currentSex == "Man") thisRowIsMan[i] = TRUE
            
        }
            
        medicinesMenDF   = medicinesMenDF[thisRowIsMan,]
        medicinesWomenDF = medicinesWomenDF[!thisRowIsMan,]
        
        # Put both into a list that we will use later to iterate
        medicinesDFs      = newList(2)
        medicinesDFs[[1]] = medicinesMenDF
        medicinesDFs[[2]] = medicinesWomenDF
    }
    
    # Get the general statistics for every disease
    {

        currentGeneralSummaryMen          = summarizeCategorical(medicinesMenDF, 2,   roundMe = 2)
        currentSpecializationSummaryMen   = summarizeCategorical(medicinesMenDF, 3,   roundMe = 2)
        currentGeneralSummaryWomen        = summarizeCategorical(medicinesWomenDF, 2, roundMe = 2)
        currentSpecializationSummaryWomen = summarizeCategorical(medicinesWomenDF, 3, roundMe = 2)            
        
        writeTableLATEX(currentGeneralSummaryMen,          BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for MEN by ATC group",
                        overrideTableName = "GeneralMedicineMen",    heightProportion = 0.1)
        
        writeTableLATEX(currentGeneralSummaryWomen,        BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for WOMEN by ATC group",
                        overrideTableName = "GeneralMedicineWomen",  heightProportion = 0.1)
        
        writeTableLATEX(currentSpecializationSummaryMen,   BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for MEN by brand",
                        overrideTableName = "SpecificMedicineMen",   widthProportion = 0.5, heightProportion = 0.5)
        
        writeTableLATEX(currentSpecializationSummaryWomen, BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for WOMEN by brand",
                        overrideTableName = "SpecificMedicineWomen", widthProportion = 0.5, heightProportion = 0.5)        
        
        
        
    }
    
    # Make a table of medicine-free men and women for reference
    {
        sickMenIDs      = unique(medicinesMenDF$ID)
        sickWomenIDs    = unique(medicinesWomenDF$ID)
        healthyMenIDs   = menOnlyTable$ID[!(menOnlyTable$ID     %in% sickMenIDs)]
        healthyWomenIDs = womenOnlyTable$ID[!(womenOnlyTable$ID %in% sickWomenIDs)]
        
        healthyMenDF    = completeTable[completeTable$ID %in% healthyMenIDs,]
        healthyWomenDF  = completeTable[completeTable$ID %in% healthyWomenIDs,]
        
        menOnlyTable$CurrentMedicine   = "Healthy"
        womenOnlyTable$CurrentMedicine = "Healthy"
        
        for(i in 1:nrow(menOnlyTable)){
            
            currentID = menOnlyTable$ID[i]
            
            if(currentID %in% sickMenIDs)   menOnlyTable$CurrentMedicine[i]   = "Medicated"
            
        }
        
        for(i in 1:nrow(womenOnlyTable)){
            
            currentID = womenOnlyTable$ID[i]
            
            if(currentID %in% sickWomenIDs) womenOnlyTable$CurrentMedicine[i] = "Sick"
            
        }
        
    }
    
    # Find out how many valid medicines we have for each sex
    # Prepare a summary DF accordingly
    {
    
        validMedicinesMenDF      = currentSpecializationSummaryMen[currentSpecializationSummaryMen$Count >= minimumMedicineRequiered,]
        menMedicinesList         = as.character(validMedicinesMenDF[,1])
        totalMedicinesMen        = length(menMedicinesList)
            
        validMedicinesWomenDF    = currentSpecializationSummaryWomen[currentSpecializationSummaryWomen$Count >= minimumMedicineRequiered,]
        womenMedicinesList       = as.character(validMedicinesWomenDF[,1])
        totalMedicinesWomen      = length(womenMedicinesList)
        
        summaryMedicinesMenDF             = DF(totalBiomarkers, totalMedicinesMen+1)
        colnames(summaryMedicinesMenDF)   = c("Protein", menMedicinesList)
        summaryMedicinesMenDF[,1]         = biomarkersMetadataDF$Protein
        
        summaryMedicinesWomenDF           = DF(totalBiomarkers, totalMedicinesWomen+1)
        colnames(summaryMedicinesWomenDF) = c("Protein", womenMedicinesList)        
        summaryMedicinesWomenDF[,1]       = biomarkersMetadataDF$Protein
    }
    
    
    # For those we have enough people, compare biomarkers with reference
    {
    
        # Make a DF that contain the template for the results.
        # This has, the biomarker name, the significance value with the 3 options
        # (no correction, benjamini, bonferroni), the average for the disease
        # group, and the average for the control group.
        #
        # The template for men and women is the same, and only the averages
        # and significance changes, so let create one for each.
        templateMenDF           = DF(totalBiomarkers, 7)
        colnames(templateMenDF) = c("Protein", "No correction", "Benjamini", "Bonferroni", "Avg Medicated", "Avg Healthy", "Image")
        templateMenDF[,1]       = biomarkersMetadataDF$Protein
        templateWomenDF         = templateMenDF
        for(i in 1:totalBiomarkers){
        
            templateMenDF[i,6]   =  mean(healthyMenDF[,   (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)
            templateWomenDF[i,6] =  mean(healthyWomenDF[, (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)    
            
        }
        
        menResultsList   = newList(totalMedicinesMen)
        womenResultsList = newList(totalMedicinesWomen)
        
        
        
        # From here onward we do the analysis
        currentSex                 = "Men"
        currentDiseasesBySex       = medicinesMenDF
        currentSpecializationTable = currentSpecializationSummaryMen
        currentHealthyTable        = healthyMenDF
        currentSexTable            = menOnlyTable
        currentResultsList         = menResultsList
        currentTemplate            = templateMenDF
        currentMetaSummary         = summaryMedicinesMenDF
        
        # For each sex (men and women)
        for(s in 1:2){
            
            # Set the proper tables
            if (s == 2){
                
                currentSex                 = "Women" 
                currentDiseasesBySex       = medicinesWomenDF
                currentSpecializationTable = currentSpecializationSummaryWomen        
                currentHealthyTable        = healthyWomenDF
                currentSexTable            = womenOnlyTable
                currentResultsList         = womenResultsList
                currentTemplate            = templateWomenDF
                currentMetaSummary         = summaryMedicinesWomenDF
            } 

            # Get all the diseases that are above the given disease threshold
            validDiseasesDF      = currentSpecializationTable[currentSpecializationTable$Count >= minimumMedicineRequiered,]
            currentDiseaseList   = as.character(validDiseasesDF[,1])
            totalCurrentDiseases = length(currentDiseaseList)

            # For each disease
            for(d in 1:totalCurrentDiseases){

                # Get the disease
                currentDisease = currentDiseaseList[d]
                
                # Get all the IDs for people that coincide with the current disease
                peopleWithThisDisease  = currentDiseasesBySex[currentDiseasesBySex$Brand == currentDisease,]$ID
                totalPeopleWithDisease = length(peopleWithThisDisease)
                diseasePeopleDF        = completeTable[completeTable$ID %in% peopleWithThisDisease,]
                
                # Prepare the template
                diseaseSpecificTemplate = currentTemplate
                
                # For each biomarker    
                for(b in 1:totalBiomarkers){

                    # Init the biomarker
                    currentBiomarkerName = biomarkersMetadataDF$Protein[b]
                    
                    # Prepare the subtable for this case
                    currentDataDF           = DF(nrow(currentHealthyTable) + totalPeopleWithDisease , 2)
                    colnames(currentDataDF) = c(currentBiomarkerName, "CurrentHealth")
                    currentDataDF[,2]       = c(rep("Healthy", nrow(currentHealthyTable)) , rep("Medicated", totalPeopleWithDisease))
                    currentDataDF[,1]       = c(currentHealthyTable[, (firstBiomarkerIndex + b - 1)] , diseasePeopleDF[, (firstBiomarkerIndex + b - 1)])

                    # Do the analysis
                    currentPValue                = simpleCategoricalPValue(currentDataDF, 2, 1,    skipUnknowns = TRUE)
                    diseaseSpecificTemplate[b,2] = currentPValue
                    diseaseSpecificTemplate[b,5] = mean(diseasePeopleDF[, (firstBiomarkerIndex + b - 1)], na.rm = TRUE)
                    
                }
                
                # The analysis is finish, do the benjamini and bonferroni corrections
                diseaseSpecificTemplate[,3] = p.adjust(diseaseSpecificTemplate[,2], method="fdr") 
                diseaseSpecificTemplate[,4] = p.adjust(diseaseSpecificTemplate[,2], method="bonferroni") 
                # Transform into asterisk
                diseaseSpecificTemplate[,2] = getAsterkisPValue(diseaseSpecificTemplate[,2], nsEmpty = TRUE)
                diseaseSpecificTemplate[,3] = getAsterkisPValue(diseaseSpecificTemplate[,3], nsEmpty = TRUE)
                diseaseSpecificTemplate[,4] = getAsterkisPValue(diseaseSpecificTemplate[,4], nsEmpty = TRUE)
                # Round the averages
                diseaseSpecificTemplate[,5] = round(diseaseSpecificTemplate[,5],2)
                diseaseSpecificTemplate[,6] = round(diseaseSpecificTemplate[,6],2)
                
                # Add the results to the metasummary
                currentMetaSummary[,(d+1)]  = diseaseSpecificTemplate[,4]
                
                
                # Write the table into disk
                
                currentDiseaseClean = cleanWeirdCharacters(currentDisease) #Make the disease name Latex compatible for the filename
                  
                currentCaption   = paste0( currentSex," table for biomarkers significance, medicine ",currentDisease)
                currentTableName = paste0("MedicinesSummaryii",s,"ii",d) 
                
                latexString = writeTableLATEX(diseaseSpecificTemplate, BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = currentCaption,
                                              overrideTableName = currentTableName, heightProportion = 0.5)
                
                
                
                latexString = paste0("input{../../../../results/biomarkers/tables/medicines/", latexString[[2]], ".tex}")
                
                print(latexString)
                
                # Write the table into the list
                currentResultsList[[d]] = diseaseSpecificTemplate
                
            }
            
            # Save the metasummary, because R is a stupid language and
            # passing variable as reference is so difficult these days -_-
            if (s == 1) summaryMedicinesMenDF   = currentMetaSummary
            if (s == 2) summaryMedicinesWomenDF = currentMetaSummary
            
            
        }
        
        

            
    }
        
    
    # Write the summaries into disk
    writeTableLATEX(summaryMedicinesMenDF,   BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = "Significant biomarkers for medicines and men, addjusted for Bonferroni",
                    overrideTableName = "MenMedicinesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    # Write the summaries into disk
    writeTableLATEX(summaryMedicinesWomenDF, BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = "Significant biomarkers for medicines and women, addjusted for Bonferroni",
                    overrideTableName = "WomenMedicinesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    
    # Do the same analysis as before, but for women only, and hormonal with
    # specific look at hormonal type by levels of progesteron / estradiol
    # take healthy women (disease) only this time
    if(FALSE){
        
        # Get how many hormonal we are using
        totalHormonalModalities = length(unique(womenMenstruatingTable$HormonalContraceptives))
        
        # Prepare the dataset with the results
        hormonalResultsDF           = DF(totalBiomarkers,4)
        colnames(hormonalResultsDF) = c("Protein", "Significance", "Benjamini", "Bonferroni")
        
        # Find all p-values
        for(i in 1:totalBiomarkers){
    
            # Get the indexes for the biomarker and the variable
            currentBiomarkerIndex = allNDLIndex[i]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[i]
        
            # Get the result of the t-test
            currentPValue          = simpleCategoricalPValue(womenMenstruatingTable, hormonalIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
            #currentSignificance    = getAsterkisPValue(currentPValue)
            
            hormonalResultsDF[i,1] = currentBiomarkerName
            hormonalResultsDF[i,2] = currentPValue
            
        }
        
        # Do the Benjamini and Bonferroni Corrections
        {
            
            hormonalResultsDF[,3] = p.adjust(hormonalResultsDF[,2],   method="fdr") 
            hormonalResultsDF[,4] = p.adjust(hormonalResultsDF[,2],   method="bonferroni") 
            
            
        }
        
        # I have no idea of why this need to be done, but the code doesn't
        # works if I use DF directly instead of DF2
        hormonalResultsDF2           = hormonalResultsDF
        
        # Convert to significance for easy reading
        {
            
            for(i in 1:totalBiomarkers){
                
                hormonalResultsDF2[i,2] = getAsterkisPValue(hormonalResultsDF[i,2])
                hormonalResultsDF2[i,3] = getAsterkisPValue(hormonalResultsDF[i,3])
                hormonalResultsDF2[i,4] = getAsterkisPValue(hormonalResultsDF[i,4])
                
            }

        }
        
        # Save to disk
        {
        
            write.csv2(hormonalResultsDF2,  HORMONAL_FILEPATH)
            
            
        }
            
    }
    
}

#++---------------------------------------------------------------------------
# Check my friends average biomarker (stratify by sex and highschool) againts
# my own biomarkers levels. Check for R2 and p-value
#---------------------------------------------------------------------------
{
    # I need to create the friendship matrix for later
    myOverallFriendshipMatrix = getFriendshipMatrix(overallEdgesDF, totalPeople)
    
    # First, let do it without stratification
    # There is nothing relevant in here
    {
    
        # Prepare the DF where we accumulate the results
        resultsSimpleDF           = DF(totalBiomarkers,5)
        colnames(resultsSimpleDF) = c("Protein", "Sex", "Highschool", "R2","Pvalue")
        
        # Get how many people have friends
        peopleWithFriendsDF    = completeTable[completeTable$OverallConnections > 0,]
        totalPeopleWithFriends = nrow(peopleWithFriendsDF)
        
        # Init the counter for the DF where we write results
        myCounter = 1
        
        # For each biomarker
        for(i in 1:totalBiomarkers){
        
            # Prepare the blank DF where we put the dataset
            # Some of these can be NA since not everyone friend has the biomarkers
            # analysis done (or could even be under LOD)
            regressionAllDF = DF(totalPeopleWithFriends,2)
            colnames(regressionAllDF) = c("MyFriendsAverage", "MyLevel")
            
            currentBiomarkerName = biomarkersMetadataDF$Acronym[i]
            
            
            # For each person
            for(j in 1:totalPeopleWithFriends){
            
                # Get the person relevant information
                currentPersonID       = peopleWithFriendsDF[j, IDIndex     ]
                currentPersonBioLevel = peopleWithFriendsDF[j, NDLIndex+i-1]
                
                # Find the friends surrounding you (you nominate or nominate you)
                currentPersonFriends  = unique(c(getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[4]],
                                                 getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[5]]))
                    
                currentTotalFriends   = length(currentPersonFriends) 
                
                currentPersonFriendsAverageBioLevel = 0
                currentPersonFriendsNonNAFriends    = 0
                
                # Find the friends average by accumulating into the variable
                # but check that you are not adding NA values
                for(k in 1:currentTotalFriends){
                    
                    # Get the biomarker level
                    currentFriendID       = currentPersonFriends[k]
                    currentFriendBioLevel = peopleWithFriendsDF[k, NDLIndex+i-1]
                    
                    # If the biomarker is non-NA, added to the average
                    if(!is.na(currentFriendBioLevel)){
                        
                        currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel + currentFriendBioLevel
                        currentPersonFriendsNonNAFriends    = currentPersonFriendsNonNAFriends    + 1
                    }
                    
                }
                    
                # If we have friends with non NA values, add everything to the
                # proper DF, otherwise skip and leave a NA line, that is fine
                if(currentPersonFriendsNonNAFriends>0){
                    
                    currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel / currentPersonFriendsNonNAFriends
                    
                    regressionAllDF[j,1] = currentPersonFriendsAverageBioLevel
                    regressionAllDF[j,2] = currentPersonBioLevel
                }
                    
                 
                    
            }
                
            # Clean the data from NA data
            regressionAllDF = regressionAllDF[   !is.na(regressionAllDF[,1]),  ]
                    
            # Run the model
            # Plot the graph
                    
            currentPlotTitle     = paste0(currentBiomarkerName," for men and women, all highschools")
            currentTableOverride = paste0(currentBiomarkerName,"_men_women_allHS")
                    
            myBioModel = doSimpleRegression(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS,
                                            plotTitle = currentPlotTitle,
                                            overrideTableName = currentTableOverride)
                    
            mySummary = summary(myBioModel)
                    
                    
            # Add results to dataframe        
            resultsSimpleDF[myCounter,1] = currentBiomarkerName
            resultsSimpleDF[myCounter,2] = "Both"
            resultsSimpleDF[myCounter,3] = "All"
            resultsSimpleDF[myCounter,4] = mySummary$r.squared
            resultsSimpleDF[myCounter,5] = glance(myBioModel)$p.value[[1]]
                    
            myCounter = myCounter + 1
            
            
            
            
            
            
        }
        
        # Transform the results into a readable asterisk table (if any)
        resultsSimpleRoundedDF         = resultsSimpleDF
        resultsSimpleRoundedDF$R2      = round(resultsSimpleRoundedDF$R2, 2)
        resultsSimpleRoundedDF$Pvalue  = getAsterkisPValue(resultsSimpleRoundedDF$Pvalue)
        
        write.csv2(resultsSimpleRoundedDF, SCATTER_SIMPLE_ROUNDED_FILEPATH)
        
        print("There is almost no results in here")
        view(resultsSimpleRoundedDF)
                
    }
    
    # Now we do the same but stratifying by highschool and sex
    {
        
        # Get the highschools info
        myHighschools    = levels(completeTable[,highSchoolIndex])
        totalHighschools = length(myHighschools)
        
        # Sex info we already know (men women)
        
        # Prepare the DF where we accumulate the results
        resultsComplexDF           = DF(totalBiomarkers*2*totalHighschools,5)
        colnames(resultsComplexDF) = c("Protein", "Sex", "Highschool","R2","Pvalue")
        myCounter                  = 1
        
        # For men and women
        for(i in 1:2){
        
            currentSex = "Man"
            if(i == 2) currentSex = "Woman"
            
            for(j in 1:totalHighschools){
                
                currentHighschool = myHighschools[j]
                
                # Get the table of people within the same sex and highschool
                currentSubtable = completeTable[ completeTable[,sexIndex]        == currentSex &
                                                 completeTable[,highSchoolIndex] == currentHighschool, ]
                
                peopleWithFriendsDF    = currentSubtable[currentSubtable[ , overallConnectionsIndex] > 0,]
                totalPeopleWithFriends = nrow(peopleWithFriendsDF)
                
                # For each biomarker
                for(k in 1:totalBiomarkers){
                    
                    # Prepare the blank DF where we put the dataset
                    # Some of these can be NA since not everyone friend has the biomarkers
                    # analysis done (or could even be under LOD)
                    regressionAllDF = DF(totalPeopleWithFriends,2, defaultValue = NA)
                    colnames(regressionAllDF) = c("MyFriendsAverage", "MyLevel")
                    
                    currentBiomarkerName = biomarkersMetadataDF$Protein[k]
                    
                    # For each person, find their friends averages
                    for(x in 1:totalPeopleWithFriends){
                        
                        # Get the person relevant information
                        currentPersonID       = peopleWithFriendsDF[x, IDIndex     ]
                        currentPersonBioLevel = completeTable[currentPersonID, NDLIndex + k - 1]
                        
                        # If your biomarker is not NA, do everything
                        # otherwise skip to the next person
                        if(!is.na(currentPersonBioLevel)){
                        
                            # Find the friends surrounding you (you nominate or nominate you)
                            currentPersonFriends  = unique(c(getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[4]],
                                                             getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[5]]))
                            
                            currentTotalFriends   = length(currentPersonFriends) 
                            
                            currentPersonFriendsAverageBioLevel = 0
                            currentPersonFriendsNonNAFriends    = 0
                            
                            # Find the friends average by accumulating into the variable
                            # but check that you is a valid friend.
                            #     Same sex
                            #     Same highschool
                            #     No NA values
                            for(y in 1:currentTotalFriends){
                                
                                # Get the biomarker level
                                currentFriendID       = currentPersonFriends[y]
                                currentFriendBioLevel = completeTable[currentFriendID, NDLIndex + k - 1]
                                currentFriendSex      = as.character(completeTable[currentFriendID,sexIndex])
                                currentFriendHS       = as.character(completeTable[currentFriendID,highSchoolIndex])
                                
                                # If the biomarker is non-NA, added to the average
                                if(!is.na(currentFriendBioLevel)  & 
                                   currentFriendSex == currentSex &
                                   currentFriendHS  == currentHighschool){
                                    
                                    currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel + currentFriendBioLevel
                                    currentPersonFriendsNonNAFriends    = currentPersonFriendsNonNAFriends    + 1
                                }
                                
                            }
                            
                            # If we have friends with non NA values, add everything to the
                            # proper DF, otherwise skip and leave a NA line, that is fine
                            if(currentPersonFriendsNonNAFriends>0){
                                
                                currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel / currentPersonFriendsNonNAFriends
                                
                                regressionAllDF[x,1] = currentPersonFriendsAverageBioLevel
                                regressionAllDF[x,2] = currentPersonBioLevel
                            }
                            
                                
                        }
                    
                    }
                
                    # Clean the data from NA data
                    regressionAllDF = regressionAllDF[   !is.na(regressionAllDF[,1]),  ]
                    
                    # Run the model
                    # Plot the graph
                    
                    currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " in ", currentHighschool)
                    currentTableOverride = paste0(currentBiomarkerName,"_",currentSex,"_",currentHighschool)
                    
                    myBioModel = doSimpleRegression(regressionAllDF, 1, 2, BIOMARKERS_FOLDER,
                                                    plotTitle = currentPlotTitle,
                                                    overrideTableName = currentTableOverride)
                    
                    mySummary = summary(myBioModel)
                    
                    
                    
                    resultsComplexDF[myCounter,1] = currentBiomarkerName
                    resultsComplexDF[myCounter,2] = currentSex
                    resultsComplexDF[myCounter,3] = currentHighschool
                    resultsComplexDF[myCounter,4] = mySummary$r.squared
                    #resultsComplexDF[myCounter,5] = as.numeric(mySummary$fstatistic[1])
                    resultsComplexDF[myCounter,5] = mySummary$coefficients[2,4]
                    
                    myCounter = myCounter + 1
                    
        }
        
    }
    
    
}
    
    }

    # Write to disk
    # (but round numbers first)
    resultsComplexRoundedDF    = resultsComplexDF
    resultsComplexRoundedDF$R2 = round(resultsComplexRoundedDF$R2, 2)
    
    for( i in 1:nrow(resultsComplexRoundedDF)){
    
        resultsComplexRoundedDF$Pvalue[i] = getAsterkisPValue(resultsComplexDF$Pvalue[i])

    }
    # -- These are the raw results
    write.csv2(resultsComplexRoundedDF, SCATTER_STRATOS_ROUNDED_FILEPATH)
    write.csv2(resultsComplexDF,        SCATTER_STRATOS_FILEPATH)
    
    # We have waaaay too many significances, let delete those that are ns 
    # and include that in the paper, the rest of the raw result can be found
    # at the github page
    
    # -- Delete those that are ns
    resultsMyVSFriendBioPaper = resultsComplexRoundedDF
    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Pvalue != "ns",]
    # -- Sort by Sex, Highschool, and Protein in that order
    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[order(resultsMyVSFriendBioPaper$Sex,resultsMyVSFriendBioPaper$Highschool, resultsMyVSFriendBioPaper$Protein),]
    # -- The table is too big, divide into two tables, for men and women
    resultsMyVSFriendBioPaperMen    = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Sex == "Man",]
    resultsMyVSFriendBioPaperWomen  = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Sex == "Woman",]
    
    
    
    writeTableLATEX(resultsMyVSFriendBioPaperMen, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Protein for men, which are similar to your male friend, stratify by highschool",
                    overrideTableName = "biofriendsMalesHighschool", widthProportion = 0.7, heightProportion = 0.4)
    
    writeTableLATEX(resultsMyVSFriendBioPaperWomen, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Protein for women, which are similar to your male friend, stratify by highschool",
                    overrideTableName = "biofriendsFemalesHighschool", widthProportion = 0.7, heightProportion = 0.4)
    
}

#++---------------------------------------------------------------------------
# Check friends distances
#---------------------------------------------------------------------------
{
    # This variables are for the final results
    menFriendDistances = NA
    menFriendsTotal    = NA
    menEnemiesDistances = NA
    menEnemiesTotal     = NA
    menRatios           = NA
    
    womenFriendDistances = NA
    womenFriendsTotal    = NA
    womenEnemiesDistances = NA
    womenEnemiesTotal     = NA
    womenRatios           = NA
    
    # Prepare the tables with the NDL values for men and women
    # Inclue people with non NA values only
    {
        menNDLOnlyTable   = menOnlyTable[,  c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]
        womenNDLOnlyTable = womenOnlyTable[,c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]
        
        keepTheseMen            = !is.na(menBiomarkersNDLTable[,2])
        keepTheseWomen          = !is.na(womenBiomarkersNDLTable[,2])
        
        menBiomarkersNDLTable   = menBiomarkersNDLTable[keepTheseMen,]
        keepMenIDs              = menBiomarkersNDLTable[keepTheseMen,]$ID
        totalMen                = length(keepMenIDs)
        
        womenBiomarkersNDLTable = womenBiomarkersNDLTable[keepTheseWomen,]
        keepWomenIDs            = womenBiomarkersNDLTable[keepTheseWomen,]$ID
        totalWomen              = length(keepWomenIDs)
        
        sexNDLTablesList      = newList(2)
        sexNDLTablesList[[1]] = menOnlyTable
        sexNDLTablesList[[2]] = womenOnlyTable
    }
    
    # We want to continue with only people who has
    #     - 2 friends of the same sex, who don't have NA values
    {
    
        # Repeat for men and women
        for(z in 1:2){
            
            currentSex = "Man"
            if(z == 2) currentSex = "Woman"
            
            currentTable   = sexNDLTablesList[[z]]
            totalTableRows = nrow(currentTable)
            
            currentTableIDs = currentTable$ID
            
            keepTheseRows  = rep(TRUE, totalTableRows)
            
            # For each person in this table
            for(j in 1:totalTableRows){
                
                currentID = currentTableIDs[j]
                
                # Find the friends for this ID with same sex
                currentSameSexFriends = unique(c(getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[4]],
                                                 getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[5]]))
                currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% currentTableIDs]
                totalCurrentFriends   = length(currentSameSexFriends)
                
                # If you have 2 friends or more you stay, otherwise, marked
                # for deletion
                if(totalCurrentFriends < 2){
                    
                    keepTheseRows[j] = FALSE
                    
                }
                
            }
            
            # Update the table
            currentTable          = currentTable[keepTheseRows,]
            sexNDLTablesList[[z]] = currentTable
            totalTableRows        = nrow(currentTable)
            currentTableIDs       = currentTable$ID
            
            # Make the list of friends for each person
            # This is superannoying to do in R since it doesn't have pointers. This is
            # completely unnaceptable performance wise, and this really need to be done in
            # C++. I need to start using Rcpp and we done with all this crap.
            {
                
                popularityTable           = DF(totalTableRows,2)
                colnames(popularityTable) = c("ID", "Total Friends")
                popularityLists           = newList(totalTableRows)
                
                for(k in 1:totalTableRows){
                    
                    currentID = currentTable$ID[k]
                    
                    # Get the friends of this person
                    myCurrentFriends     = unique(c(getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[4]],
                                                    getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[5]]))
                    
                    # Filter out people of different sex and people who are same sex
                    # but with NA values
                    myCurrentFriends = myCurrentFriends[myCurrentFriends %in% currentTableIDs]
                    
                    # Get the total
                    totalCurrentFriends  = length(myCurrentFriends)
                    
                    # Make the list of friends, added to the big list, and register the total
                    popularityTable[k,1] = currentID
                    popularityTable[k,2] = totalCurrentFriends
                    
                    if(totalCurrentFriends > 0){
                        
                        popularityLists[[k]] = myCurrentFriends
                        
                    }
                    
                }
                
            }
            
            # Now, for each person, we are going to measure the distance to 
            # friends biomarkers, and compare that to the distance of people
            # who are not your friends biomarkers.
            #
            # If friendhship has anyhthing to do with this, the distance should
            # be smaller in avarage for the friend group
            {
                yesFriendsAllDistancesVector = rep(0, totalBiomarkers)
                yesFriendsAllDistancesIndex  = rep(1, totalBiomarkers)
                
                nonFriendsAllDistancesVector = rep(0, totalBiomarkers)
                nonFriendsAllDistancesIndex  = rep(1, totalBiomarkers)
                
                # For each person, and for each biomarkers
                for(i in 1:totalTableRows){
                    
                    print(   round(100*i/totalTableRows,2)   )
                    
                    # Get ID
                    myCurrentID = currentTable$ID[i]
                    
                    # Get the friends of this person
                    listIndex           = row.names(popularityTable[popularityTable$ID == myCurrentID,])
                    myCurrentFriends    = popularityLists[[as.numeric(listIndex)]]
                    totalCurrentFriends = popularityTable[popularityTable$ID == myCurrentID,2]
                    
                    # If you have more than one friend, find the total distance, otherwise, set it to 0
                    if(totalCurrentFriends > 0){
                        
                        # Let's find the average distance for each biomarker for each friend
                        for(j in 1:totalBiomarkers){
                            
                            # In here we can set up the limit for valid biomarkers,
                            # under LOD or not. Until that is debated, keep going.
                            myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                            currentDistance = 0
                            
                            # If I don't have any value, skip this person
                            if(!is.na(myBiomarker)){
                                
                                # For each of my friends
                                for(k in 1:totalCurrentFriends){
                                    
                                # Get the ID
                                myFriendID        = myCurrentFriends[k]
                                    
                                # IDs are consecutive, and we only need to measure the edges once.
                                # So if your friend ID is smaller than you ID, skip it.
                                if(myFriendID < myCurrentID){
                                        
                                    # Get your friend biomarker
                                    myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                    
                                    # of course some values are NA,
                                    # and the stupid R default is to make X+NA = NA,
                                    # which makes this code more unreadable
                                    if(!is.na(myFriendBiomarker)){
                                            
                                        # We don't standarize the distance with 
                                        # respect my biomarker here
                                        currentBioDistance = (myBiomarker - myFriendBiomarker)^2
                                        
                                        yesFriendsAllDistancesVector[j] = yesFriendsAllDistancesVector[j] + currentBioDistance
                                        yesFriendsAllDistancesIndex[j]  = yesFriendsAllDistancesIndex[j]  + 1
                                            
                                    }
                                        
                                }
                                    
                            }
                            
                                # For everybody else how is not my friend
                                for(k in 1:totalTableRows){
                                
                                # Get the ID
                                myNonFriendID = currentTable$ID[k]
                                
                                # If this is friend, skip it
                                if(myNonFriendID %in% myCurrentFriends == FALSE){
                                    
                                    # IDs are consecutive, and we only need to measure the edges once.
                                    # So if your friend ID is smaller than you ID, skip it.
                                    if(myNonFriendID < myCurrentID){
                                        
                                        # Get your friend biomarker
                                        myNonFriendBiomarker = completeTable[myNonFriendID,(NDLIndex+j-1)]    
                                        
                                        # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                        # which makes this code more unreadable
                                        if(!is.na(myNonFriendBiomarker)){
                                            
                                            # We don't standarize the distance with respect my biomarker here
                                            currentBioDistance = (myBiomarker - myNonFriendBiomarker)^2
                                            
                                            nonFriendsAllDistancesVector[j] = nonFriendsAllDistancesVector[j] + currentBioDistance
                                            nonFriendsAllDistancesIndex[j]  = nonFriendsAllDistancesIndex[j]  + 1
                                            
                                        }
                                        
                                    }
                                    
                                     
                                }
                                    
                            }
                                
                            }
                                
                                
                        }
                            
                            
                    }            
                    
                }
                
                # Finally get the total Score
                yesFriendsAverageDistanceVector = yesFriendsAllDistancesVector/yesFriendsAllDistancesIndex
                nonFriendsAverageDistanceVector = nonFriendsAllDistancesVector/nonFriendsAllDistancesIndex
               
                if(currentSex == "Man"){
                    
                    menFriendDistances  = yesFriendsAllDistancesVector
                    menFriendsTotal     = yesFriendsAllDistancesIndex
                    menEnemiesDistances = nonFriendsAllDistancesVector
                    menEnemiesTotal     = nonFriendsAllDistancesIndex
                    menRatios           = nonFriendsAverageDistanceVector/yesFriendsAverageDistanceVector
                    
                }
                else{
                    
                    womenFriendDistances  = yesFriendsAllDistancesVector
                    womenFriendsTotal     = yesFriendsAllDistancesIndex
                    womenEnemiesDistances = nonFriendsAllDistancesVector
                    womenEnemiesTotal     = nonFriendsAllDistancesIndex
                    womenRatios           = nonFriendsAverageDistanceVector/yesFriendsAverageDistanceVector
                }
                
            }
        }
            
    }
    
    
    distancesDF = DF(totalBiomarkers,3)
    colnames(distancesDF) = c("Protein", "Men", "Women")
    for(i in 1:totalBiomarkers){
        
        distancesDF[i,1] = biomarkersMetadataDF$Protein[i]
        distancesDF[i,2] = round(menRatios[i],2)
        distancesDF[i,3] = round(womenRatios[i],2)
    }
    
    write.csv2(distancesDF, DISTANCES_FILEPATH)
    
    writeTableLATEX(distancesDF, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Ratio of square distances between each person friend and each person non-friends, stratify by sex. Values greater than 1 indicate that friends have a smaller difference in biomarkers levels compared with their non-friends counterpart.",
                    overrideTableName = "distancesBiomarkers", widthProportion = 0.5, heightProportion = 0.4)
 
    # How many have a distance greater than 1.1 or lower than 0.9
    sum(distancesDF$Men > 1.1)
    sum(distancesDF$Men < 0.9)
    sum(distancesDF$Women > 1.1)
    sum(distancesDF$Women < 0.9)
}

#---------------------------------------------------------------------------
# Check vitamin D levels with respect biomarkers (done in blood already)
#---------------------------------------------------------------------------
if(FALSE){
    
    # Prepare the DF with the results
    resultsVitDDF           = DF(totalBiomarkers, 7)
    colnames(resultsVitDDF) = c("Protein"," Men Simple ","Men Benjamini ","Men Bonferroni","Women Simple","Women Benjamini","Women Bonferroni")
    
    # For men and women
    for(i in 1:2){
        
        currentSubtable = menOnlyTable
        currentSex      = "Man"
        
        if(i == 2){
            
            currentSex  = "Woman"
            currentSubtable = womenOnlyTable
        }
            
        for(k in 1:totalBiomarkers){
            
            currentBiomarkerIndex = allNDLIndex[k]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
            
            # Clean the data from NA data
            currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
            currentSubtable = currentSubtable[!is.na(currentSubtable[, vitamimDIndex]),]
            
            # Run the model
            # Plot the graph
            currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and Vitamim D (nmol/L)")
            currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_vitamimD")
                    
            myBioModel = doSimpleRegression(currentSubtable, vitamimDIndex, currentBiomarkerIndex, BIOMARKERS_FOLDER,
                                            plotTitle = currentPlotTitle,
                                            overrideTableName = currentTableOverride)
                    
            mySummary = summary(myBioModel)
                    
            resultsVitDDF[k,1] = currentBiomarkerName
            
            if(i == 1) resultsVitDDF[k,2] = mySummary$coefficients[2,4]
            else       resultsVitDDF[k,5] = mySummary$coefficients[2,4]
            
        }
        
    }
    
    # Addjust p-values
    resultsVitDDF[,3] = p.adjust(resultsVitDDF[,2],   method="fdr") 
    resultsVitDDF[,4] = p.adjust(resultsVitDDF[,2],   method="bonferroni") 
    
    resultsVitDDF[,6] = p.adjust(resultsVitDDF[,5],   method="fdr") 
    resultsVitDDF[,7] = p.adjust(resultsVitDDF[,5],   method="bonferroni") 
    
    # Get significances
    {
            
        resultsVitDDF2 = resultsVitDDF
        
        for(i in 1:totalBiomarkers){

            resultsVitDDF2[i,2] = getAsterkisPValue(resultsVitDDF[i,2])
            resultsVitDDF2[i,3] = getAsterkisPValue(resultsVitDDF[i,3])
            resultsVitDDF2[i,4] = getAsterkisPValue(resultsVitDDF[i,4])
            resultsVitDDF2[i,5] = getAsterkisPValue(resultsVitDDF[i,5])
            resultsVitDDF2[i,6] = getAsterkisPValue(resultsVitDDF[i,6])
            resultsVitDDF2[i,7] = getAsterkisPValue(resultsVitDDF[i,7])            
        }

    }
    
    
    # Write to disk
    write.csv2(resultsVitDDF2,  SCATTER_VITAMIMD_FILEPATH)
    
    
}

# Is highschool diet associated? or does each person bring their own luchbox



#---------------------------------------------------------------------------
#
# Review this, probably done in numerical analysis already!
#
# Check BMI levels with respect biomarkers, but divided in men and women
#
# -- In here we have the usual none, bonferroni, and benjamini adjustments 
#
# -- However, we have two models here. One for the continuous data (BMI)
#    and another one for the categorical data (BMI categorical "Underweight", 
#    "Healthy", "Overweight" and so on.
#
# -- The categorical model is done already where we did all the combinations
#    posible. I'm repeating the same here to double check the results,
#    and to also have the table all the biomarkers, even those that had
#    no significance. Also to create the categorical plots.
#
# -- These results are funny in the sense that the continous data is not
#    significance, but the categorical data is (T_T)
#---------------------------------------------------------------------------
if(FALSE){
    
    # BMI levels
    BMILevels   = levels(completeTable$BMICategorical)
    totalLevels = length(BMILevels)
    
    # Create DF where to save results for the averages
    # (Left part is for men, Right part is for women)
    summaryBMIDF    = DF(totalBiomarkers, (2 * totalLevels) + 1)
    colnames(summaryBMIDF) = c("Biomarker", BMILevels, BMILevels)
    summaryBMIDF[,1] = biomarkersMetadataDF$Protein
    
    # Create the blank DF where to save the results of the significance
    # This is for the continous case (BMI)
    resultsBMIDF           = DF(totalBiomarkers, 7)
    colnames(resultsBMIDF) = c("Protein"," Men Simple ","Men Benjamini ","Men Bonferroni","Women Simple","Women Benjamini","Women Bonferroni")
    resultsBMIDF[,1]       = biomarkersMetadataDF$Protein
    
    # Create the blank DF where to save the results of the significance
    # This is for the categorical case (BMI Categorical)
    resultsCategoricalBMIDF           = resultsBMIDF
    
    
    # For men and women
    for(i in 1:2){
        
        currentSubtable = biomenOnlyTable
        currentSex      = "Man"
        
        if(i == 2){
            
            currentSex      = "Woman"
            currentSubtable = biowomenOnlyTable
        }
            
        for(k in 1:totalBiomarkers){
            
            currentBiomarkerIndex = allNDLIndex[k]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
            
            # Clean the data from NA data
            currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
            currentSubtable = currentSubtable[!is.na(currentSubtable[, BMIIndex]),]
            
            # ---------------------------------------------------
            # Continuous case:
            # ---------------------------------------------------
            # Run the model and draw the plot
            currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and BMI (kg/m^2)")
            currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_BMI")
                    
            myBioModel = doSimpleRegression(currentSubtable, BMIIndex, currentBiomarkerIndex, BIOMARKERS_FOLDER,
                                            plotTitle = currentPlotTitle,
                                            overrideTableName = currentTableOverride)

            # Save the results
            mySummary = summary(myBioModel)
            
            if(i == 1){
                resultsBMIDF[k,2] = mySummary$coefficients[2,4]  
            } 
            else{
                resultsBMIDF[k,5] = mySummary$coefficients[2,4]   
            }
                                
            # ---------------------------------------------------
            # Categorical case
            # ---------------------------------------------------
            # Get the p-value and do a categorical boxplot
            categoricalResults  = simpleCategoricalPValue(currentSubtable, BMICatIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
            
            # Make a nice title
            currentBoxplotTitle = paste0(currentBiomarkerName, " with respect BMI for ", currentSex)
            
            # Do the plot
            myBoxplotResults    = doCategoricalBoxPlot (currentSubtable,
                                                        BMICatIndex,
                                                        currentBiomarkerIndex,
                                                        BIOMARKERS_FOLDER,
                                                        colorsVector = COLOR_VECTOR_BMI,
                                                        plotTitle    = currentBoxplotTitle,
                                                        overrideImageWidth = 7,
                                                        plotYLabel   = currentBiomarkerName,
                                                        showPValues = FALSE)
  
            # Save the results
            # ---- For the p-values
            if(i == 1){
                resultsCategoricalBMIDF[k,2] = categoricalResults
            }
            else{
                resultsCategoricalBMIDF[k,5] = categoricalResults
            }
            
            # ---- For averages
            
            # Since the Unknown are ignored, I'm going to skip the 
            # averages that are already found in the return variable,
            # and do the 5 of them manually
            
            if(i == 1){
                
                for(j in 1:totalLevels){
                
                    currentLevel = BMILevels[j]
                    currentMean  = mean(biomenOnlyTable[biomenOnlyTable[,BMICatIndex] == currentLevel,currentBiomarkerIndex])
                    summaryBMIDF[k, (j+1)] = currentMean
                        
                }
                
            }
            else{
                
                 for(j in 1:totalLevels){
                
                    currentLevel = BMILevels[j]
                    currentMean  = mean(biowomenOnlyTable[biowomenOnlyTable[,BMICatIndex] == currentLevel,currentBiomarkerIndex])
                    summaryBMIDF[k, (j+6)] = currentMean
                        
                }
                
                
            }

        }
        
    }
    
    # Addjust p-values
    # ---- Continous case
    resultsBMIDF[,3] = p.adjust(resultsBMIDF[,2],   method="fdr") 
    resultsBMIDF[,4] = p.adjust(resultsBMIDF[,2],   method="bonferroni") 
    
    resultsBMIDF[,6] = p.adjust(resultsBMIDF[,5],   method="fdr") 
    resultsBMIDF[,7] = p.adjust(resultsBMIDF[,5],   method="bonferroni") 
    # ---- Categorical case
    resultsCategoricalBMIDF[,3] = p.adjust(resultsCategoricalBMIDF[,2],   method="fdr") 
    resultsCategoricalBMIDF[,4] = p.adjust(resultsCategoricalBMIDF[,2],   method="bonferroni") 
    
    resultsCategoricalBMIDF[,6] = p.adjust(resultsCategoricalBMIDF[,5],   method="fdr") 
    resultsCategoricalBMIDF[,7] = p.adjust(resultsCategoricalBMIDF[,5],   method="bonferroni")     
    
    # Get significances
    {
            
        # ---- Continous case
        resultsBMIDF2 = resultsBMIDF
        
        for(i in 1:totalBiomarkers){

            resultsBMIDF2[i,2] = getAsterkisPValue(resultsBMIDF[i,2])
            resultsBMIDF2[i,3] = getAsterkisPValue(resultsBMIDF[i,3])
            resultsBMIDF2[i,4] = getAsterkisPValue(resultsBMIDF[i,4])
            resultsBMIDF2[i,5] = getAsterkisPValue(resultsBMIDF[i,5])
            resultsBMIDF2[i,6] = getAsterkisPValue(resultsBMIDF[i,6])
            resultsBMIDF2[i,7] = getAsterkisPValue(resultsBMIDF[i,7])            
        }

        # ---- Categorical case
        resultsCategoricalBMIDF2 = resultsCategoricalBMIDF
        
        for(i in 1:totalBiomarkers){

            resultsCategoricalBMIDF2[i,2] = getAsterkisPValue(resultsCategoricalBMIDF[i,2])
            resultsCategoricalBMIDF2[i,3] = getAsterkisPValue(resultsCategoricalBMIDF[i,3])
            resultsCategoricalBMIDF2[i,4] = getAsterkisPValue(resultsCategoricalBMIDF[i,4])
            resultsCategoricalBMIDF2[i,5] = getAsterkisPValue(resultsCategoricalBMIDF[i,5])
            resultsCategoricalBMIDF2[i,6] = getAsterkisPValue(resultsCategoricalBMIDF[i,6])
            resultsCategoricalBMIDF2[i,7] = getAsterkisPValue(resultsCategoricalBMIDF[i,7])            
        }
        
        
    }
    
    
     
    
    # Write to disk
    write.csv2(resultsBMIDF2,            SCATTER_BMI_FILEPATH)
    write.csv2(resultsCategoricalBMIDF2, BOXPLOT_BMI_FILEPATH)
    write.csv2(summaryBMIDF,             AVERAGES_BMI_NDL_TABLE_FILEPATH)
      
    
}

#++---------------------------------------------------------------------------
# Spread of BMI in the network
#---------------------------------------------------------------------------
{
    
    # BMI bias analysis by simulations
    {
        
        # Get all edges into a list
        edgeList      = newList(TOTAL_NETWORKS)
        edgeList[[1]] = overallEdgesDF
        edgeList[[2]] = physicalEdgesDF
        edgeList[[3]] = schoolEdgesDF
        edgeList[[4]] = sportsEdgesDF
        edgeList[[5]] = homeEdgesDF
        edgeList[[6]] = otherEdgesDF     
        
        # Run the bias analysis with respect the carrier variables
        CarrierBiasDF = doCategoricalBiasAnalysis(completeTable,
                                                  edgeList,
                                                  BMICatIndex,
                                                  TOTAL_SIMULATIONS,
                                                  listOfNetworksNames = NETWORK_NAMES)

        # Add the significances asterisk and save it into disk
        CarrierBiasDF2 = CarrierBiasDF[[1]]
        
        CarrierBiasDF2$BMICategorical = getAsterkisPValue(CarrierBiasDF2$BMICategorical)
        CarrierBiasDF2$SD = round(CarrierBiasDF2$SD,2) 
        
        
        current_filepath = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK, "BMI_simulation,csv")
        write.csv2(CarrierBiasDF2,  current_filepath)
        
        
        writeTableLATEX(CarrierBiasDF2, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Simulated networks (n=1000) same-to-same relationships againts the real network same-to-same relationships. All simulated network shows bias of BMI spread in the real network.",
                       overrideTableName = "bmiSimulated", widthProportion = 0.9, heightProportion = 0.05)
        
        
        
    }
    
    
    
}

#++---------------------------------------------------------------------------
# PCA analysis
#---------------------------------------------------------------------------
{

    # Do the PCA analysis
    pcaAnalysis = prcomp(menBiomarkersNDLTable, scale=TRUE)

    # Do the Scree plot
    # About 20% is explain by the first two variables, not very good for 2D plots.
    pcaAnalysis.var = pcaAnalysis$sdev^2
    pcaAnalysis.var.per = round(pcaAnalysis.var/sum(pcaAnalysis.var)*100, 1)
    barplot(pcaAnalysis.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

    ## plot pc1 and pc2
    plot(pcaAnalysis$x[,1], pcaAnalysis$x[,2])

    # Try PCA with selected immune markers only
    naturalKillerIndex = grep("Natural.killer.cell.receptor" ,         colnames(completeTable))[2]
    cd4Index           = grep("CD40L" ,                                colnames(completeTable))[2]
    cd5Index           = grep("T.cell.surface.glycoprotein.CD5" ,      colnames(completeTable))[2]
    cd6Index           = grep("T.cell.surface.glycoprotein.CD6" ,      colnames(completeTable))[2]
    macrophageIndex    = grep("Macrophage.colony.stimulating.factor" , colnames(completeTable))[2]
    monocityIndex      = grep("Monocyte.chemotactic.protein.1" ,       colnames(completeTable))[2]

    inmmuneMarkersCollection = c(naturalKillerIndex, cd4Index, cd5Index, cd6Index, macrophageIndex, monocityIndex)
    
    menImmuneTable = menOnlyTable[,inmmuneMarkersCollection]
    
    # Delete men without complete bioprofile
    {
        
        keepTheseMen = rep(TRUE, nrow(menImmuneTable))
        
        for (i in 1:nrow(menImmuneTable)){
            
            
            print(sum(is.na(menImmuneTable[i,])))
            
            if(sum(is.na(menImmuneTable[i,])) > 0) keepTheseMen[i] = FALSE
            
        }
        
    }
    
    menImmuneTable = menImmuneTable[keepTheseMen,]
    
    
    # Do the PCA analysis
    pcaAnalysis = prcomp(menImmuneTable, scale=TRUE)

    # Do the Scree plot
    # About 65% is explain by the first two variables, ok, lets call it good enough
    pcaAnalysis.var = pcaAnalysis$sdev^2
    pcaAnalysis.var.per = round(pcaAnalysis.var/sum(pcaAnalysis.var)*100, 1)
    barplot(pcaAnalysis.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

    ## plot pc1 and pc2
    for(i in 1:length(inmmuneMarkersCollection)){
    
        plot(pcaAnalysis$x[,1], pcaAnalysis$x[,i])
            
    }
    
        
}

#---------------------------------------------------------------------------
# Autorocorrelation of BMI with all cofunding variables ????
#---------------------------------------------------------------------------
{
    
}

#---------------------------------------------------------------------------
# Network in time FF1 FF2
#---------------------------------------------------------------------------
{
    
}


