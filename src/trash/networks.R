# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("indexes.R",      encoding="utf-8")

# How many disease should be simulated.
# This number should be high if you want nice results
# Or zero or very low if you just want to test things.
TOTAL_DISEASE_SIMULATIONS = 10

# -----------------------------------
#     Log for the inputs{} in latex
# -----------------------------------
{
  
  logTXTFileConnection = file(NETWORK_LOG_PATH, 'w')
  logLine              = paste( "GENERAL DATA LOG at: ", RIGHT_NOW, sep = "")
  write( logLine ,
         file = logTXTFileConnection,
         append = FALSE)
}

# Basic statistics
{

  # Make the dataframe where we save the info
  totalNetworks            = length(allGraphs)
  networksInfoDF           = data.frame(matrix(NA, nrow = TOTAL_NETWORKS, ncol = 12))
  colnames(networksInfoDF) = c("Name", "Total Connections",
                               "AvgFollowing", "AvgPopularity", "AvgReciprocity", "AvgConnection",
                               "SdFollowing",  "SdPopularity",  "SdReciprocity",  "SdConnection",
                               "AvgPathLength")

  # Fill the name of the rows
  # ---- Name
  {
    networksInfoDF[1,1] = "Overall"
    networksInfoDF[2,1] = "Physical"
    networksInfoDF[3,1] = "School"
    networksInfoDF[4,1] = "Sport"
    networksInfoDF[5,1] = "Home"
    networksInfoDF[6,1] = "Other"
  }

  # ---- Everything else
  for(i in 1:totalNetworks){

    # ---- Total Connections
    networksInfoDF[i,2] = nrow(allEdges[[i]])

    # ---- Following, popularity, reciprocity and connections and path length
    {
      # -------- Overall
      {
        networksInfoDF[1,3]  = mean(completeTable$OverallFollowing,   na.rm=TRUE)
        networksInfoDF[1,4]  = mean(completeTable$OverallPopularity , na.rm=TRUE)
        networksInfoDF[1,5]  = mean(completeTable$OverallReciprocity, na.rm=TRUE)
        networksInfoDF[1,6]  = mean(completeTable$OverallConnections, na.rm=TRUE)

        networksInfoDF[1,7]  = sd(completeTable$OverallFollowing,     na.rm=TRUE)
        networksInfoDF[1,8]  = sd(completeTable$OverallPopularity ,   na.rm=TRUE)
        networksInfoDF[1,9]  = sd(completeTable$OverallReciprocity,   na.rm=TRUE)
        networksInfoDF[1,10] = sd(completeTable$OverallConnections,   na.rm=TRUE)

        networksInfoDF[1,11] = mean_distance(overallGraph, directed = TRUE, unconnected = TRUE)
      }

      # -------- Physical
      {
        networksInfoDF[2,3]  = mean(completeTable$PhysicalFollowing,   na.rm=TRUE)
        networksInfoDF[2,4]  = mean(completeTable$PhysicalPopularity , na.rm=TRUE)
        networksInfoDF[2,5]  = mean(completeTable$PhysicalReciprocity, na.rm=TRUE)
        networksInfoDF[2,6]  = mean(completeTable$PhysicalConnections, na.rm=TRUE)

        networksInfoDF[2,7]  = sd(completeTable$PhysicalFollowing,     na.rm=TRUE)
        networksInfoDF[2,8]  = sd(completeTable$PhysicalPopularity ,   na.rm=TRUE)
        networksInfoDF[2,9]  = sd(completeTable$PhysicalReciprocity,   na.rm=TRUE)
        networksInfoDF[2,10] = sd(completeTable$PhysicalConnections,   na.rm=TRUE)

        networksInfoDF[2,11] = mean_distance(physicalGraph, directed = TRUE, unconnected = TRUE)
      }

      # -------- Home
      {
        networksInfoDF[3,3]  = mean(completeTable$HomeFollowing,   na.rm=TRUE)
        networksInfoDF[3,4]  = mean(completeTable$HomePopularity , na.rm=TRUE)
        networksInfoDF[3,5]  = mean(completeTable$HomeReciprocity, na.rm=TRUE)
        networksInfoDF[3,6]  = mean(completeTable$HomeConnections, na.rm=TRUE)

        networksInfoDF[3,7]  = sd(completeTable$HomeFollowing,     na.rm=TRUE)
        networksInfoDF[3,8]  = sd(completeTable$HomePopularity ,   na.rm=TRUE)
        networksInfoDF[3,9]  = sd(completeTable$HomeReciprocity,   na.rm=TRUE)
        networksInfoDF[3,10] = sd(completeTable$HomeConnections,   na.rm=TRUE)

        networksInfoDF[3,11] = mean_distance(homeGraph, directed = TRUE, unconnected = TRUE)
      }

      # -------- School
      {
        networksInfoDF[4,3]  = mean(completeTable$SchoolFollowing,   na.rm=TRUE)
        networksInfoDF[4,4]  = mean(completeTable$SchoolPopularity , na.rm=TRUE)
        networksInfoDF[4,5]  = mean(completeTable$SchoolReciprocity, na.rm=TRUE)
        networksInfoDF[4,6]  = mean(completeTable$SchoolConnections, na.rm=TRUE)

        networksInfoDF[4,7]  = sd(completeTable$SchoolFollowing,     na.rm=TRUE)
        networksInfoDF[4,8]  = sd(completeTable$SchoolPopularity ,   na.rm=TRUE)
        networksInfoDF[4,9]  = sd(completeTable$SchoolReciprocity,   na.rm=TRUE)
        networksInfoDF[4,10] = sd(completeTable$SchoolConnections,   na.rm=TRUE)

        networksInfoDF[4,11] = mean_distance(schoolGraph, directed = TRUE, unconnected = TRUE)
      }

      # -------- Sport
      {
        networksInfoDF[5,3]  = mean(completeTable$SportsFollowing,   na.rm=TRUE)
        networksInfoDF[5,4]  = mean(completeTable$SportsPopularity , na.rm=TRUE)
        networksInfoDF[5,5]  = mean(completeTable$SportsReciprocity, na.rm=TRUE)
        networksInfoDF[5,6]  = mean(completeTable$SportsConnections, na.rm=TRUE)

        networksInfoDF[5,7]  = sd(completeTable$SportsFollowing,     na.rm=TRUE)
        networksInfoDF[5,8]  = sd(completeTable$SportsPopularity ,   na.rm=TRUE)
        networksInfoDF[5,9]  = sd(completeTable$SportsReciprocity,   na.rm=TRUE)
        networksInfoDF[5,10] = sd(completeTable$SportsConnections,   na.rm=TRUE)

        networksInfoDF[5,11] = mean_distance(sportsGraph, directed = TRUE, unconnected = TRUE)
      }

      # -------- Others
      {
        networksInfoDF[6,3]  = mean(completeTable$OtherFollowing,   na.rm=TRUE)
        networksInfoDF[6,4]  = mean(completeTable$OtherPopularity , na.rm=TRUE)
        networksInfoDF[6,5]  = mean(completeTable$OtherReciprocity, na.rm=TRUE)
        networksInfoDF[6,6]  = mean(completeTable$OtherConnections, na.rm=TRUE)

        networksInfoDF[6,7]  = sd(completeTable$OtherFollowing,     na.rm=TRUE)
        networksInfoDF[6,8]  = sd(completeTable$OtherPopularity ,   na.rm=TRUE)
        networksInfoDF[6,9]  = sd(completeTable$OtherReciprocity,   na.rm=TRUE)
        networksInfoDF[6,10] = sd(completeTable$OtherConnections,   na.rm=TRUE)

        networksInfoDF[6,11] = mean_distance(othersGraph, directed = TRUE, unconnected = TRUE)
      }

    }

  }

  # ---- Add a few funny things
  networksInfoDF$FrienshipCoefficient   = networksInfoDF$AvgConnection/networksInfoDF$`Total Connections`
  networksInfoDF$ReciprocityCoefficient = networksInfoDF$AvgReciprocity/networksInfoDF$`Total Connections`

  # ---- Write the results
  writeTableLATEX(networksInfoDF, NETWORK_FOLDER, roundMe = 2,
                  transposeTable = TRUE,
                  tableCaption = "Overview of the different networks statistics.")  
  
}

# Graph plots
# (you can't really see much here, is just a giant blob in must of them)
{

  # In this list, we keep the image file path for each of the plots
  myListOfPlots = rep(NA, totalNetworks)
    
  myListOfPlotsObjects = vector("list", length = (totalNetworks))
  
  # Each plot for each network.
  for(i in 1:totalNetworks){

    currentOverridedPlotName = paste(networksInfoDF$Name[i],"_with_no_highlight", sep='')
    currentPlotTitle = paste(networksInfoDF$Name[i], sep='')

    plotResults = doGraphPlot(allEdges[[i]],  completeTable, NETWORK_FOLDER,
                              sizeVariableIndex = overallConnectionsIndex,
                              selectedLayouts = DO_THIS_LAYOUTS,
                              plotTitle = currentPlotTitle,
                              plotSubtitle = "",
                              overrideTableName = currentOverridedPlotName)
    
    # Save the current plot for the grid image
    # TODO: But you need to change the target from the results folder to the latex result folder
    myListOfPlots[i] = plotResults[[2]]
    myListOfPlotsObjects[[i]] = plotResults[[1]]
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(plotResults[[3]]), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )

  }

  # Make the grid image for all the networks
  # ---- By default, the grid is divided into two columns
  #      you can change this into sqrt of images so it more squarish
  # ---- This grid only works on LATEX, it doesn't create any png image 
  totalGridColumns = 2
  totalGridRows    = ceiling(totalNetworks/totalGridColumns)
  
  gridImage = writeImageGridLATEX(myListOfPlots, totalGridRows, totalGridColumns,
                                  texFileName = "graph_grid", pageWidth = 0.9,
                                  subFloatList= NETWORK_NAMES,
                                  captionOverride = "Overview of all networks.")

  logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(gridImage), "}" , "\n")
  write( logLine , file   = logTXTFileConnection, append = TRUE )
    
  # Reciprocal relationship in the overall only.
  plotResults = doGraphPlot(reciprobalOverallEdgesDF,  phenotypeTable, NETWORK_FOLDER,
                            sizeVariableIndex = overallConnectionsIndex,
                            selectedLayouts = DO_THIS_LAYOUTS,
                            plotTitle = "Reciprocal only overall",
                            plotSubtitle = "Size based on number of undirected relationships",
                            overrideTableName = "Reciprocal_overall_noHL")
  
  logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(plotResults[[3]]), "}" , "\n")
  write( logLine , file   = logTXTFileConnection, append = TRUE )
  
  # Create the png image with the composition
  ggarrange(plotlist =  myListOfPlotsObjects , ncol = 2, nrow = 3)
  allGraphPath  = file.path(paste(NETWORK_FOLDER, "3x2Graphs.png", sep = ""))
  ggsave(allGraphPath, width = 8, height = 16)
  

}

# How well is this friendship a representation of real life
{

  doHistogramPlot2(completeTable, overviewIndex, NETWORK_FOLDER,
                   binsWidth    = 1,
                   plotTitle    = " Does these friends give a good overview of your social network? ",
                   plotSubtitle = " 0 = Low, 10 = High",
                   plotXLabel   = "Points", plotYLabel = "Total")

  print("Average score for network overview:")
  print(mean(completeTable[,overviewIndex], na.rm=T))
  
  
  
    # Modified for Dina
    {
    
        doHistogramPlot2(completeTable, overviewIndex, NETWORK_FOLDER,
                         colorsVector = c("#e8d13a"),
                         binsWidth    = 1,
                         plotTitle    = " Does these friends give a good overview of your social network? ",
                         plotSubtitle = " 0 = Low, 10 = High",
                         plotXLabel   = "Points", plotYLabel = "Total")
        
        
        
          
    }
  
  
  
  # Check which network have the better grade.
  gradeList  = rep(0,6)
  gradeTotal = rep(0,6)
  
  # For each person
  for (i in 1:totalRows) {

    # Init variables
    myID    = completeTable$ID[i]
    myGrade = completeTable$Overview[i]
    
    if(!is.na(myGrade)){
      
      totalFriendsList = rep(0,6)
      
      # Get his list of friends in each of the networks
      totalFriendsList[1] = getFrienshipTypes(myID, overallNetworkDF)[[2]]
      totalFriendsList[2] = getFrienshipTypes(myID, physicalNetworkDF)[[2]]
      totalFriendsList[3] = getFrienshipTypes(myID, homeNetworkDF)[[2]]
      totalFriendsList[4] = getFrienshipTypes(myID, schoolNetworkDF)[[2]]
      totalFriendsList[5] = getFrienshipTypes(myID, sportsNetworkDF)[[2]]
      totalFriendsList[6] = getFrienshipTypes(myID, otherNetworkDF)[[2]]

      # Add the numbers to the accumulated vector
      for(j in 1:6){

          gradeList[j]  = gradeList[j]  + myGrade * totalFriendsList[j]
          gradeTotal[j] = gradeTotal[j] + totalFriendsList[j]
                
      }
            
    }
    
  }
  
  # Create a dataframe with the results to save
  networkGradesDF          =  data.frame(matrix(NA, nrow = TOTAL_NETWORKS, ncol = 2))
  colnames(networkGradesDF) = c("Network", "Mean")
  networkGradesDF[,1] = NETWORK_NAMES
  networkGradesDF[,2] = gradeList / gradeTotal
  
  # Order by grade
  networkGradesDF = networkGradesDF[order(-networkGradesDF[,2]), ]
  
  writeTableLATEX(networkGradesDF, NETWORK_FOLDER,
                  tableCaption      = "Each one of the network sorted by the average grade",
                  overrideTableName = "NetworksOverviewGrade",
                  roundMe = 2)
  
  
}

# Overall popularity only
{
  myCurrentPlot = doHistogramPlot2(completeTable, overallPopularityIndex, NETWORK_FOLDER,
                                   binsWidth = 1,
                                   plotTitle    = "Popularity in the overall network",
                                   plotSubtitle = "( how many people likes you )",
                                   #generalFontSize = 5,
                                   titleFontSize = 10,
                                   binFontSize = 5,
                                   axysFontSize = 10,
                                   #generalMargins = c(0.1 , 0.1 , 0.1 , 0.1),
                                   #titleMargins   = c(0.1 , 0.1 , 0   , 0.1),
                                   plotXLabel = "", plotYLabel = "",
                                   overrideTableName = "Popularity_Histogram_Overall",
                                   normalizeYMaximum = 250, normalizeXMaximum = 12.4, 
                                   writeValues = TRUE)
  
}


# The histogram grid wall.
{
  
  myListOfPlots = vector("list", length = (TOTAL_NETWORKS * 4))
  
  # For each network
  for(i in 1:TOTAL_NETWORKS){
    
    # For each of the following, popularity, reciprocity and all
    # All
    # Pop
    # Foll
    # Recp
    for(j in 1:4){
      
      # -- Init
      currentIndex        = (baseIndex + 2) + (i-1)*6 + (j-1)
      currentVariableName = colnames(completeTable)[currentIndex]
      
      currentOverridedPlotNameNORMAL = paste(networksInfoDF$Name[i],"_histogram_NORMAL_", currentVariableName, sep='')
      currentOverridedPlotNameALL    = paste(networksInfoDF$Name[i],"_histogram_ALL_", currentVariableName, sep='')
      currentPlotTitle               = currentVariableName
      
      # Do the plot for the super image
      # -- 900 because the maximum amount of people with friends is 900 people with 0 friends in reciprocity in sports
      # -- 12.4 because it looks nice and nobody has more than 13 friends.
      myCurrentPlot = doHistogramPlot2(completeTable, currentIndex, NETWORK_FOLDER,
                                       binsWidth = 1,
                                       plotTitle = currentPlotTitle,
                                       #generalFontSize = 5,
                                       titleFontSize = 10,
                                       binFontSize = 1,
                                       axysFontSize = 5,
                                       #generalMargins = c(0.1 , 0.1 , 0.1 , 0.1),
                                       #titleMargins   = c(0.1 , 0.1 , 0   , 0.1),
                                       plotXLabel = "", plotYLabel = "",
                                       overrideTableName = currentOverridedPlotNameALL,
                                       normalizeYMaximum = 900, normalizeXMaximum = 12.4, 
                                       writeValues = TRUE
      )
      
      myListOfPlots[[j+(i-1)*4]] = myCurrentPlot[[1]]
      
    }
  }
  # -- All together
  ggarrange(plotlist = myListOfPlots, ncol = 4, nrow = 6)
  allHistogramsPath  = file.path(paste(NETWORK_FOLDER, "6x4Histograms.png", sep = ""))
  ggsave(allHistogramsPath)
  
  latexFilePath = writeImageLATEX(allHistogramsPath, captionText = "Histograms for all networks and type of connection.",
                                  pageWidth=1)
  
  logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(latexFilePath), "}" , "\n")
  write( logLine , file   = logTXTFileConnection, append = TRUE )
}

# People in isolation
{
  
  isolatedTable = completeTable[completeTable$OverallPopularity == 0,]
  myCurrentPlot = doHistogramPlot2(isolatedTable, 19 , NETWORK_FOLDER,
                                   binsWidth = 1,
                                   plotTitle = "People in isolation",
                                   plotXLabel = "Total people nominated by people with 0 popularity", plotYLabel = "Total people",
                                   writeValues = TRUE)
  
  
}


# People phisycal isolation by category
{
  
  isolatedTable = completeTable[completeTable$PhysicalPopularity == 0,]
  myCurrentPlot = doHistogramPlot2(isolatedTable, 19 , NETWORK_FOLDER,
                                   binsWidth = 1,
                                   plotTitle = "People in isolation",
                                   plotXLabel = "Total people nominated by people with 0 popularity", plotYLabel = "Total people",
                                   writeValues = TRUE)
  
  for (i in 1:totalImportantCategoricalIndexes){
    
    aa = summarizeCategorical(completeTable, importantCategoricalIndexes[i])
    print(aa)
  }
  
  
}

# Composition of Popularity by categorical
{
  
  # Prepare the manual layout so all the graphs have consistency
  myGraph = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
  myConstantLayoutA = create_layout(graph = myGraph, layout = DO_THIS_LAYOUTS)
  myConstantLayoutB = subset(myConstantLayoutA, name %in% myConstantLayoutA$name, x:y)

  # Do the plots and the tablets
  for (i in 1:totalImportantCategoricalIndexes) {

    # Prepare the list where we are going to save all the results
    # We keep the plots objects in here
    #
    # -- Plots
    #
    # ---- Histogram by category
    # ---- Boxplot
    # ---- Density
    # ---- Relative barplot 
    # ---- Inverse Relative barplot 
    # ---- Graph
    myListOfPlots = vector("list", length = 6)
    
    # Init the variables
    currentCategoricalIndex = importantCategoricalIndexes[i]
    currentCategoricalName  = importantCategoricalNames[i]
    curentVectorColor       = myListOfColorVectors[[currentCategoricalIndex]]
    
    
    # Feedback for the user
    print(paste0("Popularity with respect ",currentCategoricalName))
    
    # Add the subsection to the logfile
    logLine = paste0("        \\subsection{By ", currentCategoricalName , "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )

    # Init the tablets variables
    tableNamePvalues      = paste0("pvalues_",     currentCategoricalName, "_popularity")
    tableNameCentralities = paste0("centralities_",currentCategoricalName, "_popularity")
    
    captionPvalues        = paste0("P-values for ",     currentCategoricalName, " and popularity.")
    captionCentralities   = paste0("Centralities for ", currentCategoricalName, " and popularity.")
    
    
    # PLOTS
    #
    # ---- Histogram
     myHistogramResults = doCategoricalHistogramPlot(completeTable, overallPopularityIndex, currentCategoricalIndex, NETWORK_FOLDER,
                                                     binsWidth = 1,
                                                     colorsVector = curentVectorColor,
                                                     plotXLabel = "Popularity", plotYLabel = "Total")

    
    
    # ---- Boxplot
    myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                             currentCategoricalIndex,
                                             physicalPopularityIndex,
                                             NETWORK_FOLDER,
                                             colorsVector = curentVectorColor,
                                             showPValues = FALSE)

    print(myBoxplotResults[[3]])
    
    # ---- P-Values
    pValuesTable = writeTableLATEX(myBoxplotResults[[2]], NETWORK_FOLDER,
                                   overrideTableName = tableNamePvalues,
                                   intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                   tableCaption = captionPvalues, roundMe = 2)
    
    # ---- Centralities
    centralitiesTable = writeTableLATEX(myBoxplotResults[[3]], NETWORK_FOLDER,
                                        overrideTableName = tableNameCentralities,
                                        tableCaption = captionCentralities, roundMe = 2)
    
    # ---- Density
    myDensityResults = doCategoricalDensityPlot(completeTable, overallPopularityIndex, currentCategoricalIndex, NETWORK_FOLDER,
                                                plotXLabel = "Popularity",
                                                plotYLabel = "Relative Frequency",
                                                colorsVector = curentVectorColor)
    
    
    # ---- Relative A
    myRelativeAResults = doBarRelativeCombinePlot(completeTable, currentCategoricalIndex, overallPopularityIndex, NETWORK_FOLDER,
                                                  colorsVector = curentVectorColor,
                                                  supressWarnings   = TRUE) # TRUE because we are converting a number into a category
    
    # ---- Relative B
    myRelativeBResults = doBarRelativeCombinePlot(completeTable, overallPopularityIndex, currentCategoricalIndex, NETWORK_FOLDER,
                                                  supressWarnings   = TRUE) # TRUE because we are converting a number into a category
    
    # ---- Network
    myNetworkResults = doGraphPlot(overallEdgesDF,  completeTable, NETWORK_FOLDER,
                                   highlightVariable = currentCategoricalIndex,
                                   sizeVariableIndex = overallPopularityIndex,
                                   colorVectorHighlight = curentVectorColor,
                                   selectedLayouts = 'manual' , manualLayout = myConstantLayoutA,
                                   plotSubtitle = "Size based on popularity (in degree)")
    
    
    # Add the results you want to the matrix
    {
      myListOfPlots[[1]] = myHistogramResults[[1]]
      myListOfPlots[[2]] = myBoxplotResults[[1]]
      myListOfPlots[[3]] = myDensityResults[[1]]
      myListOfPlots[[4]] = myRelativeAResults[[1]]
      myListOfPlots[[5]] = myRelativeBResults[[1]]
      myListOfPlots[[6]] = myNetworkResults[[1]]      
    }
 
    # Compose the grid and write everything in the log
    {
      
      # Image with the grid
      ggarrange(plotlist = myListOfPlots, ncol = 2, nrow = 3)
      allPlotsFileName = paste("MatrixPlots_Important_Attributes_", currentCategoricalName, ".png" , sep="" )
      allPlotsPath     = file.path(paste(NETWORK_FOLDER, allPlotsFileName,                           sep = ""))
      ggsave(allPlotsPath, width = 21, height = 30)
      # writeImageLATEX(allPlotsPath, pageWidth = 1, pageHeight = 1)
    
      # Add the lines to the log
      {
        
        # Grid with all the plots
        latexFilePath = writeImageLATEX(allPlotsPath, pageWidth=1, pageHeight = 1)
        logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(latexFilePath), "}" , "\n")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        
        # 
        # logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(myBoxplotResults[[5]]), "}" , "\n")
        # write( logLine , file   = logTXTFileConnection, append = TRUE )

        # P values table      
        logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(pValuesTable), "}" , "\n")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
      
        # Caption table
        logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(centralitiesTable), "}" , "\n")
        write( logLine , file   = logTXTFileConnection, append = TRUE )  
      }
    
    }
    
  }

}

# Popularity vs all carrier definitions
{
  for (j in 1:totalConsecuenceIndexes){
    
    currentConsecuenceIndex = consecuenceIndexes[j]

      
      
      myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                               currentConsecuenceIndex,
                                               overallPopularityIndex,
                                               NETWORK_FOLDER,
                                               colorsVector = rev(COLOR_VECTOR_CARRIER),
                                               showPValues = TRUE)
      
      print(myBoxplotResults[[3]])
      
  }
  
  
}

# Between Highschool relationships proportion
{
  
}
  
# Homophily and bias
{
  
  # Homophily table for each variable without modalities
  homophilyDF = partialHomophily(overallGraph, importantCategoricalIndexes)
  homophilyDF$TotalCategories = variablesInfoDF[importantCategoricalIndexes,]$TotalCategories
  homophilyDF$Expected = (1 / variablesInfoDF[importantCategoricalIndexes,]$TotalCategories)
  
  homophilyL1Table = writeTableLATEX(homophilyDF, NETWORK_FOLDER,
                                     tableCaption = "Homophily for all relevant variables",
                                     overrideTableName = "Homophily_Table_L1_",
                                     roundMe = 3)
  
  # Prepare the p-value table for the Xi-square
  xiPvalueDF = data.frame(matrix(NA, nrow = totalImportantCategoricalIndexes, ncol = 3))
  colnames(xiPvalueDF) = c("Name", "Nodes p-Value", "Edges p-Value")
  xiPvalueDF$Name = importantCategoricalNames
  
  # Each individual table
  for (i in 1:totalImportantCategoricalIndexes) {
    
    # Init variables
    currentCategoricalName = importantCategoricalNames[i]

    # Feedback for the user
    print(paste0("Xi^2 with respect ", currentCategoricalName))

    # Add the subsection to the logfile
    logLine = paste0("        \\subsubsection{By ", currentCategoricalName , "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
            
    # Do the homophily for specific attribute
    homophilyDF         = completeHomophily(overallGraph, importantCategoricalIndexes[i])
    
    # Do the Xi analysis
    xiAnalysis          = completeXi(overallGraph, importantCategoricalIndexes[i])
    
    # Save the p-value in the overall table
    xiPvalueDF[[i,2]]   = xiAnalysis[[6]]
    xiPvalueDF[[i,3]]   = xiAnalysis[[11]]


    # Prepare and write the tables with the results
    # -- Generic description
    genericTable = writeTableLATEX(xiAnalysis[[1]], NETWORK_FOLDER,
                                   tableCaption      = paste("Overview frequency for variable ", currentCategoricalName, sep=''),
                                   overrideTableName = paste("Overview_SubTable_",               currentCategoricalName, sep=''),
                                   roundMe = 4)

    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(genericTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # -- Absolute frequency of relationships
    absoluteTable = writeTableLATEX(xiAnalysis[[2]], NETWORK_FOLDER,
                                    tableCaption      = paste("Total nodes relatiopships for variable ", currentCategoricalName, sep=''),
                                    overrideTableName = paste("Total_Nodes_SubTable_",                   currentCategoricalName, sep=''),
                                    roundMe = 4)
    
    EabsoluteTable = writeTableLATEX(xiAnalysis[[7]], NETWORK_FOLDER,
                                     tableCaption      = paste("Total edges relatiopships for variable ", currentCategoricalName, sep=''),
                                     overrideTableName = paste("Total_Edges_SubTable_",                   currentCategoricalName, sep=''),
                                     roundMe = 4)
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(absoluteTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(EabsoluteTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # -- Relative frequency of relationships
    relativeTable = writeTableLATEX(xiAnalysis[[3]], NETWORK_FOLDER,
                                    tableCaption      = paste("Relative nodes relationships for variable ", currentCategoricalName, sep=''),
                                    overrideTableName = paste("Relative_Nodes_SubTable_",                   currentCategoricalName, sep=''),
                                    roundMe = 4)
    
    ErelativeTable = writeTableLATEX(xiAnalysis[[8]], NETWORK_FOLDER,
                                     tableCaption      = paste("Relative edges relationships for variable ", currentCategoricalName, sep=''),
                                     overrideTableName = paste("Relative_Edges_SubTable_",                   currentCategoricalName, sep=''),
                                     roundMe = 4)
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(relativeTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(ErelativeTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # -- Difference with respect main relative
    differenceTable = writeTableLATEX(xiAnalysis[[4]], NETWORK_FOLDER,
                                      tableCaption      = paste("Difference nodes between relative and actual for variable ", currentCategoricalName, sep=''),
                                      overrideTableName = paste("Difference_Nodes_SubTable_",                                 currentCategoricalName, sep=''),
                                      roundMe = 4)
    
    EdifferenceTable = writeTableLATEX(xiAnalysis[[9]], NETWORK_FOLDER,
                                       tableCaption      = paste("Difference edges between relative and actual for variable ", currentCategoricalName, sep=''),
                                       overrideTableName = paste("Difference_Edges_SubTable_",                                 currentCategoricalName, sep=''),
                                       roundMe = 4)
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(differenceTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )

    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(EdifferenceTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # -- Binomial test
    binomialTable = writeTableLATEX(xiAnalysis[[5]], NETWORK_FOLDER,
                                    tableCaption      = paste("Binomial nodes double tail test for variable ", currentCategoricalName, sep=''),
                                    overrideTableName = paste("Binomial_Nodes_SubTable_",                      currentCategoricalName, sep=''),
                                    intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                    roundMe = 4)
    
    EbinomialTable = writeTableLATEX(xiAnalysis[[10]], NETWORK_FOLDER,
                                     tableCaption      = paste("Binomial edges double tail test for variable ", currentCategoricalName, sep=''),
                                     overrideTableName = paste("Binomial_Edges_SubTable_",                      currentCategoricalName, sep=''),
                                     intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                                     roundMe = 4)

    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(binomialTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
        
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(EbinomialTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    # -- Homophily
    homophilyTable = writeTableLATEX(homophilyDF, NETWORK_FOLDER,
                                     tableCaption      = paste("Homophily for variable ", currentCategoricalName, sep=''),
                                     overrideTableName = paste("Homophily_SubTable_",     currentCategoricalName, sep=''),
                                     roundMe = 4)
    
    logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(homophilyTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
  }
  
  # Final table with all the p-values
  xiPvalues = writeTableLATEX(xiPvalueDF, NETWORK_FOLDER,
                              tableCaption      = "Xi Square test for nodes, all variables ",
                              overrideTableName = "Xi_Table_Nodes",
                              intervalsNumbers  = INTERVAL_PVALUES, intervalsColors = COLOR_INTERVAL_PVALUES,
                              roundMe = 4)

  logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(xiPvalues), "}" , "\n")
  write( logLine , file   = logTXTFileConnection, append = TRUE )

}

# Reachability plots
{
  
  # Average path per network
  {
    pathInformationDF           = data.frame(matrix(NA, nrow = TOTAL_NETWORKS, ncol = 3))
    colnames(pathInformationDF) = c("Network", "Avg Path length", "SD Path length")
    pathInformationDF$Network   = NETWORK_NAMES
    
    for(i in 1:TOTAL_NETWORKS){
      
      # Get the distances matrix
      currentDistances = distances(allGraphs[[i]])
      
      # Put it into a vector
      distancesVector = melt(currentDistances)$value
      
      # Delete the infinities from the vector
      distancesVector = distancesVector[distancesVector != Inf]
      
      # Write the info into the dataframe
      pathInformationDF[i,2] = mean(distancesVector, na.rm=TRUE)
      pathInformationDF[i,3] = sd(distancesVector, na.rm=TRUE)
      
    }
    
    pathsTable = writeTableLATEX(pathInformationDF, NETWORK_FOLDER,
                                 tableCaption = "Summary of distance statistics",
                                 overrideTableName = "distancesTable",
                                 roundMe = 2)
    
    logLine = paste0("        \\input{",LATEX_FOLDER_IMAGES_RESULTS_ALL, getFileName(pathsTable), "}" , "\n")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
  }

  # Reachability plots
  {
    myRechabilityPlots = rep(NA,TOTAL_NETWORKS)
    
    for(i in 1:TOTAL_NETWORKS){
      
      currentOverridedPlotName = paste(NETWORK_NAMES[i],"_Reachavilty", sep='')
      currentPlotTitle = paste(NETWORK_NAMES[i], " Reachability", sep='')
      myRechabilityPlots[i] = doReachabilityPlot(allEdges[[i]], nodesDF, totalSteps = 10, NETWORK_FOLDER,
                                                 plotTitle = currentPlotTitle,
                                                 plotXLabel = "Number of steps", plotYLabel = "Coverage of network",
                                                 ymin = 0, ymax = 1,
                                                 plotTheme = "simple",
                                                 overrideTableName = currentOverridedPlotName)[[4]]
      
    }
    
    # Make the grid image for all the networks
    {
      totalGridColumns = 2
      totalGridRows    = ceiling(TOTAL_NETWORKS/totalGridColumns)
      
      gridImage = writeImageGridLATEX(myRechabilityPlots, totalGridRows, totalGridColumns,
                                      texFileName = "rechability_grid", pageWidth = 0.9,
                                      subFloatList= NETWORK_NAMES,
                                      captionOverride = "Overview of all reachs.")
      
      logLine = paste0("        \\input{",LATEX_FOLDER_IMAGES_RESULTS_ALL, getFileName(gridImage), "}" , "\n")
      write( logLine , file   = logTXTFileConnection, append = TRUE )    
    }
    
  }

}

# Disease simulator
{
  if(TOTAL_DISEASE_SIMULATIONS>0){
    
    doSimulationPlot(overallEdgesDF, nodesDF, NETWORK_FOLDER,
                     directedPlot = FALSE,
                     totalSteps = 30, totalSimulations = TOTAL_DISEASE_SIMULATIONS,
                     plotTitle  = "Simulation of spread in the overall network",
                     plotXLabel = "Number of steps", plotYLabel = "Coverage of network",
                     overrideCaption = "Simulation of coverage for our fictional disease.",
                     ymin = 0, ymax = 1)
    
  }
  
}


# Close the TXT connections
close(logTXTFileConnection)

# Update the latex file
source("latex.R", encoding="utf-8")


# Testing stuff
if(FALSE){
  

  # Prepare the manual layout so all the graphs have consistency
  {
    myGraph = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
    myConstantLayoutA = create_layout(graph = myGraph, layout = "mds")
    myConstantLayoutB = subset(myConstantLayoutA, name %in% myConstantLayoutA$name, x:y)    
  }

  # Save the plots here
  myListOfPlots = vector("list", length = totalImportantCategoricalIndexes)
  
  # For each categorical
  for (i in 1:totalImportantCategoricalIndexes) {

    # Get all the categories
    {
      currentCategoricalIndex = importantCategoricalIndexes[i]
      currentCategoricalName  = importantCategoricalNames[i]
      curentVectorColor       = myListOfColorVectors[[currentCategoricalIndex]]
      myHighlightVariable     = currentCategoricalIndex
      myCurrentSummary        = summarizeCategorical(completeTable, myHighlightVariable)
      uniqueCategories        = myCurrentSummary[,1]
      totalUniques            = length(uniqueCategories)          
    }

    # Prepare the vectors where we are going to put the first centers
    {
      centroidsX     = rep(0,totalUniques)
      centroidsY     = rep(0,totalUniques)
      radianDistance = 0
      radianChunks   = 0
      if(totalUniques>1){
        radianDistance = (2*pi)/totalUniques
        radianChunks   = seq(0,(totalUniques-1)) * radianDistance
      }
      minX  = min(myConstantLayoutB$x)
      maxX  = max(myConstantLayoutB$x)
      minY  = min(myConstantLayoutB$y)
      maxY  = max(myConstantLayoutB$y)
      centerX = (maxX + minX)/2
      centerY = (maxY + minY)/2
      diffX   = maxX - minX
      firstCircleRadious  = diffX/4               # Radious distance is arbitrary, it just looks nice with /4
      
      centroidsX = centerX + cos(radianChunks) * firstCircleRadious
      centroidsY = centerY + sin(radianChunks) * firstCircleRadious
    }
   
    # For each category, find the coordinates for each node
    {
      # -- The coordinates goes into this matrixes
      coordinatesX = matrix(NA, nrow = totalUniques, ncol = max(myCurrentSummary[,2]))
      coordinatesY = matrix(NA, nrow = totalUniques, ncol = max(myCurrentSummary[,2]))
      # -- The radious of each individual circle
      secondCircleRadious  = firstCircleRadious
      if(totalUniques>1) secondCircleRadious = sqrt((centroidsX[1]-centroidsX[2])^2 + (centroidsY[1]-centroidsY[2])^2)/4 # /1 overlap
                                                                                                                         # / 2 touch each other
                                                                                                                         # / 4 there is a space in between 
      
      # -- Fill each row
      for (i in 1:totalUniques) {
        
        # Get how many are in this particular modality
        totalCurrentModality = myCurrentSummary[i,2]
        
        # Create the two vectors where you are going to write this
        minicentroidsX     = rep(0,totalCurrentModality)
        minicentroidsY     = rep(0,totalCurrentModality)
        radianDistance = 0
        radianChunks   = 0
        if(totalCurrentModality>1){
          radianDistance = (2*pi)/totalCurrentModality
          radianChunks   = seq(0,(totalCurrentModality-1)) * radianDistance
        }
        
        minicentroidsX = centroidsX[i] + cos(radianChunks) * secondCircleRadious
        minicentroidsY = centroidsY[i] + sin(radianChunks) * secondCircleRadious
        
        # Do this one by one , since R doesn't want to relpace the whole row ¬¬
        for(j in 1:totalCurrentModality){
          
          coordinatesX[i,j] = minicentroidsX[j]
          coordinatesY[i,j] = minicentroidsY[j]
          
        }
        
      }      
    }
  
    # Change the x,y coordinate of each node in the layout
    {
      myIndexes = rep(1,totalUniques)
      for (i in 1:nrow(myConstantLayoutB)) {
        
        # Get the category
        currentCategory = completeTable[i,myHighlightVariable]
        
        # Grab the index of this category
        currentIndex = grep(currentCategory, uniqueCategories)
        
        # Assign that corresponding coordinates
        myConstantLayoutB$x[i] = coordinatesX[currentIndex, myIndexes[currentIndex]]
        myConstantLayoutB$y[i] = coordinatesY[currentIndex, myIndexes[currentIndex]]
        
        # Increase the index for that modality
        myIndexes[currentIndex] = myIndexes[currentIndex] + 1
        
        
      }      
    }

    # Do the plots
    # -- Manual name
    myPlotName = paste0("Graph_Circle_", currentCategoricalName)
    # -- Experimental edges
    experimentalEdgesDF = overallEdgesDF
    # -- Initialize the edges color category
    experimentalEdgesDF$Same = "Not Equal"
    for (j in 1:nrow(experimentalEdgesDF)) {
      
      # get the From and To IDs
      fromHere = experimentalEdgesDF$from[j]
      toHere   = experimentalEdgesDF$to[j]
      
      # Get the category of from and to
      fromCategory = as.character(completeTable[fromHere,myHighlightVariable])
      toCategory   = as.character(completeTable[toHere,  myHighlightVariable])
      
      # Add the Same category
      if(fromCategory == toCategory){
        
        experimentalEdgesDF$Same[j] = fromCategory
        
      }
      
    }
    
    edgeColorIndex    = grep("^Same$", colnames(experimentalEdgesDF))
    myEdgeColorVector = NULL
    if( !is.null(curentVectorColor)) myEdgeColorVector = c(curentVectorColor, "#7F7F7F")
    
    # -- Add the same factor level order
    myCurrentFactors = levels(completeTable[,myHighlightVariable])
    if(is.null(myCurrentFactors)) myCurrentFactors = as.character(uniqueCategories)
    experimentalEdgesDF$Same = factor(experimentalEdgesDF$Same, levels = c(myCurrentFactors, "Not Equal"))
    
    myNetworkResults =  doGraphPlot(experimentalEdgesDF,  completeTable, NETWORK_FOLDER,
                                    highlightVariable = myHighlightVariable,
                                    colorVectorHighlight = curentVectorColor,
                                    sizeVariableIndex = overallPopularityIndex,
                                    colorVectorEdge = myEdgeColorVector,
                                    edgesHighlight  = edgeColorIndex,
                                    selectedLayouts = 'manual' , manualLayout = myConstantLayoutB,
                                    overrideTableName = myPlotName,
                                    plotSubtitle = "Test")
    
    myListOfPlots = myNetworkResults[[1]]
    
         
  }
 
  
  # Highschol test
  {
    
    
    # Get all the categories
    {
      currentCategoricalIndex = highSchoolIndex
      currentCategoricalName  = "Highschool"
      curentVectorColor       = NULL
      myHighlightVariable     = currentCategoricalIndex
      myCurrentSummary        = summarizeCategorical(completeTable, myHighlightVariable, sorted="none")
      uniqueCategories        = myCurrentSummary[,1]
      totalUniques            = length(uniqueCategories)          
    }
    
    # Prepare the vectors where we are going to put the first centers
    {
      centroidsX     = rep(0,totalUniques)
      centroidsY     = rep(0,totalUniques)
      radianDistance = 0
      radianChunks   = 0
      if(totalUniques>1){
        radianDistance = (2*pi)/totalUniques
        radianChunks   = seq(0,(totalUniques-1)) * radianDistance
      }
      minX  = min(myConstantLayoutB$x)
      maxX  = max(myConstantLayoutB$x)
      minY  = min(myConstantLayoutB$y)
      maxY  = max(myConstantLayoutB$y)
      centerX = (maxX + minX)/2
      centerY = (maxY + minY)/2
      diffX   = maxX - minX
      firstCircleRadious  = diffX/4               # Radious distance is arbitrary, it just looks nice with /4
      
      centroidsX = centerX + cos(radianChunks) * firstCircleRadious
      centroidsY = centerY + sin(radianChunks) * firstCircleRadious
    }
    
    # For each category, find the coordinates for each node
    {
      # -- The coordinates goes into this matrixes
      coordinatesX = matrix(NA, nrow = totalUniques, ncol = max(myCurrentSummary[,2]))
      coordinatesY = matrix(NA, nrow = totalUniques, ncol = max(myCurrentSummary[,2]))
      # -- The radious of each individual circle
      secondCircleRadious  = firstCircleRadious
      if(totalUniques>1) secondCircleRadious = sqrt((centroidsX[1]-centroidsX[2])^2 + (centroidsY[1]-centroidsY[2])^2)/4 # /1 overlap
      # / 2 touch each other
      # / 4 there is a space in between 
      
      # -- Fill each row
      for (i in 1:totalUniques) {
        
        # Get how many are in this particular modality
        totalCurrentModality = myCurrentSummary[i,2]
        
        # Create the two vectors where you are going to write this
        minicentroidsX     = rep(0,totalCurrentModality)
        minicentroidsY     = rep(0,totalCurrentModality)
        radianDistance = 0
        radianChunks   = 0
        if(totalCurrentModality>1){
          radianDistance = (2*pi)/totalCurrentModality
          radianChunks   = seq(0,(totalCurrentModality-1)) * radianDistance
        }
        
        minicentroidsX = centroidsX[i] + cos(radianChunks) * secondCircleRadious
        minicentroidsY = centroidsY[i] + sin(radianChunks) * secondCircleRadious
        
        # Do this one by one , since R doesn't want to relpace the whole row ¬¬
        for(j in 1:totalCurrentModality){
          
          coordinatesX[i,j] = minicentroidsX[j]
          coordinatesY[i,j] = minicentroidsY[j]
          
        }
        
      }      
    }
    
    # Change the x,y coordinate of each node in the layout
    {
      myIndexes = rep(1,totalUniques)
      for (i in 1:nrow(myConstantLayoutB)) {
        
        # Get the category
        currentCategory = completeTable[i,myHighlightVariable]
        
        # Grab the index of this category
        currentIndex = grep(currentCategory, uniqueCategories)
        
        # Assign that corresponding coordinates
        myConstantLayoutB$x[i] = coordinatesX[currentIndex, myIndexes[currentIndex]]
        myConstantLayoutB$y[i] = coordinatesY[currentIndex, myIndexes[currentIndex]]
        
        # Increase the index for that modality
        myIndexes[currentIndex] = myIndexes[currentIndex] + 1
        
        
      }      
    }
    
    # Do the plots
    # -- Manual name
    myPlotName = paste0("Graph_Circle_", currentCategoricalName)
    # -- Experimental edges
    experimentalEdgesDF = overallEdgesDF
    # -- Initialize the edges color category
    experimentalEdgesDF$Same       = "Not Equal"
    experimentalEdgesDF$SameBinary = FALSE
    for (j in 1:nrow(experimentalEdgesDF)) {
      
      # get the From and To IDs
      fromHere = experimentalEdgesDF$from[j]
      toHere   = experimentalEdgesDF$to[j]
      
      # Get the category of from and to
      fromCategory = as.character(completeTable[fromHere,myHighlightVariable])
      toCategory   = as.character(completeTable[toHere,  myHighlightVariable])
      
      # Get the SPAType same status
      fromCategory2 = as.character(completeTable[fromHere,spaT1IndexComplete])
      toCategory2   = as.character(completeTable[toHere,  spaT1IndexComplete])
      
      # Add the Same category
      if(fromCategory == toCategory){
        
        experimentalEdgesDF$Same[j] = fromCategory
        
      }
      
      # Add the Same category
      if(fromCategory2 == toCategory2){
        
        experimentalEdgesDF$SameBinary[j] = TRUE
        
      }
      
      
    }
    
    edgeColorIndex    = grep("^Same$", colnames(experimentalEdgesDF))
    myEdgeColorVector = NULL
    if( !is.null(curentVectorColor)) myEdgeColorVector = c(curentVectorColor, "#7F7F7F")
    
    # -- Add the same factor level order
    myCurrentFactors = levels(completeTable[,myHighlightVariable])
    if(is.null(myCurrentFactors)) myCurrentFactors = as.character(uniqueCategories)
    experimentalEdgesDF$Same = factor(experimentalEdgesDF$Same, levels = c(myCurrentFactors, "Not Equal"))

    myNetworkResults =  doGraphPlot(experimentalEdgesDF,  completeTable, NETWORK_FOLDER,
                                    highlightVariable = myHighlightVariable,
                                    colorVectorHighlight = curentVectorColor,
                                    sizeVariableIndex = overallPopularityIndex,
                                    colorVectorEdge = myEdgeColorVector,
                                    edgesHighlight  = edgeColorIndex,
                                    selectedLayouts = 'manual' , manualLayout = myConstantLayoutB,
                                    overrideTableName = myPlotName,
                                    plotSubtitle = "Test")
    
    
    myPlotName          = paste0("Graph_Circle2_", currentCategoricalName)
    edgeColorIndex2     = grep("^SameBinary$", colnames(experimentalEdgesDF))
    myEdgeColorVector2  = c("#FFFFFF", "#FF0000")
        
    myNetworkResults =  doGraphPlot(experimentalEdgesDF,  completeTable, NETWORK_FOLDER,
                                    highlightVariable = myHighlightVariable,
                                    colorVectorHighlight = curentVectorColor,
                                    sizeVariableIndex = overallPopularityIndex,
                                    edgesHighlight  = edgeColorIndex2,
                                    colorVectorEdge = myEdgeColorVector2,
                                    selectedLayouts = 'manual' , manualLayout = myConstantLayoutB,
                                    overrideTableName = myPlotName,
                                    plotSubtitle = "Test")
    
    
    myListOfPlots = myNetworkResults[[1]]
    
    
    
    
  }
   
}


# Community detection
{
  # # No communities showing :(
  # overallGraphUN  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = F)
  # 
  # ceb <- cluster_edge_betweenness(overallGraphUN)
  # dendPlot(ceb, mode="hclust")
  # plot(ceb, overallGraphUN)  
}

