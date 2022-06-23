# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

# -----------------------------------
# Remember to run loadData.R first!
# -----------------------------------

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

# How many disease should be simulated.
# This number should be high if you want nice results
# Or zero or very low if you just want to test things.
TOTAL_DISEASE_SIMULATIONS = 250


# When running this automatically you need to select a target set of indexes
targetedIndexes      = lifeStyleCategoricalIndexes
totalTargetedIndexes = length(targetedIndexes)
targetedNames        = colnames(completeTable)[targetedIndexes]

# When running the EGRM model, it can't be referenced by indexes, and the
# R formula can't be created programatically either ( I hate you! ), so please
# change that manually with whatever variables you want.


# ------------------------------------------------------------------------------
# Do some basic graph plots
# ------------------------------------------------------------------------------
{
    
    # Each of the network with no highlights
    {

        # In this list, we keep the image file path for each of the plots
        myListOfPlots        = rep(NA, TOTAL_NETWORKS)
        myListOfPlotsObjects = newList(TOTAL_NETWORKS)
        
        # Each plot for each network.
        for(i in 1:TOTAL_NETWORKS){

            currentOverridedPlotName = paste(NETWORK_NAMES[i],"_with_no_highlight", sep='')
            currentPlotTitle         = paste(NETWORK_NAMES[i], sep='')
            currentOverrideCaption   = "Size based on number of undirected relationships"

            plotResults = doGraphPlot(allEdges[[i]],  completeTable, NETWORK_FOLDER,
                                      sizeVariableIndex = overallConnectionsIndex,
                                      selectedLayouts = DO_THIS_LAYOUTS,
                                      plotTitle    = currentPlotTitle,
                                      plotSubtitle = "Size based on number of undirected relationships",
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
        #
        # ---- The grid creates two plots, one plot for LATEX where latex itself
        #      creates the composition with the different individual plots. And
        #      an independent PNG file with all the composition. Use whatever
        #      you like best.
        totalGridColumns = 2
        totalGridRows    = ceiling(totalNetworks/totalGridColumns)
  
        # ---- FOR LATEX
        gridImage = writeImageGridLATEX(myListOfPlots, totalGridRows, totalGridColumns,
                                        texFileName = "graph_grid", pageWidth = 0.9,
                                        subFloatList= NETWORK_NAMES,
                                        captionOverride = "Overview of all networks.")

        logLine = paste0("        \\input{",LATEX_SRC_RELATIVE_PATH_TO_ALL, getFileName(gridImage), "}" , "\n")
        write( logLine , file   = logTXTFileConnection, append = TRUE )

        # ---- PNG
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows)
        allGraphPath  = file.path(paste(NETWORK_FOLDER, "3x2Graphs.png", sep = ""))
        ggsave(allGraphPath, width = 8, height = 16)
        
    }
      
    # Each of the network for each of the targeted variables
    {
        
        # Make a constant layout for consistence between plots
        myGraph = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
        myConstantOverallLayout = create_layout(graph = myGraph, layout = 'mds')
        
        for(i in 1:totalTargetedIndexes){
                    
            currentHighlightIndex     = targetedIndexes[[i]]
            
            currentPlotTitle          = paste0("Overall Network for ", targetedNames[i])
            currentPlotSubtitle       = "Size based on number of undirected relationships"
            currentOverridedPlotName  = paste0("OverallDF_",targetedNames[i])
            currentOverrideCaption    = paste0("Graph with the network for ",targetedNames[i],".")
        
            doGraphPlot(overallEdgesDF,  completeTable, NETWORK_FOLDER,
                        highlightVariable    = currentHighlightIndex,
                        colorVectorHighlight = myListOfColorVectors[[currentHighlightIndex]],
                        sizeVariableIndex    = overallConnectionsIndex,
                        manualLayout         = myConstantOverallLayout,
                        plotTitle            = currentPlotTitle,
                        plotSubtitle         = currentPlotSubtitle,
                        overrideTableName    = currentOverridedPlotName,
                        overrideCaption      = currentOverrideCaption) 
            
        }

        
        
    }
}      


# ------------------------------------------------------------------------------
# Do some histograms with interesting statistics
# ------------------------------------------------------------------------------
{
    
    # How well is this friendship a representation of real life
    {
         doHistogramPlot2(completeTable, overviewIndex, NETWORK_FOLDER,
                         colorsVector = COLOR_FRIENDSHIP,
                         binsWidth    = 1,
                         plotTitle    = " Does these friends give a good overview of your social network? ",
                         plotSubtitle = " 0 = Low, 10 = High",
                         plotXLabel   = "Points", plotYLabel = "Total")       
    }

    # Histogram grid wall showing network statistics
    {
  
        myListOfPlots = newList(TOTAL_NETWORKS * 4)
  
        # For each network
        for(i in 1:TOTAL_NETWORKS){
    
            # For each of the following, popularity, reciprocity and all
            # All
            # Pop
            # Foll
            # Recp
            for(j in 1:4){
      
                # -- Init
                currentIndex        = baseNetworkConnectionIndex + (i-1)*4 + (j-1)
                currentVariableName = colnames(completeTable)[currentIndex]
      
                currentOverridedPlotNameNORMAL = paste(NETWORK_NAMES[i],"_histogram_NORMAL_", currentVariableName, sep='')
                currentOverridedPlotNameALL    = paste(NETWORK_NAMES[i],"_histogram_ALL_",    currentVariableName, sep='')
                currentPlotTitle               = currentVariableName
      
                # Do the plot for the super image
                # -- 920 because the maximum amount of people with friends is 911 people with 0 friends in reciprocity in sports
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
                                                 normalizeYMaximum = 920, normalizeXMaximum = 12.4, 
                                                 writeValues = TRUE)
      
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

}

# ------------------------------------------------------------------------------
# Do some basic barplots and boxplots
# ------------------------------------------------------------------------------
{

    # Does people with more friends report to have a better general health?
    #
    # ---- Not really, p-value of the xi² table is 0.6
    #      There are some exception if you look at each combination manually
    #      and the overrepresentation seems to have a positive trend with
    #      respect health and number of friends. But if this is true, is not
    #      been pick up by the xi² test.
    #
    #      For the boxplot, there is no significant p-value in between any box
    #      either.
    {
    
        doBarRelativeCombinePlot(completeTable, healthIndex, overallPopularityIndex, NETWORK_FOLDER,
                                 colorsVector = COLOR_VECTOR_HEALTH,
                                 plotTitle    = "Does people with high popularity report better general health?", 
                                 plotXLabel   = "How many people nominate you as friend",
                                 plotYLabel   = "Reletive number of General Health")
            
        
        xiResults = categoricalXi(completeTable, healthIndex, overallPopularityIndex, NETWORK_FOLDER)
        xiResults[[8]]                        
        

        myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                                 healthIndex,
                                                 overallPopularityIndex,
                                                 NETWORK_FOLDER,
                                                 colorsVector = COLOR_VECTOR_HEALTH,
                                                 angleXLabels = 45,
                                                 overrideImageWidth = 6,
                                                 showPValues = FALSE)      
        
        myBoxplotResults[[2]]
        
    }
    
    
        
}

# ------------------------------------------------------------------------------
# Check how the people rate their network of friends
#
#     This goes to each ID (aprox 1000), and get each friend for each network.
#     As R does horribly slow doing loops, you might want to skip this sometimes
# ------------------------------------------------------------------------------
if(TRUE){

    gradeList  = rep(0,6)
    gradeTotal = rep(0,6)
  
    # For each person
    for (i in 1:totalRows) {

        # Init variables
        myID    = completeTable$ID[i]
        myGrade = completeTable$Overview[i]
    
        # If you actually have a grade, find out things...
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

# ------------------------------------------------------------------------------
# Analyze whether or not people tend to be bias towards making friends with
# people of an specific characteristics
# ------------------------------------------------------------------------------
{
    
   
    # EGRM
    # ------------------------------------------------------------------------------    
    if(TRUE){

        # Do the homophily analysis first and see what going on, there is some obvious
        # bias for almost everybody in the network.
        partialHomophilyDF  = partialHomophily(overallGraph,  targetedIndexes)
        completeHomophilyDF = completeHomophily(overallGraph, targetedIndexes)
  
        # Save the CSV to disk (no constant file here, manually for now)
        write.csv(partialHomophilyDF,  "partialHomophily.csv")
        write.csv(completeHomophilyDF, "completeHomophily.csv")
        
        # Do it again with the proper egrm model for variable level
        {
            
            # Prepare the friendship matrix with numbers
            {
                # The friend matrix, as 1 (friend) and 0 (non friend)
                # -- Friends if the original code is defined by rows (each row a maximum of 5)
                tempOverall      = overallNetworkDF
                tempOverall$ID   = NULL
                friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
            }
            
            # Create the network models
            myOverallNetwork = as.network(x = friendshipMatrix, 
                                          directed = FALSE, 
                                          loops   = FALSE, 
                                          matrix.type = "adjacency")
            
            myDirectedOverallNetwork = as.network(x = friendshipMatrix, 
                                                  directed = TRUE, 
                                                  loops   = FALSE, 
                                                  matrix.type = "adjacency")
            
            
            # Set the IDs  
            network.vertex.names(myOverallNetwork)         = completeTable$ID
            network.vertex.names(myDirectedOverallNetwork) = completeTable$ID
            
            # Set the categorical attributes (there will be no numerical values)
            for(i in 1:totalTargetedIndexes){
    
                currentIndex = targetedIndexes[i]
                currentName  = targetedNames[i]
  
                set.vertex.attribute(myOverallNetwork,         currentName, as.vector(completeTable[,currentIndex]))
                set.vertex.attribute(myDirectedOverallNetwork, currentName, as.vector(completeTable[,currentIndex]))
      
            }
            
            # Check that is working
            {
            
                # Do a plot    
                # (this will show a horrible plot, just to test that the network object is working)
                # plot(myOverallNetwork,vertex.col="Sex")
                
                # Check the Density (igraph do this too, and better)
                # gden(myOverallNetwork)                
                
            }

            # ERGM
            {
  
                # By edges only
                myRandomModel = ergm(myOverallNetwork ~ edges)  
                summary(myRandomModel)              

                 # Check that the edges coincide with density (it is)
                logit2prob(-5.26) # I don't know how to get the value :(
                                  # Is the value under MLE result Estimate
                
                
                # Create the formula for the ERGM programatically (stupid R syntax -_- )
                # It can't be done as egrm is not standarized @_@ , what. the. hell.
                if(FALSE){

                    formulaString = "myOverallNetwork ~ edges +"
                                
                    for(i in 1:totalTargetedIndexes){
                        formulaString = paste0(formulaString, " ", "nodematch(\"", targetedNames[i], "\") + ") 
                    }            
        
                    formulaString = substr(formulaString,1,nchar(formulaString)-1) # Delete last +
                    
                    myFormula = as.formula(formulaString)
                }
                
                
                # By some variables (everybody get together with similar peers as shown in the homophily table)
                myHomoModel = ergm(myOverallNetwork ~ edges +
                                   nodematch("Sex") +  nodematch("GeneralHealth") +
                                   nodematch("BMICategorical") +  nodematch("Smoke") +
                                   nodematch("Snuff") +  nodematch("Alcohol") +
                                   nodematch("SportsLeisure") + nodematch("SummerTransport") +
                                   nodematch("WinterTransport") + nodematch("ScreenTime"))
                
                summary(myHomoModel)
                
                # Check for the Goodness-of-fit diagnostics
                gofPlot = gof(myHomoModel ~ model) 
                plot(gofPlot)
                        
                # Same but adding reciprocity
                myRecipHomoModel = ergm(myDirectedOverallNetwork ~ edges +
                                        nodematch("Sex") +  nodematch("GeneralHealth") +
                                        nodematch("BMICategorical") +  nodematch("Smoke") +
                                        nodematch("Snuff") +  nodematch("Alcohol") +
                                        nodematch("SportsLeisure") + nodematch("SummerTransport") +
                                        nodematch("WinterTransport") + nodematch("ScreenTime") + 
                                        mutual)
                
                summary(myRecipHomoModel)    
                mcmc.diagnostics(myRecipHomoModel); # looking at the model diagnostics
                plot(myRecipHomoModel$sample) # the MCMC sample of network statistics
     
            }
  
  


            
        }
        
    } 


    
    # By X2 of relationship tables
    
    
    # By clustering definition    
    if(FALSE){
    
        # This detects clusters and give you the ID of each member of each cluster
        # The is up to you to figure it out why they group like this or what they have in common
        eb <- cluster_edge_betweenness(undirectedOverallGraph)
        eb
        dendPlot(eb, mode="hclust")
        plot(eb, undirectedOverallGraph)  
        
        # This will be interesting if we plot a graph of comunities where
        # ---- Each node is a comunity (there are like 35 of them)
        # ---- Each edge means that there is at least a friendship from someone from comunity A to comunity B
        #          Edges are weigted by number of friendships, so it can be 5 friendships between A to B, 30 between A to C, and so on

        # This, I have no idea what it does
        imc <- cluster_infomap(undirectedOverallGraph)
        membership(imc)
        communities(imc)
    
    }
    
    # By popularity
    
}

# ------------------------------------------------------------------------------
# Superclusters
# ------------------------------------------------------------------------------
{

        # This detects clusters and give you the ID of each member of each cluster
        # The is up to you to figure it out why they group like this or what they have in common
        clustersList  = cluster_edge_betweenness(undirectedOverallGraph)
        totalClusters = length(clustersList)
        
        # Create a matrix of NCluster x NClusters where we count how many friendships we have
        # Later we will melt this and create a comunities edges DF
        clustersFriendship           = DF(totalClusters, totalClusters+1, defaultValue = 0)
        colnames(clustersFriendship) = c(1:totalClusters, "ID") # Keep ID at the end so we don't need to do funky things with the column indexes
        clustersFriendship$ID        = 1:totalClusters
        
        # For each person
        # Do a +1 into the friendship matrix
        for(i in 1:totalRows){
        
            # Get your ID
            currentID = completeTable$ID[i]
            
            # Find your cluster (trivial since is already sorted by index)
            currentCluster = clustersList$membership[i]
            
            # Find all your friends    
            currentFriendshipResults = getFrienshipTypes(currentID, overallNetworkDF)
            currentFriends           = currentFriendshipResults[[5]] # Who do I nominate (keep direction for now)
            currentTotalFriends      = length(currentFriends)
            
            # Find the cluster of each friend
            if(currentTotalFriends > 0){
            
                currentFriendsCluster = clustersList$membership[currentFriends]
                totalFriendsClusters  = length(currentFriendsCluster)
                
                for(j in 1:totalFriendsClusters){
                
                    # Again, R horrible, no ++ operator
                    clustersFriendship[currentCluster, currentFriendsCluster[j]] = clustersFriendship[currentCluster, currentFriendsCluster[j]] + 1
                        
                }
                
            }
            
        }
        
        # Now we melt by cluster ID to get a list of edges
        meltedCluster           = melt(clustersFriendship, id.vars = "ID")
        colnames(meltedCluster) = c("from", "to", "value")        
        meltedCluster = meltedCluster[meltedCluster$value != 0, ]
        
        clusterEdgesDF      = meltedCluster
        clusterEdgesDF$from = as.character(clusterEdgesDF$from)
        clusterEdgesDF$to   = as.character(clusterEdgesDF$to)
        
        # We need a DF for the clusters too, even though is empty
        # I'm going to create something random here with popularity as one of the variables
        # ---- ID is ID
        # ---- Size is how many people are in the cluster
        # ---- Popularity is how many nominations it receives from other clusters
        # ---- Self is how many nominations are within the cluster
        #
        # Cluster with only one person get 0 self nomination, this is fine
        # Is also possible that we have a cluster of people who hate each others
        # and nobody get nominated, then Self is also 0 and also fine.
        clustersDF           = DF(totalClusters, 5)
        colnames(clustersDF) = c("ID", "Size", "Popularity", "Self", "Highschool")
        for(i in 1:totalClusters){
        
            clustersDF$ID[i]         = i
            clustersDF$Size[i]       = sum(clustersList$membership == i)
            clustersDF$Popularity[i] = sum(clusterEdgesDF[clusterEdgesDF$from != i & clusterEdgesDF$to == i,]$value)
            clustersDF$Self[i]       = sum(clusterEdgesDF[clusterEdgesDF$from == i & clusterEdgesDF$to == i,]$value)
            
            # Find out which highshool is the majority of members of that community
            currentClusterMembers = completeTable[clustersList$membership == i,]
            
            clustersDF$Highschool[i] = as.character(summarizeCategorical(currentClusterMembers, highSchoolIndex, sorted="top")[1,1])

        }
    
        # The library is not prepare right now to do edge thickness as variable and need some hacking, for now, I do this manually
        #
        # We do a plot with:
        # ---- Nodes as Clusters
        #      The color of the node goes from 0 to Self nomination.
        #      The size of the node is proportional to the number of people inside the cluster
        #
        # ---- Edges with no colors
        #      The thickness of the edge is how many people from this cluster nominate people from the other cluster
        
        # Create the graph object
        # ---- But first delete the edges to self nodes since we are not going to use that
        clusterEdgesNoSelfDF = clusterEdgesDF
        uniqueFroms          = unique(clusterEdgesNoSelfDF$from)
        keepTheseEdges       = rep(FALSE, nrow(clusterEdgesDF))
        for(i in 1:length(uniqueFroms)){
        
            currentIndex = uniqueFroms[i]
                
            keepTheseEdges = keepTheseEdges | (clusterEdgesNoSelfDF$from == currentIndex & clusterEdgesNoSelfDF$to == currentIndex)
            
        }
        keepTheseEdges = !keepTheseEdges
        clusterEdgesNoSelfDF = clusterEdgesNoSelfDF[keepTheseEdges, ]
        # ---- Actual graph object
        myClusterGraph = graph_from_data_frame(clusterEdgesNoSelfDF, vertices = clustersDF, directed = FALSE)    
        
        # Add the weight for the edges
        E(myClusterGraph)$weight = clusterEdgesNoSelfDF$value
        
        # Delete the edges to self, since we are not going to use that
        
        myClusterPlot = ggraph(myClusterGraph, layout = 'mds') +
            
                               geom_edge_link0(aes(edge_width = weight),
                                               edge_colour = "black",
                                               alpha = 0.2) +
            
                               geom_node_point(aes(fill  = clustersDF$Self,
                                                   color = clustersDF$Highschool),
                                               size = clustersDF$Size/3,
                                               stroke = 1,
                                               shape = 21) +
            
                               geom_node_text(aes(label = clustersDF$Highschool,
                                                   color = clustersDF$Highschool),
                                               family="sans") +
            
                               scale_color_manual(values= colorRampPalette(brewer.pal(5, "Spectral"))(8) ) +
            
                               scale_fill_gradient(low = "black", high = "purple") +

                               theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_rect(fill = "grey", color = "pink"),
                                     axis.line        = element_blank(),
                                     axis.text.x      = element_blank(),
                                     axis.text.y      = element_blank(),
                                     axis.ticks       = element_blank()) +
        
                              labs(title    = "Communities by edge betweenness algorithm",
                                   subtitle = "Each node is a community, size proportional to total members",
                                   fill     = "Relationships within community",
                                   color    = "Highschool")
            
            

        myClusterPlot            

        
        ggsave( paste0(NETWORK_FOLDER,"/clusters.png") , plot = myClusterPlot, width = 20, height = 20)
        
        ggsave        
          
        
        ggraph(gotS1,layout = "stress")+
  geom_edge_link0(aes(edge_width = weight),edge_colour = "grey66")+
  geom_node_point(aes(fill = clu,size = size),shape=21)+
  geom_node_text(aes(filter = size>=26, label = name),family="serif")+
  scale_fill_manual(values = got_palette)+
  scale_edge_width_continuous(range = c(0.2,3))+
  scale_size_continuous(range = c(1,6))+
  theme_graph()+
  theme(legend.position = "bottom")
        
        
        myClusterPlot
        
        
    
}


# ------------------------------------------------------------------------------
# Infection movie
#     You definetly want to keep this not running, it takes a very long time
# ------------------------------------------------------------------------------
if(TRUE){
    
    # Create the infection DF
    infectionDF = DF(totalRows,4)
    colnames(infectionDF)   = c("ID", "Infected", "HighSchool", "Connections")
    infectionDF$ID          = completeTable$ID
    infectionDF$Infected    = completeTable$HighSchool
    infectionDF$Infected    = factor(infectionDF$Infected , levels = c( "Infected", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8"))            
    infectionDF$Connections = completeTable$OverallConnections
    
    simColors = c("#DD66DD", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD")
    
    # Give the disease to 5 random people in Highshool 1 since is the one more
    # interconnected.
    h1Students = completeTable[completeTable$HighSchool == "H1",]
    patientsZero = sample(h1Students$ID, 5)
    infectionDF[infectionDF$ID %in% patientsZero,]$Infected = "Infected"
    
    # Make a constant layout for consistence between plots
    myGraph = graph_from_data_frame(schoolEdgesDF, vertices = infectionDF, directed = FALSE)
    myConstantSchoolLayout = create_layout(graph = myGraph, layout = 'mds')
    
    # First do the plot zero with the initial state
    currentPlotTitle          = paste0("Infection time 0")
    currentPlotSubtitle       = paste0("Total Infected = 5")
    currentOverridedPlotName  = paste0("Infection_0")
        
    doGraphPlot(schoolEdgesDF,  infectionDF, NETWORK_FOLDER,
                highlightVariable    = 2,
                colorVectorHighlight = simColors,
                sizeVariableIndex    = 4,
                manualLayout         = myConstantSchoolLayout,
                plotTitle            = currentPlotTitle,
                plotSubtitle         = currentPlotSubtitle,
                overrideTableName    = currentOverridedPlotName) 
    
    
    # Run the simulation and do the plots
    for(i in 1:TOTAL_DISEASE_SIMULATIONS){
    
        print(paste0("Simulation ",i))
        
        # Do the simulation
        # (This is a very quick code, the simulation simulator is more efficient)
        
        # -- Get the infected IDs
        zombies      = infectionDF[infectionDF$Infected == "Infected",]$ID
        totalZombies = length(zombies)
        # -- For each infected, try to spread the disease to their friends
        for(j in 1:totalZombies){
        
            # Get the zombie ID
            currentZombieID = zombies[j]
            
            # Get the people you nominate
            zombieNominations = schoolEdgesDF[schoolEdgesDF$from == currentZombieID,]$to
            # Get the people that nominate you
            zombiePopularity = schoolEdgesDF[schoolEdgesDF$to == currentZombieID,]$from
            # Join it
            zombieFriends = unique(c(zombieNominations, zombiePopularity))
            
            # Get the zombie friends (you nominate or nominate you)
            # zombieFriends = unique(c(getFrienshipTypes(currentZombieID, friendshipMatrix)[[4]],
            #                          getFrienshipTypes(currentZombieID, friendshipMatrix)[[5]]))
            
            totalZombieFriends = length(zombieFriends)
            
            # If you have more than one friend, try to spread the disease
            if(totalZombieFriends > 0){
            
                # Roll your luck
                currentGivingDiseaseRoll = runif(totalZombieFriends, 0, 1)
                
                for(k in 1:totalZombieFriends){
                
                    # Check if you are saved
                    if(currentGivingDiseaseRoll[k] < 0.05){
                    
                        # R is horrible, I'm getting real tire of your crap with casting from string to numbers and viceversa ~~
                        newInfectedID = as.numeric(zombieFriends[k])
                        
                        infectionDF$Infected[newInfectedID] = "Infected"
                            
                    }
                    
                        
                }    
                
            }
            
        }
        
        
        
        # Do the plot
        
        currentPlotTitle          = paste0("Infection time ",i)
        currentPlotSubtitle       = paste0("Total Infected = ", totalZombies)
        currentOverridedPlotName  = paste0("Infection_",i)
        
        doGraphPlot(schoolEdgesDF,  infectionDF, NETWORK_FOLDER,
                    highlightVariable    = 2,
                    colorVectorHighlight = simColors,
                    sizeVariableIndex    = 4,
                    manualLayout         = myConstantSchoolLayout,
                    plotTitle            = currentPlotTitle,
                    plotSubtitle         = currentPlotSubtitle,
                    overrideTableName    = currentOverridedPlotName) 
    }
    
}


# Close the TXT connections
close(logTXTFileConnection)

# Update the latex file
#source("latex.R", encoding="utf-8")

