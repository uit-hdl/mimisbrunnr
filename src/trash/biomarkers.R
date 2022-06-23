# This script generates the biomarkers study
# 
# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(ergm)
library(statnet)

source("indexesV2.R",      encoding="utf-8")


# Get how many biomarkers you have.
totalBiomarkersColumns = ncol(biomarkersTable) - 3
totalBiomarkers        = totalBiomarkersColumns/2  # LOD and NDL are the same

# Get the appropiate indexes
LODIndex = firstBiomarkerIndex
NDLIndex = totalBiomarkers + firstBiomarkerIndex

# Get some general statistics about biomarkers
biomarkersSummary = DF(totalBiomarkers, 5)
colnames(biomarkersSummary) = c("Variable", "LOD Average", "NDL Average", "LOD SD", "NDL SD")
# -- Init the names
for(i in 1:totalBiomarkers){

    biomarkersSummary[i,1] = colnames(completeTable)[firstBiomarkerIndex + i - 1]
    # LOD
    biomarkersSummary[i,2] = mean(completeTable[,LODIndex + i - 1] , na.rm = TRUE )
    biomarkersSummary[i,4] = sd(completeTable[,LODIndex + i - 1] ,   na.rm = TRUE )
    # NDL
    biomarkersSummary[i,3] = mean(completeTable[,NDLIndex + i - 1] , na.rm = TRUE )
    biomarkersSummary[i,5] = sd(completeTable[,NDLIndex + i - 1] ,   na.rm = TRUE )
}

# Stratify by men and women, since they have different protein levels activation.
# As seen previously, the homophily of frienship is about 85% by sex, so we are
# not going to loose too many friends.
{
    
    biomarkersSummaryMen   = biomarkersSummary
    biomarkersSummaryWomen = biomarkersSummary
    
    for(i in 1:totalBiomarkers){

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
        
        
    }
 
    biomarkersSummaryMen$LOD_Diff = (biomarkersSummaryMen$`LOD Average`-biomarkersSummaryWomen$`LOD Average`)/biomarkersSummaryMen$`LOD SD`
    biomarkersSummaryMen$NDL_Diff = (biomarkersSummaryMen$`NDL Average`-biomarkersSummaryWomen$`NDL Average`)/biomarkersSummaryMen$`NDL SD`
       
}


# Lets do the PCA for both sexes
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

deletedMenIDs   = menOnlyTable[!keepTheseMen,]$ID
deletedWomenIDs = womenOnlyTable[!keepTheseWomen,]$ID
keepMenIDs      = menOnlyTable[keepTheseMen,]$ID
keepWomenIDs    = womenOnlyTable[keepTheseWomen,]$ID

# From here on, we have tables clean of NAs
totalMen   = nrow(menBiomarkersLODTable)
totalWomen = nrow(womenBiomarkersLODTable)

# Try the z-score normalization between friends (men)
{
normalizationResultsDF = DF(totalBiomarkers,6)
colnames(normalizationResultsDF) = c("Variable", "Average Enemies", "Average Friends", "SD Friends", "p-value (t-test)", "Sample size")
normalizationResultsDF$Variable = biomarkersSummary$Variable
    
for(i in 1:totalBiomarkers){
    
    # Feeback
    print(round(i/totalBiomarkers,2))
    
    currentBiomarkerIndex = NDLIndex + i - 1
    
    averageEnemies = 0
    averageFriends = 0
    sdFriends      = 0
    pvalue         = 0
    sampleSize     = 0
    
    # For men only
    for(j in 1:totalMen){
        
        # Does this person has 2 or more friends of my same sex? (in and out nominations)
        currentID             = keepMenIDs[j]
        currentSameSexFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                                         getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
        
        currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
        
        totalCurrentFriends   = length(currentSameSexFriends)
        
        if(totalCurrentFriends >= 2){

            # Get the ID of this cluster of friends
            currentClusterIDs = c(currentID,currentSameSexFriends)
            currentClusterFriendsRows = menOnlyTable$ID %in% currentClusterIDs
            currentClusterEnemiesRows = !currentClusterFriendsRows
             
            # Get the subvectors of proteins for this people
            currentBiomarkersFriends = menOnlyTable[currentClusterFriendsRows, currentBiomarkerIndex]
            currentBiomarkersEnemies = menOnlyTable[currentClusterEnemiesRows, currentBiomarkerIndex]
            totalBiomarkersFriends   = sum(!is.na(currentBiomarkersFriends))
            totalBiomarkersEnemies   = sum(!is.na(currentBiomarkersEnemies))
            
            # Does your friends and enemies have a sample size of at least 3 or more?
            if(totalBiomarkersFriends >= 3 & totalBiomarkersFriends >= 3){
                
                # Find the average and SD for your friends (including you)
                currentFriendsAverage = mean(currentBiomarkersFriends, na.rm = TRUE)
                currentFriendsSD      = sd(currentBiomarkersFriends,   na.rm = TRUE)
                # Find the average and SD for not your friends (you are not included here)
                currentEnemiesAverage = mean(currentBiomarkersEnemies, na.rm = TRUE)
                currentEnemiesSD      = sd(currentBiomarkersEnemies,   na.rm = TRUE)            
            
                # Find the relative score for each (friends is 1, enemies is whatever)
                currentZFriends = 1
                currentZEnemies = currentEnemiesAverage/currentFriendsAverage
            
                sampleSize     = sampleSize     + 1
                averageEnemies = averageEnemies + currentEnemiesAverage
                averageFriends = averageFriends + currentFriendsAverage
                sdFriends      = sdFriends      + currentFriendsSD
                
                currentPvalue  = pnorm(currentEnemiesAverage, mean= currentFriendsAverage, sd=currentFriendsSD, lower.tail=FALSE)
                
                pvalue = pvalue + currentPvalue
                
            }

        }

    }
    
    if(sampleSize > 0){
    
        normalizationResultsDF[i,2] = averageEnemies/sampleSize
        normalizationResultsDF[i,3] = averageFriends/sampleSize
        normalizationResultsDF[i,4] = sdFriends/sampleSize
        #normalizationResultsDF[i,5] = pnorm(normalizationResultsDF[i,2], mean= normalizationResultsDF[i,3], sd=normalizationResultsDF[i,4], lower.tail=FALSE)
        normalizationResultsDF[i,5] = pvalue/sampleSize
        normalizationResultsDF[i,6] = sampleSize
            
    }

}    
}

# Try the z-score normalization between friends (women)
{
normalizationResultsWDF = DF(totalBiomarkers,6)
colnames(normalizationResultsWDF) = c("Variable", "Average Enemies", "Average Friends", "SD Friends", "p-value (t-test)", "Sample size")
normalizationResultsWDF$Variable = biomarkersSummary$Variable
    
for(i in 1:totalBiomarkers){
    
    # Feeback
    print(round(i/totalBiomarkers,2))
    
    currentBiomarkerIndex = NDLIndex + i - 1
    
    averageEnemies = 0
    averageFriends = 0
    sdFriends      = 0
    pvalue         = -1
    sampleSize     = 0
    
    # For men only
    for(j in 1:totalWomen){
        
        # Does this person has 2 or more friends of my same sex? (in and out nominations)
        currentID             = keepWomenIDs[j]
        currentSameSexFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                                         getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
        
        currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepWomenIDs]
        
        totalCurrentFriends   = length(currentSameSexFriends)
        
        if(totalCurrentFriends >= 2){

            # Get the ID of this cluster of friends
            currentClusterIDs         = c(currentID,currentSameSexFriends)
            currentClusterFriendsRows = womenOnlyTable$ID %in% currentClusterIDs
            currentClusterEnemiesRows = !currentClusterFriendsRows
             
            # Get the subvectors of proteins for this people
            currentBiomarkersFriends = womenOnlyTable[currentClusterFriendsRows, currentBiomarkerIndex]
            currentBiomarkersEnemies = womenOnlyTable[currentClusterEnemiesRows, currentBiomarkerIndex]
            totalBiomarkersFriends   = sum(!is.na(currentBiomarkersFriends))
            totalBiomarkersEnemies   = sum(!is.na(currentBiomarkersEnemies))
            
            # Does your friends and enemies have a sample size of at least 3 or more?
            if(totalBiomarkersFriends >= 3 & totalBiomarkersFriends >= 3){
                
                # Find the average and SD for your friends (including you)
                currentFriendsAverage = mean(currentBiomarkersFriends, na.rm = TRUE)
                currentFriendsSD      = sd(currentBiomarkersFriends,   na.rm = TRUE)
                # Find the average and SD for not your friends (you are not included here)
                currentEnemiesAverage = mean(currentBiomarkersEnemies, na.rm = TRUE)
                currentEnemiesSD      = sd(currentBiomarkersEnemies,   na.rm = TRUE)            
            
                # Find the relative score for each (friends is 1, enemies is whatever)
                currentZFriends = 1
                currentZEnemies = currentEnemiesAverage/currentFriendsAverage
            
                sampleSize     = sampleSize     + 1
                averageEnemies = averageEnemies + currentEnemiesAverage
                averageFriends = averageFriends + currentFriendsAverage
                sdFriends      = sdFriends      + currentFriendsSD
                
            }

        }

    }
    
    if(sampleSize > 0){
    
        normalizationResultsWDF[i,2] = averageEnemies/sampleSize
        normalizationResultsWDF[i,3] = averageFriends/sampleSize
        normalizationResultsWDF[i,4] = sdFriends/sampleSize
        normalizationResultsWDF[i,5] = pnorm(normalizationResultsWDF[i,2], mean= normalizationResultsWDF[i,3], sd=normalizationResultsWDF[i,4], lower.tail=FALSE)
        normalizationResultsWDF[i,6] = sampleSize
            
    }

}    
}


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
menImmuneTable = menImmuneTable[keepTheseMen,]
    
# Do the PCA analysis
pcaAnalysis = prcomp(menImmuneTable, scale=TRUE)

# Do the Scree plot
# About 65% is explain by the first two variables, ok, lets call it good enough
pcaAnalysis.var = pcaAnalysis$sdev^2
pcaAnalysis.var.per = round(pcaAnalysis.var/sum(pcaAnalysis.var)*100, 1)
barplot(pcaAnalysis.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## plot pc1 and pc2
plot(pcaAnalysis$x[,1], pcaAnalysis$x[,2])


metaAverageFriends = 0
metaAverageEnemies = 0
metaSDFriends      = 0
metaSDEnemies      = 0
sampleSize         = 0

# For each person
for(j in 1:totalMen){

    print(j/totalMen)
    
    # Grab the ID and coordinates    
    myX  = pcaAnalysis$x[j,1]
    myY  = pcaAnalysis$x[j,2]
    myID = keepMenIDs[j]    
    
    # Find the friends for this ID with same sex
    currentSameSexFriends = unique(c(getFrienshipTypes(myID, friendshipMatrix)[[4]],
                                     getFrienshipTypes(myID, friendshipMatrix)[[5]]))
    currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
    totalCurrentFriends   = length(currentSameSexFriends)

    # If you have 3 friends or more
    if(totalCurrentFriends >= 3){

        # Get the ID of my friends (done)
        currentClusterFriendsRows = currentSameSexFriends
        # Get the IDs of my enemies (and not me)
        currentClusterEnemiesRows = keepMenIDs[!(keepMenIDs %in% c(currentSameSexFriends, myID))]

        # Find the averages between friends and enemies
        totalFriends           = totalCurrentFriends
        totalEnemies           = length(currentClusterEnemiesRows)
        distancesFriends       = rep(0, totalFriends)
        distancesEnemies       = rep(0, totalEnemies)
        
        for(k in 1:totalFriends){
        
            # Grab the friend ID and coordinates
            friendID  = currentClusterFriendsRows[k]
            friendPCA = grep(TRUE, keepMenIDs == friendID)
            
            friendX = pcaAnalysis$x[friendPCA,1]
            friendY = pcaAnalysis$x[friendPCA,2]
            
            distancesFriends[k] = distance2D(myX, friendX, myY, friendY)
                
        }
        
        for(k in 1:totalEnemies){
        
            # Grab the friend ID and coordinates
            enemyID  = currentClusterEnemiesRows[k]
            enemyPCA = grep(TRUE, keepMenIDs == enemyID)
            
            enemyX = pcaAnalysis$x[enemyPCA,1]
            enemyY = pcaAnalysis$x[enemyPCA,2]
            
            distancesEnemies[k] = distance2D(myX, enemyX, myY, enemyY)
                
        }
        
        averageDistanceFriends = mean(distancesFriends)
        sdDistanceFriends      = sd(distancesFriends)
        averageDistanceEnemies = mean(distancesEnemies)
        sdDistanceEnemies      = sd(distancesFriends)
        
        metaAverageFriends = metaAverageFriends + averageDistanceFriends
        metaAverageEnemies = metaAverageEnemies + averageDistanceEnemies
        metaSDFriends      = metaSDFriends      + sdDistanceFriends
        metaSDEnemies      = metaSDEnemies      + sdDistanceEnemies
        
        sampleSize = sampleSize + 1
                        
    }
    

    
}

metaAverageFriends = metaAverageFriends/sampleSize
metaAverageEnemies = metaAverageEnemies/sampleSize
metaSDFriends      = metaSDFriends/sampleSize
metaSDEnemies      = metaSDEnemies/sampleSize

pnorm(metaAverageEnemies, mean= metaAverageFriends, sd=metaSDFriends, lower.tail=FALSE)




# Check biomarkers by sport activity
unique(completeTable$SportsFrequency)

unique(completeTable$SportsLeisure)

bioSummaryDF           = DF(totalBiomarkersColumns,2)
colnames(bioSummaryDF) = c("variable", "pMin")

for(i in 1:totalBiomarkersColumns){
    
    bioIndex = firstBiomarkerIndex + i - 1
    
    myBoxplotResults = doCategoricalBoxPlot (menOnlyTable,
                                             sportsIndex,
                                             bioIndex,
                                             PAPER_FOLDER,
                                             colorsVector = COLOR_VECTOR_SPORTS,
                                             showPValues=FALSE)

    bioSummaryDF[i,1] = colnames(completeTable)[bioIndex]
    bioSummaryDF[i,2] = min(melt(myBoxplotResults[[2]])$value, na.rm=TRUE)
        
}



# Make the biomarkers distribution study, including LOD
{

#     menBiomarkersLODTable   = menBiomarkersLODTable[keepTheseMen,]
# menBiomarkersNDLTable   = menBiomarkersNDLTable[keepTheseMen,]
# womenBiomarkersLODTable = womenBiomarkersLODTable[keepTheseWomen,]
# womenBiomarkersNDLTable = womenBiomarkersNDLTable[keepTheseWomen,]
    
    # For each biomarkers, we are going to study the frequency distribution
    # and add the information into Boxplots.
    
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
    
    # Now, the first thing is to check if there is a significant difference between
    # men and women (spoiler, there is) so we can justify the stratification
    # later on
    menWomenBiomarkersDF           = DF(totalBiomarkers, 2)
    colnames(menWomenBiomarkersDF) = c("Protein", "p-value", "sign")
    for(i in 1:totalBiomarkers){
        
        print(   round(100*i/totalBiomarkers,2)   )
        
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
          
         # Write everything into table
         menWomenBiomarkersDF[i,1] = currentBoxplotTitle
         menWomenBiomarkersDF[i,2] = currentPValue
         menWomenBiomarkersDF[i,3] = getAsterkisPValue(currentPValue)
          
          
    }
    
    # The second thing is to study the distribution of each variable with respect each explanatory variable
    #
    # -- First we do a general pass to see how many Proteins are under LOD
    generalLODStatisticsDF           = DF(totalBiomarkers, 4)
    colnames(generalLODStatisticsDF) = c("Protein", "N_Under_LOD", "Prt_Under_LOD", "N_NA")
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

                if(myBatch == "20160977"){
                    LODValueVector[j] = as.numeric(LOD977Values[i])
                }
                
                if(myBatch == "20160383"){
                    LODValueVector[j] = as.numeric(LOD386Values[i])
                }
                
                # This batch means that we perform both batches for this person
                # So we are going to take the minimum of both LOD
                if(myBatch == "99999999"){
                    LODValueVector[j] = min(as.numeric(LOD386Values[i]), as.numeric(LOD977Values[i]))
                }
            }
        }
        totalUnderLOD = sum(completeTable[,NDLIndex+i-1] < LODValueVector , na.rm=TRUE)
        
        # Write the LOD statistics
        generalLODStatisticsDF[i,1] = biomarkersMetadataDF$Protein[i]
        generalLODStatisticsDF[i,2] = totalUnderLOD
        generalLODStatisticsDF[i,3] = totalUnderLOD/totalPeople
        generalLODStatisticsDF[i,4] = totalMissing
        
    }
    
    # -- Now we study for each categorical variable
    explanatoryIndexes        = allCategoricalIndexes
    totalExplanatoryIndexes   = length(explanatoryIndexes)
    # specificLODStatisticsDF   = DF(totalBiomarkers, (totalExplanatoryIndexes + 4))
    # colnames(LODStatisticsDF) = c("Protein", "N_Under_LOD", "Prt_Under_LOD", colnames(completeTable[,explanatoryIndexes]) )
    
    # Create an empty dataframe with all categorical variables, and all categories
    # for each variable. In here we are going to fill:
    #
    # - Under LOD for each category
    # - Whether the varaible has a significant p-value or not
    #specificLODStatisticsDF           = readyDFModalities(completeTable, explanatoryIndexes, rep(1, totalBiomarkers), skipUnknowns = FALSE)
    #colnames(specificLODStatisticsDF) = biomarkersMetadataDF$Protein
    PValuesLODStatisticsDF            = readyDFVariables(completeTable, explanatoryIndexes, rep(1, totalBiomarkers))
    colnames(PValuesLODStatisticsDF)  = c("Variable", biomarkersMetadataDF$Protein)
    
    # For each variable and each biomarker, start doing t-tests and ANOVAs
    for(i in 1:totalExplanatoryIndexes){
        
        for(j in 1:totalBiomarkers){
        
            PValuesLODStatisticsDF[i,j+1] = simpleCategoricalPValue(completeTable, explanatoryIndexes[i], (NDLIndex+j-1), skipUnknowns = TRUE)
            
        }
        
    }
    
    # As stated before, these values are not very good because we need to stratify by sex anyway, so lets do that
    stratifyIndexes      = explanatoryIndexes[2:totalExplanatoryIndexes]
    totalStratifyIndexes = totalExplanatoryIndexes - 1
    
    menOnly_PValuesLODStatisticsDF            = readyDFVariables(completeTable, stratifyIndexes, rep(1, totalBiomarkers))
    colnames(menOnly_PValuesLODStatisticsDF)  = c("Variable", biomarkersMetadataDF$Protein)
    menOnly_AsterisksLODStatisticsDF          = menOnly_PValuesLODStatisticsDF
    
    # For each variable and each biomarker, start doing t-tests and ANOVAs
    for(i in 1:totalStratifyIndexes){
        
        print( round(100*i/totalStratifyIndexes,2) )
        
        for(j in 1:totalBiomarkers){
        
            currentPValue = simpleCategoricalPValue(menOnlyTable, stratifyIndexes[i], (NDLIndex+j-1), skipUnknowns = TRUE)
            
            menOnly_PValuesLODStatisticsDF[i,j+1]   = currentPValue
            menOnly_AsterisksLODStatisticsDF[i,j+1] = getAsterkisPValue(currentPValue)
            
        }
        
    }
    
    
    
    
}

# MEN ONLY NOW; DO WOMEN LATER

# Biomarkers and the social network
{
    
    # We are going to check if your friends are closer to you via Biomarkers.
    
    # --
    # We need to stratify by sex first, so we only study men/women, with a valid
    # NDL biomarkers, that is not NA, and that has more than 15% above LOD, and
    # who has at least 2 friends of the same sex.
    
    # Original dataframe
    menBiomarkersNDLTable   = menOnlyTable[,c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]

    # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
    # We just need to check column 1 as the rest of the columns just follow along
    keepTheseMen          = !is.na(menBiomarkersNDLTable[,2])
    menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
    keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
    totalMen              = length(keepMenIDs)

    # We only want people who has 2 friends of the same sex and also have non-NA
    # values in their biomarkers.
    keepTheseMen = rep(TRUE, totalMen)
    for(j in 1:totalMen){

        currentID = menBiomarkersNDLTable$ID[j]
        
        # Find the friends for this ID with same sex
        currentSameSexFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                                         getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
        currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
        totalCurrentFriends   = length(currentSameSexFriends)

        # If you have 2 friends or more
        if(totalCurrentFriends < 2){
            
            keepTheseMen[j] = FALSE
            
        }
         
    }
    
    menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
    keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
    totalMen              = length(keepMenIDs)
    
    # Finally, the 15% above LOD we don't care right now. We are going to do
    # all the variables, and then check for that later.

    
    # Make the list of friends for each person
    # This is superannoying to do in R since it doesn't have pointers. This is
    # completely unnaceptable performance wise, and this really need to be done in
    # C++. I need to start using Rcpp and we done with all this crap.
    {

        popularityTable = DF(totalMen,2)
        colnames(popularityTable) = c("ID", "Total Friends")
    
        popularityLists = newList(totalMen)
    
        for(k in 1:totalMen){
    
            currentID = menBiomarkersNDLTable$ID[k]
            
            # Get the friends of this person
            myCurrentFriends     = getUndirectedFriends(currentID, overallNetworkDF)
            
            # Filter out people of different sex and people who are same sex
            # but with NA values
            myCurrentFriends = myCurrentFriends[myCurrentFriends %in% keepMenIDs]
            
            # Get the total
            totalCurrentFriends  = length(myCurrentFriends)
                    
            # Make the list of friends, added to the big list, and register the total
            popularityTable[k,1] = currentID
            popularityTable[k,2] = totalCurrentFriends
        
            if(totalCurrentFriends > 0){
        
                popularityLists[[k]] = newList(totalCurrentFriends)
            
                for(m in 1:totalCurrentFriends){
                
                    popularityLists[[k]][[m]] = myCurrentFriends[m]
                
                }
                
            }
        
    }
        
    }
    
    # Check average of friends biomarkers, this is to be compared with the simulations
    # The idea is to measure the total distance.
    {

        # Create a DF where we put all the results
        distanceValuesDF           = DF(totalMen, totalBiomarkers, defaultValue=0)
        colnames(distanceValuesDF) = colnames(completeTable)[NDLIndex:(NDLIndex + totalBiomarkers - 1)]
        allDistancesVector         = sum(popularityTable$`Total Friends`)
        allDistancesIndex          = 1
        
        # For each person, and for each biomarkers
        for(i in 1:totalMen){
        
            print(   round(100*i/totalMen,2)   )
        
            # Get ID
            myCurrentID = menBiomarkersNDLTable$ID[i]
        
            # Get the friends of this person
            myCurrentFriends    = popularityLists[[i]]
            totalCurrentFriends = popularityTable$`Total Friends`[i]
        
            # If you have more than one friend, find the total distance, otherwise, set it to 0
            if(totalCurrentFriends > 0){
            
                # Let's find the average distance for each biomarker for each friend
                for(j in 1:totalBiomarkers){
                
                    # Only account for valid biomarkers, when you have more than 15%
                    # values bellow LOD, that is not a valid biomarker.
                    currentLODLost = generalLODStatisticsDF$Prt_Under_LOD[j]
                    if(currentLODLost < 0.15){
                        
                        myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                        currentDistance = 0
                        
                        # For each of my friends
                        for(k in 1:totalCurrentFriends){
                
                            # Get the ID
                            myFriendID        = myCurrentFriends[[k]]
                            
                            # IDs are consecutive, and we only need to measure the edges once.
                            # So if your friend ID is smaller than you ID, skip it.
                            if(myFriendID < myCurrentID){
                            
                                # Get your friend biomarker
                                myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                
                                # We standarize the distance with respect my biomarker, consider to be 100%
                                #
                                # currentDistance   = currentDistance + (myBiomarker - myFriendBiomarker)^2
                                #
                                # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                # which makes this code more unreadable
                                if(!is.na(myFriendBiomarker)){
                        
                                    currentBioDistance = (1 - myFriendBiomarker/myBiomarker)^2
                                    currentDistance    = currentDistance + currentBioDistance
                                    allDistancesVector[allDistancesIndex] = currentBioDistance
                                    allDistancesIndex = allDistancesIndex + 1
                            
                                }
                                
                            }
                    


                    }

                        # Write the results
                        distanceValuesDF[i,j] = currentDistance
                                                    
                    }
                
                }            
                
            }
            else{
            
                distanceValuesDF[i,] = 0
            
            }

        }
    
        # Finally get the total Score
        realTotalDistance   = sum(distanceValuesDF, na.rm=TRUE)
        realAverageDistance = sum(allDistancesVector) / sum(popularityTable$`Total Friends`)
        
    }
    # --

    # Now that we have the total distance, check it again againts the simulations
    # -- If the distance is greater in the simulation, it means we have a correlation    
    totalSimulations = 1000
    
    simulationAverages = rep(0, totalSimulations)
    
    for(z in 1:totalSimulations){
        
        # We need to create a random network with the same topology, for both
        # men and women, and repeat the whole thing again (cleaning out men only, with two friends, blah blah)
        
        # This vector will tells us how people make friends with random people
        randomIDVector = sample(completeTable$ID)
        
        # It goes like this:
        #
        # Original ID ----> New random ID
        #       1                 5
        #       2                 3
        #       3                 1
        #             ...
        #
        # Person 1 is still a man, with his current biomarkers and all of that.
        # but we change this:
        newRandomOverallDF = overallEdgesDF
        
        # You might get warnings here because there are people who don't nominate or get nobody nominations,
        # and thus are not in the original from or to vector
        newRandomOverallDF$from   = mapvalues(newRandomOverallDF$from, from = completeTable$ID, to = randomIDVector)
        newRandomOverallDF$to     = mapvalues(newRandomOverallDF$to,   from = completeTable$ID, to = randomIDVector)
        
        newRandomFriendshipMatrix = getFriendshipMatrix(newRandomOverallDF, totalPeople)
        
        newRandomOverallNetworkDF    = as.data.frame(newRandomFriendshipMatrix)
        newRandomOverallNetworkDF$ID = completeTable$ID
        
        
        # From here onwards this is just a copypaste of the previous code
        # This is shitty programing and need to functionarize this soon
        {
         
            
            
            
            # --
            
            # We need to stratify by sex first, so we only study men/women, with a valid
            # NDL biomarkers, that is not NA, and that has more than 15% above LOD, and
            # who has at least 2 friends of the same sex.
    
            # Original dataframe
            menBiomarkersNDLTable   = menOnlyTable[,c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]

            # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
            # We just need to check column 1 as the rest of the columns just follow along
            keepTheseMen          = !is.na(menBiomarkersNDLTable[,2])
            menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
            keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
            totalMen              = length(keepMenIDs)
            
            # We only want people who has 2 friends of the same sex and also have non-NA
            # values in their biomarkers.
            keepTheseMen = rep(TRUE, totalMen)
            for(j in 1:totalMen){

                currentID = menBiomarkersNDLTable$ID[j]
        
                # Find the friends for this ID with same sex
                currentSameSexFriends = unique(c(getFrienshipTypes(currentID, newRandomFriendshipMatrix)[[4]],
                                                 getFrienshipTypes(currentID, newRandomFriendshipMatrix)[[5]]))
                currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
                totalCurrentFriends   = length(currentSameSexFriends)

                # If you have 2 friends or more
                if(totalCurrentFriends < 2){
            
                    keepTheseMen[j] = FALSE
            
                }
         
            }
    
            menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
            keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
            totalMen              = length(keepMenIDs)
    
            # Finally, the 15% above LOD we don't care right now. We are going to do
            # all the variables, and then check for that later.
            
            
            # Make the list of friends for each person
            # This is superannoying to do in R since it doesn't have pointers. This is
            # completely unnaceptable performance wise, and this really need to be done in
            # C++. I need to start using Rcpp and we done with all this crap.
            {

                popularityTable = DF(totalMen,2)
                colnames(popularityTable) = c("ID", "Total Friends")
    
                popularityLists = newList(totalMen)
    
                for(k in 1:totalMen){
    
                    currentID = menBiomarkersNDLTable$ID[k]
            
                    # Get the friends of this person
                    myCurrentFriends     = getUndirectedFriends(currentID, newRandomOverallNetworkDF)
            
                    # Filter out people of different sex and people who are same sex
                    # but with NA values
                    myCurrentFriends = myCurrentFriends[myCurrentFriends %in% keepMenIDs]
            
                    # Get the total
                    totalCurrentFriends  = length(myCurrentFriends)
                    
                    # Make the list of friends, added to the big list, and register the total
                    popularityTable[k,1] = currentID
                    popularityTable[k,2] = totalCurrentFriends
        
                    if(totalCurrentFriends > 0){
                
                        popularityLists[[k]] = newList(totalCurrentFriends)
                    
                        for(m in 1:totalCurrentFriends){
                        
                            popularityLists[[k]][[m]] = myCurrentFriends[m]
                        
                        }
                        
                    }
        
                }
        
            }
            
            
            # Check average of friends biomarkers, this is to be compared with the simulations
            # The idea is to measure the total distance.
            {

            # Create a DF where we put all the results
            distanceValuesDF           = DF(totalMen, totalBiomarkers, defaultValue=0)
            colnames(distanceValuesDF) = colnames(completeTable)[NDLIndex:(NDLIndex + totalBiomarkers - 1)]
            allDistancesVector         = sum(popularityTable$`Total Friends`)
            allDistancesIndex          = 1
        
            # For each person, and for each biomarkers
            for(i in 1:totalMen){
            
                print(  paste0(z,":", round(100*i/totalMen,2))   )
            
                # Get ID
                myCurrentID = menBiomarkersNDLTable$ID[i]
            
                # Get the friends of this person
                myCurrentFriends    = popularityLists[[i]]
                totalCurrentFriends = popularityTable$`Total Friends`[i]
            
                # If you have more than one friend, find the total distance, otherwise, set it to 0
                if(totalCurrentFriends > 0){
                
                    # Let's find the average distance for each biomarker for each friend
                    for(j in 1:totalBiomarkers){
                    
                        # Only account for valid biomarkers, when you have more than 15%
                        # values bellow LOD, that is not a valid biomarker.
                        currentLODLost = generalLODStatisticsDF$Prt_Under_LOD[j]
                        if(currentLODLost < 0.15){
                            
                            myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                            currentDistance = 0
                            
                            # For each of my friends
                            for(k in 1:totalCurrentFriends){
                    
                                # Get the ID
                                myFriendID        = myCurrentFriends[[k]]
                                
                                # IDs are consecutive, and we only need to measure the edges once.
                                # So if your friend ID is smaller than you ID, skip it.
                                if(myFriendID < myCurrentID){
                                
                                    # Get your friend biomarker
                                    myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                    
                                    # We standarize the distance with respect my biomarker, consider to be 100%
                                    #
                                    # currentDistance   = currentDistance + (myBiomarker - myFriendBiomarker)^2
                                    #
                                    # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                    # which makes this code more unreadable
                                    if(!is.na(myFriendBiomarker)){
                            
                                        currentBioDistance = (1 - myFriendBiomarker/myBiomarker)^2
                                        currentDistance    = currentDistance + currentBioDistance
                                        allDistancesVector[allDistancesIndex] = currentBioDistance
                                        allDistancesIndex = allDistancesIndex + 1
                                
                                    }
                                    
                                }
                        
    
    
                        }
    
                            # Write the results
                            distanceValuesDF[i,j] = currentDistance
                                                        
                        }
                    
                    }            
                    
                }
                else{
                
                    distanceValuesDF[i,] = 0
                
                }
    
            }
    
        # Finally get the total Score
        simulatedTotalDistance   = sum(distanceValuesDF, na.rm=TRUE)
        simulatedAverageDistance = sum(allDistancesVector) / sum(popularityTable$`Total Friends`)
        
    }
            
            
            
            
            
            
            
            
               
        }
        
        simulationAverages[z] = simulatedAverageDistance
        
    }
 
    # Do the typical t-test as always
    simulationsMetaAverage = mean(simulationAverages)
    simulationsSD          = sd(simulationAverages)
    
    pValueBioSimulation    = pnorm(realAverageDistance, mean=simulationsMetaAverage, sd=simulationsSD, lower.tail=FALSE)
       
}


#HIATUS LNAM
{
# 
#         builtInLNAM(completeTable[,NDLIndex], completeTable,
#                             friendshipMatrix = NULL, devianceMatrix = NULL,
#                             epsilon = 0.01){    
    
}


# FML, that didn't work, let's try biomarkers one by one no normalization
{
    
    # We are going to check if your friends are closer to you via Biomarkers.
    
    # --
    # We need to stratify by sex first, so we only study men/women, with a valid
    # NDL biomarkers, that is not NA, and that has more than 15% above LOD, and
    # who has at least 2 friends of the same sex.
    
    # Original dataframe
    menBiomarkersNDLTable   = menOnlyTable[,c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]

    # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
    # We just need to check column 1 as the rest of the columns just follow along
    keepTheseMen          = !is.na(menBiomarkersNDLTable[,2])
    menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
    keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
    totalMen              = length(keepMenIDs)

    # We only want people who has 2 friends of the same sex and also have non-NA
    # values in their biomarkers.
    keepTheseMen = rep(TRUE, totalMen)
    for(j in 1:totalMen){

        currentID = menBiomarkersNDLTable$ID[j]
        
        # Find the friends for this ID with same sex
        currentSameSexFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                                         getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
        currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
        totalCurrentFriends   = length(currentSameSexFriends)

        # If you have 2 friends or more
        if(totalCurrentFriends < 2){
            
            keepTheseMen[j] = FALSE
            
        }
         
    }
    
    menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
    keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
    totalMen              = length(keepMenIDs)
    
    # Finally, the 15% above LOD we don't care right now. We are going to do
    # all the variables, and then check for that later.

    
    # Make the list of friends for each person
    # This is superannoying to do in R since it doesn't have pointers. This is
    # completely unnaceptable performance wise, and this really need to be done in
    # C++. I need to start using Rcpp and we done with all this crap.
    {

        popularityTable = DF(totalMen,2)
        colnames(popularityTable) = c("ID", "Total Friends")
    
        popularityLists = newList(totalMen)
    
        for(k in 1:totalMen){
    
            currentID = menBiomarkersNDLTable$ID[k]
            
            # Get the friends of this person
            myCurrentFriends     = getUndirectedFriends(currentID, overallNetworkDF)
            
            # Filter out people of different sex and people who are same sex
            # but with NA values
            myCurrentFriends = myCurrentFriends[myCurrentFriends %in% keepMenIDs]
            
            # Get the total
            totalCurrentFriends  = length(myCurrentFriends)
                    
            # Make the list of friends, added to the big list, and register the total
            popularityTable[k,1] = currentID
            popularityTable[k,2] = totalCurrentFriends
        
            if(totalCurrentFriends > 0){
        
                popularityLists[[k]] = newList(totalCurrentFriends)
            
                for(m in 1:totalCurrentFriends){
                
                    popularityLists[[k]][[m]] = myCurrentFriends[m]
                
                }
                
            }
        
    }
        
    }
    
    # Check average of friends biomarkers, this is to be compared with the simulations
    # The idea is to measure the total distance.
    {

        # Create a DF where we put all the results
        #distanceValuesDF           = DF(totalMen, totalBiomarkers, defaultValue=0)
        #colnames(distanceValuesDF) = colnames(completeTable)[NDLIndex:(NDLIndex + totalBiomarkers - 1)]
        allDistancesVector         = rep(0, totalBiomarkers)
        allDistancesIndex          = rep(1, totalBiomarkers)
        
        # For each person, and for each biomarkers
        for(i in 1:totalMen){
        
            print(   round(100*i/totalMen,2)   )
        
            # Get ID
            myCurrentID = menBiomarkersNDLTable$ID[i]
        
            # Get the friends of this person
            myCurrentFriends    = popularityLists[[i]]
            totalCurrentFriends = popularityTable$`Total Friends`[i]
        
            # If you have more than one friend, find the total distance, otherwise, set it to 0
            if(totalCurrentFriends > 0){
            
                # Let's find the average distance for each biomarker for each friend
                for(j in 1:totalBiomarkers){
                
                    # Only account for valid biomarkers, when you have more than 15%
                    # values bellow LOD, that is not a valid biomarker.
                    currentLODLost = generalLODStatisticsDF$Prt_Under_LOD[j]
                    if(currentLODLost < 0.15){
                        
                        myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                        currentDistance = 0
                        
                        # For each of my friends
                        for(k in 1:totalCurrentFriends){
                
                            # Get the ID
                            myFriendID        = myCurrentFriends[[k]]
                            
                            # IDs are consecutive, and we only need to measure the edges once.
                            # So if your friend ID is smaller than you ID, skip it.
                            if(myFriendID < myCurrentID){
                            
                                # Get your friend biomarker
                                myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                

                                #
                                # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                # which makes this code more unreadable
                                if(!is.na(myFriendBiomarker)){

                                    # We don't standarize the distance with respect my biomarker here
                                    #
                                    currentBioDistance = (myBiomarker - myFriendBiomarker)^2
                                                            
                                    #currentBioDistance = (1 - myFriendBiomarker/myBiomarker)^2
                                    #currentDistance    = currentDistance + currentBioDistance
                                    allDistancesVector[j] = allDistancesVector[j] + currentBioDistance
                                    allDistancesIndex[j]  = allDistancesIndex[j] + 1

                                }
                                
                            }
                    


                    }

                        # Write the results
                        #distanceValuesDF[i,j] = currentDistance
                                                    
                    }
                
                }            
                
            }
            # else{
            # 
            #     distanceValuesDF[i,] = 0
            # 
            # }

        }
    
        # Finally get the total Score
        #realTotalDistance   = sum(distanceValuesDF, na.rm=TRUE)
        #realAverageDistance = sum(allDistancesVector) / sum(popularityTable$`Total Friends`)
        
        realAverageDistanceVector = allDistancesVector/allDistancesIndex
        
        
        
    }
    # --

    # Now that we have the total distance, check it again againts the simulations
    # -- If the distance is greater in the simulation, it means we have a correlation    
    totalSimulations = 100
    
    simulationAveragesDF = DF(totalSimulations, totalBiomarkers)
    
    for(z in 1:totalSimulations){
        
        # We need to create a random network with the same topology, for both
        # men and women, and repeat the whole thing again (cleaning out men only, with two friends, blah blah)
        
        # This vector will tells us how people make friends with random people
        randomIDVector = sample(completeTable$ID)
        
        # It goes like this:
        #
        # Original ID ----> New random ID
        #       1                 5
        #       2                 3
        #       3                 1
        #             ...
        #
        # Person 1 is still a man, with his current biomarkers and all of that.
        # but we change this:
        newRandomOverallDF = overallEdgesDF
        
        # You might get warnings here because there are people who don't nominate or get nobody nominations,
        # and thus are not in the original from or to vector
        newRandomOverallDF$from   = mapvalues(newRandomOverallDF$from, from = completeTable$ID, to = randomIDVector)
        newRandomOverallDF$to     = mapvalues(newRandomOverallDF$to,   from = completeTable$ID, to = randomIDVector)
        
        newRandomFriendshipMatrix = getFriendshipMatrix(newRandomOverallDF, totalPeople)
        
        newRandomOverallNetworkDF    = as.data.frame(newRandomFriendshipMatrix)
        newRandomOverallNetworkDF$ID = completeTable$ID
        
        
        # From here onwards this is just a copypaste of the previous code
        # This is shitty programing and need to functionarize this soon
    
        
        
        #--
        
        
        # --
        # We need to stratify by sex first, so we only study men/women, with a valid
        # NDL biomarkers, that is not NA, and that has more than 15% above LOD, and
        # who has at least 2 friends of the same sex.
        
        # Original dataframe
        menBiomarkersNDLTable   = menOnlyTable[,c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]
    
        # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
        # We just need to check column 1 as the rest of the columns just follow along
        keepTheseMen          = !is.na(menBiomarkersNDLTable[,2])
        menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
        keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
        totalMen              = length(keepMenIDs)

        # We only want people who has 2 friends of the same sex and also have non-NA
        # values in their biomarkers.
        keepTheseMen = rep(TRUE, totalMen)
        for(j in 1:totalMen){
    
            currentID = menBiomarkersNDLTable$ID[j]
            
            # Find the friends for this ID with same sex
            currentSameSexFriends = unique(c(getFrienshipTypes(currentID, newRandomFriendshipMatrix)[[4]],
                                             getFrienshipTypes(currentID, newRandomFriendshipMatrix)[[5]]))
            currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
            totalCurrentFriends   = length(currentSameSexFriends)
    
            # If you have 2 friends or more
            if(totalCurrentFriends < 2){
                
                keepTheseMen[j] = FALSE
                
            }
             
        }
        
        menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
        keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
        totalMen              = length(keepMenIDs)
    
        # Finally, the 15% above LOD we don't care right now. We are going to do
        # all the variables, and then check for that later.

    
        # Make the list of friends for each person
        # This is superannoying to do in R since it doesn't have pointers. This is
        # completely unnaceptable performance wise, and this really need to be done in
        # C++. I need to start using Rcpp and we done with all this crap.
        {
    
            popularityTable = DF(totalMen,2)
            colnames(popularityTable) = c("ID", "Total Friends")
        
            popularityLists = newList(totalMen)
        
            for(k in 1:totalMen){
        
                currentID = menBiomarkersNDLTable$ID[k]
                
                # Get the friends of this person
                myCurrentFriends     = getUndirectedFriends(currentID, newRandomOverallNetworkDF)
                
                # Filter out people of different sex and people who are same sex
                # but with NA values
                myCurrentFriends = myCurrentFriends[myCurrentFriends %in% keepMenIDs]
                
                # Get the total
                totalCurrentFriends  = length(myCurrentFriends)
                        
                # Make the list of friends, added to the big list, and register the total
                popularityTable[k,1] = currentID
                popularityTable[k,2] = totalCurrentFriends
            
                if(totalCurrentFriends > 0){
            
                    popularityLists[[k]] = newList(totalCurrentFriends)
                
                    for(m in 1:totalCurrentFriends){
                    
                        popularityLists[[k]][[m]] = myCurrentFriends[m]
                    
                    }
                    
                }
            
            }
            
        }
    
        # Check average of friends biomarkers, this is to be compared with the simulations
        # The idea is to measure the total distance.
        {

        # Create a DF where we put all the results
        allDistancesVector         = rep(0, totalBiomarkers)
        allDistancesIndex          = rep(1, totalBiomarkers)
        
        # For each person, and for each biomarkers
        for(i in 1:totalMen){
        
            print(  paste0(z,":", round(100*i/totalMen,2))   )
            
            # Get ID
            myCurrentID = menBiomarkersNDLTable$ID[i]
        
            # Get the friends of this person
            myCurrentFriends    = popularityLists[[i]]
            totalCurrentFriends = popularityTable$`Total Friends`[i]
        
            # If you have more than one friend, find the total distance, otherwise, set it to 0
            if(totalCurrentFriends > 0){
            
                # Let's find the average distance for each biomarker for each friend
                for(j in 1:totalBiomarkers){
                
                    # Only account for valid biomarkers, when you have more than 15%
                    # values bellow LOD, that is not a valid biomarker.
                    currentLODLost = generalLODStatisticsDF$Prt_Under_LOD[j]
                    if(currentLODLost < 0.15){
                        
                        myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                        currentDistance = 0
                        
                        # For each of my friends
                        for(k in 1:totalCurrentFriends){
                
                            # Get the ID
                            myFriendID        = myCurrentFriends[[k]]
                            
                            # IDs are consecutive, and we only need to measure the edges once.
                            # So if your friend ID is smaller than you ID, skip it.
                            if(myFriendID < myCurrentID){
                            
                                # Get your friend biomarker
                                myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                

                                #
                                # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                # which makes this code more unreadable
                                if(!is.na(myFriendBiomarker)){

                                    # We don't standarize the distance with respect my biomarker here
                                    #
                                    currentBioDistance = (myBiomarker - myFriendBiomarker)^2
                                                            
                                    #currentBioDistance = (1 - myFriendBiomarker/myBiomarker)^2
                                    #currentDistance    = currentDistance + currentBioDistance
                                    allDistancesVector[j] = allDistancesVector[j] + currentBioDistance
                                    allDistancesIndex[j]  = allDistancesIndex[j] + 1

                                }
                                
                            }
                    


                    }

                        # Write the results
                        #distanceValuesDF[i,j] = currentDistance
                                                    
                    }
                
                }            
                
            }

        }
    
        # Finally get the total Score
        #realTotalDistance   = sum(distanceValuesDF, na.rm=TRUE)
        #realAverageDistance = sum(allDistancesVector) / sum(popularityTable$`Total Friends`)
        
        simulationAverageDistanceVector = allDistancesVector/allDistancesIndex
        
        simulationAveragesDF[z,] = simulationAverageDistanceVector
        
        
        }
        # --
        
        
    }
 
    # Do the typical t-test as always
    simulationsMetaAverageVector = rep(0,totalBiomarkers)
    simulationsSDVector          = rep(0,totalBiomarkers)
    pValueBioSimulationVector    = rep(0,totalBiomarkers)
    for(i in 1:totalBiomarkers){
        simulationsMetaAverageVector[i] = mean(simulationAveragesDF[,i])
        simulationsSDVector[i]          = sd(simulationAveragesDF[,i])
        pValueBioSimulationVector[i]    = pnorm(realAverageDistanceVector[i], mean=simulationsMetaAverageVector[i], sd=simulationsSDVector[i], lower.tail=FALSE)       
    }
    
    pValueBioSimulationVector[realAverageDistanceVector!=0]
    
    pValueBioSimulationVector[pValueBioSimulationVector[realAverageDistanceVector!=0] < 0.10]
    
    
    
    
        
        
        
        
        
        
        
        
        
        
        #--
    
        
    
}


#FML2 that didn't work either, try the all by all aproach
{

    # --
    # We need to stratify by sex first, so we only study men/women, with a valid
    # NDL biomarkers, that is not NA, and that has more than 15% above LOD, and
    # who has at least 2 friends of the same sex.
    
    # Original dataframe
    menBiomarkersNDLTable   = menOnlyTable[,c(1,NDLIndex:(NDLIndex+totalBiomarkers-1))]

    # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
    # We just need to check column 1 as the rest of the columns just follow along
    keepTheseMen          = !is.na(menBiomarkersNDLTable[,2])
    menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
    keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
    totalMen              = length(keepMenIDs)

    # We only want people who has 2 friends of the same sex and also have non-NA
    # values in their biomarkers.
    keepTheseMen = rep(TRUE, totalMen)
    for(j in 1:totalMen){

        currentID = menBiomarkersNDLTable$ID[j]
        
        # Find the friends for this ID with same sex
        currentSameSexFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                                         getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
        currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% keepMenIDs]
        totalCurrentFriends   = length(currentSameSexFriends)

        # If you have 2 friends or more
        if(totalCurrentFriends < 2){
            
            keepTheseMen[j] = FALSE
            
        }
         
    }
    
    menBiomarkersNDLTable = menBiomarkersNDLTable[keepTheseMen,]
    keepMenIDs            = menBiomarkersNDLTable[keepTheseMen,]$ID
    totalMen              = length(keepMenIDs)
    
    # Finally, the 15% above LOD we don't care right now. We are going to do
    # all the variables, and then check for that later.

    
    # Make the list of friends for each person
    # This is superannoying to do in R since it doesn't have pointers. This is
    # completely unnaceptable performance wise, and this really need to be done in
    # C++. I need to start using Rcpp and we done with all this crap.
    {

        popularityTable = DF(totalMen,2)
        colnames(popularityTable) = c("ID", "Total Friends")
    
        popularityLists = newList(totalMen)
    
        for(k in 1:totalMen){
    
            currentID = menBiomarkersNDLTable$ID[k]
            
            # Get the friends of this person
            myCurrentFriends     = getUndirectedFriends(currentID, overallNetworkDF)
            
            # Filter out people of different sex and people who are same sex
            # but with NA values
            myCurrentFriends = myCurrentFriends[myCurrentFriends %in% keepMenIDs]
            
            # Get the total
            totalCurrentFriends  = length(myCurrentFriends)
                    
            # Make the list of friends, added to the big list, and register the total
            popularityTable[k,1] = currentID
            popularityTable[k,2] = totalCurrentFriends
        
            if(totalCurrentFriends > 0){
        
                popularityLists[[k]] = newList(totalCurrentFriends)
            
                for(m in 1:totalCurrentFriends){
                
                    popularityLists[[k]][[m]] = myCurrentFriends[m]
                
                }
                
            }
        
    }
        
    }
    
    # Check average of friends biomarkers, this is to be compared with the simulations
    # The idea is to measure the total distance.
    {

        # Create a DF where we put all the results
        #distanceValuesDF           = DF(totalMen, totalBiomarkers, defaultValue=0)
        #colnames(distanceValuesDF) = colnames(completeTable)[NDLIndex:(NDLIndex + totalBiomarkers - 1)]
        allDistancesVector           = rep(0, totalBiomarkers)
        allDistancesIndex            = rep(1, totalBiomarkers)
        
        nonFriendsAllDistancesVector = rep(0, totalBiomarkers)
        nonFriendsAllDistancesIndex  = rep(1, totalBiomarkers)
        
        # For each person, and for each biomarkers
        for(i in 1:totalMen){
        
            print(   round(100*i/totalMen,2)   )
        
            # Get ID
            myCurrentID = menBiomarkersNDLTable$ID[i]
        
            # Get the friends of this person
            myCurrentFriends    = popularityLists[[i]]
            totalCurrentFriends = popularityTable$`Total Friends`[i]
        
            # If you have more than one friend, find the total distance, otherwise, set it to 0
            if(totalCurrentFriends > 0){
            
                # Let's find the average distance for each biomarker for each friend
                for(j in 1:totalBiomarkers){
                
                    # Only account for valid biomarkers, when you have more than 15%
                    # values bellow LOD, that is not a valid biomarker.
                    currentLODLost = generalLODStatisticsDF$Prt_Under_LOD[j]
                    if(currentLODLost < 0.15){
                        
                        myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                        currentDistance = 0
                        
                        # For each of my friends
                        for(k in 1:totalCurrentFriends){
                
                            # Get the ID
                            myFriendID        = myCurrentFriends[[k]]
                            
                            # IDs are consecutive, and we only need to measure the edges once.
                            # So if your friend ID is smaller than you ID, skip it.
                            if(myFriendID < myCurrentID){
                            
                                # Get your friend biomarker
                                myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                

                                #
                                # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                # which makes this code more unreadable
                                if(!is.na(myFriendBiomarker)){

                                    # We don't standarize the distance with respect my biomarker here
                                    #
                                    currentBioDistance = (myBiomarker - myFriendBiomarker)^2
                                                            
                                    #currentBioDistance = (1 - myFriendBiomarker/myBiomarker)^2
                                    #currentDistance    = currentDistance + currentBioDistance
                                    allDistancesVector[j] = allDistancesVector[j] + currentBioDistance
                                    allDistancesIndex[j]  = allDistancesIndex[j] + 1

                                }
                                
                            }
                    


                    }

                        # Write the results
                        #distanceValuesDF[i,j] = currentDistance
                                         
                        
                        # For everybody in the matrix
                        for(k in 1:totalMen){
                
                            # Get the ID
                            myNonFriendID = menBiomarkersNDLTable$ID[k]
                            
                            # IDs are consecutive, and we only need to measure the edges once.
                            # So if your friend ID is smaller than you ID, skip it.
                            if(myNonFriendID < myCurrentID){
                            
                                # Get your friend biomarker
                                myNonFriendBiomarker = completeTable[myNonFriendID,(NDLIndex+j-1)]    
                                

                                #
                                # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                # which makes this code more unreadable
                                if(!is.na(myFriendBiomarker)){

                                    # We don't standarize the distance with respect my biomarker here
                                    #
                                    currentBioDistance = (myBiomarker - myNonFriendBiomarker)^2
                                                            
                                    #currentBioDistance = (1 - myFriendBiomarker/myBiomarker)^2
                                    #currentDistance    = currentDistance + currentBioDistance
                                    nonFriendsAllDistancesVector[j] = nonFriendsAllDistancesVector[j] + currentBioDistance
                                    nonFriendsAllDistancesIndex[j]  = nonFriendsAllDistancesIndex[j]  + 1

                                }
                                
                            }
                            
                        }
                    


                    }
                        
                
                }            
                
            }
   

        }
    
        # Finally get the total Score
        #realTotalDistance   = sum(distanceValuesDF, na.rm=TRUE)
        #realAverageDistance = sum(allDistancesVector) / sum(popularityTable$`Total Friends`)
        
        realAverageDistanceVector = allDistancesVector/allDistancesIndex
        
        nonFriendsAverageDistanceVector = nonFriendsAllDistancesVector/nonFriendsAllDistancesIndex
        
        
        nonFriendsAverageDistanceVector/realAverageDistanceVector
        
    }
    
}
