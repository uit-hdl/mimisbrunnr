# In this script we take the ready to read .csv data and create another ready
# to read data which is filtered by whatever standards we deem necessary. Later
# on, we load this filtered data instead.


# Set working directory to file location and load libraries
# -----------------------------------------------------------------------------
{
  this.dir = dirname(parent.frame(2)$ofile)
  setwd(this.dir)
  
  source("constants.R", encoding="utf-8")
  source("analysis.R",  encoding="utf-8")
}

# Init the log
# -----------------------------------------------------------------------------
{
  logTXTFileConnection = file(FILTERING_LOG_PATH, 'w')
  logLine              = paste( "FILTERING DATA LOG at: ", RIGHT_NOW, sep = "")
  write( logLine ,
         file = logTXTFileConnection,
         append = FALSE)
  
  
  dataFilteringLogDF            = data.frame(matrix(NA, nrow = 2, ncol = 2))
  colnames(dataFilteringLogDF)  = c("Concept", "Total")
  dataFilteringLogDF$Concept[1] = "By age restrictions:"
  dataFilteringLogDF$Concept[2] = "By missing information:"
}

# Read the data into DFs
# -----------------------------------------------------------------------------
{
  
  # Individual tables and complete table
  phenotypeTable    = read.csv2(READY_DATA_PHENOTYPES_FILEPATH,  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  networkTable      = read.csv2(READY_DATA_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  aureusTable       = read.csv2(READY_DATA_AUREUS_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  medicineTable     = read.csv2(READY_DATA_MEDICINE_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  completeTable     = read.csv2(READY_DATA_COMPLETE_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  # Drugs and contraceptives
  drugsTable          = read.csv2(READY_DATA_DRUG_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  contraceptivesTable = read.csv2(READY_DATA_CONTRACEPTIVES_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  # Network
  overallNetworkDF  = read.csv2(READY_DATA_OVERALL_NETWORK_FILEPATH,  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  physicalNetworkDF = read.csv2(READY_DATA_PHYSICAL_NETWORK_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  homeNetworkDF     = read.csv2(READY_DATA_HOME_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  schoolNetworkDF   = read.csv2(READY_DATA_SCHOOL_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  sportsNetworkDF   = read.csv2(READY_DATA_SPORTS_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  otherNetworkDF    = read.csv2(READY_DATA_OTHERS_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  totalOriginalRows = nrow(phenotypeTable)
}

# Fix the column names in each network table
# So it goes from "X851" to "851"
# ( T 3 T ) whyyyy
# -----------------------------------------------------------------------------
{
  totalRows = nrow(overallNetworkDF)
  
  colnames(overallNetworkDF)  = c(c(1:totalRows),"ID")
  colnames(physicalNetworkDF) = c(c(1:totalRows),"ID")
  colnames(homeNetworkDF)     = c(c(1:totalRows),"ID")
  colnames(schoolNetworkDF)   = c(c(1:totalRows),"ID")
  colnames(sportsNetworkDF)   = c(c(1:totalRows),"ID")
  colnames(otherNetworkDF)    = c(c(1:totalRows),"ID")
}

# -----------------------------------------------------------------------------
# ALL FILTERS ARE APPLY HERE
# -----------------------------------------------------------------------------
{
  # Blacklisted IDs, there is no data in the row
  # (This code is for the TSD only)
  # missingDataRows = c(20056619)
  totalLostByLackOfData = 0
  
  # Filter by age
  {
    ageLimit       = 99
    rowsByAge      =    (completeTable$Age <= ageLimit)
    totalLostByAge = sum(completeTable$Age >  ageLimit)
    
    dataFilteringLogDF$Total[1] = totalLostByAge
    
  }
}
  
  
# -----------------------------------------------------------------------------
# RECONSTRUCT THE TABLES BASED ON THE INFO WE LOST
# -----------------------------------------------------------------------------

# IDs that are going to be deleted. If there is more than one, we need to
# reconstruct the entire network information for each variable.
{
  keepTheseRows   = rowsByAge        # a|b
  invalidIDs      = completeTable[!keepTheseRows,]$ID
  totalInvalidIDs = length(invalidIDs)  
}

# Remake all the basic tables
{
  # Basic tables
  NEWphenotypeTable = phenotypeTable[keepTheseRows,]
  NEWaureusTable    = aureusTable[keepTheseRows,]
  NEWmedicineTable  = medicineTable[keepTheseRows,]
  
  # Network table
  NEWnetworkTable   = networkTable[keepTheseRows,]
  
  # Relational tables
  # (We remove the invalid data later)
  NEWdrugUseDF        = drugUseDF
  NEWcontraceptivesDF = contraceptivesDF
  NEWdiseasesDF       = diseasesDF
  
  totalIDs = nrow(NEWphenotypeTable)
}

# If we didn't filter anything, then do nothing and keep the same tables.
# Otherwise, do the proper filtering
if(totalInvalidIDs == 0){
  
  # Assign the NEW tables to the OLD tables directly

  # Host
  NEWphenotypeTable   = phenotypeTable
  NEWaureusTable      = aureusTable
  NEWmedicineTable    = medicineTable
  NEWcompleteTable    = completeTable
  
  # Relational
  NEWdrugUseDF        = drugUseDF
  NEWcontraceptivesDF = contraceptivesDF
  NEWdiseasesDF       = diseasesDF
  
  # Networks
  NEWnetworkTable      = networkTable
  NEWoverallNetworkDF  = overallNetworkDF
  NEWphysicalNetworkDF = physicalNetworkDF
  NEWhomeNetworkDF     = homeNetworkDF
  NEWschoolNetworkDF   = schoolNetworkDF
  NEWsportsNetworkDF   = sportsNetworkDF
  NEWotherNetworkDF    = otherNetworkDF
  
}


else{

  
  # ----------------------------------------------------------------------------
  # From here we do the same as in data cleaning, only now we have a "new" table
  # ----------------------------------------------------------------------------
  {
    
    # Keep track of all key changes
    replacementKeysDF           = data.frame(matrix(NA, nrow = totalIDs, ncol = 2))
    colnames(replacementKeysDF) = c("Old_Key", "New_Key")
    
    # For each existing ID, keep track of how many friends are missing
    missingFriendsDF            = data.frame(matrix(NA, nrow = totalInvalidIDs, ncol = 7))
    colnames(missingFriendsDF)  = c("Key", "Popularity", "Nominations", "Reciprocal", "Internal Popularity", "Internal Nominations", "Internal Reciprocal")
    
    # We start by the invalid IDs that will change to 0
    # Later on, we replace the old keys to new keys, that coincidentally might get
    # the same ID as what we call invalid ID now, but that is just a coincidence,
    # and by the time that happens, it will be no possible confusion anymore.
    
    # Prepare the log for writing the keys deletions
    {
      logLine = paste("These keys were deleted in the filtering process: ")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      logLine = paste("    ----    ")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
    }
    
    # For each invalid ID, delete that ID from the friendship matrix, and keep
    # track of everything that was deleted for logging purposes.
    for(i in 1:totalInvalidIDs){
      
      # Save the current key
      replacementKey               = invalidIDs[i]
      
      # Write the current key in the log
      logLine = paste("    ", replacementKey)
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      
      # ---- Each one of the friends that now are loosing a friend
      #      For each of the IDs (i) replace it in the entire row for 0
      #
      #      Again, we have no idea in which order this might be written, so we
      #      need to do it not very efficiently and replace the entire column.
      #      (I hate you R, and your lack of pointers)
      {
        NEWnetworkTable$Friend1 = replace(NEWnetworkTable$Friend1, NEWnetworkTable$Friend1 == replacementKey, 0)
        NEWnetworkTable$Friend2 = replace(NEWnetworkTable$Friend2, NEWnetworkTable$Friend2 == replacementKey, 0)
        NEWnetworkTable$Friend3 = replace(NEWnetworkTable$Friend3, NEWnetworkTable$Friend3 == replacementKey, 0)
        NEWnetworkTable$Friend4 = replace(NEWnetworkTable$Friend4, NEWnetworkTable$Friend4 == replacementKey, 0)
        NEWnetworkTable$Friend5 = replace(NEWnetworkTable$Friend5, NEWnetworkTable$Friend5 == replacementKey, 0)      
      }
      
      # Update the rest of the information if needed in the networks
      {
        # -- Physical
        NEWnetworkTable$Friend1Physical = replace(NEWnetworkTable$Friend1Physical, NEWnetworkTable$Friend1 == 0, 0)
        NEWnetworkTable$Friend2Physical = replace(NEWnetworkTable$Friend2Physical, NEWnetworkTable$Friend2 == 0, 0)
        NEWnetworkTable$Friend3Physical = replace(NEWnetworkTable$Friend3Physical, NEWnetworkTable$Friend3 == 0, 0)
        NEWnetworkTable$Friend4Physical = replace(NEWnetworkTable$Friend4Physical, NEWnetworkTable$Friend4 == 0, 0)
        NEWnetworkTable$Friend5Physical = replace(NEWnetworkTable$Friend5Physical, NEWnetworkTable$Friend5 == 0, 0)
        # -- School
        NEWnetworkTable$Friend1School   = replace(NEWnetworkTable$Friend1School, NEWnetworkTable$Friend1 == 0, 0)
        NEWnetworkTable$Friend2School   = replace(NEWnetworkTable$Friend2School, NEWnetworkTable$Friend2 == 0, 0)
        NEWnetworkTable$Friend3School   = replace(NEWnetworkTable$Friend3School, NEWnetworkTable$Friend3 == 0, 0)
        NEWnetworkTable$Friend4School   = replace(NEWnetworkTable$Friend4School, NEWnetworkTable$Friend4 == 0, 0)
        NEWnetworkTable$Friend5School   = replace(NEWnetworkTable$Friend5School, NEWnetworkTable$Friend5 == 0, 0)
        # -- Sport
        NEWnetworkTable$Friend1Sport    = replace(NEWnetworkTable$Friend1Sport, NEWnetworkTable$Friend1 == 0, 0)
        NEWnetworkTable$Friend2Sport    = replace(NEWnetworkTable$Friend2Sport, NEWnetworkTable$Friend2 == 0, 0)
        NEWnetworkTable$Friend3Sport    = replace(NEWnetworkTable$Friend3Sport, NEWnetworkTable$Friend3 == 0, 0)
        NEWnetworkTable$Friend4Sport    = replace(NEWnetworkTable$Friend4Sport, NEWnetworkTable$Friend4 == 0, 0)
        NEWnetworkTable$Friend5Sport    = replace(NEWnetworkTable$Friend5Sport, NEWnetworkTable$Friend5 == 0, 0)
        # -- Home
        NEWnetworkTable$Friend1Home     = replace(NEWnetworkTable$Friend1Home, NEWnetworkTable$Friend1 == 0, 0)
        NEWnetworkTable$Friend2Home     = replace(NEWnetworkTable$Friend2Home, NEWnetworkTable$Friend2 == 0, 0)
        NEWnetworkTable$Friend3Home     = replace(NEWnetworkTable$Friend3Home, NEWnetworkTable$Friend3 == 0, 0)
        NEWnetworkTable$Friend4Home     = replace(NEWnetworkTable$Friend4Home, NEWnetworkTable$Friend4 == 0, 0)
        NEWnetworkTable$Friend5Home     = replace(NEWnetworkTable$Friend5Home, NEWnetworkTable$Friend5 == 0, 0)
        # -- Other
        NEWnetworkTable$Friend1Other    = replace(NEWnetworkTable$Friend1Other, NEWnetworkTable$Friend1 == 0, 0)
        NEWnetworkTable$Friend2Other    = replace(NEWnetworkTable$Friend2Other, NEWnetworkTable$Friend2 == 0, 0)
        NEWnetworkTable$Friend3Other    = replace(NEWnetworkTable$Friend3Other, NEWnetworkTable$Friend3 == 0, 0)
        NEWnetworkTable$Friend4Other    = replace(NEWnetworkTable$Friend4Other, NEWnetworkTable$Friend4 == 0, 0)
        NEWnetworkTable$Friend5Other    = replace(NEWnetworkTable$Friend5Other, NEWnetworkTable$Friend5 == 0, 0)    
      }
      
      # Finally, we track down how many connection we lost in this way.
      {
        myResults        = getFrienshipTypes(replacementKey, overallNetworkDF)
        myPopularity     = myResults[[1]]
        myNominations    = myResults[[2]]
        myReciprocals    = myResults[[3]]
        myPopularityIDs  = myResults[[4]]
        myNominationsIDs = myResults[[5]]
        myReciprocalsIDs = myResults[[6]]      
      }
      
      # Write it in the log the lost connections
      {
        logLine = paste("        Was popular with: ")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        logLine = paste("        ", myPopularityIDs)
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        logLine = paste("        Was nominating: ")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        logLine = paste("        ", myNominationsIDs)
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        logLine = paste("        Reciprocals: ")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        logLine = paste("        ", myReciprocalsIDs)
        write( logLine , file   = logTXTFileConnection, append = TRUE )    
      }
      
      # Write it in the dataframe log the new info
      # This is what we use later to plot the number of missing friends
      missingFriendsDF[i,1] = replacementKey
      missingFriendsDF[i,2] = myPopularity
      missingFriendsDF[i,3] = myNominations
      missingFriendsDF[i,4] = myReciprocals
      missingFriendsDF[i,5] = sum(invalidIDs %in% myPopularityIDs)
      missingFriendsDF[i,6] = sum(invalidIDs %in% myNominationsIDs)
      missingFriendsDF[i,7] = sum(invalidIDs %in% myReciprocalsIDs)
      
      # Update the diseases, drugs, and contraceptive table
      # ---- Drug use
      counterVariable     = nrow(NEWdrugUseDF)
      NEWdrugUseDF        = NEWdrugUseDF[NEWdrugUseDF$ID != replacementKey,]
      counterVariable     = counterVariable - nrow(NEWdrugUseDF)
      if(counterVariable>0){
        
        logLine = paste0("        Was using ", counterVariable, " drugs.")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        
      }
      
      
      # ---- Contraceptive use
      counterVariable     = nrow(NEWcontraceptivesDF)
      NEWcontraceptivesDF = NEWcontraceptivesDF[NEWcontraceptivesDF$ID != replacementKey,]
      counterVariable     = counterVariable - nrow(NEWcontraceptivesDF)
      if(counterVariable>0){
        
        logLine = paste0("        Was using ", counterVariable, " contraceptives.")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        
      }
      
      # ---- Disesases
      counterVariable     = nrow(NEWdiseasesDF)
      NEWdiseasesDF       = NEWdiseasesDF[NEWdiseasesDF$ID != replacementKey,]
      counterVariable     = counterVariable - nrow(NEWdiseasesDF)
      if(counterVariable>0){
        
        logLine = paste0("        Was having ", counterVariable, " diseases.")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        
      } 
      
      # Testing...
      # if(replacementKey == 142){
      #   
      #   print("142")
      #   print("------")
      #   print(myResults)
      #   print("------")
      #   print(myNominationsIDs)
      #   print(invalidIDs)
      #   print(invalidIDs %in% myNominationsIDs)
      #   print(sum(invalidIDs %in% myNominationsIDs))
      #   
      # }
      
    }
    
    
    # At this point, the new tables don't contain any invalid IDs anymore. We
    # just need to replace everything to consecutive IDs again. And recount how
    # many friends and popularity each person have.
    
    
    # Init the log for writing the keys replacements
    {
      logLine = paste("These keys replaced, from post cleaning ID to their new post filter ID: ")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      logLine = paste("    ----    ")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
    }
    
    # For each valid ID...
    for(i in 1:totalIDs){
      
      # Save the current key
      replacementKey               = NEWphenotypeTable$ID[i]
      
      # Set the new key in every table
      #
      #    (take into account that different tables might be sorted differently)
      #    (default sorting is the phenotype table, whatever that is)
      #
      # -- Phenotype table
      NEWphenotypeTable$ID[i] = i
      # -- Aureus table
      NEWaureusTable$ID       =  replace(NEWaureusTable$ID,   NEWaureusTable$ID    == replacementKey, i)
      # -- Medicine table
      NEWmedicineTable$ID     =  replace(NEWmedicineTable$ID, NEWmedicineTable$ID  == replacementKey, i)
      
      
      # -- Network table
      # ---- IDs
      NEWnetworkTable$ID = replace(NEWnetworkTable$ID, NEWnetworkTable$ID == replacementKey, i)
      # ---- Each one of the friends
      NEWnetworkTable$Friend1 = replace(NEWnetworkTable$Friend1, NEWnetworkTable$Friend1 == replacementKey, i)
      NEWnetworkTable$Friend2 = replace(NEWnetworkTable$Friend2, NEWnetworkTable$Friend2 == replacementKey, i)
      NEWnetworkTable$Friend3 = replace(NEWnetworkTable$Friend3, NEWnetworkTable$Friend3 == replacementKey, i)
      NEWnetworkTable$Friend4 = replace(NEWnetworkTable$Friend4, NEWnetworkTable$Friend4 == replacementKey, i)
      NEWnetworkTable$Friend5 = replace(NEWnetworkTable$Friend5, NEWnetworkTable$Friend5 == replacementKey, i)
      
      
      # Each of the relational tables
      # ---- The disease table
      NEWdiseasesDF$ID        = replace(NEWdiseasesDF$ID,       NEWdiseasesDF$ID       == replacementKey, i)
      # ---- The medicine table
      NEWdrugUseDF$ID         = replace(NEWdrugUseDF$ID,        NEWdrugUseDF$ID        == replacementKey, i)
      # ---- The contraceptive table
      NEWcontraceptivesDF$ID  = replace(NEWcontraceptivesDF$ID, NEWcontraceptivesDF$ID == replacementKey, i)
      
      
      # Write that in the log
      
      # ---- txt
      logLine = paste(replacementKey, " -> ", i)
      write( logLine ,
             file   = logTXTFileConnection,
             append = TRUE )
      
      # ---- dataframe
      replacementKeysDF$Old_Key[i] = replacementKey
      replacementKeysDF$New_Key[i] = i
      
    }
    
    # Empty line for the txt log
    logLine = paste("    ----    ")
    write( logLine , file   = logTXTFileConnection, append = TRUE )
    
    
  }
  
  
  # ----------------------------------------------------------------------------
  # We need to create the 6 network from scratch again, same as in data cleaning
  # ----------------------------------------------------------------------------
  {
    
    # At this point we have the NEWnetworkTable ready.
    #
    # Remember that:
    #
    # In Friend1, Friend2,...Friend 5 we have an ID
    # In Friend1Phyisical, Friend2Physical... we have a boolean.
    
    
    # ---- Transform the network table into proper frienship matrix
    #      There are 6 graphs, all of them directed.
    #      -- Overall friendship
    #      -- Physical
    #      -- Home
    #      -- School
    #      -- Sports
    #      -- Others
    
    # Create the skeleton dataframe for each network
    {
      # Check all the IDs
      NEWnetworkIDs = NEWphenotypeTable$ID
      totalIDs      = length(NEWnetworkIDs)
      
      # Create the basic DF from where later we will create the edges data frames
      NEWoverallNetworkDF    = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      NEWphysicalNetworkDF   = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      NEWhomeNetworkDF       = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      NEWschoolNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      NEWsportsNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      NEWotherNetworkDF      = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      
      # Ensure that the rows and the columns refers to the same ID; for each of the networks we have
      # -- Overall
      NEWoverallNetworkDF$ID                     = NEWnetworkIDs
      colnames(NEWoverallNetworkDF)[1:totalIDs]  = NEWnetworkIDs
      # -- Physical
      NEWphysicalNetworkDF$ID                    = NEWnetworkIDs
      colnames(NEWphysicalNetworkDF)[1:totalIDs] = NEWnetworkIDs
      # -- Home
      NEWhomeNetworkDF$ID                        = NEWnetworkIDs
      colnames(NEWhomeNetworkDF)[1:totalIDs]     = NEWnetworkIDs
      # -- School
      NEWschoolNetworkDF$ID                      = NEWnetworkIDs
      colnames(NEWschoolNetworkDF)[1:totalIDs]   = NEWnetworkIDs
      # -- Sports
      NEWsportsNetworkDF$ID                      = NEWnetworkIDs
      colnames(NEWsportsNetworkDF)[1:totalIDs]   = NEWnetworkIDs
      # -- Others
      NEWotherNetworkDF$ID                       = NEWnetworkIDs
      colnames(NEWotherNetworkDF)[1:totalIDs]    = NEWnetworkIDs
      
    }
    
    # For each of the people in the network
    # Fill all their edges information
    for(i in 1:totalIDs){
      
      # Identify each of your friends
      FriendA = NEWnetworkTable$Friend1[i]
      FriendB = NEWnetworkTable$Friend2[i]
      FriendC = NEWnetworkTable$Friend3[i]
      FriendD = NEWnetworkTable$Friend4[i]
      FriendE = NEWnetworkTable$Friend5[i]
      
      # For each of the network, add that friendship link if it exist
      # -- The overall network doesn't care about anything, everyone comes here
      if(FriendA > 0) NEWoverallNetworkDF[i, FriendA] = 1
      if(FriendB > 0) NEWoverallNetworkDF[i, FriendB] = 1
      if(FriendC > 0) NEWoverallNetworkDF[i, FriendC] = 1
      if(FriendD > 0) NEWoverallNetworkDF[i, FriendD] = 1
      if(FriendE > 0) NEWoverallNetworkDF[i, FriendE] = 1
      # -- Physical
      if(FriendA > 0 && NEWnetworkTable$Friend1Physical[i] == 1) NEWphysicalNetworkDF[i, FriendA] = 1
      if(FriendB > 0 && NEWnetworkTable$Friend2Physical[i] == 1) NEWphysicalNetworkDF[i, FriendB] = 1
      if(FriendC > 0 && NEWnetworkTable$Friend3Physical[i] == 1) NEWphysicalNetworkDF[i, FriendC] = 1
      if(FriendD > 0 && NEWnetworkTable$Friend4Physical[i] == 1) NEWphysicalNetworkDF[i, FriendD] = 1
      if(FriendE > 0 && NEWnetworkTable$Friend5Physical[i] == 1) NEWphysicalNetworkDF[i, FriendE] = 1
      # -- Home
      if(FriendA > 0 && NEWnetworkTable$Friend1Home[i] == 1) NEWhomeNetworkDF[i, FriendA] = 1
      if(FriendB > 0 && NEWnetworkTable$Friend2Home[i] == 1) NEWhomeNetworkDF[i, FriendB] = 1
      if(FriendC > 0 && NEWnetworkTable$Friend3Home[i] == 1) NEWhomeNetworkDF[i, FriendC] = 1
      if(FriendD > 0 && NEWnetworkTable$Friend4Home[i] == 1) NEWhomeNetworkDF[i, FriendD] = 1
      if(FriendE > 0 && NEWnetworkTable$Friend5Home[i] == 1) NEWhomeNetworkDF[i, FriendE] = 1
      # -- School
      if(FriendA > 0 && NEWnetworkTable$Friend1School[i] == 1) NEWschoolNetworkDF[i, FriendA] = 1
      if(FriendB > 0 && NEWnetworkTable$Friend2School[i] == 1) NEWschoolNetworkDF[i, FriendB] = 1
      if(FriendC > 0 && NEWnetworkTable$Friend3School[i] == 1) NEWschoolNetworkDF[i, FriendC] = 1
      if(FriendD > 0 && NEWnetworkTable$Friend4School[i] == 1) NEWschoolNetworkDF[i, FriendD] = 1
      if(FriendE > 0 && NEWnetworkTable$Friend5School[i] == 1) NEWschoolNetworkDF[i, FriendE] = 1
      # -- Sports
      if(FriendA > 0 && NEWnetworkTable$Friend1Sport[i] == 1) NEWsportsNetworkDF[i, FriendA] = 1
      if(FriendB > 0 && NEWnetworkTable$Friend2Sport[i] == 1) NEWsportsNetworkDF[i, FriendB] = 1
      if(FriendC > 0 && NEWnetworkTable$Friend3Sport[i] == 1) NEWsportsNetworkDF[i, FriendC] = 1
      if(FriendD > 0 && NEWnetworkTable$Friend4Sport[i] == 1) NEWsportsNetworkDF[i, FriendD] = 1
      if(FriendE > 0 && NEWnetworkTable$Friend5Sport[i] == 1) NEWsportsNetworkDF[i, FriendE] = 1
      # -- Others
      if(FriendA > 0 && NEWnetworkTable$Friend1Other[i] == 1) NEWotherNetworkDF[i, FriendA] = 1
      if(FriendB > 0 && NEWnetworkTable$Friend2Other[i] == 1) NEWotherNetworkDF[i, FriendB] = 1
      if(FriendC > 0 && NEWnetworkTable$Friend3Other[i] == 1) NEWotherNetworkDF[i, FriendC] = 1
      if(FriendD > 0 && NEWnetworkTable$Friend4Other[i] == 1) NEWotherNetworkDF[i, FriendD] = 1
      if(FriendE > 0 && NEWnetworkTable$Friend5Other[i] == 1) NEWotherNetworkDF[i, FriendE] = 1
      
    }
    
    # Now that you have the edges, you can initialize the edge counting you left blank before
    for(i in 1:totalIDs){
      
      # Print the progress bar
      print(getProgressCharacters((100*i)/totalIDs))
      print("")
      print("Initializing networks, please wait...")
      
      # -- For the overall
      {
        
        # Get the list of everyone that likes me
        popularThisIDsOverall                   = NEWoverallNetworkDF[ (NEWoverallNetworkDF[,i] == 1), ]$ID
        NEWphenotypeTable$OverallPopularity[i]  = length(popularThisIDsOverall)
        
        # Get the list of everyone I like
        followingThisIDsOverall                 = NEWoverallNetworkDF[NEWoverallNetworkDF[i,1:totalIDs] == 1,]$ID
        NEWphenotypeTable$OverallFollowing [i]  = length(followingThisIDsOverall)
        
        # Check the union of both sets for all relationship
        allRelationshipsOverall                 = union( popularThisIDsOverall, followingThisIDsOverall )
        totalAllRelationshipsOverall            = length(allRelationshipsOverall)
        NEWphenotypeTable$OverallConnections[i] = totalAllRelationshipsOverall
        
        # Check the intersection of both sets for reciprocity
        realciprocallRelationshipsOverall       = intersect( popularThisIDsOverall, followingThisIDsOverall )
        NEWphenotypeTable$OverallReciprocity[i] = length(realciprocallRelationshipsOverall)
        
      }
      
      # -- Physical
      {
        
        popularThisIDs                           = NEWphysicalNetworkDF[    (NEWphysicalNetworkDF[,i] == 1), ]$ID
        NEWphenotypeTable$PhysicalPopularity[i]  = length(popularThisIDs)
        followingThisIDs                         = NEWphysicalNetworkDF[NEWphysicalNetworkDF[i,1:totalIDs] == 1,]$ID
        NEWphenotypeTable$PhysicalFollowing [i]  = length(followingThisIDs)
        allRelationshipsPhysical                 = union( popularThisIDs, followingThisIDs )
        totalAllRelationshipsPhysical            = length(allRelationshipsPhysical)
        NEWphenotypeTable$PhysicalConnections[i] = totalAllRelationshipsPhysical
        realciprocallRelationships               = intersect( popularThisIDs, followingThisIDs )
        NEWphenotypeTable$PhysicalReciprocity[i] = length(realciprocallRelationships)
        
      }
      
      # -- Home
      {
        
        popularThisIDs                       = NEWhomeNetworkDF[    (NEWhomeNetworkDF[,i] == 1), ]$ID
        NEWphenotypeTable$HomePopularity[i]  = length(popularThisIDs)
        followingThisIDs                     = NEWhomeNetworkDF[NEWhomeNetworkDF[i,1:totalIDs] == 1,]$ID
        NEWphenotypeTable$HomeFollowing [i]  = length(followingThisIDs)
        allRelationshipsHome                 = union( popularThisIDs, followingThisIDs )
        totalAllRelationshipsHome            = length(allRelationshipsHome)
        NEWphenotypeTable$HomeConnections[i] = totalAllRelationshipsHome
        realciprocallRelationships           = intersect( popularThisIDs, followingThisIDs )
        NEWphenotypeTable$HomeReciprocity[i] = length(realciprocallRelationships)
        
      }
      
      # -- School
      {
        
        popularThisIDs                         = NEWschoolNetworkDF[  (NEWschoolNetworkDF[,i] == 1), ]$ID
        NEWphenotypeTable$SchoolPopularity[i]  = length(popularThisIDs)
        followingThisIDs                       = NEWschoolNetworkDF[NEWschoolNetworkDF[i,1:totalIDs] == 1,]$ID
        NEWphenotypeTable$SchoolFollowing [i]  = length(followingThisIDs)
        allRelationshipsSchool                 = union( popularThisIDs, followingThisIDs )
        totalAllRelationshipsSchool            = length(allRelationshipsSchool)
        NEWphenotypeTable$SchoolConnections[i] = totalAllRelationshipsSchool
        realciprocallRelationships             = intersect( popularThisIDs, followingThisIDs )
        NEWphenotypeTable$SchoolReciprocity[i] = length(realciprocallRelationships)
        
      }
      
      # -- Sport
      {
        
        popularThisIDs                         = NEWsportsNetworkDF[    (NEWsportsNetworkDF[,i] == 1), ]$ID
        NEWphenotypeTable$SportsPopularity[i]  = length(popularThisIDs)
        followingThisIDs                       = NEWsportsNetworkDF[NEWsportsNetworkDF[i,1:totalIDs] == 1,]$ID
        NEWphenotypeTable$SportsFollowing [i]  = length(followingThisIDs)
        allRelationshipsSports                 = union( popularThisIDs, followingThisIDs )
        totalAllRelationshipsSports            = length(allRelationshipsSports)
        NEWphenotypeTable$SportsConnections[i] = totalAllRelationshipsSports
        realciprocallRelationships             = intersect( popularThisIDs, followingThisIDs )
        NEWphenotypeTable$SportsReciprocity[i] = length(realciprocallRelationships)
        
      }
      
      # -- Others
      {
        
        popularThisIDs                        = NEWotherNetworkDF[    (NEWotherNetworkDF[,i] == 1), ]$ID
        NEWphenotypeTable$OtherPopularity[i]  = length(popularThisIDs)
        followingThisIDs                      = NEWotherNetworkDF[NEWotherNetworkDF[i,1:totalIDs] == 1,]$ID
        NEWphenotypeTable$OtherFollowing [i]  = length(followingThisIDs)
        allRelationshipsOthers                = union( popularThisIDs, followingThisIDs )
        totalAllRelationshipsOthers           = length(allRelationshipsOthers)
        NEWphenotypeTable$OtherConnections[i] = totalAllRelationshipsOthers
        realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
        NEWphenotypeTable$OtherReciprocity[i] = length(realciprocallRelationships)
        
      }
      
      # -- Specials
      {
        
        # ---- For each person in each of my relationships
        
        # -------- Overall
        {
          totalSANasal  = 0
          totalSAThroat = 0
          totalSA       = 0 
          
          # If you have more than zero friend
          if(totalAllRelationshipsOverall > 0){
            
            # For each friend
            for(j in 1:totalAllRelationshipsOverall){
              
              # Get your friend
              myFriendID = allRelationshipsOverall[j]
              
              # Check if he/she has SA
              SAStatusNasal  = NEWaureusTable$E_NasalCarrier[myFriendID]
              SAStatusThroat = NEWaureusTable$E_ThroatCarrier[myFriendID]
              SAStatus       = NEWaureusTable$E_Carrier[myFriendID]
              
              # Add to the amount of infected
              if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
              if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
              if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
              
            }
            
            totalSANasal  = totalSANasal  / totalAllRelationshipsOverall
            totalSAThroat = totalSAThroat / totalAllRelationshipsOverall
            totalSA       = totalSA       / totalAllRelationshipsOverall
            
          }
          
          NEWphenotypeTable$OverallFriendsWithSANasal[i]  = totalSANasal
          NEWphenotypeTable$OverallFriendsWithSAThroat[i] = totalSAThroat
          NEWphenotypeTable$OverallFriendsWithSA[i]       = totalSA
          
        }
        
        # -------- Physical
        {
          totalSANasal  = 0
          totalSAThroat = 0
          totalSA       = 0 
          
          # If you have more than zero friend
          if(totalAllRelationshipsPhysical > 0){
            
            # For each friend
            for(j in 1:totalAllRelationshipsPhysical){
              
              # Get your friend
              myFriendID = allRelationshipsPhysical[j]
              
              # Check if he/she has SA
              SAStatusNasal  = NEWaureusTable$E_NasalCarrier[myFriendID]
              SAStatusThroat = NEWaureusTable$E_ThroatCarrier[myFriendID]
              SAStatus       = NEWaureusTable$E_Carrier[myFriendID]
              
              # Add to the amount of infected
              if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
              if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
              if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
              
            }
            
            totalSANasal  = totalSANasal  / totalAllRelationshipsPhysical
            totalSAThroat = totalSAThroat / totalAllRelationshipsPhysical
            totalSA       = totalSA       / totalAllRelationshipsPhysical
            
          }
          
          NEWphenotypeTable$PhysicalFriendsWithSANasal[i]  = totalSANasal
          NEWphenotypeTable$PhysicalFriendsWithSAThroat[i] = totalSAThroat
          NEWphenotypeTable$PhysicalFriendsWithSA[i]       = totalSA
          
        }
        
        # -------- Home
        {
          totalSANasal  = 0
          totalSAThroat = 0
          totalSA       = 0 
          
          # If you have more than zero friend
          if(totalAllRelationshipsHome > 0){
            
            # For each friend
            for(j in 1:totalAllRelationshipsHome){
              
              # Get your friend
              myFriendID = allRelationshipsHome[j]
              
              # Check if he/she has SA
              SAStatusNasal  = NEWaureusTable$E_NasalCarrier[myFriendID]
              SAStatusThroat = NEWaureusTable$E_ThroatCarrier[myFriendID]
              SAStatus       = NEWaureusTable$E_Carrier[myFriendID]
              
              # Add to the amount of infected
              if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
              if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
              if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
              
            }
            
            totalSANasal  = totalSANasal  / totalAllRelationshipsHome
            totalSAThroat = totalSAThroat / totalAllRelationshipsHome
            totalSA       = totalSA       / totalAllRelationshipsHome
            
          }
          
          NEWphenotypeTable$HomeFriendsWithSANasal[i]  = totalSANasal
          NEWphenotypeTable$HomeFriendsWithSAThroat[i] = totalSAThroat
          NEWphenotypeTable$HomeFriendsWithSA[i]       = totalSA
          
        }
        
        # -------- School
        {
          totalSANasal  = 0
          totalSAThroat = 0
          totalSA       = 0 
          
          # If you have more than zero friend
          if(totalAllRelationshipsSchool > 0){
            
            # For each friend
            for(j in 1:totalAllRelationshipsSchool){
              
              # Get your friend
              myFriendID = allRelationshipsSchool[j]
              
              # Check if he/she has SA
              SAStatusNasal  = NEWaureusTable$E_NasalCarrier[myFriendID]
              SAStatusThroat = NEWaureusTable$E_ThroatCarrier[myFriendID]
              SAStatus       = NEWaureusTable$E_Carrier[myFriendID]
              
              # Add to the amount of infected
              if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
              if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
              if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
              
            }
            
            totalSANasal  = totalSANasal  / totalAllRelationshipsSchool
            totalSAThroat = totalSAThroat / totalAllRelationshipsSchool
            totalSA       = totalSA       / totalAllRelationshipsSchool
            
          }
          
          NEWphenotypeTable$SchoolFriendsWithSANasal[i]  = totalSANasal
          NEWphenotypeTable$SchoolFriendsWithSAThroat[i] = totalSAThroat
          NEWphenotypeTable$SchoolFriendsWithSA[i]       = totalSA
          
        }
        
        # -------- Sports
        {
          totalSANasal  = 0
          totalSAThroat = 0
          totalSA       = 0 
          
          # If you have more than zero friend
          if(totalAllRelationshipsSports > 0){
            
            # For each friend
            for(j in 1:totalAllRelationshipsSports){
              
              # Get your friend
              myFriendID = allRelationshipsSports[j]
              
              # Check if he/she has SA
              SAStatusNasal  = NEWaureusTable$E_NasalCarrier[myFriendID]
              SAStatusThroat = NEWaureusTable$E_ThroatCarrier[myFriendID]
              SAStatus       = NEWaureusTable$E_Carrier[myFriendID]
              
              # Add to the amount of infected
              if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
              if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
              if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
              
            }
            
            totalSANasal  = totalSANasal  / totalAllRelationshipsSports
            totalSAThroat = totalSAThroat / totalAllRelationshipsSports
            totalSA       = totalSA       / totalAllRelationshipsSports
            
          }
          
          NEWphenotypeTable$SportsFriendsWithSANasal[i]  = totalSANasal
          NEWphenotypeTable$SportsFriendsWithSAThroat[i] = totalSAThroat
          NEWphenotypeTable$SportsFriendsWithSA[i]       = totalSA
          
        }
        
        # -------- Others
        {
          totalSANasal  = 0
          totalSAThroat = 0
          totalSA       = 0 
          
          # If you have more than zero friend
          if(totalAllRelationshipsOthers > 0){
            
            # For each friend
            for(j in 1:totalAllRelationshipsOthers){
              
              # Get your friend
              myFriendID = allRelationshipsOthers[j]
              
              # Check if he/she has SA
              SAStatusNasal  = NEWaureusTable$E_NasalCarrier[myFriendID]
              SAStatusThroat = NEWaureusTable$E_ThroatCarrier[myFriendID]
              SAStatus       = NEWaureusTable$E_Carrier[myFriendID]
              
              # Add to the amount of infected
              if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
              if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
              if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
              
            }
            
            totalSANasal  = totalSANasal  / totalAllRelationshipsOthers
            totalSAThroat = totalSAThroat / totalAllRelationshipsOthers
            totalSA       = totalSA       / totalAllRelationshipsOthers
            
          }
          
          NEWphenotypeTable$OthersFriendsWithSANasal[i]  = totalSANasal
          NEWphenotypeTable$OthersFriendsWithSAThroat[i] = totalSAThroat
          NEWphenotypeTable$OthersFriendsWithSA[i]       = totalSA
          
        }
        
      }
      
    }
    
    
  }
  
  # Join tables with proper info together
  # -----------------------------------------------------------------------------
  {
    NEWcompleteTable          = NEWphenotypeTable %>% left_join(NEWaureusTable ,   by="ID")
    NEWcompleteTable          = NEWcompleteTable  %>% left_join(NEWmedicineTable , by="ID")
  
    NEWcompleteTable$Overview = NEWnetworkTable$Overwiew  # 0 to 10, how good this network describes your life
  }
    
}

# Write them into the filter place
# Now the update is finish. Write the CSVs into disk so we don't have to do this
# over and over again everytime we run the scripts
{
  
  # The phenotype table
  # The network   table
  # The aureus    table
  
  # The complete  table
  write.csv2(NEWphenotypeTable, file = FILTER_DATA_PHENOTYPES_FILEPATH, row.names = FALSE)
  write.csv2(NEWaureusTable,    file = FILTER_DATA_AUREUS_FILEPATH,     row.names = FALSE)
  write.csv2(NEWmedicineTable,  file = FILTER_DATA_MEDICINE_FILEPATH,   row.names = FALSE)
  
  write.csv2(NEWcompleteTable,  file = FILTER_DATA_COMPLETE_FILEPATH,   row.names = FALSE)
  
  # The relational tables
  write.csv2(NEWdrugUseDF,        file = FILTER_DATA_DRUG_FILEPATH,           row.names = FALSE, fileEncoding="UTF-8")
  write.csv2(NEWcontraceptivesDF, file = FILTER_DATA_CONTRACEPTIVES_FILEPATH, row.names = FALSE, fileEncoding="UTF-8")
  write.csv2(NEWdiseasesDF,       file = FILTER_DATA_DISEASES_FILEPATH,       row.names = FALSE, fileEncoding="UTF-8")

  
  # The overall  friendship matrix
  # The physical friendship matrix
  # The home     friendship matrix
  # The school   friendship matrix
  # The sport    friendship matrix
  # The other    friendship matrix
  write.csv2(NEWnetworkTable,   file = FILTER_DATA_NETWORK_FILEPATH,    row.names = FALSE)
  
  write.csv2(NEWoverallNetworkDF,  file = FILTER_DATA_OVERALL_NETWORK_FILEPATH   , row.names = FALSE)
  write.csv2(NEWphysicalNetworkDF, file = FILTER_DATA_PHYSICAL_NETWORK_FILEPATH , row.names = FALSE)
  write.csv2(NEWhomeNetworkDF,     file = FILTER_DATA_HOME_NETWORK_FILEPATH      , row.names = FALSE)
  write.csv2(NEWschoolNetworkDF,   file = FILTER_DATA_SCHOOL_NETWORK_FILEPATH    , row.names = FALSE)
  write.csv2(NEWsportsNetworkDF,   file = FILTER_DATA_SPORTS_NETWORK_FILEPATH    , row.names = FALSE)
  write.csv2(NEWotherNetworkDF,    file = FILTER_DATA_OTHERS_NETWORK_FILEPATH    , row.names = FALSE)
  
}

# Close the TXT connections
close(logTXTFileConnection)

# Write the Latex tables with respect the data that we missed
#writeGenericTableLATEX(logDF, LOGS_FOLDER, tableCaption = "Summary of lost connections.")

writeTableLATEX(dataFilteringLogDF, LOGS_FOLDER, tableCaption = "Summary of lost connections at filtering.")

# Plot the boxplots for missing data
if(totalInvalidIDs != 0){
  myYMaximum = max( max(missingFriendsDF$Popularity) , max(missingFriendsDF$Nominations) )
  
  doBoxPlot(missingFriendsDF, 3, LOGS_FOLDER, 
            colorsVector = COLOR_RED_MED,
            plotTitle    = "Nominations lost during filtering; from people remaining, to dissapeared people.",
            plotSubtitle = "",
            plotXLabel   = "Missing Friends", plotYLabel = "Total",
            plotTheme    = "simple",
            ymin = 0, ymax = myYMaximum,
            overrideCaption = "Boxplot for number of missing edges from existing keys to deleted keys during filtering.")
  
  doBoxPlot(missingFriendsDF, 2, LOGS_FOLDER, 
            colorsVector = COLOR_BLUE_MED,
            plotTitle    = "Popularity of people that have dissapear during filtering.",
            plotSubtitle = "",
            plotXLabel   = "Missing Friends", plotYLabel = "Total",
            plotTheme    = "simple",
            ymin = 0, ymax = myYMaximum,
            overrideCaption = "Boxplot for number of missing edges by existing keys to deleted keys during filtering.")
  
  
}

# Update the latex folder
source("latex.R", encoding="utf-8")

print("Data filtering completed!")

if(totalInvalidIDs == 0) print("No filter was necessary!")
