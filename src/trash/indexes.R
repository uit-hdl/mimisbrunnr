# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

# MAKE SURE THAT YOU HAVE LOADED THE DATA !!!!

# Individual indexes
{

  # -- Phenotypes
  {
    # ---- General Indexes
    IDIndex          = grep("ID",               colnames(completeTable))
    sexIndex         = grep("Sex",              colnames(completeTable))
    #painIndex       = grep("Pain_Resistance",  colnames(phenotypeTable))
    smokeIndex       = grep("Smoke",            colnames(completeTable))
    snuffIndex       = grep("Snuff",            colnames(completeTable))
    alcoholIndex     = grep("^Alcohol$",        colnames(completeTable))
    schoolIndex      = grep("^School$",         colnames(completeTable))
    ageIndex         = grep("^Age$",            colnames(completeTable))
    BMIIndex         = grep("^BMI$",            colnames(completeTable))
    BMICatIndex      = grep("^BMICategorical$", colnames(completeTable))
    AntibioticsIndex = grep("Antibiotics",      colnames(completeTable))
    AntiBranchIndex  = grep("^AntiBrand$",      colnames(completeTable))
    sportsIndex      = grep("^Sports$",         colnames(completeTable))
    activityIndex    = grep("^Active$",         colnames(completeTable))

    highSchoolIndex  = grep("^HighSchoolID$",   colnames(completeTable))
    classIndex       = grep("^Class$",          colnames(completeTable))
    
    #hormonalIndex    = grep("^Class$",          colnames(completeTable))
    
    # ---- Pain Indexes
    overallPainAvgIndex = grep("^OverallFollowingPainAverage$", colnames(completeTable))

    # ---- Special anchor for doing network automatically
    baseIndex                   = overallPainAvgIndex
    
    # ---- Special anchor for doing SA automatically
    SABaseIndex    = grep("^S1_C_NasalColonize$",          colnames(completeTable))
      
  }

  # -- S. Aureus
  {
    
    # ---- Did something grow?
    nasalGrowthIndex            = grep("^NasalGrowth$",            colnames(completeTable))
    throatGrowthIndex           = grep("^ThroatGrowth$",           colnames(completeTable))
    # ---- Is present?
    nasalAureusIndex            = grep("^NasalAureus$",            colnames(completeTable))
    throatAureusIndex           = grep("^ThroatAureus$",           colnames(completeTable))
    # ---- How much?
    populationNasalIndex        = grep("^NasalPopulation$",        colnames(completeTable))
    populationThroatIndex       = grep("^ThroatPopulation$",       colnames(completeTable))
    # ---- After Enrichment final grow?
    populationNasalEnrichIndex  = grep("^EnrichNasalPopulation$",  colnames(completeTable))
    populationThroatEnrichIndex = grep("^EnrichThroatPopulation$", colnames(completeTable))
    # ---- Coagulase test?
    nasalCoagulationTestIndex   = grep("^CoagulaseNasal$",         colnames(completeTable))
    throatCoagulationTestIndex  = grep("^CoagulaseThroat$",        colnames(completeTable))
    # ---- Carrier?
    #
    # In here, we define manually each of the indexes for each of the possible
    # carrier definitions. For the paper we only use S1+S2 Coagulase Nasal
    # and S1+S2 Enrichment Nasal. Which we define here to later organize them
    # into the set of important categories.
    #
    # But it should be defined one by one to all manually. To avoid that, you
    # have the special index anchor before to do all of them
    #
    # Colonize coagulase carrier
    originalDefinitionCarrierIndex = grep("^S1_C_Colonize$",         colnames(completeTable))
    
    # swap1ColonizeIndex
    # swap2ColonizeIndex
    # swap1EnrichmentIndex
    # swap2EnrichmentIndex
    
    
    
    # Colonize enrichment carrier
    # Persistance coagulase carrier
    nasalCoagulaseCarrierIndex   = grep("^C_NasalCarrier$",         colnames(completeTable))
    throatCoagulaseCarrierIndex  = grep("^C_ThroatCarrier$",        colnames(completeTable))
    carrierCoagulaseIndex        = grep("^C_Carrier$",              colnames(completeTable))
    # Persistant enrichment carrier
    nasalEnrichmentCarrierIndex  = grep("^E_NasalCarrier$",         colnames(completeTable))
    throatEnrichmentCarrierIndex = grep("^E_ThroatCarrier$",        colnames(completeTable))
    carrierEnrichmentIndex       = grep("^E_Carrier$",              colnames(completeTable))
    # ---- Other stuff
    dateIndexComplete           = grep("^Date$",                   colnames(completeTable))
    # nasalCarrierIndexComplete   = grep("^NasalCarrier$",           colnames(completeTable))
    # throatCarrierIndexComplete  = grep("^ThroatCarrier$",          colnames(completeTable))
    spaTClonningIndexComplete   = grep("^SPAThroatClonning$",      colnames(completeTable))
    spaT1IndexComplete          = grep("^SPAThroat1$",             colnames(completeTable))
    spaT2IndexComplete          = grep("^SPAThroat2$",             colnames(completeTable))
    spaN1IndexComplete          = grep("^SPANasal1$",              colnames(completeTable))
    spaN2IndexComplete          = grep("^SPANasal2$",              colnames(completeTable))

  }

  # -- Medicine
  {
    
    
    
  }
  
  # -- Blood
  firstBloodIndex = grep("^Estradiol_E2_.nmol.L.$",  colnames(completeTable))
  vitamimDIndex   = grep("^X25.OH.D_.nmol.L.$",  colnames(completeTable))
  totalBloodIndex = vitamimDIndex - firstBloodIndex + 1
  bloodIndexes    = c(firstBloodIndex:vitamimDIndex)
  
  # -- Network indexes
  {

    overviewIndex            = grep("^Overview$",                    colnames(completeTable))

    overallConnectionsIndex  = grep("^OverallConnections$",          colnames(completeTable))
    overallPopularityIndex   = grep("^OverallPopularity$" ,          colnames(completeTable))
    overallFWBugNasalIndex   = grep("^OverallFriendsWithSANasal$" ,  colnames(completeTable))
    overallFWBugThroatIndex  = grep("^OverallFriendsWithSAThroat$" , colnames(completeTable))
    overallFWSAIndex         = grep("^OverallFriendsWithSA$",        colnames(completeTable))

    physicalConnectionsIndex = grep("^PhysicalConnections$",         colnames(completeTable))
    physicalPopularityIndex  = grep("^PhysicalPopularity$" ,         colnames(completeTable))
    physicalFWBugNasalIndex  = grep("^PhysicalFriendsWithSANasal$" , colnames(completeTable))
    physicalFWBugThroatIndex = grep("^PhysicalFriendsWithSAThroat$", colnames(completeTable))
    physicalFWSAIndex        = grep("^PhysicalFriendsWithSA$",       colnames(completeTable))

    homeConnectionsIndex     = grep("^HomeConnections$",             colnames(completeTable))
    homePopularityIndex      = grep("^HomePopularity$" ,             colnames(completeTable))
    homeFWBugNasalIndex      = grep("^HomeFriendsWithSANasal$" ,     colnames(completeTable))
    homeFWBugThroatIndex     = grep("^HomeFriendsWithSAThroat$" ,    colnames(completeTable))
    homeFWSAIndex            = grep("^HomeFriendsWithSA$" ,          colnames(completeTable))

    sportsConnectionsIndex   = grep("^SportsConnections$",           colnames(completeTable))
    sportsPopularityIndex    = grep("^SportsPopularity$" ,           colnames(completeTable))
    sportsFWBugNasalIndex    = grep("^SportsFriendsWithSANasal$" ,   colnames(completeTable))
    sportsFWBugThroatIndex   = grep("^SportsFriendsWithSAThroat" ,   colnames(completeTable))
    sportsFWSAIndex          = grep("^SportsFriendsWithSA" ,         colnames(completeTable))

    schoolConnectionsIndex   = grep("^SchoolConnections$",           colnames(completeTable))
    schoolPopularityIndex    = grep("^SchoolPopularity$" ,           colnames(completeTable))
    schoolFWBugNasalIndex    = grep("^SchoolFriendsWithSANasal$" ,   colnames(completeTable))
    schoolFWBugThroatIndex   = grep("^SchoolFriendsWithSAThroat$" ,  colnames(completeTable))
    schoolFWSAIndex          = grep("^SchoolFriendsWithSA$" ,        colnames(completeTable))

    othersConnectionsIndex   = grep("^OthersConnections$",           colnames(completeTable))
    othersPopularityIndex    = grep("^OthersPopularity$" ,           colnames(completeTable))
    othersFWBugNasalIndex    = grep("^OthersFriendsWithSANasal$" ,   colnames(completeTable))
    othersFWBugThroatIndex   = grep("^OthersFriendsWithSAThroat$" ,  colnames(completeTable))
    othersFWSAIndex          = grep("^OthersFriendsWithSA$" ,        colnames(completeTable))

  }

  # ---- Misc.
  {
    uniqueIndexComplete        = grep("^UniqueGroup$", colnames(completeTable))
  }


}

# Collections of previous indexes
{

  # Collection of friends with SA
  {
    allFriendsBugIndexesNasal  = c(overallFWBugNasalIndex,
                                   physicalFWBugNasalIndex,
                                   homeFWBugNasalIndex,
                                   sportsFWBugNasalIndex,
                                   schoolFWBugNasalIndex,
                                   othersFWBugNasalIndex)
    
    allFriendsBugIndexesThroat = c(overallFWBugThroatIndex,
                                   physicalFWBugThroatIndex,
                                   homeFWBugThroatIndex,
                                   sportsFWBugThroatIndex,
                                   schoolFWBugThroatIndex,
                                   othersFWBugThroatIndex)
    
    allFriendsSAIndexes        = c(overallFWSAIndex,
                                   physicalFWSAIndex,
                                   homeFWSAIndex,
                                   sportsFWSAIndex,
                                   schoolFWSAIndex,
                                   othersFWSAIndex)
  }
  
  # Collection of important categorical variables
  {
    importantCategoricalIndexes      = c(sexIndex,  schoolIndex, BMICatIndex,
                                         smokeIndex, snuffIndex, alcoholIndex,
                                         sportsIndex)
    totalImportantCategoricalIndexes = length(importantCategoricalIndexes)
    importantCategoricalNames        = colnames(completeTable)[importantCategoricalIndexes]
  }
  
  # Collection of important numerical variables
  {
    importantNumericalIndexes        = c(ageIndex, BMIIndex)
    totalImportantNumericalIndexes   = length(importantNumericalIndexes)
    importantNumericalNames          = colnames(completeTable)[importantNumericalIndexes]    
  }

  # Both together
  importantIndexes = c(importantCategoricalIndexes, importantNumericalIndexes)
  totalImporantIndexes = length(importantIndexes)
  
  # For stratification
  {
    categoricalStratificationIndexes = c(sexIndex)  
    numericalStratificationIndexes = c(NA,NA,NA)
  }

  # Collection of important explanatory and consecuence variables
  {
    explanatoryIndexes      = c(sexIndex,  schoolIndex, BMICatIndex,
                                smokeIndex, snuffIndex, alcoholIndex,
                                sportsIndex)
    totalExplanatoryIndexes = length(explanatoryIndexes)
    explanatoryNames        = colnames(completeTable)[explanatoryIndexes]
    
    # Original all variables for carrier
    consecuenceIndexes      = c(SABaseIndex:(SABaseIndex+17))
    totalConsecuenceIndexes = length(consecuenceIndexes)
    consecuenceNames        = colnames(completeTable)[consecuenceIndexes]
    # Final? carrier definition
    consecuenceIndexes      = c(originalDefinitionCarrierIndex, nasalCoagulaseCarrierIndex, nasalEnrichmentCarrierIndex)
    consecuenceIndexes      = c(nasalCoagulaseCarrierIndex, nasalEnrichmentCarrierIndex)
    totalConsecuenceIndexes = length(consecuenceIndexes)
    consecuenceNames        = colnames(completeTable)[consecuenceIndexes]
    
  }
  
}

# Colors for each variable
#
# -- This tell the script that, if you use a particular index (ie the sex Index)
#    Here is where you can look up for the colors corresponding to that variable.
#    This helps a lot doing the automatic analysis and keeping color consistency
#    in between plots without having to define the colors for each plot, only
#    the index that you want to analyze.
#
{

  myListOfColorVectors = vector("list", length = ncol(completeTable))

  # Init everything to NA (R is a horrible language, can't init to NULL which is what makes sense here :( ,
  # Seriosly, there are so many bugs and inconsistencies around because of this already. I want to go back to C++ ))
  for(i in 1:ncol(completeTable)){
  
    myListOfColorVectors[[i]] = NA
    
  }
  
  # Add the specific colors
  myListOfColorVectors[[sexIndex]]           = COLOR_VECTOR_SEX     # Sex
  myListOfColorVectors[[smokeIndex]]         = COLOR_VECTOR_SMOKE   # Smoke
  myListOfColorVectors[[snuffIndex]]         = COLOR_VECTOR_SMOKE   # Snuff
  myListOfColorVectors[[sportsIndex]]        = COLOR_VECTOR_SPORTS  # Sports
  myListOfColorVectors[[BMICatIndex]]        = COLOR_VECTOR_BMI     # BMI Categorical
  # myListOfColorVectors[[nasalCarrierIndex]]  = COLOR_VECTOR_CARRIER # Nasal Carrier
  # myListOfColorVectors[[throatCarrierIndex]] = COLOR_VECTOR_CARRIER # Throat Carrier
  # myListOfColorVectors[[carrierIndex]]       = COLOR_VECTOR_CARRIER # SA Carrier

}