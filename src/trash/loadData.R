# -----------------------------------------------------------------------------
#
# This script is for loading the data only
#
# This include unformated text and converted into Date object, creating the
# proper graph object, creating the factors in the dataframes, and in general
# everything that take clean data and need to be put into R.
#
# This make possible using something else that is not R, because your data is
# ready to use by anything. Thus is important to make the two process separated
# The most efficient scenario is where you only need to read the data and don't
# have to do any transformation.
#
# If you want to transform the data, use the dataCleaning.R script
#
# If you want to filter out some data the filter.R script
#
# -----------------------------------------------------------------------------


# Add the needed libraries
library(reshape2)
#library(plyr)
library(dplyr)
library(lubridate)
library(ggpubr)
library(stringr)

library(ggraph)
library(igraph)


# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("analysis.R",  encoding="utf-8")
source("constants.R", encoding="utf-8")

# Read the data into DFs
# -----------------------------------------------------------------------------
{
  
  # Basics
  phenotypeTable    = read.csv2(FILTER_DATA_PHENOTYPES_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  aureusTable       = read.csv2(FILTER_DATA_AUREUS_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  medicineTable     = read.csv2(FILTER_DATA_MEDICINE_FILEPATH,         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  # Relational
  drugsTable          = read.csv2(FILTER_DATA_DRUG_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  contraceptivesTable = read.csv2(FILTER_DATA_CONTRACEPTIVES_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  diseasesTable       = read.csv2(FILTER_DATA_DISEASES_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  # Network
  networkTable      = read.csv2(FILTER_DATA_NETWORK_FILEPATH,          fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  overallNetworkDF  = read.csv2(FILTER_DATA_OVERALL_NETWORK_FILEPATH,  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  physicalNetworkDF = read.csv2(FILTER_DATA_PHYSICAL_NETWORK_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  homeNetworkDF     = read.csv2(FILTER_DATA_HOME_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  schoolNetworkDF   = read.csv2(FILTER_DATA_SCHOOL_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  sportsNetworkDF   = read.csv2(FILTER_DATA_SPORTS_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  otherNetworkDF    = read.csv2(FILTER_DATA_OTHERS_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}

# Fix the column names in each network table
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


# Set up the strings into factors where necessary and also the proper date format
# -----------------------------------------------------------------------------
# ---- Convert dates strings into proper date variables
{
  # -------- For the aureus table
  {
    aureusTable$Date   = as.Date(aureusTable$Date,   format = "%m/%d/%Y")
    aureusTable$S2Date = as.Date(aureusTable$S2Date, format = "%Y-%m-%d")
  }

  # -------- For the network table
  {
    networkTable$Created = as.Date(networkTable$Created, format = "%m/%d/%Y")

  }

}

# ---- Add the manual order for the categorical values, so it coincides with the color palette
{
  # -------- For the phenotype table
  {
    
    phenotypeTable$Sex              = factor(phenotypeTable$Sex,             levels = c("Man",         "Woman"))
    phenotypeTable$Smoke            = factor(phenotypeTable$Smoke          , levels = c("Never",       "Sometimes", "Daily",               "Unknown"))
    phenotypeTable$Snuff            = factor(phenotypeTable$Snuff          , levels = c("Never",       "Sometimes", "Daily",               "Unknown"))
    phenotypeTable$Sports           = factor(phenotypeTable$Sports         , levels = c("None",        "Light",     "Medium",     "Hard",  "Unknown"))
    phenotypeTable$Active           = factor(phenotypeTable$Active         , levels = c("No",          "Yes",                              "Unknown"))
    phenotypeTable$Antibiotics      = factor(phenotypeTable$Antibiotics    , levels = c("No",          "Yes",                              "Unknown"))
    phenotypeTable$BMICategorical   = factor(phenotypeTable$BMICategorical , levels = c("Underweight", "Healthy",   "Overweight", "Obese", "Unknown"))

  }

  # -------- For the aureus table
  {

    aureusTable$NasalPopulation         = factor(aureusTable$NasalPopulation         , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$ThroatPopulation        = factor(aureusTable$ThroatPopulation        , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$EnrichNasalPopulation   = factor(aureusTable$EnrichNasalPopulation   , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$EnrichThroatPopulation  = factor(aureusTable$EnrichThroatPopulation  , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))

    #aureusTable$NasalCarrier            = factor(aureusTable$NasalCarrier            , levels = c("Positive", "Negative", "Unknown"))
    #aureusTable$ThroatCarrier           = factor(aureusTable$ThroatCarrier           , levels = c("Positive", "Negative", "Unknown"))
    #aureusTable$SACarrier               = factor(aureusTable$SACarrier               , levels = c("Positive", "Negative", "Unknown"))
    
    # S.Aureus Colonization and Carrier variables
    # ---- COLONIZATION
    # ------- by the Coagulase test
    aureusTable$C_NasalColonize      = factor(aureusTable$S1_C_NasalColonize   , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$C_ThroatColonize     = factor(aureusTable$S1_C_ThroatColonize  , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$C_Colonize           = factor(aureusTable$S1_C_Colonize        , levels = c("Positive", "Negative", "Unknown"))
    # -------  by the Enrichment test
    aureusTable$E_NasalColonize      = factor(aureusTable$S1_E_NasalColonize   , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$E_ThroatColonize     = factor(aureusTable$S1_E_ThroatColonize  , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$E_Colonize           = factor(aureusTable$S1_E_Colonize        , levels = c("Positive", "Negative", "Unknown"))
    # -------  by confirmed one week coagulase test
    aureusTable$WC_NasalColonize     = factor(aureusTable$S2_C_NasalColonize  , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$WC_ThroatColonize    = factor(aureusTable$S2_C_ThroatColonize , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$WC_Colonize          = factor(aureusTable$S2_C_Colonize       , levels = c("Positive", "Negative", "Unknown"))
    # -------  by confirmed one week enrichment test
    aureusTable$WE_NasalColonize     = factor(aureusTable$S2_E_NasalColonize  , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$WE_ThroatColonize    = factor(aureusTable$S2_E_ThroatColonize , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$WE_Colonize          = factor(aureusTable$S2_E_Colonize       , levels = c("Positive", "Negative", "Unknown"))
    # ---- CARRIER
    # ----- by the Coagulase test
    aureusTable$C_NasalCarrier      = factor(aureusTable$C_NasalCarrier     , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$C_ThroatCarrier     = factor(aureusTable$C_ThroatCarrier    , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$C_Carrier           = factor(aureusTable$C_Carrier          , levels = c("Positive", "Negative", "Unknown"))
    # ----- by the Enrichment test
    aureusTable$E_NasalCarrier      = factor(aureusTable$E_NasalCarrier     , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$E_ThroatCarrier     = factor(aureusTable$E_ThroatCarrier    , levels = c("Positive", "Negative", "Unknown"))
    aureusTable$E_Carrier           = factor(aureusTable$E_Carrier          , levels = c("Positive", "Negative", "Unknown"))
    
    
  }

  # -------- From the medicine table
  {
    
    # The alcohol variable has an special requirement.
    # There are too few people that drink too much (which is good)
    # So we are going to add all of them to the previous group
    newGroup = "Twice of more per month"
    for (i in 1:nrow(medicineTable)) {
    
      if(medicineTable$Alcohol[i] == "2-4 times per month")
        medicineTable$Alcohol[i] = newGroup
      
      if(medicineTable$Alcohol[i] == "2-3 times per week")
        medicineTable$Alcohol[i] = newGroup
      
      if(medicineTable$Alcohol[i] == "4 or more times per week")
        medicineTable$Alcohol[i] = newGroup
      
    }
    
    medicineTable$Alcohol = factor(medicineTable$Alcohol , levels = c("Never", "Once per month or less", "Twice of more per month", "Unknown"))
    
    # Old factoring with the original data
    #medicineTable$Alcohol = factor(medicineTable$Alcohol , levels = c("Never", "Once per month or less", "2-4 times per month", "2-3 times per week", "4 or more times per week", "Unknown"))
    
  }
  
}

# ---- Other categorical adjustments
{
  # Add the H to the HighSchool ID, it just looks better
  phenotypeTable$HighSchoolID = paste0("H",phenotypeTable$HighSchoolID)
  phenotypeTable$HighSchoolID = as.factor(phenotypeTable$HighSchoolID)
  
}

# Check that you don't have any NA in any table that might have some hidden meaning
# -----------------------------------------------------------------------------
{
  # -- For the Phenotype table
  {

    totalPhenotypeVariables      = ncol(phenotypeTable)
    hasNAvariablesPhenotypes     = rep(FALSE,totalPhenotypeVariables)
    howManyNAvariablesPhenotypes = rep(0,totalPhenotypeVariables)
    for(i in 1:totalPhenotypeVariables){

      totalNAs = sum(is.na(phenotypeTable[,i]))
      howManyNAvariablesPhenotypes[i] = totalNAs
      if(totalNAs > 0) hasNAvariablesPhenotypes[i] = TRUE

    }

    if(sum(howManyNAvariablesPhenotypes) > 0){

      print("")
      print("WARNING!!")
      print("The phenotype table contain columns with NAs values. Please make sure that this columns are clean and this is what you want:")
      print(colnames(phenotypeTable)[hasNAvariablesPhenotypes])
      print("")

    }

  }

  # -- For the Aureus Table
  {


    totalAureusVariables      = ncol(aureusTable)
    hasNAvariablesAureus      = rep(FALSE,totalAureusVariables)
    howManyNAvariablesAureus  = rep(0,totalAureusVariables)
    for(i in 1:totalAureusVariables){

      totalNAs = sum(is.na(aureusTable[,i]))
      howManyNAvariablesAureus[i] = totalNAs
      if(totalNAs > 0) hasNAvariablesAureus[i] = TRUE

    }

    if(sum(howManyNAvariablesAureus) > 0){

      print("")
      print("WARNING!!")
      print("The aureus table contain columns with NAs values. Please make sure that this columns are clean and this is what you want:")
      print(colnames(aureusTable)[hasNAvariablesAureus])
      print("")

    }




  }

}

# Join tables with proper info together
# -----------------------------------------------------------------------------
completeTable          = phenotypeTable %>% left_join(aureusTable  , by="ID")
completeTable          = completeTable %>% left_join(medicineTable , by="ID")
completeTable$Overview = networkTable$Overwiew  # 0 to 10, how good this network describes your life


# Subset tables
# -----------------------------------------------------------------------------
{
  # Subsets by only one variable:
  {
    # Positive and negatives
    # -- By coagulase
    positiveCoagulaseTableOnly  = subset(completeTable, completeTable[,nasalCoagulaseCarrierIndex] == "Positive")
    negativeCoagulaseTableOnly  = subset(completeTable, completeTable[,nasalCoagulaseCarrierIndex] == "Negative")
    totalPositiveCoagulase      = nrow(positiveCoagulaseTableOnly)
    totalNegativeCoagulase      = nrow(negativeCoagulaseTableOnly)
    # -- By enrichment
    positiveEnrichmentTableOnly = subset(completeTable, completeTable[,nasalEnrichmentCarrierIndex] == "Positive")
    negativeEnrichmentTableOnly = subset(completeTable, completeTable[,nasalEnrichmentCarrierIndex] == "Negative")
    totalPositiveEnrichment     = nrow(positiveEnrichmentTableOnly)
    totalNegativeEnrichment     = nrow(negativeEnrichmentTableOnly)
    # -- By Sex
    menOnlyTable   = subset(completeTable, completeTable[,sexIndex] == "Man")
    womenOnlyTable = subset(completeTable, completeTable[,sexIndex] == "Woman")
    totalMenRows   = nrow(menOnlyTable)
    totalWomenRows = nrow(womenOnlyTable)
  }

}



# Add the contraceptive information
{

  # - Men takes no contraceptives                    -> "Not applicable"
  # - Women not menstruating takes no contraceptives -> "Not applicable"
  completeTable$HormonalContraceptives = "Not applicable"
    
  
  
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




# Make the proper graph objects
# -----------------------------------------------------------------------------
{

  # Nodes
  # (note that the nodes are the same for all the networks, only the edges change)
  nodesDF                  = completeTable
  nodesDF[]                = lapply(nodesDF, as.character) # Need to transform integers ID to strings IDs for whatever reason o_O , why??

  # Edges
  {

    # ---- DIRECTED
    
    # -------- Overall
    {
    meltedOverall           = melt(overallNetworkDF, id.vars = "ID")
    colnames(meltedOverall) = c("from", "to", "value")

    overallEdgesDF           = meltedOverall
    overallEdgesDF$value     = NULL
    overallEdgesDF[]         = lapply(overallEdgesDF, as.character) # Same as before 0_o
    overallEdgesDF$value     = meltedOverall$value
    overallEdgesDF           = overallEdgesDF[overallEdgesDF$value == 1, ] # Get only those with value = 1
    }
    # -------- Physical
    {
    meltedPhysical           = melt(physicalNetworkDF, id.vars = "ID")
    colnames(meltedPhysical) = c("from", "to", "value")

    physicalEdgesDF          = meltedPhysical
    physicalEdgesDF$value    = NULL
    physicalEdgesDF[]        = lapply(physicalEdgesDF, as.character)
    physicalEdgesDF$value    = meltedPhysical$value
    physicalEdgesDF          = physicalEdgesDF[physicalEdgesDF$value == 1, ]
    }
    # -------- Home
    {
    meltedHome               = melt(homeNetworkDF, id.vars = "ID")
    colnames(meltedHome)     = c("from", "to", "value")

    homeEdgesDF              = meltedHome
    homeEdgesDF$value        = NULL
    homeEdgesDF[]            = lapply(homeEdgesDF, as.character)
    homeEdgesDF$value        = meltedHome$value
    homeEdgesDF              = homeEdgesDF[homeEdgesDF$value == 1, ]
    }
    # -------- School
    {
    meltedSchool             = melt(schoolNetworkDF, id.vars = "ID")
    colnames(meltedSchool)   = c("from", "to", "value")

    schoolEdgesDF            = meltedSchool
    schoolEdgesDF$value      = NULL
    schoolEdgesDF[]          = lapply(schoolEdgesDF, as.character)
    schoolEdgesDF$value      = meltedSchool$value
    schoolEdgesDF            = schoolEdgesDF[schoolEdgesDF$value == 1, ]
    }
    # -------- Sports
    {
    meltedSports             = melt(sportsNetworkDF, id.vars = "ID")
    colnames(meltedSports)   = c("from", "to", "value")

    sportsEdgesDF            = meltedSports
    sportsEdgesDF$value      = NULL
    sportsEdgesDF[]          = lapply(sportsEdgesDF, as.character)
    sportsEdgesDF$value      = meltedSports$value
    sportsEdgesDF            = sportsEdgesDF[sportsEdgesDF$value == 1, ]
    }
    # -------- Others
    {
    meltedOther              = melt(otherNetworkDF, id.vars = "ID")
    colnames(meltedOther)    = c("from", "to", "value")

    otherEdgesDF             = meltedOther
    otherEdgesDF$value       = NULL
    otherEdgesDF[]           = lapply(otherEdgesDF, as.character)
    otherEdgesDF$value       = meltedOther$value
    otherEdgesDF             = otherEdgesDF[otherEdgesDF$value == 1, ]
    }
    
    # ---- RECIPROCAL
    
    
    
  }

  
  # Create the graph object
  {

    # Directed
    overallGraph  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = T)
    physicalGraph = graph_from_data_frame(physicalEdgesDF, vertices = nodesDF, directed = T)
    homeGraph     = graph_from_data_frame(homeEdgesDF,     vertices = nodesDF, directed = T)
    schoolGraph   = graph_from_data_frame(schoolEdgesDF,   vertices = nodesDF, directed = T)
    sportsGraph   = graph_from_data_frame(sportsEdgesDF,   vertices = nodesDF, directed = T)
    othersGraph   = graph_from_data_frame(otherEdgesDF,    vertices = nodesDF, directed = T)

    # Undirected
    undirectedOverallGraph  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = F)
    undirectedPhysicalGraph = graph_from_data_frame(physicalEdgesDF, vertices = nodesDF, directed = F)
    undirectedHomeGraph     = graph_from_data_frame(homeEdgesDF,     vertices = nodesDF, directed = F)
    undirectedSchoolGraph   = graph_from_data_frame(schoolEdgesDF,   vertices = nodesDF, directed = F)
    undirectedSportsGraph   = graph_from_data_frame(sportsEdgesDF,   vertices = nodesDF, directed = F)
    undirectedOthersGraph   = graph_from_data_frame(otherEdgesDF,    vertices = nodesDF, directed = F)
    
    # Reciprocal
    reciprocalOverallGraph  = delete_edges(overallGraph, E(overallGraph)[!which_mutual(overallGraph)])
    
    # Reciprocal Edges DF
    reciprobalOverallEdgesDF = as.data.frame(get.edgelist(reciprocalOverallGraph))

    colnames(reciprobalOverallEdgesDF) = c("from", "to")
      
  }

}

# Make a vector with all the edges and all the graph
{
  allEdges  = list(overallEdgesDF, physicalEdgesDF, schoolEdgesDF, sportsEdgesDF, homeEdgesDF, otherEdgesDF)
  allGraphs = list(overallGraph,   physicalGraph,   schoolGraph,   sportsGraph,   homeGraph,   othersGraph)
}

# Create a dataframe with all the variables meta-information
#     - Type of variable
#     - Number of categories (0 = Numerical)
#     - NA counts
# Analyze which type of variables you have in each column
# -----------------------------------------------------------------------------
{

  # How many columns and rows
  totalColumns = ncol(completeTable)
  totalRows    = nrow(completeTable)

  # The main dataframe
  variablesInfoDF           =  data.frame(matrix(NA, nrow = totalColumns, ncol = 4))
  colnames(variablesInfoDF) = c("VariableID", "Type", "TotalCategories", "TotalNAValues")

  # Initialize the variables IDs
  variablesInfoDF$VariableID =  colnames(completeTable)

  # What tipe of data for each column
  myClasses            = sapply(completeTable, class)

  # Simplify types to boolean
  # - FALSE = Numerical
  # - TRUE  = Categorical
  categoricalColumns = rep(FALSE,totalColumns)

  for(i in 1:totalColumns){

    if(myClasses[[i]] == "character" || myClasses[[i]] == "factor"){
      categoricalColumns[i] = TRUE
      variablesInfoDF$Type[i] = "categorical"
    }
    else{
      variablesInfoDF$Type[i] = "numerical"
    }

  }

  # Get how many for each
  totalCategorical = sum(categoricalColumns)
  totalNumerical   = length(categoricalColumns) - totalCategorical

  # For those who are categorical, check how many categories are in each column
  # Here, a 0 means no categories, which implies numerical
  totalCategoriesList = rep(0,totalColumns)

  for(i in 1:totalColumns){

    if(categoricalColumns[i] == TRUE){

      totalCategoriesList[i] = length(unique(completeTable[,i]))

    }

  }

  variablesInfoDF$TotalCategories = totalCategoriesList

  # Check the NA values
  for(i in 1:totalColumns){
    variablesInfoDF$TotalNAValues[i] = sum(is.na(completeTable[,i]))
  }

}