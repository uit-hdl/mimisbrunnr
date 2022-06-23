# -----------------------------------------------------------------------------
# This script presents a descriptive summary of each variable to the user.
# It gives two indpendent reports, before, and after the filtering options
#
#
#
# The default filtering takes away the invalid values in each variable (NOT REALLY FOR NUMERICAL????)
#     - Numbers that indicate NAs
#     - Impossible values (ie: glucemic level of 50000)
#
# It will give you:
#     - A general overall plot for every variable (table plots)
#     - An automatic descriptive plot for every variable
#           + For categorical variables, a barplot with absolute frequencies
#           + For numerical variables:
#                 * A histogram
#                 * A density plot
#                 * A QQ plot
#     - Summary CSV files
#           + For categorical variables:
#                 * Has NA values?
#           + For numerical variables:
#                 * Minimum
#                 * Maximum
#                 * Average and Median
#                 * Standard Deviation
#                 * Normality test values
#                   (Shapiro-Wilk’s test., p-value > 0.05 => NORMALITY)
#           + For each row:
#                 * Is complete?
#                 * Total NAs
#                 * Total "Unknown"s
#                 * How many columns are filled (so a lot of many NA/NULL in the
#                   variables, will have low % for that row. This row need to be
#                   probably filtered out.)
#
# Finally, the script also take a look into tailored results, such as the
# the SA aureus test are consistance with some secondary test and so on.
# -----------------------------------------------------------------------------

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("indexes.R",      encoding="utf-8")

# -----------------------------------------------------------------------------
# AUTOMATIC PART
# -----------------------------------------------------------------------------
{

  # Do the overall variables plot
  # ---- Phenotype Table
  # ---- Network
  # ---- Aureus
  #doTableplot(aureusTable,    automaticControlFolder)
  #doTableplot(phenotypeTable, automaticControlFolder)
  #doTableplot(networkTable,   automaticControlFolder)

  # How many columns and rows
  totalColumns = ncol(completeTable)
  totalRows    = nrow(completeTable)

  # How many of each variables
  totalCategorical = sum(   variablesInfoDF$Type == "categorical"  )
  totalNumerical   = sum(   variablesInfoDF$Type == "numerical"    )

  # Prepare the dataframes for each type of variable
  categoricalControlInfoDF  =  data.frame(matrix(NA, nrow = totalCategorical, ncol = 4))
  numericalControlInfoDF    =  data.frame(matrix(NA, nrow = totalNumerical,   ncol = 8))

  # -- Categorical
  #        Total NAs, Total Unknown,
  # -- Numerical
  #        Minimum, Maximum, Average and Median, Standard Deviation, Normality test
  colnames(categoricalControlInfoDF) = c("VariableID", "TotalCategories", "TotalNA", "TotalUnknown")
  colnames(numericalControlInfoDF)   = c("VariableID", "Minimum", "Maximum", "Mean", "Median", "SD", "Normality test", "TotalNA")

  # Create the indexes for writing in their respective DFs
  categoricalIndex = 1
  numericalIndex   = 1

  # For each variable
  #for(i in 1:3){
  for(i in 1:totalColumns){

    print("----------")
    print(i)
    print(variablesInfoDF$VariableID[i])
    print("----------")

    # Get common info
    currentID              = variablesInfoDF$VariableID[i]
    currentTotalNA         = variablesInfoDF$TotalNA[i]

    # Check what type do we have
    if(variablesInfoDF$Type[i] == "categorical"){

      # Specific categorical info
      currentTotalCategories = variablesInfoDF$TotalCategories[i]
      currentTotalUnknown    = sum(completeTable[,i] == "Unknown")

      # Try to do a normal barplot, if you can't do a long one
      # if(currentTotalCategories<=16){
      # 
      #   print(" - Normal barplot...")
      # 
      #   doBarPlot(completeTable, i, automaticControlFolder,
      #             plotTitle = "Automatic Variable Control",
      #             plotSubtitle = variablesInfoDF$VariableID[i],
      #             plotXLabel = variablesInfoDF$VariableID[i],
      #             plotYLabel = "Total")
      # 
      # }
      # else{
      # 
      #   doLongBarPlot(completeTable, i, automaticControlFolder,
      #                 plotTitle = "Automatic Variable Control", plotSubtitle = variablesInfoDF$VariableID[i], plotXLabel = variablesInfoDF$VariableID[i], plotYLabel = "Total")
      # 
      #   logMessage = paste( "LOG: I didn't make the categories control plot for ",
      #                       variablesInfoDF$VariableID[i],
      #                       " because it has more than ",
      #                       CATEGORY_LIMIT,
      #                       " and the user has specify that image would be too big.",
      #                       "A long bar plot was generated instead.",
      #                       sep = '')
      # 
      #   print(logMessage)
      # 
      # }

      print(" - Writting data...")

      # Write the info into the data frame
      categoricalControlInfoDF$VariableID[categoricalIndex]      = currentID
      categoricalControlInfoDF$TotalCategories[categoricalIndex] = currentTotalCategories
      categoricalControlInfoDF$TotalNA[categoricalIndex]         = currentTotalNA
      categoricalControlInfoDF$TotalUnknown[categoricalIndex]    = currentTotalUnknown

      categoricalIndex = categoricalIndex + 1

      print(" - DONE! next...")

    }
    else{

      # Specific numerical info
      currentMinimum   = min(completeTable[,i],    na.rm = TRUE)
      currentMaximum   = max(completeTable[,i],    na.rm = TRUE)
      currentMean      = mean(completeTable[,i],   na.rm = TRUE)
      currentMedian    = median(completeTable[,i], na.rm = TRUE)
      currentSD        = sd(completeTable[,i],     na.rm = TRUE)
      currentNormality = 0 # This might not be possible, need to do the QQ analysis first

      # print("Histogram")

      # Can't do a Histograms for dates (yet)
      # if(is.Date(completeTable[,i]) == FALSE) {
      # 
      #   # Histogram
      #   doHistogramPlot2(completeTable, i, automaticControlFolder,
      #                    plotTitle = "Automatic Variable Control",
      #                    plotSubtitle = variablesInfoDF$VariableID[i],
      #                    plotXLabel = variablesInfoDF$VariableID[i], plotYLabel = "Total")
      # 
      # }


# 
#       print("Density")
# 
#       # Density plot
#       doDensityPlot(completeTable, i, automaticControlFolder,
#                     plotTitle = "Automatic Variable Control",
#                     plotSubtitle = variablesInfoDF$VariableID[i],
#                     plotXLabel = variablesInfoDF$VariableID[i], plotYLabel = "Total")

      print("QQ")

      
      currentNormality   = -1
      if(is.Date(completeTable[,i]) == FALSE){
        areAllEqual        = sd(completeTable[,i], na.rm = TRUE)
        if(areAllEqual != 0)
          currentNormality = shapiro.test(completeTable[,i])$p.value        
      }
      
      # Can't do a QQ for dates (yet)
      if(is.Date(completeTable[,i]) == FALSE) {

        # QQ plot
        currentNormality = doQQPlot(completeTable, i, automaticControlFolder,
                                    plotTitle = "Automatic Variable Control",
                                    plotSubtitle = variablesInfoDF$VariableID[i],
                                    plotXLabel = variablesInfoDF$VariableID[i], plotYLabel = "Teoretical quantiles")

      }

      print(" - Writting data...")

      # Write the info into the data frame
      numericalControlInfoDF$VariableID[numericalIndex]       = currentID
      numericalControlInfoDF$Minimum[numericalIndex]          = currentMinimum
      numericalControlInfoDF$Maximum[numericalIndex]          = currentMaximum
      numericalControlInfoDF$Mean[numericalIndex]             = currentMean
      numericalControlInfoDF$Median[numericalIndex]           = currentMedian
      numericalControlInfoDF$SD[numericalIndex]               = currentSD
      numericalControlInfoDF$`Normality test`[numericalIndex] = currentNormality
      numericalControlInfoDF$TotalNA[numericalIndex]          = currentTotalNA

      numericalIndex = numericalIndex + 1

    }


  }

  # Write the logs into file
  write.csv2(categoricalControlInfoDF, CONTROL_CATEGORICAL_CSV)
  write.csv2(numericalControlInfoDF,   CONTROL_NUMERICAL_CSV)

}



# Put all of this into a separate script that check for data consistency


# TODO: The enrichment process may go from NO to YES, but never from YES to NO

# TODO: In the network, if you have 3 friends, you can't have friend number 4 being friend in school for example
# TODO: Same if you have 3 friends, you can not have no friend number 1 in school as NA, it should be a yes or no mandatory
# TODO: Your total connections should be equal or grater than out connections and in connections
# TODO: Your reciprocity should be equal or smaller than your out connections
# TODO: The amount of overall edges is greater or equal that the amount of physical, home, ... edges
# TODO: Max(Popularity, Following)  + Min(Popularity, Following) - Reciprocity = Total Relationships

# TODO: You cannot be friend with yourself, all diagonals of all friendship matrices should be = 0


# Check that a yes in antibiotics have an associated antibiotic branch
# {
#   # ---- Select only rows with a YES/NO in antibiotics
#   onlyAntibioticsTable = subset(phenotypeTable, Antibiotics == "Yes")
#   noneAntibioticsTable = subset(phenotypeTable, Antibiotics == "No")
# 
#   # ---- Do the plots
#   doBarPlot(onlyAntibioticsTable, AntiBranchIndex,  filepath_yesAntibioticsBarPlot,
#             plotTitle = "Antibiotics branch of people that takes antibiotics", plotSubtitle = "Unknown branches are correct", plotCaption = "Source: Tromsø 7", plotXLabel = "Antibiotics Branches", plotYLabel = "Absolute frequency")
# 
#   doBarPlot(noneAntibioticsTable, AntiBranchIndex,  filepath_nonAntibioticsBarPlot,
#             plotTitle = "Antibiotics branch of people that DO NOT takes antibiotics", plotSubtitle = "There should be no branches in here", plotCaption = "Source: Tromsø 7", plotXLabel = "Antibiotics Branches", plotYLabel = "Absolute frequency")
# 
# }

# Outside activity should be the same as sport activity
# {
#   onlyActivePeopleTable = subset(phenotypeTable, Active == "Yes")
#   noneActivePeopleTable = subset(phenotypeTable, Active == "No")
# 
#   doBarPlot(onlyActivePeopleTable, sportsIndex,  filepath_yesActivityBarPlot,
#             plotTitle = "Sport activity for people that are active", plotSubtitle = "Redundant info, everyone should be light or higher", plotCaption = "Source: Tromsø 7", plotXLabel = "Sport level", plotYLabel = "Absolute frequency")
# 
#   doBarPlot(noneActivePeopleTable, sportsIndex,  filepath_nonActivityBarPlot,
#             plotTitle = "Sport activity for people that are NOT active", plotSubtitle = "Redundant info, everyone should be None", plotCaption = "Source: Tromsø 7", plotXLabel = "Sport level", plotYLabel = "Absolute frequency")
# 
# }



# -----------------------------------------------------------------------------
# SA
# -----------------------------------------------------------------------------
# {
#   
#   # Did something grow?
#   doBarPlot(completeTable, nasalGrowthIndex,   MANUAL_CONTROL_FOLDER,
#             plotTitle = "How much of something grew in the nose",   plotYLabel = "Absolute frequency")
#   doBarPlot(completeTable, throatGrowthIndex,  MANUAL_CONTROL_FOLDER,
#             plotTitle = "How much of something grew in the throat", plotYLabel = "Absolute frequency")
#   
#   
# }




# The Aureus column of grow, is either no or yes; if it is yes, it should be light, medium or high. But if it is NO, there shouldn't be any light medium or high
{

  # Count only places where the experiment was actually a success
  positiveAureusGrowNasalTable  = subset(aureusTable, NasalAureus  == "Yes" & NasalGrowth  == "Yes")
  negativeAureusGrowNasalTable  = subset(aureusTable, NasalAureus  == "No"  & NasalGrowth  == "Yes")
  positiveAureusGrowThroatTable = subset(aureusTable, ThroatAureus == "Yes" & ThroatGrowth == "Yes")
  negativeAureusGrowThroatTable = subset(aureusTable, ThroatAureus == "No"  & ThroatGrowth == "Yes")

  # positiveAureusGrowNasalTable  = subset(completeTable, completeTable[,nasalAureusIndex]  == "Yes" & NasalGrowth  == "Yes")
  # negativeAureusGrowNasalTable  = subset(completeTable, NasalAureus  == "No"  & NasalGrowth  == "Yes")
  # positiveAureusGrowThroatTable = subset(completeTable, ThroatAureus == "Yes" & ThroatGrowth == "Yes")
  # negativeAureusGrowThroatTable = subset(completeTable, ThroatAureus == "No"  & ThroatGrowth == "Yes")
  

  # doBarPlot(positiveAureusGrowNasalTable, populationNasalIndex,   filepath_positiveGrowNasalBarPlot,
  #           plotTitle = "How much S.Aureus grew in positive cultives?", plotSubtitle = "(NASAL) Control info, it should be from Light onwards",  plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Nasal", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(positiveAureusGrowNasalTable, populationNasalIndex,  CONTROL_FOLDER,
  #           plotTitle = "How much S.Aureus grew in positive cultives?", plotSubtitle = "(NASAL) Control info, it should be from Light onwards",  plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Nasal", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(negativeAureusGrowNasalTable, populationNasalIndex,   filepath_negativeGrowNasalBarPlot,
  #           plotTitle = "How much S.Aureus grew in negative cultives?", plotSubtitle = "(NASAL) Control info, it should be none!",               plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Nasal", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(positiveAureusGrowThroatTable, populationThroatIndex, filepath_positiveGrowThroatBarPlot,
  #           plotTitle = "How much S.Aureus grew in positive cultives?", plotSubtitle = "(THROAT) Control info, it should be from Light onwards", plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Throat", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(negativeAureusGrowThroatTable, populationThroatIndex, filepath_negativeGrowThroatBarPlot,
  #           plotTitle = "How much S.Aureus grew in negative cultives?", plotSubtitle = "(THROAT) Control info, it should be none!",              plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Throat", plotYLabel = "Absolute frequency")
  # 

}


# The Aureus test has a control variable, if Coagulase is negative and SA is positive, that is impossible
{

  # Use only people where we performed the experiment
  temporatlTableNasal  = subset(aureusTable, NasalGrowth   == "Yes")
  temporatlTableThroat = subset(aureusTable, ThroatGrowth  == "Yes")

  doCategoriesHeatmap(temporatlTableNasal,  populationNasalIndex, nasalCoagulationTestIndex,  plotFilePath = filepath_coagulaseNasalHeatMap,
                      plotTitle = "Compare SA grow with coagulase test", plotSubtitle = "(NASAL) Positive coagulase = SA Grow; negative coagulase = something else grow", plotCaption = "Source: Tromsø 7", plotXLabel = "S.A. Population grow in nasal", plotYLabel = "Coagulase test")

  doCategoriesHeatmap(temporatlTableThroat,  populationThroatIndex, throatCoagulationTestIndex,  plotFilePath = filepath_coagulaseThroatHeatMap,
                      plotTitle = "Compare SA grow with coagulase test", plotSubtitle = "(THROAT) Positive coagulase = SA Grow; negative coagulase = something else grow", plotCaption = "Source: Tromsø 7", plotXLabel = "S.A. Population grow in throat", plotYLabel = "Coagulase test")

  # Divide by the results of the coagulase test
  positiveCoagulaseNasalTable  = subset(temporatlTableNasal, CoagulaseNasal  == "Positive")
  negativeCoagulaseNasalTable  = subset(temporatlTableNasal, CoagulaseNasal  == "Negative")
  positiveCoagulaseThroatTable = subset(temporatlTableThroat, CoagulaseThroat == "Positive")
  negativeCoagulaseThroatTable = subset(temporatlTableThroat, CoagulaseThroat == "Negative")


  # doBarPlot(positiveCoagulaseNasalTable, nasalAureusIndex,   filepath_positiveCoagulaseNasalBarPlot,
  #           plotTitle = "Is there S.Aureus in places where Coagulase test was positive?", plotSubtitle = "(NASAL) Control info, Coagulase should be positive for SA only", plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Nasal", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(negativeCoagulaseNasalTable, nasalAureusIndex,   filepath_negativeCoagulaseNasalBarPlot,
  #           plotTitle = "Is there S.Aureus in places where Coagulase test was negative?", plotSubtitle = "(NASAL) Control info, Coagulase should be positive for SA only", plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Nasal", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(positiveCoagulaseThroatTable, throatAureusIndex, filepath_positiveCoagulaseThroatBarPlot,
  #           plotTitle = "Is there S.Aureus in places where Coagulase test was positive?", plotSubtitle = "(THROAT) Control info, Coagulase should be positive for SA only", plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Throat", plotYLabel = "Absolute frequency")
  # 
  # doBarPlot(negativeCoagulaseThroatTable, throatAureusIndex, filepath_negativeCoagulaseThroatBarPlot,
  #           plotTitle = "Is there S.Aureus in places where Coagulase test was negative?", plotSubtitle = "(THROAT) Control info, Coagulase should be positive for SA only", plotCaption = "Source: Tromsø 7", plotXLabel = "Grow of S.Aureus in Throat", plotYLabel = "Absolute frequency")



}

# The enrichment process may go from light to rich for example, but never from medium to non for example
{

  yesNasalEnriched  = subset(aureusTable, EnrichNasalAureus   == "Yes")
  nonNasalEnriched  = subset(aureusTable, EnrichNasalAureus   == "No")
  yesThroatEnriched = subset(aureusTable, EnrichThroatAureus  == "Yes")
  nonThroatEnriched = subset(aureusTable, EnrichThroatAureus  == "No")


  # If it has not beeing enriched, the grow should stay the same
  # Is also possible that we consider this a NA or other similar non valid argument
  # -- Nasal
  doCategoriesHeatmap(nonNasalEnriched,  populationNasalIndex,  populationNasalEnrichIndex,  plotFilePath = filepath_negativeEnrichmentNasalGrowHeatPlot,
                      plotTitle = "What happens for non enrichment?", plotSubtitle = "(NASAL) Everything should stay the same or lower", plotCaption = "Source: Tromsø 7", plotXLabel = "S.Aureus before enrichment", plotYLabel = "S.Aureus after enrichment")
  # -- Throat
  doCategoriesHeatmap(nonThroatEnriched, populationThroatIndex, populationThroatEnrichIndex, plotFilePath = filepath_negativeEnrichmentThroatGrowHeatPlot,
                      plotTitle = "What happens for non enrichment?", plotSubtitle = "(THROAT) Everything should stay the same or lower", plotCaption = "Source: Tromsø 7", plotXLabel = "S.Aureus before enrichment", plotYLabel = "S.Aureus after enrichment")

  # If it has beeing enriched, the grow should stay the same or higher
  # -- Nasal
  doCategoriesHeatmap(yesNasalEnriched,  populationNasalIndex,  populationNasalEnrichIndex,  plotFilePath = filepath_positiveEnrichmentNasalGrowHeatPlot,
                      plotTitle = "What happens after enrichment?", plotSubtitle = "(NASAL) Everything should stay the same or higher", plotCaption = "Source: Tromsø 7", plotXLabel = "S.Aureus before enrichment", plotYLabel = "S.Aureus after enrichment")
  # -- Throat
  doCategoriesHeatmap(yesThroatEnriched, populationThroatIndex, populationThroatEnrichIndex, plotFilePath = filepath_positiveEnrichmentThroatGrowHeatPlot,
                      plotTitle = "What happens after enrichment?", plotSubtitle = "(THROAT) Everything should stay the same or higher", plotCaption = "Source: Tromsø 7", plotXLabel = "S.Aureus before enrichment", plotYLabel = "S.Aureus after enrichment")


}


# All Class ID belong to 1 and only 1 High School ID




# Update the latex file
source("latex.R", encoding="utf-8")
