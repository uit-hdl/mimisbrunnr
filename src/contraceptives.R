# -----------------------------------------------------------------------------
# This script gives an extensive analysis on the use of contraceptives.
# -----------------------------------------------------------------------------

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("indexes.R",      encoding="utf-8")

# Prepare the dataframes
{
  # Separate the men
  menOnlyTable   = subset(completeTable,  completeTable[,sexIndex]    == "Man")
  
  # Get the women menstruating only
  womenOnlyTable = subset(completeTable,  completeTable[,sexIndex]    == "Woman")
  womenOnlyTable = subset(womenOnlyTable, womenOnlyTable$Menstruating == "Yes")
  
  # Add info to check for Eksem later
  EksemIDs = diseasesTable[diseasesTable$Diagnostic == "Eksem",]$ID
  womenOnlyTable$Eksem = "No"
  for (i in 1:nrow(womenOnlyTable)) {
    
    # Get the ID
    currentID = womenOnlyTable$ID[i]
    
    if(currentID %in% EksemIDs) womenOnlyTable$Eksem[i] = "Yes"
    
  }
  
  # Prepare a new column with the type of hormonal contraceptives that they use
  womenOnlyTable$Hormonal = "None or non-hormonal"
  
  # Grab some indexes for later
  #typeIndex     = grep("^Contraceptives$", colnames(womenOnlyTable))
  hormonalIndex   = grep("^Hormonal$",   colnames(womenOnlyTable))
  progestinIndex  = grep("Progesterone", colnames(womenOnlyTable))
  estradiolIndex  = grep("Estradiol",    colnames(womenOnlyTable))
  eksemlIndex     = grep("Eksem",        colnames(womenOnlyTable))
}


# XI² TABLES FOR HORMONAL USE WITH RESPECT CARRIER, NASAL, AND THROAT
{
  # Check for 4 things.
  # -- (A) Non hormonal, progestin only, progestin + Estradiol
  # -- (B) Non hormonal, progestin only, progestin + low estradiol, progesting + high estradiol
  # -- (C) Non hormonal, hormonal
  # -- (D) Non hormonal + hormonal, progestin only
  
  # ------------------------------------------------------
  #               A 
  #  Non hormonal, progestin only, progestin + Estradiol
  # -------------------------------------------------------
  {
    
    # Add the info for progesting and progesting + estradiol
    womenOnlyTable$Hormonal = "None or non-hormonal"
    totalContraceptives = nrow(contraceptivesTable)
    for (i in 1:totalContraceptives) {
      
      # Get the ID
      currentID = contraceptivesTable$ID[i]
      
      # Get the hormonal type
      currentHormonal = contraceptivesTable$Hormonal[i]
      
      # If it is a non-hormonal type, register it on the new column
      if(currentHormonal!="Non-hormonal"){
        
        womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = currentHormonal
        
      }
      
    }
    
    # Perform the X^2 test
    
    # Take away those which are unknown for the xi-square
    womenNotUknownTable = womenOnlyTable
    womenNotUknownTable = womenNotUknownTable[ womenNotUknownTable$Hormonal != "Unknown", ]
    
    # Save the xi-sq for nasal, throat, and carrier
    myResultsCarrier       = categoricalXi(womenNotUknownTable, hormonalIndex, carrierIndex,       HORMONAL_FOLDER)
    myResultsNasalCarrier  = categoricalXi(womenNotUknownTable, hormonalIndex, nasalCarrierIndex,  HORMONAL_FOLDER)
    myResultsThroatCarrier = categoricalXi(womenNotUknownTable, hormonalIndex, throatCarrierIndex, HORMONAL_FOLDER)
    
    # Show results
    print("------ A (N,P,P+E) -------------------")
    print(myResultsCarrier[[7]])
    print(myResultsNasalCarrier[[7]])
    print(myResultsThroatCarrier[[7]])
    
  }
  
  # ------------------------------------------------------
  #               B
  #  Non hormonal, progestin only, progestin + high estradiol, progestin + low estradiol, 
  # -------------------------------------------------------
  {
    
    # Add the info for progesting and progesting + estradiol
    womenOnlyTable$Hormonal = "None or non-hormonal"
    totalContraceptives = nrow(contraceptivesTable)
    for (i in 1:totalContraceptives) {
      
      # Get the ID
      currentID = contraceptivesTable$ID[i]
      
      # Get the hormonal type
      currentHormonal = contraceptivesTable$Hormonal[i]
      
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
      
      # If it is a non-hormonal type, register it on the new column
      if(currentHormonal!="Non-hormonal"){
        
        womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = currentHormonal
        
      }
      
    }
    
    # Perform the X^2 test
    
    # Take away those which are unknown for the xi-square
    womenNotUknownTable = womenOnlyTable
    womenNotUknownTable = womenNotUknownTable[ womenNotUknownTable$Hormonal != "Unknown", ]
    
    # Save the xi-sq for nasal, throat, and carrier
    myResultsCarrier       = categoricalXi(womenNotUknownTable, hormonalIndex, carrierIndex,       HORMONAL_FOLDER)
    myResultsNasalCarrier  = categoricalXi(womenNotUknownTable, hormonalIndex, nasalCarrierIndex,  HORMONAL_FOLDER)
    myResultsThroatCarrier = categoricalXi(womenNotUknownTable, hormonalIndex, throatCarrierIndex, HORMONAL_FOLDER)
    
    # Show results
    print("------ B (N,P,LE,HE) -------------------")
    print(myResultsCarrier[[7]])
    print(myResultsNasalCarrier[[7]])
    print(myResultsThroatCarrier[[7]])
    
  }
  
  # ------------------------------------------------------
  #               C
  #  Non hormonal, hormonal
  # -------------------------------------------------------
  {
    
    # Add the info for progesting and progesting + estradiol
    womenOnlyTable$Hormonal = "None or non-hormonal"
    totalContraceptives = nrow(contraceptivesTable)
    for (i in 1:totalContraceptives) {
      
      # Get the ID
      currentID = contraceptivesTable$ID[i]
      
      # Get the hormonal type
      currentHormonal = contraceptivesTable$Hormonal[i]
      
      # If it is a non-hormonal type, register it on the new column
      if(currentHormonal!="Non-hormonal"){
        
        if(currentHormonal!="Unknown"){
          
          womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = "Hormonal"
          
        }
        else{
          womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = "Unknown"
        }
        
      }
      
    }
    
    # Perform the X^2 test
    
    # Take away those which are unknown for the xi-square
    womenNotUknownTable = womenOnlyTable
    womenNotUknownTable = womenNotUknownTable[ womenNotUknownTable$Hormonal != "Unknown", ]
    
    # Save the xi-sq for nasal, throat, and carrier
    myResultsCarrier       = categoricalXi(womenNotUknownTable, hormonalIndex, carrierIndex,       HORMONAL_FOLDER)
    myResultsNasalCarrier  = categoricalXi(womenNotUknownTable, hormonalIndex, nasalCarrierIndex,  HORMONAL_FOLDER)
    myResultsThroatCarrier = categoricalXi(womenNotUknownTable, hormonalIndex, throatCarrierIndex, HORMONAL_FOLDER)
    
    # Show results
    print("------ C (N,H) -------------------")
    print(myResultsCarrier[[7]])
    print(myResultsNasalCarrier[[7]])
    print(myResultsThroatCarrier[[7]])
    
  }
  
  # ------------------------------------------------------
  #               D
  #  Non hormonal + Combination, Progestin
  # -------------------------------------------------------
  {
    
    # Add the info for progesting and progesting + estradiol
    womenOnlyTable$Hormonal = "None or Combination"
    totalContraceptives = nrow(contraceptivesTable)
    for (i in 1:totalContraceptives) {
      
      # Get the ID
      currentID = contraceptivesTable$ID[i]
      
      # Get the hormonal type
      currentHormonal = contraceptivesTable$Hormonal[i]
      
      # If it is a non-hormonal type, register it on the new column
      if(currentHormonal!="Non-hormonal"){
        
        if(currentHormonal!="Unknown"){
          
          if(currentHormonal!="Progestin-Estradiol"){
            
            womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = "Progestin"
            
          }
          
        }
        else{
          womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = "Unknown"
        }
        
      }
      
    }
    
    # Perform the X^2 test
    
    # Take away those which are unknown for the xi-square
    womenNotUknownTable = womenOnlyTable
    womenNotUknownTable = womenNotUknownTable[ womenNotUknownTable$Hormonal != "Unknown", ]
    
    # Save the xi-sq for nasal, throat, and carrier
    myResultsCarrier       = categoricalXi(womenNotUknownTable, hormonalIndex, carrierIndex,       HORMONAL_FOLDER)
    myResultsNasalCarrier  = categoricalXi(womenNotUknownTable, hormonalIndex, nasalCarrierIndex,  HORMONAL_FOLDER)
    myResultsThroatCarrier = categoricalXi(womenNotUknownTable, hormonalIndex, throatCarrierIndex, HORMONAL_FOLDER)
    
    # Show results
    print("------ D (N&P+E,P) -------------------")
    print(myResultsCarrier[[7]])
    print(myResultsNasalCarrier[[7]])
    print(myResultsThroatCarrier[[7]])
    
  }
}

# DESCRIPTIVE PLOTS FOR THE HC USE (Case B , Non hormonal, Progesting, High Estradiol, Low Estradiol)
{
  
  totalContraceptives = nrow(contraceptivesTable)
  
  # Add the info for progesting and progesting + estradiol
  # This is the combination with better p-value
  womenOnlyTable$Hormonal = "None or non-hormonal"
  totalContraceptives = nrow(contraceptivesTable)
  for (i in 1:totalContraceptives) {
    
    # Get the ID
    currentID = contraceptivesTable$ID[i]
    
    # Get the hormonal type
    currentHormonal = contraceptivesTable$Hormonal[i]
    
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
    
    # If it is a non-hormonal type, register it on the new column
    if(currentHormonal!="Non-hormonal"){
      
      womenOnlyTable[womenOnlyTable$ID == currentID,]$Hormonal = currentHormonal
      
    }
    
  }
  
  
  
  
  # Make plots with the general information
  
  # Women divided by the type of hormones in the contraceptive
  {
    # -- Absolute
    barplotResults = doBarPlot(womenOnlyTable, hormonalIndex, HORMONAL_FOLDER,
                               plotTitle = "Absolute frequency of use of contraceptives grouped by hormonal content.")
    # -- Pie
    relplotResults = doBarPlot(womenOnlyTable, hormonalIndex, HORMONAL_FOLDER,
                               plotTitle = "Relative frequency of use of contraceptives grouped by hormonal content.",
                               cropNumbers = 0.05,
                               countingType = "identity",
                               polarCoordinates = TRUE)    
  }

  # If a woman takes contraceptives, which type  
  {
    
    # -- Absolute
    barplotResults = doBarPlot(contraceptivesDF, 4, HORMONAL_FOLDER, # 4 is the type
                               plotTitle = "Absolute frequency of hormonal contraceptives grouped by type.") 
    # -- Pie
    relplotResults = doBarPlot(contraceptivesDF, 4, HORMONAL_FOLDER,
                               plotTitle = "Relative frequency of hormonal contraceptives grouped by type.",
                               cropNumbers = 0.05,
                               countingType = "identity",
                               polarCoordinates = TRUE)
  }
  
  # Women divided by hormone type and relative frequency of carrier
  {
    
    relCombinePlotResults = doBarRelativeCombinePlot(womenOnlyTable,
                                                     hormonalIndex,
                                                     carrierIndex,
                                                     HORMONAL_FOLDER,
                                                     plotTitle = "Relative frequency of type of hormonal content grouped by carrier status.")
    
    relCombinePlotResults = doBarRelativeCombinePlot(womenOnlyTable,
                                                     carrierIndex,
                                                     hormonalIndex,
                                                     HORMONAL_FOLDER,
                                                     colorsVector = COLOR_VECTOR_CARRIER,
                                                     plotTitle = "Relative frequency of carrier status grouped by hormonal content.")
    
    relCombinePlotResults = doBarRelativeCombinePlot(womenOnlyTable,
                                                     nasalCarrierIndex,
                                                     hormonalIndex,
                                                     HORMONAL_FOLDER,
                                                     colorsVector = COLOR_VECTOR_CARRIER,
                                                     plotTitle = "Relative frequency of type of hormonal content grouped by nasal carrier status.")
    
    relCombinePlotResults = doBarRelativeCombinePlot(womenOnlyTable,
                                                     hormonalIndex,
                                                     nasalCarrierIndex,
                                                     HORMONAL_FOLDER,
                                                     plotTitle = "Relative frequency of nasal carrier status grouped by hormonal content.")
  }
  
}

# BOXPLOT FOR BLOOD VARIABLES
{
  myBoxplotResultsP = doCategoricalBoxPlot (womenOnlyTable,
                                            nasalCarrierIndex,
                                            progestinIndex,
                                            HORMONAL_FOLDER,
                                            showPValues  = TRUE,
                                            colorsVector = COLOR_VECTOR_CARRIER)
  
  myBoxplotResultsE = doCategoricalBoxPlot (womenOnlyTable,
                                            nasalCarrierIndex,
                                            estradiolIndex,
                                            HORMONAL_FOLDER,
                                            showPValues  = TRUE,
                                            colorsVector = COLOR_VECTOR_CARRIER)
  
  
  # 0 is estradiol, the rest is progesterone, testosterone, SHBG, LH, FSH, HBA, Albumin, 25(OH)D
  for (i in 0:8) {
    
    myBoxplotResultsE = doCategoricalBoxPlot (womenOnlyTable,
                                              nasalCarrierIndex,
                                              estradiolIndex+i,
                                              HORMONAL_FOLDER,
                                              showPValues  = TRUE,
                                              colorsVector = COLOR_VECTOR_CARRIER)
    
  }
}

# Endo vs Exo progestin
{
  # Divided women on exogenous and endogenous P
  womanExoProgestin  = womenOnlyTable[womenOnlyTable$Hormonal == "Low Estrogen" | womenOnlyTable$Hormonal == "Progestin" | womenOnlyTable$Hormonal == "High Estrogen",]
  womanEndoProgestin = womenOnlyTable[womenOnlyTable$Hormonal == "None or non-hormonal",]
  
  # Women that don't take hormonal
  myBoxplotResultsExoP = doCategoricalBoxPlot (womanExoProgestin,
                                               nasalCarrierIndex,
                                               progestinIndex,
                                               HORMONAL_FOLDER,
                                               showPValues  = TRUE,
                                               colorsVector = COLOR_VECTOR_CARRIER)
  
  # Women that takes hormonal
  myBoxplotResultsEndoP = doCategoricalBoxPlot (womanEndoProgestin,
                                                nasalCarrierIndex,
                                                progestinIndex,
                                                HORMONAL_FOLDER,
                                                showPValues  = TRUE,
                                                colorsVector = COLOR_VECTOR_CARRIER) 
  
  # Compare carriers population
  
  print("Exo with respect nasal carrier")
  print(sum(womanExoProgestin[,nasalCarrierIndex] == "Positive")/nrow(womanExoProgestin))
  print("Endo with respect nasal carrier")
  print(sum(womanEndoProgestin[,nasalCarrierIndex] == "Positive")/nrow(womanEndoProgestin))
  
  
  # Divided women on exogenous and endogenous E
  womanExoEstrogen  = womenOnlyTable[womenOnlyTable$Hormonal == "Low Estrogen" | womenOnlyTable$Hormonal == "High Estrogen",]
  #womanEndoEstrogen = womenOnlyTable[womenOnlyTable$Hormonal == "None or non-hormonal" | womenOnlyTable$Hormonal == "Progestin",]
  womanEndoEstrogen = womenOnlyTable[womenOnlyTable$Hormonal == "None or non-hormonal",]
  
  # Women that take external estrogen
  myBoxplotResultsExoP = doCategoricalBoxPlot (womanExoEstrogen,
                                               nasalCarrierIndex,
                                               estradiolIndex,
                                               HORMONAL_FOLDER,
                                               showPValues  = TRUE,
                                               colorsVector = COLOR_VECTOR_CARRIER)
  
  # Women that take external estrogen
  myBoxplotResultsExoP = doCategoricalBoxPlot (womanEndoEstrogen,
                                               nasalCarrierIndex,
                                               estradiolIndex,
                                               HORMONAL_FOLDER,
                                               showPValues  = TRUE,
                                               colorsVector = COLOR_VECTOR_CARRIER)
  
  print("Exo Estrogen with respect nasal carrier")
  print(sum(womanExoEstrogen[,nasalCarrierIndex] == "Positive")/nrow(womanExoEstrogen))
  print("Endo Estrogen with respect nasal carrier")
  print(sum(womanEndoEstrogen[,nasalCarrierIndex] == "Positive")/nrow(womanEndoEstrogen))
  
  
}











# # Take away those which are unknown for the xi-square
# womenNotUknownTable = womenOnlyTable
# womenNotUknownTable = womenNotUknownTable[ womenNotUknownTable$Hormonal != "Unknown", ]

# # Save the xi-sq for both combinations
# # p-v = 0.6
# #myResultsCarrier = categoricalXi(womenOnlyTable, hormonalIndex, carrierIndex, HORMONAL_FOLDER)
# # p-v = 0.6
# myResultsCarrier1 = categoricalXi(womenOnlyTable,      hormonalIndex, nasalCarrierIndex, HORMONAL_FOLDER)
# # p-v = 0.6
# myResultsCarrier2 = categoricalXi(womenNotUknownTable, hormonalIndex, nasalCarrierIndex, HORMONAL_FOLDER)


# p-v = 0.46
#myResultsCarrier = categoricalXi(womenNotUknownTable, hormonalIndex, carrierIndex, HORMONAL_FOLDER)

# But Estradiol is very suspicious

# womenPPETable = womenNotUknownTable
# womenPPETable = womenPPETable[ womenPPETable$Hormonal != "None or non-hormonal", ]
# 
# myResultsCarrier = categoricalXi(womenPPETable, hormonalIndex, carrierIndex, HORMONAL_FOLDER)





# Testosterone show slighly for women, but nothing for men
# testosteroneIndex  = grep("Testosterone", colnames(womenOnlyTable))
# 
# myBoxplotResultsE = doCategoricalBoxPlot (menOnlyTable,
#                                           carrierIndex,
#                                           testosteroneIndex,
#                                           HORMONAL_FOLDER,
#                                           showPValues  = TRUE,
#                                           colorsVector = COLOR_VECTOR_CARRIER)






# Test for snuff, women only blabla
# p-v = 0.6
# myResultsCarrier = categoricalXi(womenOnlyTable, snuffIndex, nasalCarrierIndex, HORMONAL_FOLDER)
# aa = myResultsCarrier[[2]]
# aa = aa[1:3,]
