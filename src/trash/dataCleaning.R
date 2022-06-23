# -----------------------------------------------------------------------------
#
# This script is for reading RAW data and clean it and prepare it for further
# analysis.
#
# The script does:
#
#   - Read the files with weird column name and transform them into human
#     readable column names
#
#   - Change the numerical codes (ie 1 = Man , 0 = Woman) into human readable
#     strings.
#
#   - Replace all IDs for indexes, so a person ID 11223300 goes into 1, which
#     makes easy finding and indexing later. Also anonymize the data further.
#
#   - Replace NA / NULL values for numerical values encoding whatever we need
#     in each case.
#
#   - Add new info base on the RAW info. For example the BMI category from
#     the BMI number, or the SA carrier status based on the results of the
#     tests.
#
#   - Create the 6 friendship network matrices. IT DOES NOT MELT the matrix.
#
# All of this is then saved in different CSVs which are ready to be read and
# run further analysis. You can use these CSVs in any program you want and is
# not R dependent. But if you want to use R, then go to load2.R script to
# load those CSV into memory in R.
#
# -----------------------------------------------------------------------------

# Add the needed libraries

library(plyr)      # You need to load dplyr in other libraries for the plot to
                   # work properly, this setup completely mess up everything, 
                   # but dplyr is total shit doing the mutates whether plyr
                   # is plain and simple.

library(dplyr)     # left_join in the complete table and %% mutate
                   #
                   # I don't have enough word to describe how much I hate
                   # dplyr. It doesn't make sense. Is complicated to read.
                   # is not that great in performance either. You need to write
                   # a lot of lines to do something very simple!. I HATE YOU!!!
                   #
                   # Just look at the mutate functions, are a complete mess!
                   # THAT IT! I __*REFUSE*__ to use DPLYR for mutating.
                   # The bug with the graphic libraries stays. YOU CAN'T EVEN
                   # MUTATE NAs , need to do yet another pass through the whole
                   # dataset! WTH?? .default doesn't even mean default(), is
                   # does mean everything that is not NA or NULL. AAAAHHH!!!!!

# Set working directory to file location
# -----------------------------------------------------------------------------
{

  this.dir = dirname(parent.frame(2)$ofile)
  setwd(this.dir)

  #source("tools.R",      encoding="utf-8")
  source("constants.R",  encoding="utf-8")

}

# Init the log
# -----------------------------------------------------------------------------
{
  logTXTFileConnection = file(CLEANING_LOG_PATH, 'w')
  logLine              = paste( "CLEANING DATA LOG at: ", RIGHT_NOW, sep = "")
  write( logLine ,
         file = logTXTFileConnection,
         append = FALSE)
  
  
  dataCleaningLogDF            = data.frame(matrix(NA, nrow = 4, ncol =3))
  colnames(dataCleaningLogDF)  = c("Concept", "Total", "Relative")
  dataCleaningLogDF$Concept[1] = "Total IDs:"
  dataCleaningLogDF$Concept[2] = "- Total Deleted IDs:"
  dataCleaningLogDF$Concept[3] = "- Total Remaining IDs:"
  dataCleaningLogDF$Concept[4] = "Total Lost Edges:"
}

# Read the data into DFs
# -----------------------------------------------------------------------------
{
# ---- Main tables
originalPhenotypeTable = read.csv(FRIENSHIP_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
originalNetworkTable   = read.csv(FRIENSHIP_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
originalAureusTable    = read.csv(FRIENSHIP_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
originalHormonalTable  = read.csv(HORMONAL_FILEPATH,  fileEncoding = "ISO-8859-14", stringsAsFactors = FALSE, sep = ";") # Thank you windows for destroying every standard of text encoding everywhere ever.

# ---- Count how many rows we have in the original data
originalRows                  = nrow(originalPhenotypeTable)
  

# ---- Read the variables that you want for each table
# --------- Create a new dataframe with the same rows as the original but with empty columns
#           The empty column part is easier to have some random column and take it away later
{
  phenotypeTable            =  data.frame(matrix(NA, nrow = originalRows, ncol = 1))
  colnames(phenotypeTable)  = c("Aux1")
  
  networkTable              =  data.frame(matrix(NA, nrow = originalRows, ncol = 1))
  colnames(networkTable)    = c("Aux1")
  
  aureusTable               =  data.frame(matrix(NA, nrow = originalRows, ncol = 1))
  colnames(aureusTable)     = c("Aux1")
  
  medicineTable             =  data.frame(matrix(NA, nrow = originalRows, ncol = 1))
  colnames(medicineTable)   = c("Aux1")

}

# ---------- Add the desired variables
#            Only original data, you can transform data later
{
  # -------------- For the phenotype table
  {
    phenotypeTable$ID           = originalPhenotypeTable$pers_key_ff1
    phenotypeTable$Age          = originalPhenotypeTable$AGE_FF1
    phenotypeTable$Sex          = originalPhenotypeTable$SEX_FF1
    phenotypeTable$BMI          = originalPhenotypeTable$BMI_FF1
    phenotypeTable$School       = originalPhenotypeTable$HIGH_SCHOOL_MAIN_PROGRAM_FF1
    phenotypeTable$Class        = originalPhenotypeTable$HIGH_SCHOOL_CLASS_FF1
    phenotypeTable$HighSchoolID = originalPhenotypeTable$HIGH_SCHOOL_NAME_FF1
    phenotypeTable$Smoke        = originalPhenotypeTable$SMOKE_FF1
    phenotypeTable$Snuff        = originalPhenotypeTable$SNUFF_FF1
    phenotypeTable$Sports       = originalPhenotypeTable$PHYS_ACT_LEISURE_FF1
    phenotypeTable$Active       = originalPhenotypeTable$PHYS_ACT_OUTSIDE_SCHOOL_FF1 # This variable is very weird, it ask the person whether he is active in sports
                                                                                     # But the previous questions is exactly that with more extra information
                                                                                     # In the original table you have even more info about the frequency of sports and what not

    phenotypeTable$Antibiotics  = originalPhenotypeTable$ANTIBIOTICS_FF1
    phenotypeTable$AntiBrand    = originalPhenotypeTable$ANTIBIOTICS_BRAND1_FF1 # There is a second column in the original table, but nobody takes 2 different antibiotics, only one branch
  }

  # -------------- For the network table
  {
      # ------------------ Basic stuff
      networkTable$ID        = originalNetworkTable$pers_key_ff1
      networkTable$Created   = originalNetworkTable$NETWORK_DATE_FF1
      networkTable$Overwiew  = originalNetworkTable$NETWORK_OVERVIEW_FF1 # 0 to 10, how good this describe your life

      # ------------------ Your 5 friens, if any
      networkTable$Friend1   = originalNetworkTable$FRIEND_1_FF1
      networkTable$Friend2   = originalNetworkTable$FRIEND_2_FF1
      networkTable$Friend3   = originalNetworkTable$FRIEND_3_FF1
      networkTable$Friend4   = originalNetworkTable$FRIEND_4_FF1
      networkTable$Friend5   = originalNetworkTable$FRIEND_5_FF1

      # ------------------ The contact with your friends

      # ---------------------- Friend 1
      networkTable$Friend1Physical = originalNetworkTable$FRIEND1_PHYSICAL_CONTACT_FF1
      networkTable$Friend1School   = originalNetworkTable$FRIEND1_CONTACT_SCHOOL_FF1
      networkTable$Friend1Sport    = originalNetworkTable$FRIEND1_CONTACT_SPORT_FF1
      networkTable$Friend1Home     = originalNetworkTable$FRIEND1_CONTACT_HOME_FF1
      networkTable$Friend1Other    = originalNetworkTable$FRIEND1_CONTACT_OTHER_FF1

      # ---------------------- Friend 2
      networkTable$Friend2Physical = originalNetworkTable$FRIEND2_PHYSICAL_CONTACT_FF1
      networkTable$Friend2School   = originalNetworkTable$FRIEND2_CONTACT_SCHOOL_FF1
      networkTable$Friend2Sport    = originalNetworkTable$FRIEND2_CONTACT_SPORT_FF1
      networkTable$Friend2Home     = originalNetworkTable$FRIEND2_CONTACT_HOME_FF1
      networkTable$Friend2Other    = originalNetworkTable$FRIEND2_CONTACT_OTHER_FF1

      # ---------------------- Friend 3
      networkTable$Friend3Physical = originalNetworkTable$FRIEND3_PHYSICAL_CONTACT_FF1
      networkTable$Friend3School   = originalNetworkTable$FRIEND3_CONTACT_SCHOOL_FF1
      networkTable$Friend3Sport    = originalNetworkTable$FRIEND3_CONTACT_SPORT_FF1
      networkTable$Friend3Home     = originalNetworkTable$FRIEND3_CONTACT_HOME_FF1
      networkTable$Friend3Other    = originalNetworkTable$FRIEND3_CONTACT_OTHER_FF1

      # ---------------------- Friend 4
      networkTable$Friend4Physical = originalNetworkTable$FRIEND4_PHYSICAL_CONTACT_FF1
      networkTable$Friend4School   = originalNetworkTable$FRIEND4_CONTACT_SCHOOL_FF1
      networkTable$Friend4Sport    = originalNetworkTable$FRIEND4_CONTACT_SPORT_FF1
      networkTable$Friend4Home     = originalNetworkTable$FRIEND4_CONTACT_HOME_FF1
      networkTable$Friend4Other    = originalNetworkTable$FRIEND4_CONTACT_OTHER_FF1

      # ---------------------- Friend 5
      networkTable$Friend5Physical = originalNetworkTable$FRIEND5_PHYSICAL_CONTACT_FF1
      networkTable$Friend5School   = originalNetworkTable$FRIEND5_CONTACT_SCHOOL_FF1
      networkTable$Friend5Sport    = originalNetworkTable$FRIEND5_CONTACT_SPORT_FF1
      networkTable$Friend5Home     = originalNetworkTable$FRIEND5_CONTACT_HOME_FF1
      networkTable$Friend5Other    = originalNetworkTable$FRIEND5_CONTACT_OTHER_FF1



    }

  # -------------- For the S.Aureus table
  {

      # ------------------ Basic stuff
      aureusTable$ID               = originalAureusTable$pers_key_ff1
      aureusTable$Date             = originalAureusTable$DATE_CULTURE_DAY0_FF1
      aureusTable$S2Date           = originalHormonalTable$new_attendance_date
      # ------------------ Experiment worked
      aureusTable$NasalGrowth      = originalAureusTable$CONTROL_NASAL_DAY2_FF1   # Did something grew in the nose
      aureusTable$ThroatGrowth     = originalAureusTable$CONTROL_THROAT_DAY2_FF1  # Did something grew in the throat (This two variables are tells if the S.Aureus is negative )
      #                                                                             (is because the experiment fail, or there is no SA at all)
      # ------------------ If the experiment worked, how much did the SA grow
      aureusTable$NasalAureus      = originalAureusTable$STAPH_NASAL_DAY2_FF1
      aureusTable$ThroatAureus     = originalAureusTable$STAPH_THROAT_DAY2_FF1
      aureusTable$NasalPopulation  = originalAureusTable$STAPH_GROWTH_NASAL_DAY2_FF1
      aureusTable$ThroatPopulation = originalAureusTable$STAPH_GROWTH_THROAT_DAY2_FF1

      # ------------------ If the experiment worked, we enrich the plate to make grow even more, how did it grow after that
      aureusTable$EnrichNasalAureus      = originalAureusTable$STAPH_NASAL_ENRICH_FF1
      aureusTable$EnrichThroatAureus     = originalAureusTable$STAPH_THROAT_ENRICH_FF1
      aureusTable$EnrichNasalPopulation  = originalAureusTable$STAPH_GROWTH_NASAL_ENRICH_FF1
      aureusTable$EnrichThroatPopulation = originalAureusTable$STAPH_GROWTH_THROAT_ENRICH_FF1

      # ------------------ Coagulase test to check for the presence of SA (positive) or S.Epidermitis or S.Saprophyticus.
      aureusTable$CoagulaseNasal        = originalAureusTable$STAPH_COAGULASE_NASAL_FF1
      aureusTable$CoagulaseThroat       = originalAureusTable$STAPH_COAGULASE_THROAT_FF1
      aureusTable$CoagulaseEnrichNasal  = originalAureusTable$STAPH_COAG_NASAL_ENRICH_FF1
      aureusTable$CoagulaseEnrichThroat = originalAureusTable$STAPH_COAG_THROAT_ENRICH_FF1

      # ------------------ SPA-Typing variables
      aureusTable$SPAThroat1            = originalAureusTable$SPA_THROAT1_FF1
      aureusTable$SPAThroat2            = originalAureusTable$SPA_THROAT2_FF1
      aureusTable$SPAThroatClonning     = originalAureusTable$CC_THROAT1_FF1
      aureusTable$SPAThroatCount        = originalAureusTable$CCN_THROAT1_FF1
      aureusTable$SPANasal1             = originalAureusTable$SPA_NASAL1_FF1
      aureusTable$SPANasal2             = originalAureusTable$SPA_NASAL2_FF1

      # ------------------- Freezer variables (we use these for redundancy later)
      aureusTable$FreezerNasalID        = originalHormonalTable$freeze_number_staph_nasal_ff1
      aureusTable$FreezerThroatID       = originalHormonalTable$freeze_number_staph_throat_ff1
      

    }

  # -------------- For the Medicine table
  {
    
    # Personal info that can respect the BC normal form for normalization
    
    # -- We don't need to include the ID. First there is no ID. And second the
    #    person anonymizing the data was not very clever apparently, and just
    #    deleted the ID column. Everything else remain the same in the same
    #    order for all the variables.
    #
    #    So we just copy the ID column from the phenotypes so we can merge both
    #    tables later
    # -- ID
    medicineTable$ID                    = originalPhenotypeTable$pers_key_ff1
    # -- Antropometry
    medicineTable$Height                = originalHormonalTable$height_ff1
    medicineTable$Weight                = originalHormonalTable$weight_ff1
    # -- Recreational drugs
    medicineTable$Alcohol               = originalHormonalTable$alcohol_frequency_ff1
    # -- Medicine (contiune later)
    medicineTable$AntiATC               = originalHormonalTable$antibiotics_atc1_ff1
    # -- Menstruation cycle
    medicineTable$Menstruating          = originalHormonalTable$menses_ff1
    medicineTable$MenstruationRegular   = originalHormonalTable$menses_regularity_ff1
    medicineTable$MenstruationCycle     = originalHormonalTable$menses_cycle_length_ff1
    medicineTable$MenstruationLastStart = originalHormonalTable$menses_start_date_ff1
    medicineTable$ChancePregnancy       = originalHormonalTable$chance_pregnant_ff1
    medicineTable$PregnancyTest         = originalHormonalTable$pregnancy_test_result_ff1
    # -- Puberty
    # ---- Menstruation
    # ---- Fix the Menstruating variable inconsistencies with respect the Menarche variable
    #      this rules are written in the report for verbose explanation.
    for(i in 1:originalRows){
     
      menstrualInfo = originalHormonalTable$menses_ff1[i]
      menarcheInfo  = originalHormonalTable$menarche_ff1[i]
      
      if(is.na(menstrualInfo)  && !is.na(menarcheInfo)) medicineTable$Menstruating[i] = menarcheInfo
      if(!is.na(menstrualInfo) &&  is.na(menarcheInfo)) medicineTable$Menstruating[i] = menstrualInfo
       
    }
    # ---- Combine years and months of start to get a proper numerical value
    medicineTable$MenstruationStart = as.numeric(originalHormonalTable$menarche_age_year_ff1 + (originalHormonalTable$menarche_age_month_ff1 / 12))
    # ---- Boys only
    medicineTable$PubertyHairYear       = originalHormonalTable$pubic_hair_age_male_ff1
    medicineTable$PubertyBodyHair       = originalHormonalTable$puberty_boys_hair_body_ff1
    medicineTable$PubertyFaceHair       = originalHormonalTable$puberty_boys_hair_face_ff1
    medicineTable$PubertyHeight         = originalHormonalTable$puberty_boys_height_ff1
    medicineTable$PubertyVoice          = originalHormonalTable$puberty_boys_voice_ff1
    # -- Nutrition
    medicineTable$FatFish               = originalHormonalTable$fat_fish_ff1
    medicineTable$LeanFish              = originalHormonalTable$lean_fish_ff1
    medicineTable$SeagullEggs           = originalHormonalTable$seagull_eggs_ff1
    medicineTable$ReindeerMeat          = originalHormonalTable$reindeer_ff1
    # -- Biomarkers
    medicineTable$`Estradiol_E2_(nmol/L)` = originalHormonalTable$s_estradiol_ff1
    medicineTable$`Progesterone_(nmol/L)` = originalHormonalTable$s_progesterone_ff1
    medicineTable$`Testosterone_(nmol/L)` = originalHormonalTable$s_testosterone_ff1
    medicineTable$`SHBG_(nmol/L)`         = originalHormonalTable$s_shbg_ff1
    medicineTable$`LH_(IU/L)`             = originalHormonalTable$s_lh_ff1
    medicineTable$`FSH_(IU/L)`            = originalHormonalTable$s_fsh_ff1
    medicineTable$`HBA1C_(%)`             = originalHormonalTable$s_hba1c_ff1
    medicineTable$`Albumin_(g/L)`         = originalHormonalTable$albumin_ff1
    medicineTable$`25(OH)D_(nmol/L)`      = originalHormonalTable$s_25_vitd_ff1
    # -- Contraceptives (see next table, goes the same as medicine)
    medicineTable$Contraceptives        = "Unknown"
    # -- Sociology (nothing here)

    
    # -- These also come from the original hormonal table, but are mixed to get an average
    medicineTable$Waist = (originalHormonalTable$waist1_ff1 + originalHormonalTable$waist2_ff1) / 2
    medicineTable$Hip   = (originalHormonalTable$hip1_ff1   + originalHormonalTable$hip2_ff1)   / 2
    
    # -- Information regarding were the swab samples are valid or not
    # -- Extra information for the AUREUS definition of CARRIER / COLONIZED / NON CARRIER
    medicineTable$WeekNasalGrowth           = originalHormonalTable$control_nasal_day2_ff11   # Did something grew in the nose swab 2?
    medicineTable$WeekThroatGrowth          = originalHormonalTable$control_throat_day2_ff11
    medicineTable$WeekValidNasalCoagulase   = originalHormonalTable$staph_nasal_day2_ff11
    medicineTable$WeekValidNasalEnrichment  = originalHormonalTable$staph_nasal_enrich_ff11
    medicineTable$WeekValidThroatCoagulase  = originalHormonalTable$staph_throat_day2_ff11
    medicineTable$WeekValidThroatEnrichment = originalHormonalTable$staph_throat_enrich_ff11  
    
    
    # ---- COLONIZATION
    # ------- by the Coagulase test
    medicineTable$CoagulaseNasalColonize       = originalHormonalTable$staph_coagulase_nasal_ff1
    medicineTable$CoagulaseThroatColonize      = originalHormonalTable$staph_coagulase_throat_ff1

    # -------  by the Enrichment test
    medicineTable$EnrichmentNasalColonize      = originalHormonalTable$staph_coag_nasal_enrich_ff1
    medicineTable$EnrichmentThroatColonize     = originalHormonalTable$staph_coag_throat_enrich_ff1
    
    # -------  by confirmed one week coagulase test
    medicineTable$WeekCoagulaseNasalColonize   = originalHormonalTable$staph_coagulase_nasal_ff11
    medicineTable$WeekCoagulaseThroatColonize  = originalHormonalTable$staph_coagulase_throat_ff11
    
    # -------  by confirmed one week enrichment test
    medicineTable$WeekEnrichmentNasalColonize  = originalHormonalTable$staph_coag_nasal_enrich_ff11
    medicineTable$WeekEnrichmentThroatColonize = originalHormonalTable$staph_coag_throat_enrich_ff11
    

    # Information to create a proper relational table from the original table
    {
      
      # For medicines
      {
        # Create a new temporal table
        tempTable    = originalHormonalTable
        # Add the IDs
        tempTable$ID = phenotypeTable$ID
        # Delete the columns that we don't need
        tempTable[,c(1:41)]                   = NULL
        tempTable[,c(18:(ncol(tempTable))-1)] = NULL
        # Take away people who don't do drugs
        tempTable = tempTable[tempTable$medication_daily_ff1 ==1,]
        # Take away people who do OTHERS drugs but are still registered as drug users
        totalRows = nrow(tempTable)
        keepRows  = rep(FALSE, totalRows)
        for(i in 1:totalRows){
          
          if(!is.na(tempTable$medication_atc1_ff1[i])) #R is a horrible language, and NA != "" is not FALSE, is NA, so you need to check for NA independently.
            if( tempTable$medication_atc1_ff1[i] != "" ) keepRows[i] = TRUE
            
        }
        tempTable = tempTable[keepRows,]
        totalRows = sum(keepRows)
        # Now we have all the data that we need, now let melt it by medication
        # -- We need to know, how many rows we need in the new dataframe. Count how
        #    many values are missing in columsn 2,3,4 and 5. We garanteed now that
        #    1 is not empty.
        na2 = sum(tempTable$medication_atc2_ff1 != "") # R is a horrible language, you can't call your variable 2na, but you can na2
        na3 = sum(tempTable$medication_atc3_ff1 != "")
        na4 = sum(tempTable$medication_atc4_ff1 != "")
        na5 = sum(tempTable$medication_atc5_ff1 != "")
        totalCells = totalRows + na2 + na3 + na4 + na5
        
        # -- Create the final new dataframe 
        drugUseDF = data.frame(matrix(NA, nrow = totalCells, ncol = 5))
        colnames(drugUseDF) = c("ID", "Brand", "ATC", "Regularity", "Content")
        cellID     = 1
        atcIndexes = c(3,6,9,12,15)
        
        # -- For each row of the temporal table
        for(i in 1:totalRows){
          
          # For each atc code
          for(j in 1:5){
            
            # get the atc code index
            currentIndex = atcIndexes[j]
            
            # For each ATC which is not empty
            if(tempTable[i,currentIndex]!=""){
              
              # Add the info to the final DF
              drugUseDF[cellID,1] = tempTable$ID[i]
              drugUseDF[cellID,2] = tempTable[i , currentIndex - 1]
              drugUseDF[cellID,3] = tempTable[i , currentIndex]
              drugUseDF[cellID,4] = tempTable[i , currentIndex + 1]
              
              cellID = cellID + 1
              
            }
            
          }
          
        }
        
        # Now we have all the data ready, let finish by giving more sense to the
        # numbers, and adding some extra info about the medicine used
        for(i in 1:totalCells){
          
          # Change the regularity string
          if(is.na(drugUseDF$Regularity[i])) drugUseDF$Regularity[i] = "Unknown"
          else{
            if(drugUseDF$Regularity[i] == 1) drugUseDF$Regularity[i] = "Regularly"
            if(drugUseDF$Regularity[i] == 2) drugUseDF$Regularity[i] = "Occasionally"
          }
          
          # Add the ATC information
          drugUseDF$Content[i] = getATCInfo(drugUseDF$ATC[i])  
          
        }
      }
      
      # For diseases
      {
        # Create a new temporal table
        tempTable    = originalHormonalTable
        # Add the IDs
        tempTable$ID = phenotypeTable$ID
        # Delete the columns that we don't need
        tempTable[,c(1:29)]                   = NULL
        tempTable[,c(14:(ncol(tempTable))-1)] = NULL
        
        # Count how many diseases we have
        totalDiseases = 0
        for(i in 1:originalRows){
          
          if(tempTable$icd10_chronic_disease1_ff1[i]     != "") totalDiseases = totalDiseases + 1
          if(tempTable$icd10_chronic_disease2_ff1[i]     != "") totalDiseases = totalDiseases + 1
          if(tempTable$icd10_chronic_disease3_ff1[i]     != "") totalDiseases = totalDiseases + 1
          if(tempTable$icd10_chronic_disease4_ff1[i]     != "") totalDiseases = totalDiseases + 1
          if(tempTable$icd10_chronic_disease5_ff1[i]     != "") totalDiseases = totalDiseases + 1
          if(tempTable$chronic_disease_other_desc_ff1[i] != "") totalDiseases = totalDiseases + 1
          
        }

        # -- Create the final new dataframe 
        diseasesDF = data.frame(matrix(NA, nrow = totalDiseases, ncol = 4))
        colnames(diseasesDF) = c("ID", "Diagnostic", "ICD10", "Title")
        cellID     = 1
        icdIndexes = c(2,4,6,8,10)
        
        # -- For each row of the temporal table
        #    Process the normal ICD10 diseases first
        totalRows = nrow(tempTable)
        for(i in 1:totalRows){
          
          # For each icd code
          for(j in 1:5){
            
            # get the icd code index
            currentIndex = icdIndexes[j]
            
            # For each ATC which is not empty
            if(tempTable[i,currentIndex]!=""){
              
              # Add the info to the final DF
              diseasesDF[cellID,1] = tempTable$ID[i]
              diseasesDF[cellID,2] = tempTable[i , currentIndex - 1]
              diseasesDF[cellID,3] = tempTable[i , currentIndex]
              
              cellID = cellID + 1
              
            }
            
          }
          
        }
        
        # -- For each row of the temporal table
        #    Register the weird OTHER icd next
        for(i in 1:totalRows){
          
          currentIndex = 12
            
          # For each ATC which is not empty
          if(tempTable[i,currentIndex]!=""){
              
            # Add the info to the final DF
            diseasesDF[cellID,1] = tempTable$ID[i]
            diseasesDF[cellID,2] = tempTable[i , currentIndex]
            diseasesDF[cellID,3] = "Ñ99"
                
            cellID = cellID + 1
              
          }
            
          
          
        }
        
        # Now we have all the data ready, let finish by giving more sense to the
        # ICD10 Codes
        for(i in 1:totalDiseases){
          
          # Add the ICD10 information
          diseasesDF$Title[i] = getICD10Info(diseasesDF$ICD10[i])  
          
        }
      }
      
      # For contraceptives
      # For medicines
      {
        # Create a new temporal table
        tempTable    = originalHormonalTable
        # Add the IDs
        tempTable$ID = phenotypeTable$ID
        # Delete the columns that we don't need
        tempTable[,c(1:71)]                   = NULL
        tempTable[,c(14:(ncol(tempTable))-1)] = NULL

        # Take away people who don't use any contraceptives
        tempTable = tempTable[!is.na(tempTable$contraceptives_type_ff1),]
        totalRows = nrow(tempTable)
        
        # Nobody uses vaginal contraceptives, so delete those columns
        tempTable$vaginal_contracept_atc_ff1  = NULL
        tempTable$vaginal_contracept_name_ff1 = NULL
        
        
        # Now we have all the data that we need, now let melt by contraceptives
        # -- Create the final new dataframe 
        contraceptivesDF = data.frame(matrix(NA, nrow = totalRows, ncol = 5))
        colnames(contraceptivesDF) = c("ID", "Brand", "ATC", "Type", "Hormonal")
        
        
        # The rules for the hormonal column goes as follows
        #
        # Non-Hormonal:        Condons
        # Progestin-only:      Cerazette, Nexplanon, Depo-provera
        # Progestin-Estradiol: Mercilon, Yasminelle, Loette 28, Nuvaring,
        #                      Marvelon, Yasmin, Microgynon, Oralcon, Diane,
        #                      Synfase, Evra)
        # Unknown:             Any other brand/type 
        
        # -- For each row of the temporal table
        for(i in 1:totalRows){
          
          # Get the basic variables
          currentID       = tempTable$ID[i]
          currentBrand    = "Unknown"
          currentATC      = "Unknown"
          currentType     = tempTable$contraceptives_type_ff1[i]
          currentHormonal = "Unknown"
          
          # Pills
          if(currentType == 1){
          
            # Fix the type
            currentType     = "Oral"
            # Get the name
            currentBrand    = tempTable$oral_contracept_name_ff1[i]
            
            # If the name is "Annet", this is not a contraceptive, this is the
            # norwegian word for "Other", so this is still unknown. Same for
            # an empty string
            if(currentBrand != "Annet" && currentBrand != ""){
              
              # Get the ATC
              currentATC      = tempTable$oral_contracept_atc_ff1[i]
              if(currentATC == "") currentATC = "Unknown"
              
              # Get the type of hormonal
              currentHormonal = getHormonalType(currentBrand)
            }
            else{
              currentBrand    = "Unknown"
            }
            
          }

          # Injected
          if(currentType == 2){
            
            # Fix the type
            currentType     = "Injected"
            # Get the name
            currentBrand    = tempTable$injected_contracept_name_ff1[i]
            
            # If the name is "Annet", this is not a contraceptive, this is the
            # norwegian word for "Other", so this is still unknown. Same for
            # an empty string
            if(currentBrand != "Annet" && currentBrand != ""){
              
              # Get the ATC
              currentATC      = tempTable$injected_contracept_atc_ff1[i]
              if(currentATC == "") currentATC = "Unknown"
              
              # Get the type of hormonal
              currentHormonal = getHormonalType(currentBrand)
            }
            else{
              currentBrand    = "Unknown"
            }
            
          }
          
          # Subdermal
          if(currentType == 3){
            
            # Fix the type
            currentType     = "Subdermal"
            # Get the name
            currentBrand    = tempTable$subdermal_contracept_name_ff1[i]
            
            # If the name is "Annet", this is not a contraceptive, this is the
            # norwegian word for "Other", so this is still unknown. Same for
            # an empty string
            if(currentBrand != "Annet" && currentBrand != ""){
              
              # Get the ATC
              currentATC      = tempTable$oral_contracept_atc_ff1[i]
              if(currentATC == "") currentATC = "Unknown"
              
              # Get the type of hormonal
              currentHormonal = getHormonalType(currentBrand)
            }
            else{
              currentBrand    = "Unknown"
            }
            
          }
          
          # Condons
          if(currentType == 4){
            
            # Fix the type and hormonal type
            currentType     = "Condons"
            currentHormonal = "Non-hormonal"

          }
            
          # Skin
          if(currentType == 5){
            
            # Fix the type
            currentType     = "Skin"
            # Get the name
            currentBrand    = tempTable$contracep_skin_patch_name_ff1[i]
            
            # If the name is "Annet", this is not a contraceptive, this is the
            # norwegian word for "Other", so this is still unknown. Same for
            # an empty string
            if(currentBrand != "Annet" && currentBrand != ""){
              
              # Get the ATC
              currentATC      = tempTable$contracep_skin_patch_atc_ff1[i]
              if(currentATC == "") currentATC = "Unknown"
              
              # Get the type of hormonal
              currentHormonal = getHormonalType(currentBrand)
            }
            else{
              currentBrand    = "Unknown"
            }
            
          }
          
          # This is vaginal, and never happens
          #if(currentType == 6)
          
          # Others (pretty much no idea of what's going on here)
          if(currentType == 7){
            
            # Fix the type and hormonal type
            currentType     = "Other"
            currentHormonal = "Unknown"
            
          }
          
          # Now that you have all the info properly set, put it into the
          # contraceptives dataframe
          contraceptivesDF$ID[i]       = currentID
          contraceptivesDF$Brand[i]    = currentBrand
          contraceptivesDF$ATC[i]      = currentATC
          contraceptivesDF$Type[i]     = currentType
          contraceptivesDF$Hormonal[i] = currentHormonal
          
          # Also, update the the medicineTable with the contraceptive type
          medicineTable[phenotypeTable$ID == currentID,]$Contraceptives = currentType
          
        }
        
      }
      
    }
    
    # Some extra things
    # Count how many medicine each person takes
    for(i in 1:originalRows){
      medicineTable$TotalMedication[i] = sum(drugUseDF$ID  == phenotypeTable$ID[i])
    }
    # Count how many diseases each person has
    for(i in 1:originalRows){
      medicineTable$TotalDiseases[i]   = sum(diseasesDF$ID == phenotypeTable$ID[i])
    }
    # Add the proper contraceptive info for those which we have
    # (already done)
    
  }

  # ---------- Delete the garbage
  {
      phenotypeTable$Aux1    = NULL
      networkTable$Aux1      = NULL
      aureusTable$Aux1       = NULL
      medicineTable$Aux1     = NULL
      
  }

}

# ---------- Transform and clean data
#            Only original data, you can transform data later
{

    # ---- Convert numbers into categories
    {
      # -------- For the phenotype table
      {

        phenotypeTable$Sex    = mapvalues( phenotypeTable$Sex,
                                           from = c( 1,    "1",    0,      "0",     "",         " ",        "  ",       NA),
                                           to   = c("Man", "Man", "Woman", "Woman", "Unknown",  "Unknown",  "Unknown",  "Unknown"))

        phenotypeTable$School = mapvalues( phenotypeTable$School,
                                           from = c( "1",       "2",      "3",           NA),
                                           to   = c( "General", "Sports", "Vocational",  "Unknown"))

        phenotypeTable$Smoke  = mapvalues( phenotypeTable$Smoke,
                                           from = c( "1",      "2",        "3",     NA),
                                           to   = c( "Never", "Sometimes", "Daily", "Unknown"))
        
        phenotypeTable$Snuff  = mapvalues( phenotypeTable$Snuff,
                                           from = c( "1",      "2",        "3",     NA),
                                           to   = c( "Never", "Sometimes", "Daily", "Unknown"))

        
        # ------------- Sport and activities
        #               Sport original description is:
        #                   1 Reading, watching TV, or other sedentary activity?
        #                   2 Walking, cycling, or other forms of exercise at least 4 hours a week? (including walking or cycling to place of school, shopping, Sunday-walking, etc.)
        #                   3 Participation in recreational sports, heavy outdoor activities, snow clearing etc? (note: duration of activity at least 4 hours a week).
        #                   4 Participation in hard training or sports competitions, regularly several times a week?
        #
        #                   I transform this into 1 = None, 2 = Light, 3 = Medium, 4 = Hard
        #
        #                   (Notice that I don't consider snow cleaning to be Medium, that should be merged with Light. Hard should be the medium and professional should be hard; but whatever)

        phenotypeTable$Sports  = mapvalues( phenotypeTable$Sports,
                                            from = c( "1",      "2",        "3",     "4",    NA),
                                            to   = c( "None",   "Light",    "Medium","Hard", "Unknown"))

        phenotypeTable$Active  = mapvalues( phenotypeTable$Active,
                                            from = c( "1",    "0",  NA),
                                            to   = c( "Yes",  "No", "Unknown"))
                
        # ------------- Antibiotics
        
        phenotypeTable$Antibiotics  = mapvalues( phenotypeTable$Antibiotics,
                                                 from = c( "1",    "0",  NA),
                                                 to   = c( "Yes",  "No", "Unknown"))

        # phenotypeTable =
        # phenotypeTable %>% mutate(Antibiotics=recode(Antibiotics,
        #                                             "1"  = "Yes",
        #                                             "0"  = "No",
        #                                             NA   = "Unknown",
        #                                             .default = "Unknown"))
                
        
        # These don't need any special mapping, just transform into categorical
        
        # ------------- Highschol
        phenotypeTable$HighSchoolID = as.factor(phenotypeTable$HighSchoolID)
        
        # ------------- Class
        phenotypeTable$Class = as.factor(phenotypeTable$Class)
        
      }

      # -------- For the aureus table
      {
        # ------------- The experiment fail?
        # ------------- Nasal growth (1 = Yes, 0 = No, 9 = Non-applicable)
        
        aureusTable$NasalGrowth  = mapvalues( aureusTable$NasalGrowth,
                                              from = c( "1",  "0",  "9",               NA),
                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        # ------------- Throat growth (same)
        
        aureusTable$ThroatGrowth = mapvalues( aureusTable$ThroatGrowth,
                                              from = c( "1",  "0",  "9",               NA),
                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        # ------------- The experiment work, did it grow?
        
        aureusTable$NasalAureus = mapvalues( aureusTable$NasalAureus,
                                             from = c( "1",  "0",  "9",               NA),
                                             to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        aureusTable$ThroatAureus = mapvalues( aureusTable$ThroatAureus,
                                              from = c( "1",  "0",  "9",               NA),
                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        # ------------- The experiment work, it grew, how much?
        # IT SHOULN'T BE ANY 9 IN HERE!!! But they are :/ Same for any other L/M/H variable
        
        aureusTable$NasalPopulation = mapvalues( aureusTable$NasalPopulation,
                                                 from = c( "0",     "1",        "2",     NA,           "9"),
                                                 to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))

        aureusTable$ThroatPopulation = mapvalues( aureusTable$ThroatPopulation,
                                                  from = c( "0",     "1",        "2",     NA,           "9"),
                                                  to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))

        # ------------- The experiment work, something happen and we enrigch it, did it grow then?
        
        aureusTable$EnrichNasalAureus = mapvalues( aureusTable$EnrichNasalAureus,
                                                   from = c( "1",  "0",  "9",               NA),
                                                   to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        aureusTable$EnrichThroatAureus = mapvalues( aureusTable$EnrichThroatAureus,
                                                    from = c( "1",  "0",  "9",               NA),
                                                    to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        # ------------- The experiment work, how much grew after enrichment?
        # IT SHOULN'T BE ANY 9 IN HERE!!! But they are :/ Same for any other L/M/H variable
        
        aureusTable$EnrichNasalPopulation = mapvalues( aureusTable$EnrichNasalPopulation,
                                                       from = c( "0",     "1",        "2",     NA,        "9"),
                                                       to   = c( "Light", "Moderate", "Rich",  "None",    "Non-applicable"))

        aureusTable$EnrichThroatPopulation = mapvalues( aureusTable$EnrichThroatPopulation,
                                                        from = c( "0",     "1",        "2",     NA,        "9"),
                                                        to   = c( "Light", "Moderate", "Rich",  "None",    "Non-applicable"))

        # ------------- The experiment work, something grew, how did the coagulase test went?
        
        aureusTable$CoagulaseNasal         = mapvalues( aureusTable$CoagulaseNasal,
                                                        from = c( "1",       "0",        "9",              NA),
                                                        to   = c("Positive", "Negative", "Non-applicable", "Unknown"))

        aureusTable$CoagulaseThroat       = mapvalues( aureusTable$CoagulaseThroat,
                                                       from = c( "1",       "0",        "9",               NA),
                                                       to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))

        aureusTable$CoagulaseEnrichNasal  = mapvalues( aureusTable$CoagulaseEnrichNasal,
                                                       from = c( "1",       "0",        "9",               NA),
                                                       to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))

        aureusTable$CoagulaseEnrichThroat = mapvalues( aureusTable$CoagulaseEnrichThroat,
                                                       from = c( "1",       "0",        "9",               NA),
                                                       to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))

        # For the new definitions of colonize / carrier
        # ------------- The experiment fail?
        # ------------- Nasal growth (1 = Yes, 0 = No, 9 = Non-applicable)
        medicineTable$WeekNasalGrowth  = mapvalues( medicineTable$WeekNasalGrowth,
                                              from = c( "1",  "0",  "9",               NA),
                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))
        
        # ------------- Throat growth (same)
        medicineTable$WeekThroatGrowth = mapvalues( medicineTable$WeekThroatGrowth,
                                              from = c( "1",  "0",  "9",               NA),
                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))
        
        # SWAB 2
        # Specific test for nasal coagulase and enrichment
        # ------------- The experiment fail?
        # ------------- Nasal coagulase growth (1 = Yes, 0 = No, 9 = Non-applicable)
        medicineTable$WeekValidNasalCoagulase  = mapvalues( medicineTable$WeekValidNasalCoagulase,
                                                            from = c( "1",  "0",  "9",               NA),
                                                            to   = c("Yes", "No", "Non-applicable",  "Unknown"))
        
        # ------------- Nasal enrichment growth (1 = Yes, 0 = No, 9 = Non-applicable)
        medicineTable$WeekValidNasalEnrichment = mapvalues( medicineTable$WeekValidNasalEnrichment,
                                                    from = c( "1",  "0",  "9",               NA),
                                                    to   = c("Yes", "No", "Non-applicable",  "Unknown"))

        # Specific test for throat coagulase and enrichment (same as before)
        medicineTable$WeekValidThroatCoagulase  = mapvalues( medicineTable$WeekValidThroatCoagulase,
                                                            from = c( "1",  "0",  "9",               NA),
                                                            to   = c("Yes", "No", "Non-applicable",  "Unknown"))
        
        
        medicineTable$WeekValidThroatEnrichment = mapvalues( medicineTable$WeekValidThroatEnrichment,
                                                            from = c( "1",  "0",  "9",               NA),
                                                            to   = c("Yes", "No", "Non-applicable",  "Unknown"))
        
        
        
        medicineTable$CoagulaseNasalColonize       = mapvalues( medicineTable$CoagulaseNasalColonize,
                                                                from = c( "1",       "0",        "9",               NA),
                                                                to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
        medicineTable$CoagulaseThroatColonize       = mapvalues( medicineTable$CoagulaseThroatColonize,
                                                                 from = c( "1",       "0",        "9",               NA),
                                                                 to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
        medicineTable$EnrichmentNasalColonize       = mapvalues( medicineTable$EnrichmentNasalColonize,
                                                                 from = c( "1",       "0",        "9",               NA),
                                                                 to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
        medicineTable$EnrichmentThroatColonize       = mapvalues( medicineTable$EnrichmentThroatColonize,
                                                                  from = c( "1",       "0",        "9",               NA),
                                                                  to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))

        medicineTable$WeekCoagulaseNasalColonize       = mapvalues( medicineTable$WeekCoagulaseNasalColonize,
                                                                    from = c( "1",       "0",        "9",               NA),
                                                                    to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
        medicineTable$WeekCoagulaseThroatColonize       = mapvalues( medicineTable$WeekCoagulaseThroatColonize,
                                                                     from = c( "1",       "0",        "9",               NA),
                                                                     to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
        medicineTable$WeekEnrichmentNasalColonize       = mapvalues( medicineTable$WeekEnrichmentNasalColonize,
                                                                    from = c( "1",       "0",        "9",               NA),
                                                                    to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
        medicineTable$WeekEnrichmentThroatColonize       = mapvalues( medicineTable$WeekEnrichmentThroatColonize,
                                                                     from = c( "1",       "0",        "9",               NA),
                                                                     to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
        
      }

      # -------- For the network
      {
        # Technically, there are a bunch of 1s and 0s that represent yes or no, but I'm going to leave then
        # as they are because is faster to do math with that rather than "yes", "no", types.

        # I can change them to TRUE or FALSE, but then we lost the NAs information
        # Later on we change NAs to -1 to represent to friend in here, rather than FALSE which would mean no contact with that friend

      }

      # -------- For the medicine table
      {
        phenotypeTable$Snuff  = mapvalues( phenotypeTable$Snuff,
                                           from = c( "1",      "2",        "3",     NA),
                                           to   = c( "Never", "Sometimes", "Daily", "Unknown"))
        
        phenotypeTable$HighSchoolID = as.factor(phenotypeTable$HighSchoolID)
        
        
        medicineTable$Alcohol  = mapvalues( medicineTable$Alcohol,
                                            from = c( "1",      "2",                     "3",                   "4",                 "5",                          NA),
                                            to   = c( "Never", "Once per month or less", "2-4 times per month", "2-3 times per week", "4 or more times per week",  "Unknown"))        
      }

      # -- Menstruation cycle
      {
        medicineTable$Menstruating         = mapvalues( medicineTable$Menstruating,
                                                        from = c( "1",   "0",   NA),
                                                        to   = c( "Yes", "No", "Unknown"))
        
        medicineTable$MenstruationRegular  = mapvalues( medicineTable$MenstruationRegular,
                                                        from = c( "1",      "2",       "3",          NA),
                                                        to   = c( "Always", "Usually", "Irregular", "Unknown"))
        
        medicineTable$ChancePregnancy      = mapvalues( medicineTable$ChancePregnancy,
                                                        from = c( "1",   "0",   NA),
                                                        to   = c( "Yes", "No", "Unknown"))
        
        medicineTable$PregnancyTest        = mapvalues( medicineTable$PregnancyTest,
                                                        from = c( "1",        "0",        "2",         NA),
                                                        to   = c( "Positive", "Negative", "Uncertain", "Unknown"))       
      }

      # -- Puberty
      # ---- Boys only
      {
        medicineTable$PubertyHairYear = mapvalues( medicineTable$PubertyHairYear,
                                                   from = c( "1",               "2",  "3",  "4",  "5",  "6",  "7",             NA),
                                                   to   = c( "Younger than 10", "10", "11", "12", "13", "14", "Older than 14", "Unknown"))
        
        medicineTable$PubertyBodyHair = mapvalues( medicineTable$PubertyBodyHair,
                                                   from = c( "1",             "2",            "3",        "4",         NA),
                                                   to   = c( "Not yet grown", "Barely grown", "Underway", "Completed", "Unknown"))
        
        medicineTable$PubertyFaceHair = mapvalues( medicineTable$PubertyFaceHair,
                                                   from = c( "1",             "2",            "3",        "4",         NA),
                                                   to   = c( "Not yet grown", "Barely grown", "Underway", "Completed", "Unknown"))
        
        medicineTable$PubertyHeight   = mapvalues( medicineTable$PubertyHeight,
                                                   from = c( "1",             "2",              "3",        "4",         NA),
                                                   to   = c( "Not yet spurt", "Barely started", "Underway", "Completed", "Unknown"))
        
        medicineTable$PubertyVoice    = mapvalues( medicineTable$PubertyVoice,
                                                   from = c( "1",                "2",              "3",        "4",         NA),
                                                   to   = c( "Not yet changing", "Barely started", "Underway", "Completed", "Unknown"))
      }
      
      # -- Nutrition
      # medicineTable$FatFish               = originalHormonalTable$fat_fish_ff1
      # medicineTable$LeanFish              = originalHormonalTable$lean_fish_ff1
      # medicineTable$SeagullEggs           = originalHormonalTable$seagull_eggs_ff1
      # medicineTable$ReindeerMeat          = originalHormonalTable$reindeer_ff1
      # -- Biomarkers

      # -- Contraceptives (see next table, goes the same as medicine)
      
      # -- Sociology (nothing here)
      
      
    }

    # ----Convert numbers that are strings into proper number variables
    {

      # -------- For the phenotype table
      {

      }

      # -------- For the aureus table
      {

      }

    }

    # ---- Replace the IDs for indexes, it makes looking and writing something much more easy in general
    #      And you need to do this in all the general tables
    {

      # Keep track of all key changes
      replacementKeysDF           = data.frame(matrix(NA, nrow = originalRows, ncol = 2))
      colnames(replacementKeysDF) = c("Old_Key", "New_Key")

      # Prepare the log for writing the keys replacements
      logLine = paste("The original keys were replaced like this: ")
      write( logLine , file   = logTXTFileConnection, append = TRUE )
      logLine = paste("    ----    ")
      write( logLine , file   = logTXTFileConnection, append = TRUE )

      # Check all the IDs
      totalIDs   = length(phenotypeTable$ID)

      # For each ID, replace the ID for a consecutive number
      for(i in 1:totalIDs){

        # Save the current key
        replacementKey               = phenotypeTable$ID[i]

        # Set the new key in every table
        #
        #    (take into account that different tables might be sorted differently)
        #    (default sorting is the phenotype table, whatever that is)
        #
        # -- Phenotype table
        phenotypeTable$ID[i] = i
        # -- Aureus table
        aureusTable$ID     = replace(aureusTable$ID,  aureusTable$ID  == replacementKey,   i)
        # ---- The medicine table
        medicineTable$ID   = replace(medicineTable$ID, medicineTable$ID == replacementKey, i)
        
        # -- Network table
        # ---- IDs
        networkTable$ID = replace(networkTable$ID, networkTable$ID == replacementKey, i)
        # ---- Each one of the friends
        networkTable$Friend1 = replace(networkTable$Friend1, networkTable$Friend1 == replacementKey, i)
        networkTable$Friend2 = replace(networkTable$Friend2, networkTable$Friend2 == replacementKey, i)
        networkTable$Friend3 = replace(networkTable$Friend3, networkTable$Friend3 == replacementKey, i)
        networkTable$Friend4 = replace(networkTable$Friend4, networkTable$Friend4 == replacementKey, i)
        networkTable$Friend5 = replace(networkTable$Friend5, networkTable$Friend5 == replacementKey, i)
        
        
        # Each of the relational tables
        # ---- The disease table
        diseasesDF$ID        = replace(diseasesDF$ID, diseasesDF$ID == replacementKey, i)
        # ---- The medicine table
        drugUseDF$ID         = replace(drugUseDF$ID, drugUseDF$ID == replacementKey, i)
        # ---- The contraceptive table
        contraceptivesDF$ID  = replace(contraceptivesDF$ID, contraceptivesDF$ID == replacementKey, i)
        
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

      # Log the IDs that hasn't change yet.
      # Up to here, the phenotype table ID column has all the IDs changed from
      # 1 to 1000ish. The only things is left unchanged are friends that has
      # a nomination, but are not in the list of IDs
      {
        validKeys = unique(phenotypeTable$ID)
        selectedFriendsKeys = unique(c(networkTable$Friend1,
                                       networkTable$Friend2,
                                       networkTable$Friend3,
                                       networkTable$Friend4,
                                       networkTable$Friend5))
        selectedFriendsKeys  = setdiff(selectedFriendsKeys, NA) # Remove the NA key if any
        missingKeys          = setdiff(selectedFriendsKeys, validKeys)
        totalMissing         = length(missingKeys)
        totalRemaining       = length(validKeys)

        totalOriginalIDs     = totalMissing + totalRemaining
        
        # Log the results in the data frame
        {
          dataCleaningLogDF$Total[1]    = totalOriginalIDs
          dataCleaningLogDF$Relative[1] = "100 %"
          dataCleaningLogDF$Total[2]    = totalMissing
          dataCleaningLogDF$Relative[2] = paste(round(totalMissing/totalOriginalIDs,4)   * 100, " %", sep="")
          dataCleaningLogDF$Total[3]    = totalRemaining
          dataCleaningLogDF$Relative[3] = paste(round(totalRemaining/totalOriginalIDs,4) * 100, " %", sep="")          
        }
      }
      
      # Keep track of all edges that are lost
      {
        # In validKeys we have the keys that remain
        # In missingKeys we have the keys that are going to be throw away
        #
        # In here we keep a list of the keys that remain, and for each, a list of friends that are lost
        # People who remain, nominate people that are lost, always.      
        # (A)
        remainingToDeletedFriendsLostDF           = data.frame(matrix(NA, nrow = totalRemaining, ncol = 3))
        colnames(remainingToDeletedFriendsLostDF) = c("Key", "Total", "List")
        remainingToDeletedFriendsLostDF$Key       = validKeys
        remainingToDeletedFriendsLostDF$Total     = 0
        for(i in 1:totalRemaining){
          remainingToDeletedFriendsLostDF$List[i] = vector(mode = "list", length = 1)
        }
        
        
        # In here we keep a list of the keys that are lost,  and for each, a list of friends that remain
        # People who are lost, are nominated by people that remain, always
        # (B)
        deletedByRemainingFriendsLostDF           = data.frame(matrix(NA, nrow = totalMissing,   ncol = 3))
        colnames(deletedByRemainingFriendsLostDF) = c("Key", "Total", "List")
        deletedByRemainingFriendsLostDF$Key       = missingKeys
        deletedByRemainingFriendsLostDF$Total     = 0
        for(i in 1:totalMissing){
          deletedByRemainingFriendsLostDF$List[i] = vector(mode = "list", length = 1)
        }
        
        
        # For each missing key, record who is nominating it and update the Totals
        print("Keeping track of lost connections, please wait...")
        for (i in 1:totalMissing) {
          
          # Find, who has nominated this person, it doesn't matter if you
          # nominate him as friend 1 or friend 5.
          {
            F1 = networkTable$Friend1 == missingKeys[i] & (!is.na(networkTable$Friend1))
            F2 = networkTable$Friend2 == missingKeys[i] & (!is.na(networkTable$Friend2))
            F3 = networkTable$Friend3 == missingKeys[i] & (!is.na(networkTable$Friend3))
            F4 = networkTable$Friend4 == missingKeys[i] & (!is.na(networkTable$Friend4))
            F5 = networkTable$Friend5 == missingKeys[i] & (!is.na(networkTable$Friend5))
            
            anyFriend = (F1 | F2 | F3 | F4 | F5)
            nominateAnUnknown = networkTable$ID[ anyFriend ] # Here you have the list of people who has nominated an this particular unknown ID
            totalNominated    = length(nominateAnUnknown)
          }
          
          # Go though that list of people that are nominating this ID that is
          # going to be deleted, and add the missing friend to their respective
          # list.
          for (j in 1:totalNominated) {
            
            nominatingKey = nominateAnUnknown[j]
            
            remainingToDeletedFriendsLostDF$List[[nominatingKey]] = c(remainingToDeletedFriendsLostDF$List[[nominatingKey]], missingKeys[i])
            
          }
          
          # Go to that particular key, and add the people that are nominating
          # this person that is going to be deleted later.
          #
          # This dataframe is not sorted by index
          # so you need to find the proper row first
          myIndex = grep(TRUE, (deletedByRemainingFriendsLostDF$Key == missingKeys[i]))
          for (j in 1:totalNominated) {
            
            nominatingKey = nominateAnUnknown[j]
            
            deletedByRemainingFriendsLostDF$List[[myIndex]] = c(deletedByRemainingFriendsLostDF$List[[myIndex]],nominatingKey)  
            
          }
          
        }
        # ---- Up to here, we have all the lists we need, just count the length
        #      and update the totals of each DF
        #
        # I can't do:
        #
        # remainingToDeletedFriendsLostDF$Total = length(remainingToDeletedFriendsLostDF$List)
        # deletedByRemainingFriendsLostDF$Total = length(deletedByRemainingFriendsLostDF$List)
        #
        # Really R, I hate you and your lack of consistency in vector operation, among other many things ¬¬
        for (i in 1:totalRemaining) remainingToDeletedFriendsLostDF$Total[i] = length(remainingToDeletedFriendsLostDF$List[[i]])
        for (i in 1:totalMissing)   deletedByRemainingFriendsLostDF$Total[i] = length(deletedByRemainingFriendsLostDF$List[[i]])
        
        # ---- Finally, update the log DF with the total lost edges
        dataCleaningLogDF$Total[4]    = sum(remainingToDeletedFriendsLostDF$Total)
        dataCleaningLogDF$Relative[4] = "100 %"
        # ---- And update the TXT log also
        {
          
          # From remaining to deleted:
          
          logLine = "People remaining that are missing nomination:"
          write( logLine , file = logTXTFileConnection, append = TRUE )
          logLine = paste("    ----    ")
          write( logLine , file   = logTXTFileConnection, append = TRUE )
          
          for (i in 1:totalRemaining){
            
            currentKey   = remainingToDeletedFriendsLostDF$Key[i]
            currentTotal = remainingToDeletedFriendsLostDF$Total[i]
            currentList  = remainingToDeletedFriendsLostDF$List[[i]]
            
            if(currentTotal > 0){
              for (j in 1:currentTotal){

                logLine = paste(currentKey, " -> ", currentList[j])
                write( logLine , file   = logTXTFileConnection, append = TRUE )
                                
              }              
            }
          }
          
          # To deleted By remaining:
          
          logLine = paste("    ----    ")
          write( logLine , file   = logTXTFileConnection, append = TRUE )          
          logLine = "People deleted that were nominated by someone remaining:"
          write( logLine , file = logTXTFileConnection, append = TRUE )
          logLine = paste("    ----    ")
          write( logLine , file   = logTXTFileConnection, append = TRUE )
          
          for (i in 1:totalMissing){
            
            currentKey   = deletedByRemainingFriendsLostDF$Key[i]
            currentTotal = deletedByRemainingFriendsLostDF$Total[i]
            currentList  = deletedByRemainingFriendsLostDF$List[[i]]
            
            if(currentTotal > 0){
              for (j in 1:currentTotal){
                
                logLine = paste(currentKey, " -> ", currentList[j])
                write( logLine , file   = logTXTFileConnection, append = TRUE )
                
              }              
            }
            
            
          }
          
        }
        
        
      }
      
      # There are many NAs everywhere for the people who have less than 5 friends. We are going
      # to set those to ID = -1, meaning nobody.
      {
        networkTable$Friend1[is.na(networkTable$Friend1)] = -1
        networkTable$Friend2[is.na(networkTable$Friend2)] = -1
        networkTable$Friend3[is.na(networkTable$Friend3)] = -1
        networkTable$Friend4[is.na(networkTable$Friend4)] = -1
        networkTable$Friend5[is.na(networkTable$Friend5)] = -1
      }

      # Finally, there might be still keys present that don't match with anyone in the ID list
      # We set a special ID = 0, which mean an anonymous person that is not in our data
      {
        networkTable$Friend1[networkTable$Friend1 > totalIDs] = 0
        networkTable$Friend2[networkTable$Friend2 > totalIDs] = 0
        networkTable$Friend3[networkTable$Friend3 > totalIDs] = 0
        networkTable$Friend4[networkTable$Friend4 > totalIDs] = 0
        networkTable$Friend5[networkTable$Friend5 > totalIDs] = 0
      }
    
    }

    # NA/Null Cleaning
    {

      # ---- Replace the NAs in the BMI column of the phenotype table
      #      Set to -1, meaning that we don't know what the BMI is
      # phenotypeTable[is.na(phenotypeTable$BMI),]$BMI = -1

      # ---- Replace the NAs in the network table for -1s , meaning that there is no friend in here
      #      Later use this for data validation in the control script
      {

        networkTable$Friend1Physical[is.na(networkTable$Friend1Physical)] = -1
        networkTable$Friend2Physical[is.na(networkTable$Friend2Physical)] = -1
        networkTable$Friend3Physical[is.na(networkTable$Friend3Physical)] = -1
        networkTable$Friend4Physical[is.na(networkTable$Friend4Physical)] = -1
        networkTable$Friend5Physical[is.na(networkTable$Friend5Physical)] = -1

        networkTable$Friend1Home[is.na(networkTable$Friend1Home)] = -1
        networkTable$Friend2Home[is.na(networkTable$Friend2Home)] = -1
        networkTable$Friend3Home[is.na(networkTable$Friend3Home)] = -1
        networkTable$Friend4Home[is.na(networkTable$Friend4Home)] = -1
        networkTable$Friend5Home[is.na(networkTable$Friend5Home)] = -1

        networkTable$Friend1School[is.na(networkTable$Friend1School)] = -1
        networkTable$Friend2School[is.na(networkTable$Friend2School)] = -1
        networkTable$Friend3School[is.na(networkTable$Friend3School)] = -1
        networkTable$Friend4School[is.na(networkTable$Friend4School)] = -1
        networkTable$Friend5School[is.na(networkTable$Friend5School)] = -1

        networkTable$Friend1Sport[is.na(networkTable$Friend1Sport)] = -1
        networkTable$Friend2Sport[is.na(networkTable$Friend2Sport)] = -1
        networkTable$Friend3Sport[is.na(networkTable$Friend3Sport)] = -1
        networkTable$Friend4Sport[is.na(networkTable$Friend4Sport)] = -1
        networkTable$Friend5Sport[is.na(networkTable$Friend5Sport)] = -1

        networkTable$Friend1Other[is.na(networkTable$Friend1Other)] = -1
        networkTable$Friend2Other[is.na(networkTable$Friend2Other)] = -1
        networkTable$Friend3Other[is.na(networkTable$Friend3Other)] = -1
        networkTable$Friend4Other[is.na(networkTable$Friend4Other)] = -1
        networkTable$Friend5Other[is.na(networkTable$Friend5Other)] = -1

      }

    }



}

# ---------- Adding new columns based on previous info
#            In here the original data is ready and clean,
#            so *now* you can add whathever you want.
{

  # -------------- For the phenotype
  {

    # -------- Whether a person is pain resistance or not
    #phenotypeTable$Pain_Resistance = c("No", "Yes")[findInterval(phenotypeTable$Pain.resistance..s., c(0,70))]
    # -------- Height derived from the BMI
    #phenotypeTable$BMI             = phenotypeTable$Weight / (phenotypeTable$Height/100)^2
    #    (BMI is here, but not height and weight. Check later for double test for BMI)

    # -- BMI value to BMI categorical
    {
      phenotypeTable$BMICategorical = "Unknown"
      phenotypeTable[(0    <   phenotypeTable$BMI) & (phenotypeTable$BMI < 18.5) & !is.na(phenotypeTable$BMI), ]$BMICategorical = "Underweight"
      phenotypeTable[(18.5 <=  phenotypeTable$BMI) & (phenotypeTable$BMI < 25)   & !is.na(phenotypeTable$BMI), ]$BMICategorical = "Healthy"
      phenotypeTable[(25   <=  phenotypeTable$BMI) & (phenotypeTable$BMI < 30)   & !is.na(phenotypeTable$BMI), ]$BMICategorical = "Overweight"
      phenotypeTable[(30   <=  phenotypeTable$BMI) & (phenotypeTable$BMI < Inf)  & !is.na(phenotypeTable$BMI), ]$BMICategorical = "Obese"
    }

    # -- General friendship statistics for each network
    {

      # (notice that we can't initialize this numbers yet but we need to create the columns)

      # -- Overall network

      # The proportion of my friends that are pain tolerance
      phenotypeTable$OverallFollowingPainAverage  = 0
      # The proportion of people who wants to be my friend that are pain tolerance
      phenotypeTable$OverallPopularityPainAverage = 0

      # Total connections
      phenotypeTable$OverallConnections = 0
      # Popularity (how many people likes you, incoming edges)
      phenotypeTable$OverallPopularity  = 0
      # Following  (how many people you like, outgoing edges)
      phenotypeTable$OverallFollowing   = 0
      # Reciprocity (how many people you like, likes you back)
      phenotypeTable$OverallReciprocity = 0

      # -- Physical

      phenotypeTable$PhysicalFollowingPainAverage  = 0
      phenotypeTable$PhysicalPopularityPainAverage = 0
      phenotypeTable$PhysicalConnections = 0
      phenotypeTable$PhysicalPopularity  = 0
      phenotypeTable$PhysicalFollowing   = 0
      phenotypeTable$PhysicalReciprocity = 0

      # -- School

      phenotypeTable$SchoolFollowingPainAverage  = 0
      phenotypeTable$SchoolPopularityPainAverage = 0
      phenotypeTable$SchoolConnections = 0
      phenotypeTable$SchoolPopularity  = 0
      phenotypeTable$SchoolFollowing   = 0
      phenotypeTable$SchoolReciprocity = 0

      # -- Sports

      phenotypeTable$SportsFollowingPainAverage  = 0
      phenotypeTable$SportsPopularityPainAverage = 0
      phenotypeTable$SportsConnections = 0
      phenotypeTable$SportsPopularity  = 0
      phenotypeTable$SportsFollowing   = 0
      phenotypeTable$SportsReciprocity = 0

      # -- Home

      phenotypeTable$HomeFollowingPainAverage  = 0
      phenotypeTable$HomePopularityPainAverage = 0
      phenotypeTable$HomeConnections = 0
      phenotypeTable$HomePopularity  = 0
      phenotypeTable$HomeFollowing   = 0
      phenotypeTable$HomeReciprocity = 0

      # -- Other

      phenotypeTable$OtherFollowingPainAverage  = 0
      phenotypeTable$OtherPopularityPainAverage = 0
      phenotypeTable$OtherConnections = 0
      phenotypeTable$OtherPopularity  = 0
      phenotypeTable$OtherFollowing   = 0
      phenotypeTable$OtherReciprocity = 0

      # -- Specials

      # ---- How many of your friends at school/physical/... have SA in the nose/throat/either
      {

        phenotypeTable$OverallFriendsWithSANasal   = 0
        phenotypeTable$OverallFriendsWithSAThroat  = 0
        
        phenotypeTable$SchoolFriendsWithSANasal    = 0  
        phenotypeTable$SchoolFriendsWithSAThroat   = 0
        
        phenotypeTable$PhysicalFriendsWithSANasal  = 0
        phenotypeTable$PhysicalFriendsWithSAThroat = 0
        
        phenotypeTable$OtherFriendsWithSANasal     = 0
        phenotypeTable$OtherFriendsWithSAThroat    = 0
        
        phenotypeTable$SportsFriendsWithSANasal    = 0
        phenotypeTable$SportsFriendsWithSAThroat   = 0
        
        phenotypeTable$HomeFriendsWithSANasal      = 0
        phenotypeTable$HomeFriendsWithSAThroat     = 0
        
        
        phenotypeTable$OverallFriendsWithSA  = 0
        phenotypeTable$SchoolFriendsWithSA   = 0
        phenotypeTable$PhysicalFriendsWithSA = 0
        phenotypeTable$OtherFriendsWithSA    = 0
        phenotypeTable$SportsFriendsWithSA   = 0
        phenotypeTable$HomeFriendsWithSA     = 0        
        
      }

    }

  }

  # -------------- For the network
  {

    # (nothing in here yet)

  }

  # -------------- For aureus
  {

    # (!) Default is "Negative" by the definition of colonization / carrier.
    #     There are a lot of Unknown anyway but those default to negative
    #     always because that's how it was design to work.
    {
      
      # -- Extra information for the AUREUS definition of CARRIER / COLONIZED / NON CARRIER
      # ---- COLONIZATION
      # ------- by the Coagulase test
      aureusTable$S1_C_NasalColonize      = "Negative"
      aureusTable$S1_C_ThroatColonize     = "Negative"
      aureusTable$S1_C_Colonize           = "Negative" # (Nasal or Throat)
      # -------  by the Enrichment test
      aureusTable$S1_E_NasalColonize      = "Negative"
      aureusTable$S1_E_ThroatColonize     = "Negative"
      aureusTable$S1_E_Colonize           = "Negative"
      # -------  by confirmed one week coagulase test
      aureusTable$S2_C_NasalColonize     = "Negative"
      aureusTable$S2_C_ThroatColonize    = "Negative"
      aureusTable$S2_C_Colonize          = "Negative"
      # -------  by confirmed one week enrichment test
      aureusTable$S2_E_NasalColonize     = "Negative"
      aureusTable$S2_E_ThroatColonize    = "Negative"
      aureusTable$S2_E_Colonize          = "Negative"
      # ---- PERSISTANT CARRIER
      # ----- by the Coagulase test
      aureusTable$C_NasalCarrier      = "Negative"
      aureusTable$C_ThroatCarrier     = "Negative"
      aureusTable$C_Carrier           = "Negative" # (Nasal or Throat)
      # ----- by the Enrichment test
      aureusTable$E_NasalCarrier      = "Negative"
      aureusTable$E_ThroatCarrier     = "Negative"
      aureusTable$E_Carrier           = "Negative"      
      # ---- PROBABILITY SUMMARY
      aureusTable$P_Nasal   = 0
      aureusTable$P_Throat  = 0
      aureusTable$P_Carrier = 0
    }
    
    # For each of the rows decide if patient is nasal/throat carrier
    for(i in 1:originalRows){

      # INDIVIDUAL VARIABLES FOR COLONIZATION
      
      # Nose variables
      {
        # Swab 1 Coagulase in the nose
        if(aureusTable$NasalGrowth[i] == "Yes" || aureusTable$NasalAureus[i] == "Yes"){
          if(medicineTable$CoagulaseNasalColonize[i] == "Positive") 
            aureusTable$S1_C_NasalColonize[i] = "Positive"        
        }
        
        # Swab 1 Enrichment in the nose
        if(aureusTable$NasalGrowth[i] == "Yes" || aureusTable$EnrichNasalAureus[i] == "Yes"){
          if(medicineTable$EnrichmentNasalColonize[i] == "Positive")
            aureusTable$S1_E_NasalColonize[i] = "Positive"
        }
        
        # Swab 2 Coagulase in the nose
        if(medicineTable$WeekNasalGrowth[i] == "Yes" || medicineTable$WeekValidNasalCoagulase[i] == "Yes"){
          if(medicineTable$WeekCoagulaseNasalColonize[i] == "Positive")
            aureusTable$S2_C_NasalColonize[i] = "Positive"
        }
        
        # Swab 2 Enrichment in the nose
        if(medicineTable$WeekNasalGrowth[i] == "Yes" || medicineTable$WeekValidNasalEnrichment[i] == "Yes"){
          if(medicineTable$WeekEnrichmentNasalColonize[i] == "Positive")
            aureusTable$S2_E_NasalColonize[i] = "Positive"
        }
      }

      # Throat variables
      {
        # Swab 1 Coagulase in the throat
        if(aureusTable$ThroatGrowth[i] == "Yes" || aureusTable$ThroatAureus[i] == "Yes"){
          if(medicineTable$CoagulaseThroatColonize[i] == "Positive")
            aureusTable$S1_C_ThroatColonize[i] = "Positive"
        }
        
        # Swab 1 Enrichment in the throat
        if(aureusTable$ThroatGrowth[i] == "Yes" || aureusTable$EnrichThroatAureus[i] == "Yes"){
          if(medicineTable$EnrichmentThroatColonize[i] == "Positive")
            aureusTable$S1_E_ThroatColonize[i] = "Positive"
        }

        # Swab 2 Coagulase in the throat
        if(medicineTable$WeekThroatGrowth[i] == "Yes" || medicineTable$WeekValidThroatCoagulase[i] == "Yes"){
          if(medicineTable$WeekCoagulaseThroatColonize[i] == "Positive")
            aureusTable$S2_C_ThroatColonize[i] = "Positive"
        }
        
        # Swab 2 Enrichment in the throat
        if(medicineTable$WeekThroatGrowth[i] == "Yes" || medicineTable$WeekValidThroatEnrichment[i] == "Yes"){
          if(medicineTable$WeekEnrichmentThroatColonize[i] == "Positive")
            aureusTable$S2_E_ThroatColonize[i] = "Positive"
        }
      }

      # Probability variables
      aureusTable$P_Nasal[i]  = sum(aureusTable$S1_C_NasalColonize[i] == "Positive",
                                    aureusTable$S1_E_NasalColonize[i] == "Positive",
                                    aureusTable$S2_C_NasalColonize[i] == "Positive",
                                    aureusTable$S2_E_NasalColonize[i] == "Positive")/4
      
      aureusTable$P_Throat[i] = sum(aureusTable$S1_C_ThroatColonize[i] == "Positive",
                                    aureusTable$S1_E_ThroatColonize[i] == "Positive",
                                    aureusTable$S2_C_ThroatColonize[i] == "Positive",
                                    aureusTable$S2_E_ThroatColonize[i] == "Positive")/4
      
      aureusTable$P_Carrier[i] = (aureusTable$P_Nasal[i] + aureusTable$P_Throat[i])/2
      
      
      # COMBINATION VARIABLES FOR COLONIZATION
      # (If you are colonize in throat or nose, you are colonized)
      # (If you are negative in both, you are not colonized)
      # (If you are negative and unknown, you are unknown)
      {
       
        # S1 Nasal + S1 Throat Coagulase (original numbers from when we decided what was positive or negative)
        if(aureusTable$S1_C_NasalColonize[i] == "Positive" || aureusTable$S1_C_ThroatColonize[i] == "Positive") aureusTable$S1_C_Colonize[i] = "Positive"
        # S1 Nasal + S1 Throat Enrichment
        if(aureusTable$S1_E_NasalColonize[i] == "Positive" || aureusTable$S1_E_ThroatColonize[i] == "Positive") aureusTable$S1_E_Colonize[i] = "Positive"
        # S2 Nasal + S2 Throat Coagulase
        if(aureusTable$S2_C_NasalColonize[i] == "Positive" || aureusTable$S2_C_ThroatColonize[i] == "Positive") aureusTable$S2_C_Colonize[i] = "Positive"
        # S2 Nasal + S2 Throat Enrichment
        if(aureusTable$S2_E_NasalColonize[i] == "Positive" || aureusTable$S2_E_ThroatColonize[i] == "Positive") aureusTable$S2_E_Colonize[i] = "Positive"

        
      }
      
      # COMBINATION VARIABLES FOR PERSISTENT CARRIER
      # (If you are colonized in both original and week, you are positive)
      # (If either of original and week is negative, you are negative)
      # (If either of nasal or throat is positive carrier, you are carrier)
      # (If you are negative carrier in both nasal and throat, you are not carrier)
      # (If you are negative carrier and unknown carrier, you are unknown )
      {

        # -- S1+S2 Nasal for Coagulase and Enrichment (the only two variables that we are using at the paper)        
        if(aureusTable$S1_C_NasalColonize[i] == "Positive" && aureusTable$S2_C_NasalColonize[i] == "Positive") aureusTable$C_NasalCarrier[i] = "Positive"
        if(aureusTable$S1_E_NasalColonize[i] == "Positive" && aureusTable$S2_E_NasalColonize[i] == "Positive") aureusTable$E_NasalCarrier[i] = "Positive"
        # -- S1+S2 Throat for Coagulase and Enrichment        
        if(aureusTable$S1_C_ThroatColonize[i] == "Positive" && aureusTable$S2_C_ThroatColonize[i] == "Positive") aureusTable$C_ThroatCarrier[i] = "Positive"
        if(aureusTable$S1_E_ThroatColonize[i] == "Positive" && aureusTable$S2_E_ThroatColonize[i] == "Positive") aureusTable$E_ThroatCarrier[i] = "Positive"
        # -- (S1+S2 Nasal AND S1+S2 Throat) Carrier status for Coagulase and Enrichment
        if(aureusTable$C_NasalCarrier[i] == "Positive" || aureusTable$C_ThroatCarrier[i] == "Positive") aureusTable$C_Carrier[i] = "Positive"
        if(aureusTable$E_NasalCarrier[i] == "Positive" || aureusTable$E_ThroatCarrier[i] == "Positive") aureusTable$E_Carrier[i] = "Positive"
        
      }
      
    }

    # Add the heatmaps that shows if there is inconsistencies with respect the
    # coagulase/enrichment test and the population grow
    
    # ---- Give a factor order so the heatmaps shows nicely
    aureusTable$NasalGrowth           = factor(aureusTable$NasalGrowth          , levels = c("Non-applicable", "Yes", "No", "Unknown"))
    aureusTable$ThroatGrowth          = factor(aureusTable$ThroatGrowth         , levels = c("Non-applicable", "Yes", "No", "Unknown"))
    aureusTable$NasalPopulation       = factor(aureusTable$NasalPopulation      , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$ThroatPopulation      = factor(aureusTable$ThroatPopulation     , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$CoagulaseNasal        = factor(aureusTable$CoagulaseNasal       , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    aureusTable$CoagulaseThroat       = factor(aureusTable$CoagulaseThroat      , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    aureusTable$CoagulaseEnrichNasal  = factor(aureusTable$CoagulaseEnrichNasal , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    aureusTable$CoagulaseEnrichThroat = factor(aureusTable$CoagulaseEnrichNasal , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    
    # 
    # ---- Do the actual heatmaps
    {
      # Coagulase
      {
        doCategoricalHeatmap(aureusTable, 7, 13, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA population with respect the coagulase test",
                             plotXLabel   = "S.A. Population grow in the nose", plotYLabel = "Coagulase test in the nose",
                             overrideCaption = "Absolute frequency of population grow vs coagulase test in the nose. This image shows
                                            that both tests are consistence with each other. We only consider positive nasal carrier
                                            those with positive coagulase test."  )
        
        doCategoricalHeatmap(aureusTable, 8, 14, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA population with respect the coagulase test",
                             plotXLabel   = "S.A. Population grow in the throat", plotYLabel = "Coagulase test in the throat",
                             overrideCaption = "Absolute frequency of population grow vs coagulase test in the throat. This image shows
                                            that both tests are consistence with each other. We only consider positive throat carrier
                                            those with positive coagulase test.")
        
        doCategoricalHeatmap(aureusTable, 3, 13, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA swab with respect the coagulase test",
                             plotXLabel   = "S.A. swab collected in the nose.", plotYLabel = "Coagulase test in the nose",
                             overrideCaption = "Absolute frequency of swabs vs coagulase test in the nose. We consider carriers those
                                            which have nasal positive coagulase and with a nasal swab collected")
        
        doCategoricalHeatmap(aureusTable, 4, 14, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA swab with respect the coagulase test",
                             plotXLabel   = "S.A. swab collected in the throat.", plotYLabel = "Coagulase test in the throat",
                             overrideCaption = "Absolute frequency of swabs vs coagulase test in the nose. We consider carriers those
                                            which have throat positive coagulase and with a throat swab collected")
        
      }
      
      # Enrichment
      {
        doCategoricalHeatmap(aureusTable, 7, 15, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA population with respect the enrichment test",
                             plotXLabel   = "S.A. Population grow in the nose", plotYLabel = "Enrichment test in the nose",
                             overrideCaption = "Absolute frequency of population grow vs enrichment test in the nose. This image shows
                                            that both tests are consistence with each other. We only consider positive nasal carrier
                                            those with positive enrichment test."  )
        
        doCategoricalHeatmap(aureusTable, 8, 16, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA population with respect the enrichment test",
                             plotXLabel   = "S.A. Population grow in the throat", plotYLabel = "Enrichment test in the throat",
                             overrideCaption = "Absolute frequency of population grow vs enrichment test in the throat. This image shows
                                            that both tests are consistence with each other. We only consider positive throat carrier
                                            those with positive enrichment test.")
        
        doCategoricalHeatmap(aureusTable, 3, 15, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA swab with respect the enrichment test",
                             plotXLabel   = "S.A. swab collected in the nose.", plotYLabel = "Enrichment test in the nose",
                             overrideCaption = "Absolute frequency of swabs vs enrichment test in the nose. We consider carriers those
                                            which have nasal positive enrichment and with a nasal swab collected")
        
        doCategoricalHeatmap(aureusTable, 4, 16, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA swab with respect the enrichment test",
                             plotXLabel   = "S.A. swab collected in the throat.", plotYLabel = "Enrichment test in the throat",
                             overrideCaption = "Absolute frequency of swabs vs enrichment test in the nose. We consider carriers those
                                            which have throat positive enrichment and with a throat swab collected")
        
      }
      
    }

  }

  # -------------- For the medicine
  {
    
    # Add how many days has pass since the last period
    # (This doesn't work since the swab date, get crazy numbers from -12 to +70)
    # medicineTable$MenstruationCurrentDay = 0
    # 
    # aureusLabDates   = as.Date(aureusTable$Date,                    format = "%m/%d/%Y")
    # medicineLastDate = as.Date(medicineTable$MenstruationLastStart, format = "%Y-%m-%d")
    # 
    # medicineTable$MenstruationCurrentDay = aureusLabDates - medicineLastDate
    
    
  }
  
}

# ---- Transform the network table into proper frienship matrix
#      There are 6 graphs, all of them directed.
#      -- Overall friendship
#      -- Physical
#      -- Home
#      -- School
#      -- Sports
#      -- Others
{

  # Create the skeleton dataframe for each network
  {
    # Check all the IDs
    networkIDs = phenotypeTable$ID
    totalIDs   = length(networkIDs)

    # Create the basic DF from where later we will create the edges data frames
    overallNetworkDF    = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
    physicalNetworkDF   = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
    homeNetworkDF       = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
    schoolNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
    sportsNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
    otherNetworkDF      = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))

    # Ensure that the rows and the columns refers to the same ID; for each of the networks we have
    # -- Overall
    overallNetworkDF$ID                     = networkIDs
    colnames(overallNetworkDF)[1:totalIDs]  = networkIDs
    # -- Physical
    physicalNetworkDF$ID                    = networkIDs
    colnames(physicalNetworkDF)[1:totalIDs] = networkIDs
    # -- Home
    homeNetworkDF$ID                        = networkIDs
    colnames(homeNetworkDF)[1:totalIDs]     = networkIDs
    # -- School
    schoolNetworkDF$ID                      = networkIDs
    colnames(schoolNetworkDF)[1:totalIDs]   = networkIDs
    # -- Sports
    sportsNetworkDF$ID                      = networkIDs
    colnames(sportsNetworkDF)[1:totalIDs]   = networkIDs
    # -- Others
    otherNetworkDF$ID                       = networkIDs
    colnames(otherNetworkDF)[1:totalIDs]    = networkIDs

  }

  # For each of the people in the network
  # Fill all their edges information
  for(i in 1:totalIDs){

    # Identify each of your friends
    FriendA = networkTable$Friend1[i]
    FriendB = networkTable$Friend2[i]
    FriendC = networkTable$Friend3[i]
    FriendD = networkTable$Friend4[i]
    FriendE = networkTable$Friend5[i]

    # For each of the network, add that frienship link if it exist
    # -- The overall network doesn't care about anything, everyone comes here
    if(FriendA > 0) overallNetworkDF[i, FriendA] = 1
    if(FriendB > 0) overallNetworkDF[i, FriendB] = 1
    if(FriendC > 0) overallNetworkDF[i, FriendC] = 1
    if(FriendD > 0) overallNetworkDF[i, FriendD] = 1
    if(FriendE > 0) overallNetworkDF[i, FriendE] = 1
    # -- Physical
    if(FriendA > 0 && networkTable$Friend1Physical[i] == 1) physicalNetworkDF[i, FriendA] = 1
    if(FriendB > 0 && networkTable$Friend2Physical[i] == 1) physicalNetworkDF[i, FriendB] = 1
    if(FriendC > 0 && networkTable$Friend3Physical[i] == 1) physicalNetworkDF[i, FriendC] = 1
    if(FriendD > 0 && networkTable$Friend4Physical[i] == 1) physicalNetworkDF[i, FriendD] = 1
    if(FriendE > 0 && networkTable$Friend5Physical[i] == 1) physicalNetworkDF[i, FriendE] = 1
    # -- Home
    if(FriendA > 0 && networkTable$Friend1Home[i] == 1) homeNetworkDF[i, FriendA] = 1
    if(FriendB > 0 && networkTable$Friend2Home[i] == 1) homeNetworkDF[i, FriendB] = 1
    if(FriendC > 0 && networkTable$Friend3Home[i] == 1) homeNetworkDF[i, FriendC] = 1
    if(FriendD > 0 && networkTable$Friend4Home[i] == 1) homeNetworkDF[i, FriendD] = 1
    if(FriendE > 0 && networkTable$Friend5Home[i] == 1) homeNetworkDF[i, FriendE] = 1
    # -- School
    if(FriendA > 0 && networkTable$Friend1School[i] == 1) schoolNetworkDF[i, FriendA] = 1
    if(FriendB > 0 && networkTable$Friend2School[i] == 1) schoolNetworkDF[i, FriendB] = 1
    if(FriendC > 0 && networkTable$Friend3School[i] == 1) schoolNetworkDF[i, FriendC] = 1
    if(FriendD > 0 && networkTable$Friend4School[i] == 1) schoolNetworkDF[i, FriendD] = 1
    if(FriendE > 0 && networkTable$Friend5School[i] == 1) schoolNetworkDF[i, FriendE] = 1
    # -- Sports
    if(FriendA > 0 && networkTable$Friend1Sport[i] == 1) sportsNetworkDF[i, FriendA] = 1
    if(FriendB > 0 && networkTable$Friend2Sport[i] == 1) sportsNetworkDF[i, FriendB] = 1
    if(FriendC > 0 && networkTable$Friend3Sport[i] == 1) sportsNetworkDF[i, FriendC] = 1
    if(FriendD > 0 && networkTable$Friend4Sport[i] == 1) sportsNetworkDF[i, FriendD] = 1
    if(FriendE > 0 && networkTable$Friend5Sport[i] == 1) sportsNetworkDF[i, FriendE] = 1
    # -- Others
    if(FriendA > 0 && networkTable$Friend1Other[i] == 1) otherNetworkDF[i, FriendA] = 1
    if(FriendB > 0 && networkTable$Friend2Other[i] == 1) otherNetworkDF[i, FriendB] = 1
    if(FriendC > 0 && networkTable$Friend3Other[i] == 1) otherNetworkDF[i, FriendC] = 1
    if(FriendD > 0 && networkTable$Friend4Other[i] == 1) otherNetworkDF[i, FriendD] = 1
    if(FriendE > 0 && networkTable$Friend5Other[i] == 1) otherNetworkDF[i, FriendE] = 1

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
      popularThisIDsOverall                = overallNetworkDF[ (overallNetworkDF[,i] == 1), ]$ID
      phenotypeTable$OverallPopularity[i]  = length(popularThisIDsOverall)

      # Get the list of everyone I like
      followingThisIDsOverall              = overallNetworkDF[overallNetworkDF[i,1:totalIDs] == 1,]$ID
      phenotypeTable$OverallFollowing [i]  = length(followingThisIDsOverall)

      # Check the union of both sets for all relationship
      allRelationshipsOverall              = union( popularThisIDsOverall, followingThisIDsOverall )
      totalAllRelationshipsOverall         = length(allRelationshipsOverall)
      phenotypeTable$OverallConnections[i] = totalAllRelationshipsOverall

      # Check the intersection of both sets for reciprocity
      realciprocallRelationshipsOverall    = intersect( popularThisIDsOverall, followingThisIDsOverall )
      phenotypeTable$OverallReciprocity[i] = length(realciprocallRelationshipsOverall)

    }

    # -- Physical
    {

      popularThisIDs                        = physicalNetworkDF[    (physicalNetworkDF[,i] == 1), ]$ID
      phenotypeTable$PhysicalPopularity[i]  = length(popularThisIDs)
      followingThisIDs                      = physicalNetworkDF[physicalNetworkDF[i,1:totalIDs] == 1,]$ID
      phenotypeTable$PhysicalFollowing [i]  = length(followingThisIDs)
      allRelationshipsPhysical              = union( popularThisIDs, followingThisIDs )
      totalAllRelationshipsPhysical         = length(allRelationshipsPhysical)
      phenotypeTable$PhysicalConnections[i] = totalAllRelationshipsPhysical
      realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
      phenotypeTable$PhysicalReciprocity[i] = length(realciprocallRelationships)

    }

    # -- Home
    {

      popularThisIDs                        = homeNetworkDF[    (homeNetworkDF[,i] == 1), ]$ID
      phenotypeTable$HomePopularity[i]      = length(popularThisIDs)
      followingThisIDs                      = homeNetworkDF[homeNetworkDF[i,1:totalIDs] == 1,]$ID
      phenotypeTable$HomeFollowing [i]      = length(followingThisIDs)
      allRelationshipsHome                  = union( popularThisIDs, followingThisIDs )
      totalAllRelationshipsHome             = length(allRelationshipsHome)
      phenotypeTable$HomeConnections[i]     = totalAllRelationshipsHome
      realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
      phenotypeTable$HomeReciprocity[i]     = length(realciprocallRelationships)

    }

    # -- School
    {

      popularThisIDs                        = schoolNetworkDF[  (schoolNetworkDF[,i] == 1), ]$ID
      phenotypeTable$SchoolPopularity[i]    = length(popularThisIDs)
      followingThisIDs                      = schoolNetworkDF[schoolNetworkDF[i,1:totalIDs] == 1,]$ID
      phenotypeTable$SchoolFollowing [i]    = length(followingThisIDs)
      allRelationshipsSchool                = union( popularThisIDs, followingThisIDs )
      totalAllRelationshipsSchool           = length(allRelationshipsSchool)
      phenotypeTable$SchoolConnections[i]   = totalAllRelationshipsSchool
      realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
      phenotypeTable$SchoolReciprocity[i]   = length(realciprocallRelationships)

    }

    # -- Sport
    {

      popularThisIDs                        = sportsNetworkDF[    (sportsNetworkDF[,i] == 1), ]$ID
      phenotypeTable$SportsPopularity[i]    = length(popularThisIDs)
      followingThisIDs                      = sportsNetworkDF[sportsNetworkDF[i,1:totalIDs] == 1,]$ID
      phenotypeTable$SportsFollowing [i]    = length(followingThisIDs)
      allRelationshipsSports                = union( popularThisIDs, followingThisIDs )
      totalAllRelationshipsSports           = length(allRelationshipsSports)
      phenotypeTable$SportsConnections[i]   = totalAllRelationshipsSports
      realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
      phenotypeTable$SportsReciprocity[i]   = length(realciprocallRelationships)

    }

    # -- Others
    {

      popularThisIDs                        = otherNetworkDF[    (otherNetworkDF[,i] == 1), ]$ID
      phenotypeTable$OtherPopularity[i]     = length(popularThisIDs)
      followingThisIDs                      = otherNetworkDF[otherNetworkDF[i,1:totalIDs] == 1,]$ID
      phenotypeTable$OtherFollowing [i]     = length(followingThisIDs)
      allRelationshipsOthers                = union( popularThisIDs, followingThisIDs )
      totalAllRelationshipsOthers           = length(allRelationshipsOthers)
      phenotypeTable$OtherConnections[i]    = totalAllRelationshipsOthers
      realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
      phenotypeTable$OtherReciprocity[i]    = length(realciprocallRelationships)

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
            SAStatusNasal  = aureusTable$E_NasalCarrier[myFriendID]
            SAStatusThroat = aureusTable$E_ThroatCarrier[myFriendID]
            SAStatus       = aureusTable$E_Carrier[myFriendID]

            # Add to the amount of infected
            if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
            if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
            if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}

          }

          totalSANasal  = totalSANasal  / totalAllRelationshipsOverall
          totalSAThroat = totalSAThroat / totalAllRelationshipsOverall
          totalSA       = totalSA       / totalAllRelationshipsOverall

        }

        phenotypeTable$OverallFriendsWithSANasal[i]  = totalSANasal
        phenotypeTable$OverallFriendsWithSAThroat[i] = totalSAThroat
        phenotypeTable$OverallFriendsWithSA[i]       = totalSA
        
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
            SAStatusNasal  = aureusTable$E_NasalCarrier[myFriendID]
            SAStatusThroat = aureusTable$E_ThroatCarrier[myFriendID]
            SAStatus       = aureusTable$E_Carrier[myFriendID]
            
            # Add to the amount of infected
            if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
            if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
            if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
            
          }
          
          totalSANasal  = totalSANasal  / totalAllRelationshipsPhysical
          totalSAThroat = totalSAThroat / totalAllRelationshipsPhysical
          totalSA       = totalSA       / totalAllRelationshipsPhysical
          
        }
        
        phenotypeTable$PhysicalFriendsWithSANasal[i]  = totalSANasal
        phenotypeTable$PhysicalFriendsWithSAThroat[i] = totalSAThroat
        phenotypeTable$PhysicalFriendsWithSA[i]       = totalSA
        
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
            SAStatusNasal  = aureusTable$E_NasalCarrier[myFriendID]
            SAStatusThroat = aureusTable$E_ThroatCarrier[myFriendID]
            SAStatus       = aureusTable$E_Carrier[myFriendID]
            
            # Add to the amount of infected
            if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
            if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
            if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
            
          }
          
          totalSANasal  = totalSANasal  / totalAllRelationshipsHome
          totalSAThroat = totalSAThroat / totalAllRelationshipsHome
          totalSA       = totalSA       / totalAllRelationshipsHome
          
        }
        
        phenotypeTable$HomeFriendsWithSANasal[i]  = totalSANasal
        phenotypeTable$HomeFriendsWithSAThroat[i] = totalSAThroat
        phenotypeTable$HomeFriendsWithSA[i]       = totalSA
        
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
            SAStatusNasal  = aureusTable$E_NasalCarrier[myFriendID]
            SAStatusThroat = aureusTable$E_ThroatCarrier[myFriendID]
            SAStatus       = aureusTable$E_Carrier[myFriendID]
            
            # Add to the amount of infected
            if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
            if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
            if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
            
          }
          
          totalSANasal  = totalSANasal  / totalAllRelationshipsSchool
          totalSAThroat = totalSAThroat / totalAllRelationshipsSchool
          totalSA       = totalSA       / totalAllRelationshipsSchool
          
        }
        
        phenotypeTable$SchoolFriendsWithSANasal[i]  = totalSANasal
        phenotypeTable$SchoolFriendsWithSAThroat[i] = totalSAThroat
        phenotypeTable$SchoolFriendsWithSA[i]       = totalSA
        
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
            SAStatusNasal  = aureusTable$E_NasalCarrier[myFriendID]
            SAStatusThroat = aureusTable$E_ThroatCarrier[myFriendID]
            SAStatus       = aureusTable$E_Carrier[myFriendID]
            
            # Add to the amount of infected
            if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
            if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
            if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
            
          }
          
          totalSANasal  = totalSANasal  / totalAllRelationshipsSports
          totalSAThroat = totalSAThroat / totalAllRelationshipsSports
          totalSA       = totalSA       / totalAllRelationshipsSports
          
        }
        
        phenotypeTable$SportsFriendsWithSANasal[i]  = totalSANasal
        phenotypeTable$SportsFriendsWithSAThroat[i] = totalSAThroat
        phenotypeTable$SportsFriendsWithSA[i]       = totalSA
        
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
            SAStatusNasal  = aureusTable$E_NasalCarrier[myFriendID]
            SAStatusThroat = aureusTable$E_ThroatCarrier[myFriendID]
            SAStatus       = aureusTable$E_Carrier[myFriendID]
            
            # Add to the amount of infected
            if(SAStatusNasal  == "Positive"){ totalSANasal  = totalSANasal  + 1}
            if(SAStatusThroat == "Positive"){ totalSAThroat = totalSAThroat + 1}
            if(SAStatus       == "Positive"){ totalSA       = totalSA       + 1}
            
          }
          
          totalSANasal  = totalSANasal  / totalAllRelationshipsOthers
          totalSAThroat = totalSAThroat / totalAllRelationshipsOthers
          totalSA       = totalSA       / totalAllRelationshipsOthers
          
        }
        
        phenotypeTable$OthersFriendsWithSANasal[i]  = totalSANasal
        phenotypeTable$OthersFriendsWithSAThroat[i] = totalSAThroat
        phenotypeTable$OthersFriendsWithSA[i]       = totalSA
        
      }

    }

  }

}

}

# Join tables with proper info together
# -----------------------------------------------------------------------------
completeTable          = phenotypeTable %>% left_join(aureusTable ,   by="ID")
completeTable          = completeTable  %>% left_join(medicineTable , by="ID")

completeTable$Overview = networkTable$Overwiew  # 0 to 10, how good this network describes your life



# Now the update is finish. Write the CSVs into disk so we don't have to do this
# over and over again everytime we run the scripts
{

  # The phenotype table
  # The network   table
  # The aureus    table
  
  # The complete  table
  
  write.csv2(phenotypeTable, file = READY_DATA_PHENOTYPES_FILEPATH, row.names = FALSE)
  write.csv2(networkTable,   file = READY_DATA_NETWORK_FILEPATH,    row.names = FALSE)
  write.csv2(aureusTable,    file = READY_DATA_AUREUS_FILEPATH,     row.names = FALSE)
  write.csv2(medicineTable,  file = READY_DATA_MEDICINE_FILEPATH,   row.names = FALSE)
  
  
  write.csv2(completeTable,  file = READY_DATA_COMPLETE_FILEPATH,   row.names = FALSE)
  
  # The disease, drug and contraceptives tables
  write.csv2(drugUseDF,        file = READY_DATA_DRUG_FILEPATH,           row.names = FALSE)
  write.csv2(contraceptivesDF, file = READY_DATA_CONTRACEPTIVES_FILEPATH, row.names = FALSE)
  write.csv2(diseasesDF,       file = READY_DATA_DISEASES_FILEPATH,       row.names = FALSE)
  
  
  # The overall  friendship matrix
  # The physical friendship matrix
  # The home     friendship matrix
  # The school   friendship matrix
  # The sport    friendship matrix
  # The other    friendship matrix

  write.csv2(overallNetworkDF,  file = READY_DATA_OVERALL_NETWORK_FILEPATH   , row.names = FALSE)
  write.csv2(physicalNetworkDF, file = READY_DATA_PHYSICAL_NETWORK_FILEPATH  , row.names = FALSE)
  write.csv2(homeNetworkDF,     file = READY_DATA_HOME_NETWORK_FILEPATH      , row.names = FALSE)
  write.csv2(schoolNetworkDF,   file = READY_DATA_SCHOOL_NETWORK_FILEPATH    , row.names = FALSE)
  write.csv2(sportsNetworkDF,   file = READY_DATA_SPORTS_NETWORK_FILEPATH    , row.names = FALSE)
  write.csv2(otherNetworkDF,    file = READY_DATA_OTHERS_NETWORK_FILEPATH    , row.names = FALSE)

}

# Close the TXT connections
close(logTXTFileConnection)

# Write the Latex tables with respect the data that we missed
#writeGenericTableLATEX(logDF, LOGS_FOLDER, tableCaption = "Summary of lost connections.")

writeTableLATEX(dataCleaningLogDF, LOGS_FOLDER, 
                tableCaption      = "Summary of lost connections at data cleaning.")


# Plot the boxplots for missing data
myYMaximum = max( max(remainingToDeletedFriendsLostDF$Total) , max(deletedByRemainingFriendsLostDF$Total) )

doBoxPlot(remainingToDeletedFriendsLostDF, 2, LOGS_FOLDER, 
          colorsVector = COLOR_RED_MED,
          plotTitle    = "Nominations lost; from people remaining, to dissapeared people.",
          plotSubtitle = "",
          plotXLabel   = "Missing Friends", plotYLabel = "Total",
          plotTheme    = "simple",
          ymin = 0, ymax = myYMaximum,
          overrideCaption = "Boxplot for number of missing edges from existing keys to deleted keys.")

doBoxPlot(deletedByRemainingFriendsLostDF, 2, LOGS_FOLDER, 
          colorsVector = COLOR_BLUE_MED,
          plotTitle    = "Popularity of people that have dissapear.",
          plotSubtitle = "",
          plotXLabel   = "Missing Friends", plotYLabel = "Total",
          plotTheme    = "simple",
          ymin = 0, ymax = myYMaximum,
          overrideCaption = "Boxplot for number of missing edges by existing keys to deleted keys.")


# Update the latex folder
source("latex.R", encoding="utf-8")


print("Data cleaning completed!")


