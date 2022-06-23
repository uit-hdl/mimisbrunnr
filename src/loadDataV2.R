# -----------------------------------------------------------------------------
#
# This script is for loading the data only
#
# This include unformated text and converted into Date object, creating the
# proper graph object, creating the factors in the dataframes, and in general
# everything that take clean data and need to be put into R memory.
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
library(dplyr)
library(lubridate)
library(stringr)

library(ggraph)
library(igraph)


# I have no idea why, but R, among all the stupid things that it does,
# now has decide after an update that it will set the working directory to
# whatever it wants after running a library() call. So instead of doing this
# automatically like has always work, I net to setwd() manually with an
# absolute path and I hate it
#setwd("~/Desktop/Amalgamlab/mimisbrunnr/src")

# load our proper libraries
source("constants.R", encoding="utf-8")

#setwd("~/Desktop/Amalgamlab/mimisbrunnr/src")
# MAIN DIRECTORY THNGY
source("./lib/analysis.R",  encoding="utf-8")



# Read the data into DFs
# -----------------------------------------------------------------------------
{
  
    # Individual tables and complete table
    basicTable           = read.csv2(FILTER_DATA_BASIC_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    antropometricTable   = read.csv2(FILTER_DATA_ANTROPOMETRIC_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    menstruationTable    = read.csv2(FILTER_DATA_MENSTRUATION_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    aureusTable          = read.csv2(FILTER_DATA_AUREUS_FILEPATH,          fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    highSchoolTable      = read.csv2(FILTER_DATA_HIGHSCHOOL_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    bloodTable           = read.csv2(FILTER_DATA_BLOOD_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    questionariesTable   = read.csv2(FILTER_DATA_QUESTIONARIES_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sociologyTable       = read.csv2(FILTER_DATA_SOCIOLOGY_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    frienshipTable       = read.csv2(FILTER_DATA_FRIENDSHIP_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    pubertyMenTable      = read.csv2(FILTER_DATA_PUBERTYMEN_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    pubertyWomenTable    = read.csv2(FILTER_DATA_PUBERTYWOMEN_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    drugsTable           = read.csv2(FILTER_DATA_DRUGS_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsTable          = read.csv2(FILTER_DATA_SPORTS_FILEPATH,          fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    hygieneTable         = read.csv2(FILTER_DATA_HYGIENE_FILEPATH,         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    hospitalizationTable = read.csv2(FILTER_DATA_HOSPITALIZATION_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    biomarkersTable      = read.csv2(FILTER_DATA_BIOMARKERS_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    dietTable            = read.csv2(FILTER_DATA_DIET_FILEPATH,            fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sleepTable           = read.csv2(FILTER_DATA_SLEEP_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    # DBs tables  
    diseasesDBDF         = read.csv2(FILTER_DATA_DISEASES_FILEPATH,        fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    contraceptivesDBDF   = read.csv2(FILTER_DATA_CONTRACEPTIVES_FILEPATH,  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    medicinesDBDF        = read.csv2(FILTER_DATA_MEDICINES_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    # Networks
    overallNetworkDF  = read.csv2(FILTER_DATA_OVERALL_NETWORK_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    physicalNetworkDF = read.csv2(FILTER_DATA_PHYSICAL_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    homeNetworkDF     = read.csv2(FILTER_DATA_HOME_NETWORK_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    schoolNetworkDF   = read.csv2(FILTER_DATA_SCHOOL_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsNetworkDF   = read.csv2(FILTER_DATA_SPORTS_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    otherNetworkDF    = read.csv2(FILTER_DATA_OTHERS_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
    # Metadata information
    {
        # Biomarkers    
        biomarkersMetadataDF           = read.csv2(BIOMARKERS_METATABLE_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        colnames(biomarkersMetadataDF) = c("Acronym", "Protein", "UniProt", "LOD_Batch_20160383", "LOD_Batch_20160977", "Uniprt_Web", "Wiki_Web") # Adjust names because of weird spaces in trailing strings
        
        # Blood
        bloodMetadataDF                = read.csv2(BLOOD_METATABLE_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        colnames(bloodMetadataDF)      = c("Original", "Description", "Short", "Unit", "MenLower", "MenUpper", "WomenLower", "WomenUpper") # Adjust names because of weird spaces in trailing strings
        bloodMetadataDF$MenLower       = as.numeric(bloodMetadataDF$MenLower)
        bloodMetadataDF$MenUpper       = as.numeric(bloodMetadataDF$MenUpper)
        bloodMetadataDF$WomenLower     = as.numeric(bloodMetadataDF$WomenLower)
        bloodMetadataDF$WomenUpper     = as.numeric(bloodMetadataDF$WomenUpper)
    }

    
    

    
    # How many people we have
    totalOriginalRows = nrow(basicTable)
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
    
    # Why in hell does SPSS has the start of Gregorian Calendar in 1582 as
    # default date for POSIX origin??? There might be a minor error coercion in
    # dates due the fact that we don't know which timezone, or time origin,
    # shall we take as a reference.
    originSPSSDate = as.Date("1582-10-14" , format = "%Y-%m-%d") 
    
    # Basic Table
    {
    
        basicTable$AttendanceDateFF1  = as_datetime(basicTable$AttendanceDateFF1,  origin = originSPSSDate)
        basicTable$AttendanceDateFF12 = as_datetime(basicTable$AttendanceDateFF12, origin = originSPSSDate)
        basicTable$MedicationDateFF1  = as_datetime(basicTable$MedicationDateFF1,  origin = originSPSSDate)
               
    }
    
    # Menstruation Table
    {
    
        menstruationTable$MenstruationDate = as.Date(menstruationTable$MenstruationDate, format = "%Y-%m-%d")

    }
    
    # Aureus table
    {
    
        aureusTable$S1_AttendanceDate    = as_datetime(aureusTable$S1_AttendanceDate,    origin = originSPSSDate)
        aureusTable$S2_AttendanceDate    = as_datetime(aureusTable$S2_AttendanceDate,    origin = originSPSSDate)
        aureusTable$S1_CultureDate       = as_datetime(aureusTable$S1_CultureDate,       origin = originSPSSDate)
        #aureusTable$S2_CultureDate       = as_datetime(aureusTable$S2_CultureDate,       origin = originSPSSDate) We don't have this data, all values are NA
        aureusTable$S1_Nasal_FreezeDate  = as_datetime(aureusTable$S1_Nasal_FreezeDate,  origin = originSPSSDate)
        aureusTable$S1_Throat_FreezeDate = as_datetime(aureusTable$S1_Throat_FreezeDate, origin = originSPSSDate)
           
    }
    
    # Friendship Table
    {
        
        # In this case, the date was saved as a string (US format! ¬¬) rather than POSIX
        frienshipTable$Created = as.Date(frienshipTable$Created, format = "%m/%d/%Y")
            
    }
    
    # Blood table
    {
        bloodTable$BloodAnalysisDate    = as_datetime(bloodTable$BloodAnalysisDate,    origin = originSPSSDate)
        bloodTable$PlasmaAnalysisDate   = as_datetime(bloodTable$PlasmaAnalysisDate,   origin = originSPSSDate)
    }

    
}

# Add the manual order for the categorical values, so it coincides with the color palette
{
    
    # Yes No questions always default to order "No", "Yes", "Unknown".
    # In general, everything negative to the left, everything positive in the
    # middle, everything unknown to the right.
    
    # Basic Table
    {
      
        basicTable$Sex           = factor(basicTable$Sex,           levels = c("Man",      "Woman"                                                       ))  
        basicTable$GeneralHealth = factor(basicTable$GeneralHealth, levels = c("Very bad", "Bad",  "Neither good nor bad", "Good", "Excellent", "Unknown"))  

    }
    
    # Antropometry
    {
    
        antropometricTable$BMICategorical = factor(antropometricTable$BMICategorical , levels = c("Underweight", "Healthy",   "Overweight", "Obese", "Unknown"))    
        
    }
    
    # Menstruation
    {
        
        menstruationTable$MenstruationStart   = factor(menstruationTable$MenstruationStart,   levels = c( "No", "Yes", "Unknown"))
        menstruationTable$MenstruationRegular = factor(menstruationTable$MenstruationRegular, levels = c( "Irregular", "Usually regular", "Always regular", "Unknown"))
    }
    
    # Aureus Table
    {
        # R is horrible,  since we dont' have objects (proper objects and classes!)
        # we can't do self.table(BacterialGrow, levels = "..."), which makes things
        # much more easy and we don't need to add the same column redundancy in the
        # same line. This causes A LOT of bugs and is very frustrating.
        
        # The experiment grew something in the agar plate
        aureusTable$S1_BacterialNasalGrowth  = factor(aureusTable$S1_BacterialNasalGrowth,   levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_BacterialThroatGrowth = factor(aureusTable$S1_BacterialThroatGrowth,  levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_BacterialNasalGrowth  = factor(aureusTable$S2_BacterialNasalGrowth,   levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_BacterialThroatGrowth = factor(aureusTable$S2_BacterialThroatGrowth,  levels = c( "No", "Non-applicable", "Yes", "Unknown"))

        # The experiment grew SA in the agar plate (Direct Culture)
        aureusTable$S1_SA_Direct_NasalGrowth        = factor(aureusTable$S1_SA_Direct_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Direct_ThroatGrowth       = factor(aureusTable$S1_SA_Direct_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Direct_NasalPopulation    = factor(aureusTable$S1_SA_Direct_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S1_SA_Direct_ThroatPopulation   = factor(aureusTable$S1_SA_Direct_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Direct_NasalGrowth        = factor(aureusTable$S2_SA_Direct_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Direct_ThroatGrowth       = factor(aureusTable$S2_SA_Direct_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Direct_NasalPopulation    = factor(aureusTable$S2_SA_Direct_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Direct_ThroatPopulation   = factor(aureusTable$S2_SA_Direct_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))

        # The experiment grew SA, and we try to make it grow even more (Enrichment Broth)
        aureusTable$S1_SA_Enrich_NasalGrowth        = factor(aureusTable$S1_SA_Enrich_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Enrich_ThroatGrowth       = factor(aureusTable$S1_SA_Enrich_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Enrich_NasalPopulation    = factor(aureusTable$S1_SA_Enrich_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S1_SA_Enrich_ThroatPopulation   = factor(aureusTable$S1_SA_Enrich_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Enrich_NasalGrowth        = factor(aureusTable$S2_SA_Enrich_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Enrich_ThroatGrowth       = factor(aureusTable$S2_SA_Enrich_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Enrich_NasalPopulation    = factor(aureusTable$S2_SA_Enrich_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Enrich_ThroatPopulation   = factor(aureusTable$S2_SA_Enrich_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))

        # Coagulase test to check for the presence of SA (positive) or S.Epidermitis or S.Saprophyticus.
        aureusTable$S1_Direct_CoagulaseNasal        = factor(aureusTable$S1_Direct_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S1_Direct_CoagulaseThroat       = factor(aureusTable$S1_Direct_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S1_Enrich_CoagulaseNasal        = factor(aureusTable$S1_Enrich_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S1_Enrich_CoagulaseThroat       = factor(aureusTable$S1_Enrich_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Direct_CoagulaseNasal        = factor(aureusTable$S2_Direct_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Direct_CoagulaseThroat       = factor(aureusTable$S2_Direct_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Enrich_CoagulaseNasal        = factor(aureusTable$S2_Enrich_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Enrich_CoagulaseThroat       = factor(aureusTable$S2_Enrich_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))

        # The final variables for positives and negatives      
        # Levels are not initialize to Unknown because the default is negative for everyone
        aureusTable$S1_D_NasalColonize              = factor(aureusTable$S1_D_NasalColonize,            levels = c("Positive", "Negative"))
        aureusTable$S1_D_ThroatColonize             = factor(aureusTable$S1_D_ThroatColonize,           levels = c("Positive", "Negative"))
        aureusTable$S1_D_Colonize                   = factor(aureusTable$S1_D_Colonize,                 levels = c("Positive", "Negative"))
        aureusTable$S1_E_NasalColonize              = factor(aureusTable$S1_E_NasalColonize,            levels = c("Positive", "Negative"))
        aureusTable$S1_E_ThroatColonize             = factor(aureusTable$S1_E_ThroatColonize,           levels = c("Positive", "Negative"))
        aureusTable$S1_E_Colonize                   = factor(aureusTable$S1_E_Colonize,                 levels = c("Positive", "Negative"))
            
        aureusTable$S2_D_NasalColonize              = factor(aureusTable$S2_D_NasalColonize,            levels = c("Positive", "Negative"))
        aureusTable$S2_D_ThroatColonize             = factor(aureusTable$S2_D_ThroatColonize,           levels = c("Positive", "Negative"))
        aureusTable$S2_D_Colonize                   = factor(aureusTable$S2_D_Colonize,                 levels = c("Positive", "Negative"))
        aureusTable$S2_E_NasalColonize              = factor(aureusTable$S2_E_NasalColonize,            levels = c("Positive", "Negative"))
        aureusTable$S2_E_ThroatColonize             = factor(aureusTable$S2_E_ThroatColonize,           levels = c("Positive", "Negative"))
        aureusTable$S2_E_Colonize                   = factor(aureusTable$S2_E_Colonize,                 levels = c("Positive", "Negative"))

        aureusTable$D_NasalCarrier                  = factor(aureusTable$D_NasalCarrier,                levels = c("Positive", "Negative"))
        aureusTable$D_ThroatCarrier                 = factor(aureusTable$D_ThroatCarrier,               levels = c("Positive", "Negative"))
        aureusTable$D_Carrier                       = factor(aureusTable$D_Carrier,                     levels = c("Positive", "Negative"))
            
        aureusTable$E_NasalCarrier                  = factor(aureusTable$E_NasalCarrier,                levels = c("Positive", "Negative"))
        aureusTable$E_ThroatCarrier                 = factor(aureusTable$E_ThroatCarrier,               levels = c("Positive", "Negative"))
        aureusTable$E_Carrier                       = factor(aureusTable$E_Carrier,                     levels = c("Positive", "Negative"))
        
    } 
      
    # Highschool
    {
    
        # Add the H to the HighSchool ID, it just looks better
        highSchoolTable$HighSchool = paste0("H",highSchoolTable$HighSchool)
        highSchoolTable$HighSchool = as.factor(highSchoolTable$HighSchool)    
        
        # Add the C to the Class ID, it just looks better
        highSchoolTable$Class = paste0("C",highSchoolTable$Class)
        highSchoolTable$Class = as.factor(highSchoolTable$Class)    
        
        # Add the P to the Program ID, it just looks better
        # (There is probably a better description for this, but we don't have it)
        highSchoolTable$Programme = paste0("P",highSchoolTable$Programme)
        highSchoolTable$Programme = as.factor(highSchoolTable$Programme)            
        
        # Add the order to the main program. In this case the order doesn't
        # make sense, but we do it anyway to keep consistency in between plots
        # and subtables. (In alphabetic order)
        highSchoolTable$MainPrograme = factor(highSchoolTable$MainPrograme , levels = c("General", "Sports", "Vocational"))    
        
        
                
    }
    
    # Friendship
    {
    
        frienshipTable$YesterdaySMS = factor(frienshipTable$YesterdaySMS , levels = c( "None", "1 to 5", "6 to 10", "11 to 20", "21 to 50", ">50", "Unknown"))

    }
    
    # Blood
    # (There is nothing here)
    
    # Drugs
    {

        drugsTable$Smoke         = factor(drugsTable$Smoke        , levels = c("Never", "Sometimes", "Daily", "Unknown"))
        drugsTable$SmokePerWeek  = factor(drugsTable$SmokePerWeek , levels = c("0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        drugsTable$SmokePerDay   = factor(drugsTable$SmokePerDay  , levels = c("0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        drugsTable$Snuff         = factor(drugsTable$Snuff        , levels = c("Never", "Sometimes", "Daily", "Unknown"))
        drugsTable$SnuffPerWeek  = factor(drugsTable$SnuffPerWeek , levels = c("0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        drugsTable$SnuffPerDay   = factor(drugsTable$SnuffPerDay  , levels = c("0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        
        # The alcohol variable has an special requirement.
        # There are too few people that drink too much (which is good)
        # So we are going to add all of them to the previous group
        # Otherwise we end up with analysis of data with a categorical that has
        # only 1 sample size and things like that.
        #
        # (This is the original data)
        # Old factoring with the original data
        # medicineTable$Alcohol = factor(medicineTable$Alcohol , levels = c("Never", "Once per month or less", "2-4 times per month", "2-3 times per week", "4 or more times per week", "Unknown"))
        newGroup = "Twice of more per month"
        for (i in 1:nrow(drugsTable)) {
    
            if(drugsTable$Alcohol[i] == "2-4 times per month")      drugsTable$Alcohol[i] = newGroup
            if(drugsTable$Alcohol[i] == "2-3 times per week")       drugsTable$Alcohol[i] = newGroup
            if(drugsTable$Alcohol[i] == "4 or more times per week") drugsTable$Alcohol[i] = newGroup
      
        }
        
        drugsTable$Alcohol = factor(drugsTable$Alcohol , levels = c("Never", "Once per month or less", "Twice of more per month", "Unknown"))
        
        # The following questions are only for people who drink, however that leave
        # empty many data cells that can have useful information.
        #
        # Alcohol units doesn't contemplate the number 0 in the original dataset.
        # So we are going to correct for that too with the help of the previous variable
        drugsTable$AlcoholUnits  = factor(drugsTable$AlcoholUnits  , levels = c( "0",  "1 to 2",  "3 to 4", "5 to 6", "7 to 9", "10 or More", "Unknown"))
        drugsTable$Alcohol6Units = factor(drugsTable$Alcohol6Units , levels = c( "Never",  "Less than monthly", "Monthly", "Weekly", "Daily", "Unknown"))       

        for (i in 1:nrow(drugsTable)) {
        
            if(drugsTable$Alcohol[i] == "Never"){
        
                drugsTable$AlcoholUnits[i]  = "0"        
                drugsTable$Alcohol6Units[i] = "Never"
                
            }
                 
        }       
        
    }
        
    # Sports
    {

        sportsTable$SportsLeisure       = factor(sportsTable$SportsLeisure       , levels = c( "None",   "Light",    "Medium","Hard", "Unknown"))
        sportsTable$SportsOutsideSchool = factor(sportsTable$SportsOutsideSchool , levels = c( "No",  "Yes", "Unknown"))

        sportsTable$SportsFrequency     = factor(sportsTable$SportsFrequency     , levels = c( "Never", "Less than daily", "1 per week", "2-3 per week", "4-6 per week", "Almost every day", "Uknown"))
        sportsTable$SportHours          = factor(sportsTable$SportHours          , levels = c( "None", "About half an hour", "1 to 1.5 hours", "2 to 3 hours", "4 to 6 hours", "7 hours or more", "Uknown"))            
        sportsTable$SportsIntensity     = factor(sportsTable$SportsIntensity     , levels = c( "Not hard at all", "A bit hard", "Quite hard", "Very hard", "Extremely hard", "Uknown"))                        

        sportsTable$SummerTransport     = factor(sportsTable$SummerTransport     , levels = c( "By car or moped", "By bus", "By bike", "On foot", "Uknown"))
        sportsTable$SummerTime          = factor(sportsTable$SummerTime          , levels = c( "<5 minutes", "6-15 minutes", "16-30 minutes", "31-60", ">60 minutes", "Uknown"))                                
        sportsTable$WinterTransport     = factor(sportsTable$WinterTransport     , levels = c( "By car or moped", "By bus", "By bike", "On foot", "Uknown"))
        sportsTable$WinterTime          = factor(sportsTable$WinterTime          , levels = c( "<5 minutes", "6-15 minutes", "16-30 minutes", "31-60", ">60 minutes", "Uknown"))                                            

        sportsTable$ScreenTime          = factor(sportsTable$ScreenTime          , levels = c( "None",  "About half an hour",  "About 1 to 1,5 hours", "About 2 to 3 hours",
                                                                                               "About 4 to 6 hours", "About 7 to 9 hours", "10 hours or more", "Uknown"))                                            
         
        
    }
    
    # Diet
    {
    
        # Time of the day
        dietTable$BreakfastFrequency      = factor(dietTable$BreakfastFrequency      , levels = c( "Rarely/Never", "1-3 times per week", "4-6 times per week", "Every day",  "Uknown"))
        # -- Lunch and dinner is missing from data
        # Specific foods
        dietTable$FatFishFrequency        = factor(dietTable$FatFishFrequency        , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Uknown"))                
        dietTable$LeanFishFrequency       = factor(dietTable$LeanFishFrequency       , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Uknown"))
        dietTable$SeagullEggsFrequency    = factor(dietTable$SeagullEggsFrequency    , levels = c( "Rarely/Never", "1-3 times per year", "4-5 times per year", "6-9 times per year", "10 or more times per year", "Uknown"))            
        dietTable$ReindeerFrequency       = factor(dietTable$ReindeerFrequency       , levels = c( "Rarely/Never", "1-3 times per year", "4-5 times per year", "6-9 times per year", "10 or more times per year", "Uknown"))            
        dietTable$CheeseFrequency         = factor(dietTable$CheeseFrequency         , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Uknown"))
        dietTable$ChocolateFrequency      = factor(dietTable$ChocolateFrequency      , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Uknown"))
        dietTable$FruitsFrequency         = factor(dietTable$FruitsFrequency         , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "1-2 times per day", "3-4 times per day", "5 times or more per day", "Uknown"))
        dietTable$VegetablesFrequency     = factor(dietTable$VegetablesFrequency     , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "1-2 times per day", "3-4 times per day", "5 times or more per day", "Uknown"))            
        dietTable$DairyFrequency          = factor(dietTable$DairyFrequency          , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Uknown"))
        # Specific Drinks
        dietTable$FruitJuiceFrequency     = factor(dietTable$FruitJuiceFrequency     , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Uknown"))
        dietTable$SugarJuiceFrequency     = factor(dietTable$SugarJuiceFrequency     , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Uknown"))            
        dietTable$SugarDrinkFrequency     = factor(dietTable$SugarDrinkFrequency     , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Uknown"))            
        dietTable$SweetenerDrinkFrequency = factor(dietTable$SweetenerDrinkFrequency , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Uknown"))            
        dietTable$WaterFrequency          = factor(dietTable$WaterFrequency          , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Uknown"))                        
        # Specific pills
        dietTable$FishOilFrequency        = factor(dietTable$FishOilFrequency        , levels = c( "No",   "Sometimes",  "Yes, daily", "Unknown"))
        dietTable$VitaminsFrequency       = factor(dietTable$VitaminsFrequency       , levels = c( "No",   "Sometimes",  "Yes, daily", "Unknown"))            

    }
    
    # Sleep
    {

        sleepTable$BedTimeHourCat = factor(sleepTable$BedTimeHourCat , levels = c( "18:00 or earlier", "18:30", "19:00", "19:30",
                                                                                      "20:00", "20:30", "21:00", "21:30", "22:00",
                                                                                      "22:30", "23:00", "23:30", "00:00", "00:30",
                                                                                      "01:00", "01:30", "02:00 or later", "Unknown")) 
        sleepTable$SleepingPills  = factor(sleepTable$SleepingPills , levels = c( "Not used",   "Less frequently than every week",    "Every week, but not daily", "Daily", "Unknown"))            
           
    }

    # Contraceptives
    {
        # The proper order is no hormones, low estradiol, high progestine, progestine + estradiol    
        contraceptivesDBDF$Hormonal = factor(contraceptivesDBDF$Hormonal , levels = c("Non-hormonal", "Progestin", "High Estradiol", "Low Estradiol", "Unknown"))
        
    }
    

  }

# From here on, we can finally add all tables into a list and do things automatic
{

    allBasicTablesList = list(basicTable, antropometricTable, aureusTable,
                              highSchoolTable, bloodTable, frienshipTable,
                              drugsTable, sportsTable, biomarkersTable,
                              dietTable, sleepTable)

    allBasicTablesNames = c("Basic", "Antropometric", "Aureus", "High-school",
                            "Blood", "Friendship", "Drugs", "Sports", "Biomarkers",
                            "Diet", "Sleep")
    
    totalBasicTables   = length(allBasicTablesList)
    
}

# Check that you don't have any NA in any table that might have some hidden meaning
#
# (This actually repeat in the metainfo variables DF, so maybe skip)
# -----------------------------------------------------------------------------
{
 
    NAWarningsDF           = DF(totalBasicTables, 2)
    colnames(NAWarningsDF) = c("Table", "Total Columns with NA")
    
    # R is horrible, it uses [] and [[]] for indexing as if they are totally
    # different concept while both do exactly the same. Is just an arbitrary
    # restriction that [] is use for vectors and [[]] is use for lists.
    #
    # Seriously, R is to programming what a tricycle is to transportation.
    # Yes, is way easier to drive a tricycle than to drive a 6 shafts with a 
    # 12 gearbox truck. But transporting things in tricycles is completely
    # impractical. If people are too scare to learn how to drive a truck we
    # shouldn't be making better tricycles, is never going to work. Learn C++!
    
    for(i in 1:totalBasicTables){
        
        currentTable     = allBasicTablesList[[i]]
        currentTableName = allBasicTablesNames[i]
        
        currentTotalVariables = ncol(currentTable)
    
        hasNAvariables     = rep(FALSE, currentTotalVariables)
        howManyNAvariables = rep(0,     currentTotalVariables)    
      
        for(j in 1:currentTotalVariables){
        
            totalNAs = sum(is.na(currentTable[,j]))    
            if(totalNAs > 0){
                
                hasNAvariables[j]     = TRUE
                howManyNAvariables[j] = totalNAs
                
            }
            
        }
        
        
        if(sum(howManyNAvariables) > 0){

            print("")
            print("NAs WARNING!!")
            print(currentTableName)
            print("Please make sure that these columns are clean and this is what you want:")
            print(colnames(currentTable)[hasNAvariables])
            print("")
        
        }
          
    }
       
}


# -----------------------------------------------------------------------------  
# Join tables with proper info together
# -----------------------------------------------------------------------------
{
        
    completeTable = basicTable     %>% left_join(antropometricTable ,   by="ID")
    completeTable = completeTable  %>% left_join(menstruationTable ,    by="ID")
    completeTable = completeTable  %>% left_join(aureusTable ,          by="ID")
    completeTable = completeTable  %>% left_join(highSchoolTable ,      by="ID")
    completeTable = completeTable  %>% left_join(bloodTable ,           by="ID")
    #completeTable = completeTable  %>% left_join(questionariesTable ,   by="ID")
    #completeTable = completeTable  %>% left_join(sociologyTable ,       by="ID")
    completeTable = completeTable  %>% left_join(frienshipTable ,       by="ID")
    #completeTable = completeTable  %>% left_join(pubertyMenTable ,      by="ID")
    #completeTable = completeTable  %>% left_join(pubertyWomenTable ,    by="ID")
    completeTable = completeTable  %>% left_join(drugsTable ,           by="ID")
    completeTable = completeTable  %>% left_join(sportsTable ,          by="ID")
    #completeTable = completeTable  %>% left_join(hygieneTable ,         by="ID")
    #completeTable = completeTable  %>% left_join(hospitalizationTable , by="ID")
    completeTable = completeTable  %>% left_join(biomarkersTable ,      by="ID")
    completeTable = completeTable  %>% left_join(dietTable ,            by="ID")
    completeTable = completeTable  %>% left_join(sleepTable ,           by="ID")        
                
}


# -----------------------------------------------------------------------------
# Make the proper graph objects
# -----------------------------------------------------------------------------
{

    # Matrixes
    {
        
    
            
    }
    
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
        # Nothing here
    
    
    
    }

    # Create the graph object
    {
    
        # Directed
        overallGraph  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = T)
        physicalGraph = graph_from_data_frame(physicalEdgesDF, vertices = nodesDF, directed = T)
        schoolGraph   = graph_from_data_frame(schoolEdgesDF,   vertices = nodesDF, directed = T)
        sportsGraph   = graph_from_data_frame(sportsEdgesDF,   vertices = nodesDF, directed = T)
        homeGraph     = graph_from_data_frame(homeEdgesDF,     vertices = nodesDF, directed = T)
        othersGraph   = graph_from_data_frame(otherEdgesDF,    vertices = nodesDF, directed = T)
    
        # Undirected
        undirectedOverallGraph  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = F)
        undirectedPhysicalGraph = graph_from_data_frame(physicalEdgesDF, vertices = nodesDF, directed = F)
        undirectedSchoolGraph   = graph_from_data_frame(schoolEdgesDF,   vertices = nodesDF, directed = F)
        undirectedSportsGraph   = graph_from_data_frame(sportsEdgesDF,   vertices = nodesDF, directed = F)
        undirectedHomeGraph     = graph_from_data_frame(homeEdgesDF,     vertices = nodesDF, directed = F)
        undirectedOthersGraph   = graph_from_data_frame(otherEdgesDF,    vertices = nodesDF, directed = F)
        
        # Reciprocal
        reciprocalOverallGraph  = delete_edges(overallGraph,  E(overallGraph)[!which_mutual(overallGraph)])
        reciprocalPhysicalGraph = delete_edges(physicalGraph, E(physicalGraph)[!which_mutual(physicalGraph)])
        reciprocalSchoolGraph   = delete_edges(schoolGraph,   E(schoolGraph)[!which_mutual(schoolGraph)])
        reciprocalSportsGraph   = delete_edges(sportsGraph,   E(sportsGraph)[!which_mutual(sportsGraph)])
        reciprocalHomeGraph     = delete_edges(homeGraph,     E(homeGraph)[!which_mutual(homeGraph)])
        reciprocalOthersGraph   = delete_edges(othersGraph,   E(othersGraph)[!which_mutual(othersGraph)])
        
        # Reciprocal Edges DF
        reciprocalOverallEdgesDF  = as.data.frame(get.edgelist(reciprocalOverallGraph))
        reciprocalPhysicalEdgesDF = as.data.frame(get.edgelist(reciprocalPhysicalGraph))
        reciprocalSchoolEdgesDF   = as.data.frame(get.edgelist(reciprocalSchoolGraph))
        reciprocalSportsEdgesDF   = as.data.frame(get.edgelist(reciprocalSportsGraph))
        reciprocalHomeEdgesDF     = as.data.frame(get.edgelist(reciprocalHomeGraph))
        reciprocalOthersEdgesDF   = as.data.frame(get.edgelist(reciprocalOthersGraph))
    
        colnames(reciprocalOverallEdgesDF)  = c("from", "to")
        colnames(reciprocalPhysicalEdgesDF) = c("from", "to")
        colnames(reciprocalSchoolEdgesDF)   = c("from", "to")
        colnames(reciprocalSportsEdgesDF)   = c("from", "to")
        colnames(reciprocalHomeEdgesDF)     = c("from", "to")
        colnames(reciprocalOthersEdgesDF)   = c("from", "to")
          
      }

    # Make a vector with all the edges and all the graph
    {
        allEdges  = list(overallEdgesDF, physicalEdgesDF, schoolEdgesDF, sportsEdgesDF, homeEdgesDF, otherEdgesDF)
        allGraphs = list(overallGraph,   physicalGraph,   schoolGraph,   sportsGraph,   homeGraph,   othersGraph)
    }
  
}


# -----------------------------------------------------------------------------
# Prepare the frienship matrix with numbers
# -----------------------------------------------------------------------------
{
    # The friend matrix, as 1 (friend) and 0 (non friend)
    # -- Friends if the original code is defined by rows (each row a maximum of 5)
    tempOverall      = overallNetworkDF
    tempOverall$ID   = NULL
    friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
}

# -----------------------------------------------------------------------------
# Find the indexes for all the data
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

print(getwd())

source("indexesV2.R", encoding="utf-8")



# Subset tables
# -----------------------------------------------------------------------------
{

    # By one variable
    {
    
        # -- By Sex
        menOnlyTable   = subset(completeTable, completeTable[,sexIndex] == "Man")
        womenOnlyTable = subset(completeTable, completeTable[,sexIndex] == "Woman")
        totalMenRows   = nrow(menOnlyTable)
        totalWomenRows = nrow(womenOnlyTable)
        
        # Diseases
        # Medication
        # Contraceptives (nothing here, since only women take contraceptives)
        
    }
    
    # Individual tables
        
}


# Create a dataframe with all the variables meta-information
#     - Type of variable
#     - Number of categories (0 = Numerical)
#     - NA counts
# Analyze which type of variables you have in each column
{
    
    # How many columns and rows
    totalColumns = ncol(completeTable)
    totalRows    = nrow(completeTable)

    # The main dataframe
    variablesInfoDF           = DF((totalColumns + totalBasicTables - 1), 8)     # Each table has an ID column that you need to add (+total tables)
                                                                                 # except for the first one that is already included in the complete table (-1)
    colnames(variablesInfoDF) = c("VariableID", "Table", "Type", "TotalCategories", "TotalNAValues", "TableIndex", "LocalIndex", "UniversalIndex")
    
    currentIndex = 1
    
    # For each of the tables that we have    
    for(i in 1:totalBasicTables){
    
        # Get the table and table name
        currentTable     = allBasicTablesList[[i]]
        currentTableName = allBasicTablesNames[i]
        
        # Count how many columns you have
        currentTotalColumns = ncol(currentTable)

        for(j in 1:currentTotalColumns){
    
            # Init some basic variables
            currentVariableName   = colnames(currentTable)[j]
            currentType           = "Date"
            currentCategories     = 0
            currentNAs            = sum(is.na(currentTable[,j]))
            currentUniversalIndex = grep( paste0("^",currentVariableName,"$"),   colnames(completeTable))

            # Figure it out the type and number of categories if any
            myCurrentClass = class(currentTable[,j])

            # -- Categorical
            if(myCurrentClass == "character" || myCurrentClass == "factor"){
                
                currentType       = "Categorical"
                currentCategories = nrow(summarizeCategorical(currentTable, j, sorted = "none", crop = 0))
            
            
            }
            # -- Numerical
            if(myCurrentClass == "integer" || myCurrentClass == "numeric"){
                
                currentType       = "Numerical"
                
            }

            # Write the information into the metasummary
            variablesInfoDF$VariableID[currentIndex]      = currentVariableName
            variablesInfoDF$Table[currentIndex]           = currentTableName
            variablesInfoDF$Type[currentIndex]            = currentType
            variablesInfoDF$TotalCategories[currentIndex] = currentCategories
            variablesInfoDF$TotalNAValues[currentIndex]   = currentNAs
            variablesInfoDF$TableIndex[currentIndex]      = i
            variablesInfoDF$LocalIndex[currentIndex]      = j
            variablesInfoDF$UniversalIndex[currentIndex]  = currentUniversalIndex
                
            # R is horrible, I want to do currentIndex++ so badly
            # Even if you don't provide it in the basic package, you could at
            # least allow for overwrite operators like in C++ where you can
            # program for ++ [] [[]] - * to do whatever you want. But not here.
            currentIndex = currentIndex + 1
            
        }
        
    }
    
}


# Group indexes together. This is a brute force approach, and you should
# really use the manual indexes in the indexes script. In here you have too many
# indexes, such as the SPA-types with 200+ categories.
#
# In order to solve that a little bit in the brute force approach, we also do
# indexes that has less than the category limit we set up in the constants
# script (usually 12)
{

    bruteCategoricalIndexes      = variablesInfoDF[variablesInfoDF$Type == "Categorical",]$UniversalIndex
    totalBruteCategoricalIndexes = length(bruteCategoricalIndexes)
    
    bruteLimitCategoricalIndexes      = variablesInfoDF[variablesInfoDF$Type == "Categorical" & variablesInfoDF$TotalCategories<=CATEGORY_LIMIT ,]$UniversalIndex
    totalBruteLimitCategoricalIndexes = length(bruteLimitCategoricalIndexes)
    
    
    
}