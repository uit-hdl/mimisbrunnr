# -----------------------------------------------------------------------------
# Set the global constant for analysis
#
# ie
#
# - How much is the p-value significant?
# - The color of things
# - Folder and files names
# -----------------------------------------------------------------------------

# Load needed libraries
# -----------------------------------------------------------------------------
library(lubridate)     # as_datetime for Sys.time()

# Set working directory to file location
# -----------------------------------------------------------------------------
{

    # R, and RStudio, absolutely sucks when it comes to handling the getting and
    # setting of the working directory. I tried one million things, and nothing works
    # because it keeps changing with every update.
    #
    # So, this is bad programming, where here you have the absolute folder where
    # the project is located with respect this constants.R script.
    #
    # YOU NEED TO SOURCE THIS SCRIPT AS THE FILE LOCATION MANUALLY ( AND I HATE
    # THAT BECAUSE THERE IS NO WAY TO DO THIS RELIABLE VIA CODING!).
    #
    # Everything else hangs from this folder
    
    # THIS, DOESN'T, WORK, CONSISTENLY:
    
    # Set working directory to file location
    # -----------------------------------------------------------------------------
    # this.dir = dirname(parent.frame(2)$ofile)
    # setwd(this.dir)
    
    MAIN_PROJECT_FOLDER = file.path("/home/gromenawer/Desktop/Amalgamlab/mimisbrunnr/")
    setwd(MAIN_PROJECT_FOLDER)
    
    MAIN_CODE_FOLDER    = file.path(paste0(MAIN_PROJECT_FOLDER, "src/"))
    
}

# When you run everything in automatic mode, what things you should actually
# run. For example, you might want to skip updating the plots.
#
# All of this is deprecated
# -----------------------------------------------------------------------------
{
  #SAVE_PLOTS            = TRUE
  #SAVE_BAR_PLOTS        = TRUE
  #SAVE_BOX_PLOTS        = TRUE
  #SAVE_DENSITY_PLOTS    = TRUE
  #SAVE_REGRESSION_PLOTS = TRUE

  #LOG_P_SIGNIFICANT_ONLY  = TRUE
  #P_SIGNIFICANT_THRESHOLD = 0.05
  #N_SIGNIFICANT_SIZE      = 10
}


# Timing stuff
# -----------------------------------------------------------------------------
{
  RIGHT_NOW        = gsub("-","",gsub(" ","_",gsub(":","",as_datetime(Sys.time()))))
}


# Set Global Variables
# -----------------------------------------------------------------------------
{

  # Random date for date calculations
  #    ( This is use mostly to calculate the age at time of test
  #    Giving the age of the test is not fully correct, as there
  #    is a significant difference between two 4 year old childs
  #    that are 4 years a 1 day, and 4 years and 360 days. )
  # REFERENCE_DATE <- as.Date("2017-01-01")

  # Networks names
  NETWORK_NAMES  = c("Overall", "Physical", "School", "Sports", "Home", "Other")
  TOTAL_NETWORKS = length(NETWORK_NAMES)
  
  # Colors
  {
    
    # Use this to generate colors autimatically from a given pallette
    # myPalette       = colorRampPalette(brewer.pal(5, "RdYlGn"))
    # colorVectorEdge = myPalette(5)
      
    # General abstract colors
    
    # ---- Conceptual colors
    COLOR_FRIENDSHIP      = "#E8D13A"
      
    # ---- Missing and unknown data
    COLOR_NA              = "#7F7F7F"
    COLOR_UNKNOWN         = "#7F7F7F"

    # ---- General High to None scale, High is better
    COLOR_NONE             = "#D7191C"
    COLOR_LOW              = "#FDAE61"
    COLOR_MEDIUM           = "#FFFFBF"
    COLOR_HIGH             = "#ABDDA4"
    
    # ---- Generic two groups, Group B is better
    colorGroupA           = "#f8766d"
    colorGroupB           = "#00bfc4"
    
    # ---- Specific shades of a given color
    COLOR_WHITE           = "#ffffff"
    COLOR_RED_LOW         = "#fdd49e"
    COLOR_RED_MED         = "#fc8d59"
    COLOR_RED_HIG         = "#d7301f"
    COLOR_BLUE_LOW        = "#d0d1e6"
    COLOR_BLUE_MED        = "#74a9cf"
    COLOR_BLUE_HIG        = "#0570b0"
    
    # Specific colors for specific data
    
    # ---- Sex
    COLOR_MAN             = "#01C0DB"
    COLOR_MAN_LOW         = "#9ED3DB"
    COLOR_WOMAN           = "#EF35AE"
    COLOR_WOMAN_LOW       = "#EFC6E1"
    
    # ---- Smoke status
    COLOR_SMOKE_NEVER     = "#B7C68B"   
    COLOR_SMOKE_SOMETIMES = "#F4F0CB"
    COLOR_SMOKE_DAILY     = "#FF6666"

    # ---- Carrier status
    COLOR_CARRIER         = "#F77FEE"   # (Purple, typical dye for S.Aureus)
    COLOR_NON_CARRIER     = "#C5FAD9"   # (Green, because contrast purple)

    # --- BMI
    COLOR_BMI_UNDERWEIGHT  = "#FFFFBF"  # (Yellow, not so good)
    COLOR_BMI_HEALTHY      = "#ABDDA4"  # (Green, better)
    COLOR_BMI_OVERWEIGHT   = "#FDAE61"  # (Yellow, not so good)
    COLOR_BMI_OBESE        = "#D7191C"  # (Red, bad)

    # --- Pain
    colorPainRes          = "#f2b33d"
    colorNoPainRes        = "#9bdf38"
    
    # --- Health
    COLOR_HEALTH_VERY_BAD  = "#D7191C"
    COLOR_HEALTH_BAD       = "#FDAE61"
    COLOR_HEALTH_MEDIUM    = "#FFFFBF"
    COLOR_HEALTH_GOOD      = "#A6D96A"
    COLOR_HEALTH_EXCELLENT = "#1A9641"
    COLOR_HEALTH_UNKNOWN   = COLOR_UNKNOWN

    # Generic vectors of colors
    # It puts the previous colors into predifined vectors. Plotting functions
    # needs the vectors as argument, not the individual colors.
    COLOR_INTERVAL_PVALUES = c(COLOR_WHITE, COLOR_RED_LOW, COLOR_RED_MED, COLOR_RED_HIG,
                               COLOR_BLUE_HIG, COLOR_BLUE_MED, COLOR_BLUE_LOW, COLOR_WHITE)
    COLOR_VECTOR_ONE       = c(COLOR_NA)
    COLOR_VECTOR_SEX       = c(COLOR_MAN, COLOR_WOMAN)
    COLOR_VECTOR_SMOKE     = c(COLOR_SMOKE_NEVER, COLOR_SMOKE_SOMETIMES, COLOR_SMOKE_DAILY, COLOR_UNKNOWN)
    COLOR_VECTOR_SNUFF     = c(COLOR_SMOKE_NEVER, COLOR_SMOKE_SOMETIMES, COLOR_SMOKE_DAILY, COLOR_UNKNOWN)
    COLOR_VECTOR_CARRIER   = c(COLOR_CARRIER, COLOR_NON_CARRIER)
    COLOR_VECTOR_SPORTS    = c(COLOR_NONE, COLOR_LOW, COLOR_MEDIUM, COLOR_HIGH, COLOR_UNKNOWN)
    COLOR_VECTOR_BMI       = c(COLOR_BMI_UNDERWEIGHT, COLOR_BMI_HEALTHY, COLOR_BMI_OVERWEIGHT, COLOR_BMI_OBESE, COLOR_UNKNOWN)
    COLOR_VECTOR_HEALTH    = c(COLOR_HEALTH_VERY_BAD, COLOR_HEALTH_BAD, COLOR_HEALTH_MEDIUM, COLOR_HEALTH_GOOD, COLOR_HEALTH_EXCELLENT, COLOR_HEALTH_UNKNOWN)
    
    COLOR_VECTOR_SAME_RELATIONSHIP = c(COLOR_RED_HIG,COLOR_UNKNOWN)
    
    #colorVectorPain      <- c(colorPainRes, colorNoPainRes)
    #colorVectorGeneric2  <- c(colorGroupA, colorGroupB)
  }

  # Intervals
  INTERVAL_PVALUES = c(-0.05, -0.01, -0.001, 0, +0.001, +0.01, +0.05)

  # How many categories do you want to analyze as maximum for each column
  # If a column have more than this categories, we will skip it in the analysis
  # where we have to break down by categories
  # For example, if you run all the p-values against all the p-values, you will
  # need to do 16! combinations, which is way too much.
  CATEGORY_LIMIT = 12

  # Which layouts do you want to plots
  # There is no good layout by default. You need to run all layout manually
  # until you are satisfy with how it looks like. After some testing, this
  # layout is what it looks best. However if you want to try any other layout
  # you have all of them in ALL_LAYOUT variable (next)
  DO_THIS_LAYOUTS = c("mds")
  
  # Keep track off all layouts that you can do
  ALL_LAYOUTS = c('grid','star','circle','gem', 'dh', 'graphopt', 'mds', 'fr', 'kk', 'drl', 'lgl')
 
}


# Folders to write analysis results and reports
# -----------------------------------------------------------------------------
{
  
    # Folders where we write reports
    {
        # Main folder
        REPORTS_FILEPATH          = file.path(paste(MAIN_PROJECT_FOLDER,  "reports/",      sep = ""))
        #     /Web
        REPORTS_WEB_FILEPATH      = file.path(paste(REPORTS_FILEPATH,     "Web/",          sep = ""))
        #        /HTML
        REPORTS_WEB_HTML_FILEPATH = file.path(paste(REPORTS_WEB_FILEPATH, "HTML/",         sep = ""))        
        
    }

    # Folders where we save main results from the analysis
    {
    
        # Main folders
        RESULT_FOLDER        = file.path(paste(MAIN_PROJECT_FOLDER,"results/",        sep = ""))
    
        GENERAL_FOLDER       = file.path(paste(RESULT_FOLDER, "general/",              sep = ""))
        NETWORK_FOLDER       = file.path(paste(RESULT_FOLDER, "network/",              sep = ""))
        AUREUS_FOLDER        = file.path(paste(RESULT_FOLDER, "aureus/",               sep = ""))
        CONTROL_FOLDER       = file.path(paste(RESULT_FOLDER, "control/",              sep = ""))
        HORMONAL_FOLDER      = file.path(paste(RESULT_FOLDER, "hormonal/",             sep = ""))
        DRUGS_FOLDER         = file.path(paste(RESULT_FOLDER, "drugs/",                sep = ""))
        ANTROPOMETRIC_FOLDER = file.path(paste(RESULT_FOLDER, "antropometry/",         sep = ""))
        AUTOMATIC_FOLDER     = file.path(paste(RESULT_FOLDER, "automatic/",            sep = ""))
        PAPER_FOLDER         = file.path(paste(RESULT_FOLDER, "paper/",                sep = ""))
        LOGS_FOLDER          = file.path(paste(RESULT_FOLDER, "logs/",                 sep = ""))
        BIOMARKERS_FOLDER    = file.path(paste(RESULT_FOLDER, "biomarkers/",           sep = ""))
    
        # The control folder has two subfolders, one for the automatic control,
        # and another one for the manual control
    
        AUTOMATIC_CONTROL_FOLDER = file.path(paste(CONTROL_FOLDER, "automatic/",       sep = ""))
        MANUAL_CONTROL_FOLDER    = file.path(paste(CONTROL_FOLDER, "manual/",          sep = ""))
    
    
        # ---- Subfolders
    
        #          /Biomarkers
        #              /tables 
        BIOMARKERS_FOLDER_TABLES                        = file.path(paste(BIOMARKERS_FOLDER, "tables/",                       sep = ""))
        #                  /sex                     (sex differences)
        BIOMARKERS_FOLDER_TABLES_SEX                    = file.path(paste(BIOMARKERS_FOLDER, "tables/sex/",                   sep = ""))
        #                  /network                (everything that has to do with the network)
        BIOMARKERS_FOLDER_TABLES_NETWORK                = file.path(paste(BIOMARKERS_FOLDER, "tables/network/",               sep = ""))
        #                  /blood                  (everything that has to do with the blood and antropometry)
        BIOMARKERS_FOLDER_TABLES_BLOOD                  = file.path(paste(BIOMARKERS_FOLDER, "tables/blood/",                 sep = ""))    
        #                 /diseases
        BIOMARKERS_FOLDER_TABLES_DISEASES               = file.path(paste(BIOMARKERS_FOLDER, "tables/diseases/",              sep = ""))    
        #                 /medicines
        BIOMARKERS_FOLDER_TABLES_MEDICINES              = file.path(paste(BIOMARKERS_FOLDER, "tables/medicines/",             sep = ""))        
        #                 /host factors
        BIOMARKERS_FOLDER_TABLES_HOSTFACTORS            = file.path(paste(BIOMARKERS_FOLDER, "tables/hostfactors/",           sep = ""))            
        #                     /sex
        BIOMARKERS_FOLDER_TABLES_HOSTFACTORS_SEX        = file.path(paste(BIOMARKERS_FOLDER, "tables/hostfactors/sex/",       sep = ""))
        #                     /averages
        BIOMARKERS_FOLDER_TABLES_HOSTFACTORS_SEX        = file.path(paste(BIOMARKERS_FOLDER, "tables/hostfactors/averages/",  sep = ""))
    
        #              /images
        #                  /general                 (LOD stats, and so on)
        BIOMARKERS_FOLDER_IMAGES_GENERAL                = file.path(paste(BIOMARKERS_FOLDER, "images/general/",               sep = ""))
        #                  /sex                     (sex differences)
        BIOMARKERS_FOLDER_IMAGES_SEX                    = file.path(paste(BIOMARKERS_FOLDER, "images/sex/",                   sep = ""))
        BIOMARKERS_FOLDER_IMAGES_SEX_BOXPLOTS           = file.path(paste(BIOMARKERS_FOLDER, "images/sex/boxplots/",          sep = ""))
        #                  /antropometry           (all density, boxplot, and BMI plots)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY           = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/",          sep = ""))
        #                  /blood                  (all blood plots)
        BIOMARKERS_FOLDER_IMAGES_BLOOD                  = file.path(paste(BIOMARKERS_FOLDER, "images/blood/",                 sep = ""))
        #                  /network                (everything that has to do with the network)
        BIOMARKERS_FOLDER_IMAGES_NETWORK                = file.path(paste(BIOMARKERS_FOLDER, "images/network/",               sep = ""))
        BIOMARKERS_GRAPH_GRID                           = file.path(paste(BIOMARKERS_FOLDER_IMAGES_NETWORK, "allGraphs.png",  sep = ""))
        #                  /regression_biofriends  (For when you study biomarkesr againts friends)
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS  = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/", sep = ""))
        #                 /host factors
        #                     /sex
        BIOMARKERS_FOLDER_IMAGES_HOSTFACTORS_SEX        = file.path(paste(BIOMARKERS_FOLDER, "images/hostfactors/sex/",        sep = ""))
        #                     /levels
        BIOMARKERS_FOLDER_IMAGES_HOSTFACTORS_LEVELS     = file.path(paste(BIOMARKERS_FOLDER, "images/hostfactors/levels/",     sep = ""))
    
        # -------- /Latex
        #          Relative base path to result folder with all tables and images
        #          When generating the latex files, those .tex files need to
        #          refer to the result folder. The .tex files are in another
        #          independent folder, so these are the filepath route from the
        #          latex folder to the result folder.
        LATEX_RELATIVE_BASE_PATH                        = file.path("../../../../results",     sep = "")
        #              /general 
        LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_GENERAL = file.path(paste(LATEX_RELATIVE_BASE_PATH, "biomarkers/images/general/", sep = ""))
        #              /network
        LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_NETWORK = file.path(paste(LATEX_RELATIVE_BASE_PATH, "biomarkers/images/network/", sep = ""))
    
  }

}


# Prepare logs and results files summaries
#
# These are static plots and tables that we know before hand that we are going
# to generate. For example how many edges were lost in the data cleaning
# process. In contrast, the plots paths for each column for each table is
# generated on the fly depending on the table and column name.
# -----------------------------------------------------------------------------
{
  # Main logs
  #csvPath  = file.path(paste(LOGS_FOLDER, "resultData.csv", sep = "")) # I don't remember what this what for :(
  LOG_PATH = file.path(paste(LOGS_FOLDER, "log.txt", sep = ""))

  # Data cleaning
  CLEANING_LOG_PATH                    = file.path(paste(LOGS_FOLDER,         "cleaningLog.txt",    sep = ""))
  PREANALYSIS_LOG_PATH                 = file.path(paste(LOGS_FOLDER,         "preAnalysisLog.txt", sep = ""))
  
  #CLEANING_LOG_LATEX_TABLE             = file.path(paste(LATEX_FOLDER_TABLES, "cleaningLog.tex", sep = ""))
  #CLEANING_MISSING_FRIENDS_BOXPLOT     = file.path(paste(LOGS_FOLDER,         "Boxplot_Cleaning_MissingFriends.png",     sep = ""))
  #CLEANING_MISSING_NOMINATIONS_BOXPLOT = file.path(paste(LOGS_FOLDER,         "Boxplot_Cleaning_MissingNominations.png", sep = ""))
  
  # Data filtering
  FILTERING_LOG_PATH                    = file.path(paste(LOGS_FOLDER,         "filteringLog.txt", sep = ""))
  #FILTERING_LOG_LATEX_TABLE             = file.path(paste(LATEX_FOLDER_TABLES, "filteringLog.tex", sep = ""))
  #FILTERING_MISSING_FRIENDS_BOXPLOT     = file.path(paste(LOGS_FOLDER,         "Boxplot_Filtering_MissingFriends.png",     sep = ""))
  #FILTERING_MISSING_NOMINATIONS_BOXPLOT = file.path(paste(LOGS_FOLDER,         "Boxplot_Filtering_MissingNominations.png", sep = ""))

  # Log for each analysis
  # (Not use at the moment, but to be use later)
  GENERAL_LOG_PATH = file.path(paste(LOGS_FOLDER, "generalLog.txt", sep = ""))
  NETWORK_LOG_PATH = file.path(paste(LOGS_FOLDER, "networkLog.txt", sep = ""))
  AUREUS_LOG_PATH  = file.path(paste(LOGS_FOLDER, "aureusLog.txt",  sep = ""))
  
  # Control data
  
  #CONTROL_CATEGORICAL_CSV = file.path(paste(AUTOMATIC_CONTROL_FOLDER, "categorical.csv", sep = ""))
  #CONTRO_NUMERICAL_CSV    = file.path(paste(AUTOMATIC_CONTROL_FOLDER, "numerical.csv", sep = ""))

  # Automatic data
  #PVALUESCSV = file.path(paste(AUTOMATIC_FOLDER, "allPValues.csv", sep = ""))
  #PVALUESTXT = file.path(paste(AUTOMATIC_FOLDER, "allPValues.txt", sep = ""))

  # Network data
  # ---- Basic statistics
  #NETWORKSTATSCSV = file.path(paste(NETWORK_FOLDER, "networkStats.csv", sep = ""))

}


# Prepare the data path for where do we find the data
# -----------------------------------------------------------------------------
{
    # ---- Set folder where the data is
    {
        
        # Main data folder
        DATA_FOLDER            = file.path(paste(MAIN_PROJECT_FOLDER, "data/",          sep = ""))
        
        # Original data
        DATA_ORIGINAL          = file.path(paste(DATA_FOLDER,         "originalData/csv/",  sep = ""))

        # Data transformed and clean
        DATA_READY_FOLDER      = file.path(paste(DATA_FOLDER,         "cleanData/", sep = ""))
        DATA_FILTER_FOLDER     = file.path(paste(DATA_FOLDER,         "filterData/",   sep = ""))
        
        # Metadata with topic information
        #   This cointain things such as blood variables complete names,
        #   biomarkers LOD, biomarkers uniprot website, and so on. If you want
        DATA_META_FOLDER      = file.path(paste(DATA_FOLDER,         "metaData/", sep = ""))
                
        # Fake data
        #   This is autogenerated synthetic data that has absolutely nothing to
        #   do with the original patients
        DATA_FAKE_FOLDER     = file.path(paste(DATA_FOLDER,         "fakeData/",   sep = ""))        
    }
  
    # ---- Set the filenames
    {
        # For the original files
        {
            AUREUS_FILENAME      = "saureus_19022020.csv"
            BIOMARKERS_FILENAME  = "eutro_rafael_paakoblet.csv"
            HORMONAL_FILENAME    = "data_ut_11Juni2019.csv"
            REPORT_FILENAME      = "repor20211005.csv"
            FF2ANTRO_FILENAME    = "Perskey_FF2 antropometri.csv"
        }
  
        # For the already clean and ready to work files
        if(FALSE){
  
            # Actual variables 
            READY_DATA_BASIC_FILENAME             = "basic.csv"
            READY_DATA_FF1_ANTROPOMETRIC_FILENAME = "antropometryFF1.csv"
            READY_DATA_FF2_ANTROPOMETRIC_FILENAME = "antropometryFF2.csv"
            READY_DATA_MENSTRUATION_FILENAME      = "menstruation.csv"
            READY_DATA_DISEASES_FILENAME          = "diseases.csv"   
            READY_DATA_MEDICINES_FILENAME         = "medicines.csv" 
            READY_DATA_CONTRACEPTIVES_FILENAME    = "contraceptives.csv" 
            READY_DATA_NETWORK_FILENAME           = "network.csv"  
            READY_DATA_NETWORK_TECHNICAL_FILENAME = "networkTechnical.csv"              
            READY_DATA_AUREUS_FILENAME            = "aureus.csv"   
            READY_DATA_SWABBING_FILENAME          = "swabbing.csv"   
            READY_DATA_HIGHSCHOOL_FILENAME        = "highschool.csv"
            READY_DATA_BLOOD_FILENAME             = "blood.csv"
            READY_DATA_BLOOD_TECHNICAL_FILENAME   = "bloodTechnical.csv"
            READY_DATA_QUESTIONARIES_FILENAME     = "questionaries.csv"
            READY_DATA_SOCIOLOGY_FILENAME         = "sociology.csv"
            READY_DATA_FRIENSHIP_FILENAME         = "friendship.csv"
            READY_DATA_PUBERTYMEN_FILENAME        = "pubertyMen.csv"
            READY_DATA_PUBERTYWOMEN_FILENAME      = "pubertyWomen.csv"
            READY_DATA_DRUGS_FILENAME             = "drugs.csv" 
            READY_DATA_SPORTS_FILENAME            = "sports.csv"
            READY_DATA_HYGIENE_FILENAME           = "hygiene.csv"
            READY_DATA_HOSPITALIZATION_FILENAME   = "hospitalization.csv"
            READY_DATA_BIOMARKERS_FILENAME        = "biomarkers.csv"
            READY_DATA_DIET_FILENAME              = "diet.csv"
            READY_DATA_SLEEP_FILENAME             = "sleep.csv"        

            # Frienship networks
            READY_DATA_OVERALL_NETWORK_FILENAME        = "overallNetwork.csv"
            READY_DATA_PHYSICAL_NETWORK_FILENAME       = "physicalNetwork.csv"
            READY_DATA_HOME_NETWORK_FILENAME           = "homeNetwork.csv"
            READY_DATA_SCHOOL_NETWORK_FILENAME         = "schoolNetwork.csv"
            READY_DATA_SPORTS_NETWORK_FILENAME         = "sportsNetwork.csv"
            READY_DATA_OTHERS_NETWORK_FILENAME         = "othersNetwork.csv"      
    
            READY_DATA_FF12_OVERALL_NETWORK_FILENAME   = "overallNetworkFF12.csv"
            READY_DATA_FF12_PHYSICAL_NETWORK_FILENAME  = "physicalNetworkFF12.csv"
            READY_DATA_FF12_HOME_NETWORK_FILENAME      = "homeNetworkFF12.csv"
            READY_DATA_FF12_SCHOOL_NETWORK_FILENAME    = "schoolNetworkFF12.csv"
            READY_DATA_FF12_SPORTS_NETWORK_FILENAME    = "sportsNetworkFF12.csv"
            READY_DATA_FF12_OTHERS_NETWORK_FILENAME    = "othersNetworkFF12.csv"               
            
            # Synthetic, fake data, and so on
            READY_FAKE_DATA_FILENAME              = "fake.csv"
    
            # Unique table with everything that is important
            READY_DATA_COMPLETE_FILENAME          = "complete.csv"
      
        }
  
        # For the already filter and ready to work files
        if(FALSE){
      
            # Actual variables 
            FILTER_DATA_BASIC_FILENAME             = "basic.csv"
            FILTER_DATA_FF1_ANTROPOMETRIC_FILENAME = "antropometryFF1.csv"
            FILTER_DATA_FF2_ANTROPOMETRIC_FILENAME = "antropometryFF2.csv"
            FILTER_DATA_MENSTRUATION_FILENAME      = "menstruation.csv"
            FILTER_DATA_DISEASES_FILENAME          = "diseases.csv"   
            FILTER_DATA_MEDICINES_FILENAME         = "medicines.csv" 
            FILTER_DATA_CONTRACEPTIVES_FILENAME    = "contraceptives.csv" 
            FILTER_DATA_NETWORK_FILENAME           = "network.csv"  
            FILTER_DATA_AUREUS_FILENAME            = "aureus.csv"   
            FILTER_DATA_HIGHSCHOOL_FILENAME        = "highschool.csv"
            FILTER_DATA_BLOOD_FILENAME             = "blood.csv"
            FILTER_DATA_QUESTIONARIES_FILENAME     = "questionaries.csv"
            FILTER_DATA_SOCIOLOGY_FILENAME         = "sociology.csv"
            FILTER_DATA_FRIENSHIP_FILENAME         = "friendship.csv"
            FILTER_DATA_PUBERTYMEN_FILENAME        = "pubertyMen.csv"
            FILTER_DATA_PUBERTYWOMEN_FILENAME      = "pubertyWomen.csv"
            FILTER_DATA_DRUGS_FILENAME             = "drugs.csv" 
            FILTER_DATA_SPORTS_FILENAME            = "sports.csv"
            FILTER_DATA_HYGIENE_FILENAME           = "hygiene.csv"
            FILTER_DATA_HOSPITALIZATION_FILENAME   = "hospitalization.csv"
            FILTER_DATA_BIOMARKERS_FILENAME        = "biomarkers.csv"
            FILTER_DATA_DIET_FILENAME              = "diet.csv"
            FILTER_DATA_SLEEP_FILENAME             = "sleep.csv"        
      
            # Frienship networks
            FILTER_DATA_OVERALL_NETWORK_FILENAME   = "overallNetwork.csv"
            FILTER_DATA_PHYSICAL_NETWORK_FILENAME  = "physicalNetwork.csv"
            FILTER_DATA_HOME_NETWORK_FILENAME      = "homeNetwork.csv"
            FILTER_DATA_SCHOOL_NETWORK_FILENAME    = "schoolNetwork.csv"
            FILTER_DATA_SPORTS_NETWORK_FILENAME    = "sportsNetwork.csv"
            FILTER_DATA_OTHERS_NETWORK_FILENAME    = "othersNetwork.csv"      
      
            # Synthetic, fake data, and so on
            FILTER_FAKE_DATA_FILENAME     = "fake.csv"
          
            # Unique table with everything that is important
            FILTER_DATA_COMPLETE_FILENAME   = "complete.csv"
      
        }
    
    }
  
    # ---- Set the file paths
    {
        # For the original files
        {
            AUREUS_FILEPATH     = file.path(paste(DATA_ORIGINAL, AUREUS_FILENAME,     sep = ""))
            HORMONAL_FILEPATH   = file.path(paste(DATA_ORIGINAL, HORMONAL_FILENAME,   sep = ""))
            BIOMARKERS_FILEPATH = file.path(paste(DATA_ORIGINAL, BIOMARKERS_FILENAME, sep = ""))
            REPORT_FILEPATH     = file.path(paste(DATA_ORIGINAL, REPORT_FILENAME,     sep = ""))
            FF2ANTRO_FILEPATH   = file.path(paste(DATA_ORIGINAL, FF2ANTRO_FILENAME,   sep = ""))
        }
  
        # For the already clean and ready to work files
        if(FALSE){

            # Actual variables 
            READY_DATA_BASIC_FILEPATH             = file.path(paste(DATA_READY_FOLDER, READY_DATA_BASIC_FILENAME,             sep = ""))
            READY_DATA_FF1_ANTROPOMETRIC_FILEPATH = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF1_ANTROPOMETRIC_FILENAME, sep = ""))
            READY_DATA_FF2_ANTROPOMETRIC_FILEPATH = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF2_ANTROPOMETRIC_FILENAME, sep = ""))
            READY_DATA_MENSTRUATION_FILEPATH      = file.path(paste(DATA_READY_FOLDER, READY_DATA_MENSTRUATION_FILENAME,      sep = ""))
            READY_DATA_CONTRACEPTIVES_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_CONTRACEPTIVES_FILENAME ,   sep = ""))
            READY_DATA_NETWORK_FILEPATH           = file.path(paste(DATA_READY_FOLDER, READY_DATA_NETWORK_FILENAME,           sep = ""))
            READY_DATA_NETWORK_TECHNICAL_FILEPATH = file.path(paste(DATA_READY_FOLDER, READY_DATA_NETWORK_TECHNICAL_FILENAME, sep = ""))
            READY_DATA_AUREUS_FILEPATH            = file.path(paste(DATA_READY_FOLDER, READY_DATA_AUREUS_FILENAME,            sep = ""))
            READY_DATA_SWABBING_FILEPATH          = file.path(paste(DATA_READY_FOLDER, READY_DATA_SWABBING_FILENAME,          sep = ""))
            READY_DATA_HIGHSCHOOL_FILEPATH        = file.path(paste(DATA_READY_FOLDER, READY_DATA_HIGHSCHOOL_FILENAME,        sep = ""))
            READY_DATA_BLOOD_FILEPATH             = file.path(paste(DATA_READY_FOLDER, READY_DATA_BLOOD_FILENAME,             sep = ""))
            READY_DATA_BLOOD_TECHNICAL_FILEPATH   = file.path(paste(DATA_READY_FOLDER, READY_DATA_BLOOD_TECHNICAL_FILENAME,   sep = ""))
            READY_DATA_QUESTIONARIES_FILEPATH     = file.path(paste(DATA_READY_FOLDER, READY_DATA_QUESTIONARIES_FILENAME,     sep = ""))
            READY_DATA_SOCIOLOGY_FILEPATH         = file.path(paste(DATA_READY_FOLDER, READY_DATA_SOCIOLOGY_FILENAME,         sep = ""))
            READY_DATA_FRIENSHIP_FILEPATH         = file.path(paste(DATA_READY_FOLDER, READY_DATA_FRIENSHIP_FILENAME,         sep = ""))            
            READY_DATA_PUBERTYMEN_FILEPATH        = file.path(paste(DATA_READY_FOLDER, READY_DATA_PUBERTYMEN_FILENAME,        sep = ""))
            READY_DATA_PUBERTYWOMEN_FILEPATH      = file.path(paste(DATA_READY_FOLDER, READY_DATA_PUBERTYWOMEN_FILENAME,      sep = ""))
            READY_DATA_DRUGS_FILEPATH             = file.path(paste(DATA_READY_FOLDER, READY_DATA_DRUGS_FILENAME,             sep = ""))
            READY_DATA_SPORTS_FILEPATH            = file.path(paste(DATA_READY_FOLDER, READY_DATA_SPORTS_FILENAME,            sep = ""))
            READY_DATA_HYGIENE_FILEPATH           = file.path(paste(DATA_READY_FOLDER, READY_DATA_HYGIENE_FILENAME,           sep = ""))
            READY_DATA_HOSPITALIZATION_FILEPATH   = file.path(paste(DATA_READY_FOLDER, READY_DATA_HOSPITALIZATION_FILENAME,   sep = ""))
            READY_DATA_BIOMARKERS_FILEPATH        = file.path(paste(DATA_READY_FOLDER, READY_DATA_BIOMARKERS_FILENAME,        sep = ""))
            READY_DATA_DIET_FILEPATH              = file.path(paste(DATA_READY_FOLDER, READY_DATA_DIET_FILENAME,              sep = ""))
            READY_DATA_SLEEP_FILEPATH             = file.path(paste(DATA_READY_FOLDER, READY_DATA_SLEEP_FILENAME,             sep = ""))            
            # -- Databases
            READY_DATA_DISEASES_FILEPATH          = file.path(paste(DATA_READY_FOLDER, READY_DATA_DISEASES_FILENAME,          sep = ""))
            READY_DATA_MEDICINES_FILEPATH         = file.path(paste(DATA_READY_FOLDER, READY_DATA_MEDICINES_FILENAME,         sep = ""))
            
          
            # Networks
            # FF1
            READY_DATA_OVERALL_NETWORK_FILEPATH   = file.path(paste(DATA_READY_FOLDER, READY_DATA_OVERALL_NETWORK_FILENAME,   sep = ""))
            READY_DATA_PHYSICAL_NETWORK_FILEPATH  = file.path(paste(DATA_READY_FOLDER, READY_DATA_PHYSICAL_NETWORK_FILENAME,  sep = ""))
            READY_DATA_HOME_NETWORK_FILEPATH      = file.path(paste(DATA_READY_FOLDER, READY_DATA_HOME_NETWORK_FILENAME,      sep = ""))
            READY_DATA_SCHOOL_NETWORK_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_SCHOOL_NETWORK_FILENAME,    sep = ""))
            READY_DATA_SPORTS_NETWORK_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_SPORTS_NETWORK_FILENAME,    sep = ""))
            READY_DATA_OTHERS_NETWORK_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_OTHERS_NETWORK_FILENAME,    sep = ""))
            # FF12
            READY_DATA_FF12_OVERALL_NETWORK_FILEPATH   = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF12_OVERALL_NETWORK_FILENAME,   sep = ""))
            READY_DATA_FF12_PHYSICAL_NETWORK_FILEPATH  = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF12_PHYSICAL_NETWORK_FILENAME,  sep = ""))
            READY_DATA_FF12_HOME_NETWORK_FILEPATH      = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF12_HOME_NETWORK_FILENAME,      sep = ""))
            READY_DATA_FF12_SCHOOL_NETWORK_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF12_SCHOOL_NETWORK_FILENAME,    sep = ""))
            READY_DATA_FF12_SPORTS_NETWORK_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF12_SPORTS_NETWORK_FILENAME,    sep = ""))
            READY_DATA_FF12_OTHERS_NETWORK_FILEPATH    = file.path(paste(DATA_READY_FOLDER, READY_DATA_FF12_OTHERS_NETWORK_FILENAME,    sep = ""))            

                        
            # Complete dataframe
            READY_DATA_COMPLETE_FILEPATH          = file.path(paste(DATA_READY_FOLDER, READY_DATA_COMPLETE_FILENAME,          sep = ""))
              
            # Fake data
            READY_FAKE_FILEPATH                   = file.path(paste(DATA_READY_FOLDER, READY_FAKE_DATA_FILENAME,              sep = ""))
        }
    
        # For the already filter and ready to work files
        if(FALSE){
      
            # Actual variables 
            FILTER_DATA_BASIC_FILEPATH              = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_BASIC_FILENAME,               sep = ""))
            FILTER_DATA_FF1_ANTROPOMETRIC_FILEPATH  = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_FF1_ANTROPOMETRIC_FILENAME,   sep = ""))
            FILTER_DATA_FF2_ANTROPOMETRIC_FILEPATH  = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_FF2_ANTROPOMETRIC_FILENAME,   sep = ""))
            
            
            
            FILTER_DATA_MENSTRUATION_FILEPATH    = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_MENSTRUATION_FILENAME,    sep = ""))
            FILTER_DATA_CONTRACEPTIVES_FILEPATH  = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_CONTRACEPTIVES_FILENAME , sep = ""))
            FILTER_DATA_NETWORK_FILEPATH         = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_NETWORK_FILENAME,         sep = ""))
            FILTER_DATA_AUREUS_FILEPATH          = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_AUREUS_FILENAME,          sep = ""))
            FILTER_DATA_HIGHSCHOOL_FILEPATH      = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_HIGHSCHOOL_FILENAME,      sep = ""))
            FILTER_DATA_BLOOD_FILEPATH           = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_BLOOD_FILENAME,           sep = ""))
            FILTER_DATA_QUESTIONARIES_FILEPATH   = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_QUESTIONARIES_FILENAME,   sep = ""))
            FILTER_DATA_SOCIOLOGY_FILEPATH       = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_SOCIOLOGY_FILENAME,       sep = ""))
            FILTER_DATA_FRIENDSHIP_FILEPATH      = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_FRIENSHIP_FILENAME,       sep = ""))
            FILTER_DATA_PUBERTYMEN_FILEPATH      = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_PUBERTYMEN_FILENAME,      sep = ""))
            FILTER_DATA_PUBERTYWOMEN_FILEPATH    = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_PUBERTYWOMEN_FILENAME,    sep = ""))
            FILTER_DATA_DRUGS_FILEPATH           = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_DRUGS_FILENAME,           sep = ""))
            FILTER_DATA_SPORTS_FILEPATH          = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_SPORTS_FILENAME,          sep = ""))
            FILTER_DATA_HYGIENE_FILEPATH         = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_HYGIENE_FILENAME,         sep = ""))
            FILTER_DATA_HOSPITALIZATION_FILEPATH = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_HOSPITALIZATION_FILENAME, sep = ""))
            FILTER_DATA_BIOMARKERS_FILEPATH      = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_BIOMARKERS_FILENAME,      sep = ""))
            FILTER_DATA_DIET_FILEPATH            = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_DIET_FILENAME,            sep = ""))           
            FILTER_DATA_SLEEP_FILEPATH           = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_SLEEP_FILENAME,           sep = ""))           
            # -- Databases
            FILTER_DATA_DISEASES_FILEPATH        = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_DISEASES_FILENAME,        sep = ""))
            FILTER_DATA_MEDICINES_FILEPATH       = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_MEDICINES_FILENAME,       sep = ""))
        
            # Networks
            FILTER_DATA_OVERALL_NETWORK_FILEPATH   = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_OVERALL_NETWORK_FILENAME,   sep = ""))
            FILTER_DATA_PHYSICAL_NETWORK_FILEPATH  = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_PHYSICAL_NETWORK_FILENAME,  sep = ""))
            FILTER_DATA_HOME_NETWORK_FILEPATH      = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_HOME_NETWORK_FILENAME,      sep = ""))
            FILTER_DATA_SCHOOL_NETWORK_FILEPATH    = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_SCHOOL_NETWORK_FILENAME,    sep = ""))
            FILTER_DATA_SPORTS_NETWORK_FILEPATH    = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_SPORTS_NETWORK_FILENAME,    sep = ""))
            FILTER_DATA_OTHERS_NETWORK_FILEPATH    = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_OTHERS_NETWORK_FILENAME,    sep = ""))
          
            # Complete dataframe
            FILTER_DATA_COMPLETE_FILEPATH          = file.path(paste(DATA_FILTER_FOLDER, FILTER_DATA_COMPLETE_FILENAME,          sep = ""))
          
            # Fake data
            FILTER_FAKE_FILEPATH                   = file.path(paste(DATA_FILTER_FOLDER, FILTER_FAKE_DATA_FILENAME,              sep = ""))
        }
    }
    
    # ---- Special files with metasummaries (variables names, limits, and so on)
    {
        
        BIOMARKERS_METATABLE_FILENAME = "biomarkers/metabiomarkers.csv"
        BIOMARKERS_METATABLE_FILEPATH = file.path(paste(DATA_META_FOLDER, BIOMARKERS_METATABLE_FILENAME, sep = ""))
        
        BLOOD_METATABLE_FILENAME      = "blood/bloodTable.csv"
        BLOOD_METATABLE_FILEPATH      = file.path(paste(DATA_META_FOLDER, BLOOD_METATABLE_FILENAME,      sep = ""))
        
    }
    
}
