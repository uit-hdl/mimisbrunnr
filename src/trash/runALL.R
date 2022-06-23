# -----------------------------------------------------------------------------
# Run all the analysis of every data.
#
# If you run this it will reset the whole project to the latest version.
#
# This is not something bad, but you will loose old info if you don't backup it
# manually first.
# -----------------------------------------------------------------------------


# "Reset" the R session
# -----------------------------------------------------------------------------
{
  
  # You can restart R session via command line, but the code will be stuck here
  # then. You need to restart R manually if you want to drop all packages 
  # properly.
  
  rm(list = ls(all.names = TRUE)) # Clear all objects includes hidden objects.
  gc()                            # Free up memory and report the memory usage.

}


# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)


# Load the needed libraries
# -----------------------------------------------------------------------------
{
  
  # First, init all the constants
  source("constants.R",        encoding="utf-8")
  
  # Then load the needed libraries
  #source("tools.R",            encoding="utf-8")
  
  
  source("toolsBasic.R",       encoding="utf-8")
  source("toolsNetworks.R",    encoding="utf-8")
  source("toolsLatex.R",       encoding="utf-8")
  source("toolsPlotting.R",    encoding="utf-8")
  source("toolsSummarizers.R", encoding="utf-8")
  
  source("analysis.R",         encoding="utf-8")
  
}



# Delete files from result folders
# -----------------------------------------------------------------------------
# ---- Delete everything that is not a dir
{
  filesToDelete = list.files(RESULT_FOLDER, all.files=TRUE, include.dirs=FALSE,
                             recursive=TRUE, full.names=TRUE)
  
  do.call(file.remove, list(filesToDelete))
  
  # ---- Delete everything that is a dir
  filesToDelete = list.files(RESULT_FOLDER, all.files=TRUE, include.dirs=TRUE,
                             recursive=TRUE, full.names=TRUE)
  
  filesToDelete = rev(filesToDelete) # Reverse order so subfolders go first.
  
  do.call(file.remove, list(filesToDelete))
}


# Create the needed folders for the project
# -----------------------------------------------------------------------------
{
  
  # All warnings are turn off because you will always get a warning, these
  # folders are probably already created and you are just updating the content
  # thus suppressing the warning system here.
  
  # RESULTS FOLDERS
  {
    dir.create(file.path(RESULT_FOLDER),            showWarnings = FALSE) 
    dir.create(file.path(CONTROL_FOLDER),           showWarnings = FALSE)
    dir.create(file.path(GENERAL_FOLDER),           showWarnings = FALSE)
    dir.create(file.path(NETWORK_FOLDER),           showWarnings = FALSE)
    dir.create(file.path(AUREUS_FOLDER),            showWarnings = FALSE)
    dir.create(file.path(HORMONAL_FOLDER),          showWarnings = FALSE)
    dir.create(file.path(DRUGS_FOLDER),             showWarnings = FALSE)
    dir.create(file.path(ANTROPOMETRIC_FOLDER),     showWarnings = FALSE)
    
    dir.create(file.path(AUTOMATIC_FOLDER),         showWarnings = FALSE) 
    
  }
  
  # LOG FOLDERS
  {
    dir.create(file.path(LOGS_FOLDER),              showWarnings = FALSE)  
  }
  
  # CONTROL FOLDERS
  {
    dir.create(file.path(AUTOMATIC_CONTROL_FOLDER), showWarnings = FALSE)
    dir.create(file.path(MANUAL_CONTROL_FOLDER),    showWarnings = FALSE)    
  }
  
  # LATEX FOLDERS
  {
    # Main folder
    dir.create(file.path(LATEX_FOLDER),                             showWarnings = FALSE)  
    # Images
    dir.create(file.path(LATEX_FOLDER_IMAGES),                      showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_FIGURES),              showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_OTHERS),               showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS),              showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_ALL),          showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_AUREUS),       showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_CONTROL),      showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_GENERAL),      showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_NETWORK),      showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_HORMONAL),     showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_DRUGS),        showWarnings = FALSE)  
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_ANTROPOMETRY), showWarnings = FALSE)  
    
    dir.create(file.path(LATEX_FOLDER_IMAGES_RESULTS_LOGS),    showWarnings = FALSE)  

    # Tables
    dir.create(file.path(LATEX_FOLDER_TABLES),                 showWarnings = FALSE)  
    
  }
  
  
}



# Red the original data and prepare it so it can be read properly
# Normally you will run this only once.
#
# ---- The dataCleaning.txt log is generated here. It contains important
#      information regarding IDs that are missing (if any)
#
print("Cleaning data...")
source("dataCleaning.R",  encoding="utf-8")

# Filter the ready data by whatever metric and prepare a second data ready with
# the applied filter.
#
# ---- For example, in our case we take away anyone older than 20yo from the
#      dataset. Old people in Norwegian highschools are people with mental
#      disabilities and are outliers from the general population in respect
#      social relationships, medicine intake, biomarkers, and bloodwork.
#
print("Filtering data...")
source("filter.R",  encoding="utf-8")

# Once the data is ready, you can actually load it and modify whatever you need.
#
# We need to do some fixing for R specifically first.
# ---- Strings of dates converted into Date datatype
# ---- Manual order for the categorical variables that can be sorted
# ---- In this case, the Highschool ID instead of 34 is now H34 for clarity
# ---- And so on...
#
# In here you also get the metainformation about all the variables
# ---- How many numerical/categoricals
# ---- How many NA values in each column
#
print("Loading data...")
source("loadData.R",  encoding="utf-8")


# This script keeps track of the index of each variable.
#
# For example, we know that there is a variable called "Sex", but after the
# cleaning, filtering, loading process, how knows in which column it is.
# Specially if you didn't manipulate the data yourself.
#
# So this just gives a numerical value associated witch each name
#
# This also helps to keep track of the color coding of each variable. So if you
# do a plot with index "2", you can look up automatically which colors
# correspond to that index.
#
# In here you also select which variables are important, and which aren't, so
# you don't run crazy number of operations in the automatic analysis.
#
print("Finding Indexes...")
source("indexes.R",  encoding="utf-8")


# From here on is each specific analysis of each chapter in the report.
# You can do them in any order, or choose not to do any of these if you don't
# want to loose time.
#
# DATA CONTROL
# GENERAL ANALYIS
print("General analysis...")
source("generalAnalysis.R",  encoding="utf-8")

# SA ANALYSIS


# Update the latex folder
#
# This is actually done in each individual previous scripts during development
# But you are suppose to do it only once, when you are finish analyzing 
# everything.
source("latex.R", encoding="utf-8")