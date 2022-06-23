# -----------------------------------------------------------------------------
#
# This script contain everything related to plotting
#
# -----------------------------------------------------------------------------


# Add the needed libraries
# ---- Basics
library(ggplot2)       # Basic ggplot2 library
library(ggnewscale)    # Allows for multiple color scales in one plot
library(RColorBrewer)  # Color settins and palettes
library(shadowtext)    # Drop shadows in text for better viewing
library(latex2exp)     # Allows latex expressions in ggplot
library(qqplotr)       # QQ plots with bands
library(forcats)       # Reverse order of factors (fct_rev)
library(gridExtra)     # grid.arrange()

#library(ggcorrplot)    # Correlogram (not in use right?)
#library(lubridate)     # as_datetime for Sys.time() mostly
# library(ggpubr)
# library(stringr)
# library(scales)        # Color bre
# library(gtable)
# library(grid)


# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("toolsBasic.R",       encoding="utf-8")
source("toolsNetworks.R",    encoding="utf-8")
source("toolsLatex.R",       encoding="utf-8")
source("toolsSummarizers.R", encoding="utf-8")


# ------------------------------------------------
# GET DEFAULT PLOT VARIABLES
# ------------------------------------------------
{
  
  # This section is all the functions related to give you an automatic title,
  # subtitle, axys labels, and so for. The rules for all the functions are the
  # same:
  #
  # - If you give me a NULL, the function will prepare an automatic
  # label for that particular part of the plot.
  #
  # Notice that there is a difference in between "" and NULL in ggplot2. For
  # example, a NULL plot title, will reserve no space for the title at all.
  # While a "" plot title will show nothing, but you will have an empty space
  # occupying certain height of the image where the plot title should go.
  #
  # If you want such space to exist, give as argument an empty string, such as
  # "". This will use the string as it is and write nothing.
  #
  # Because R is a horrible language, it can't return a vector of 5 elements,
  # where some of them are NULL. If you have, let say, 2 elements that are NULL
  # then the vector will be resize automatically to size 3 instead. This makes
  # impossible in R to make a proper code that delete the title completely as
  # and argument and you need to do a very weird mixing of numerical and strings
  # instead. If you don't want the title to appear, you will have to modify the
  # ggplot2 theme manually and make title = NULL later on. Same for any other
  # element of the plot.
  #
  #
  # - If you want an actual label manually, just pass the argument as a string
  # with whatever you want. For example "This is the title of my plot".
  #
  # 
  # The function are divided by the type of variables that you have. Such as
  # categorical alone, numerical alone, numerical with numerical, and all
  # combinations up to 3 variables. You don't need to worry about calling the
  # right function, you just need to use the first function that you will see
  # now. This function take the type of plot that you are using, such as barplot
  # or regression, or whatever, and it's automatically knows what type of
  # variables you are suppose to have and use the right function automatically.
  #
  # The plots implemented so far are these:
  #
  # 1 Variable:
  #     1 Categorical:
  #         - "AbsBarplot"
  #         - "LongBarplot"
  #         - "RelBarplot"
  #     1 Numerical:
  # 2 Variables:
  #     2 Categorical
  #     2 Numerical
  #     1 Categorical 1 Numerical
  # 3 Variables:
  #     3 Categorical
  #     3 Numerical
  #     2 Categorical 1 Numerical
  #     1 Categorical 2 Numerical
  
  
  # TODO: This function is a good idea, but it gets too complicated in the
  # argument list. Not sure if this is actually useful in practice.
  # This function return the default labels regardless of the type of plot.
  # Input your type of plot, you get the appropriate function automatically
  getDefaultLabels <- function(variable1 = NULL, variable2 = NULL, variable3 = NULL,
                               myCategories = NULL, colorsVector = NULL,
                               plotTitle = NULL, plotSubtitle = NULL,
                               plotCaption = NULL, plotXLabel = NULL,
                               plotYLabel = NULL, plotType = NULL){
    
    print("sss")
    
  }
  
  
  # Get default texts and color palettes for plots initialized to NULL inputs.
  #
  # Used in the plots type:
  #
  #     - "AbsBarplot"
  #     - "LongBarplot"
  #     - "RelBarplot"
  #
  # Input:
  #     -      (string) groupingName: The name of the variable you are grouping by.
  #     - List (string) myCategories: Each of the categories that you can find in that variable.
  #     - List (string) colorsVector: If NULL, it will return an automatic color for each category.
  #                                   If not NULL, it will return the same list.
  #     -      (string) plotTitle:    Your plot title, if NULL, it will return an autogenerated title.
  #     -      (string) plotSubtitle: Your plot subtitle, if NULL, it will return an autogenerated subtitle.
  #     -      (string) plotCaption:  Your plot caption, if NULL, it will return a generic default caption.
  #     -      (string) plotXLabel:   Your plot X label, if NULL, it will return a generic default X label. Otherwise, it return the same groupingName.
  #     -      (string) plotXLabel:   Your plot Y label, if NULL, it will return a generic default Y label. Otherwise.
  #     -      (string) plotType:     Select the type of plot that you have, the options are:
  #                                       - NULL (without quotes), select a generic default type. It works fine, but you will get weird naming.
  #                                       - "AbsBarplot",          select a barplot with absolute frequencies
  # Return:
  #     - list of colors, one for each category plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
  getCategoricalDefaults <- function(groupingName, myCategories, colorsVector = NULL,
                                     plotTitle = NULL, plotSubtitle = NULL,
                                     plotCaption = NULL, plotXLabel = NULL,
                                     plotYLabel = NULL, plotType = NULL){
    
    # Get the number of categories form the categories vector
    nCategories = length(myCategories)
    
    # Prepare the defaults
    # ---- Strings for the plot
    # -------- Plot title
    if(is.null(plotTitle)){
      
      if(is.null(plotType))             plotTitle    = "Default plot title"
      if(plotType == "AbsBarplot")      plotTitle    = paste("Absolute Frequency grouped by ", groupingName, sep = "")
      if(plotType == "LongAbsBarplot")  plotTitle    = paste("Absolute Frequency grouped by ", groupingName, sep = "")
      if(plotType == "RelBarplot")      plotTitle    = paste("Relative Frequency grouped by ", groupingName, sep = "")
      if(plotType == "LongRelBarplot")  plotTitle    = paste("Relative Frequency grouped by ", groupingName, sep = "")
      
    }
    # -------- Plot subtitle
    if(is.null(plotSubtitle)){
      
      plotSubtitle = ""
      
    }
    # -------- Plot caption
    if(is.null(plotCaption)){
      
      plotCaption = ""
      
    }
    # -------- Plot X axys
    if(is.null(plotXLabel)){
      
      if(length(groupingName) <= 0){
        groupingName = "X"
      }
      
      plotXLabel = groupingName
      
    }
    # -------- Plot Y axys
    if(is.null(plotYLabel)){
      
      plotYLabel = "Y"
      
      if(plotType == "AbsBarplot")      plotYLabel = "Absolute Frequency"
      if(plotType == "LongBarplot")     plotYLabel = "Absolute Frequency"
      if(plotType == "RelBarplot")      plotYLabel = "Relative Frequency"
      
    }
    # ---- Color scheme
    if(is.null(colorsVector) || is.na(colorsVector)){
      
      myPalette    = colorRampPalette(brewer.pal(5, "Spectral"))
      colorsVector = myPalette(nCategories)
      
    }
    
    toReturn = c(list(colorsVector),
                 plotTitle,
                 plotSubtitle,
                 plotCaption,
                 plotXLabel,
                 plotYLabel)
    
    print(toReturn)
    
    return(toReturn)
    
  }
  
  # Get default text and color palettes for plots inits to NULL inputs
  # This version is for when you are grouping two categories together
  #
  # Variable A is display in the X axys
  # Variable B is display in the Y axys
  #
  # Used in the plots type:
  #
  #     - "CombinedRelBarplot"
  #
  getBiCategoricalDefaults <- function(groupingNameA, myCategoriesA, 
                                       groupingNameB, myCategoriesB, 
                                       colorsVector = NULL,
                                       plotTitle = NULL, plotSubtitle = NULL,
                                       plotCaption = NULL, plotXLabel = NULL,
                                       plotYLabel = NULL, plotType = NULL){
    
    # Get the number of categories form the categories vector
    nCategoriesA  = length(myCategoriesA)
    nCategoriesB  = length(myCategoriesB)
    
    # Prepare the defaults
    # ---- Strings for the plot
    # -------- Plot title
    if(is.null(plotTitle)){
      
      if(is.null(plotType))                plotTitle    = "Default plot title"
      if(plotType == "CombinedRelBarplot") plotTitle    = paste("Relative Frequency of ", groupingNameA, sep = "")
      if(plotType == "CombinedAbsBarplot") plotTitle    = paste("Abs Frequency of ",      groupingNameA, sep = "")
      
    }
    # -------- Plot subtitle
    if(is.null(plotSubtitle)){
      
      if(is.null(plotType))                plotSubtitle = ""
      if(plotType == "CombinedRelBarplot") plotSubtitle = paste("Grouped by ", groupingNameB, sep = "")
      if(plotType == "CombinedAbsBarplot") plotSubtitle = paste("Grouped by ", groupingNameB, sep = "")
      
    }
    # -------- Plot caption
    if(is.null(plotCaption)){
      
      plotCaption = ""
      
    }
    # -------- Plot X axys
    if(is.null(plotXLabel)){
      
      plotXLabel = groupingNameA
      
    }
    # -------- Plot Y axys
    if(is.null(plotYLabel)){
      
        if(is.null(plotType))                plotYLabel = "Y"
        if(plotType == "CombinedRelBarplot") plotYLabel = "Relative frequency"
        if(plotType == "CombinedAbsBarplot") plotYLabel = "Absolute frequency"
      
    }
    # ---- Color scheme
    if(is.null(colorsVector) || is.na(colorsVector)){
      
      myPalette    = colorRampPalette(brewer.pal(11, "Spectral"))
      colorsVector = myPalette(nCategoriesB)
      
    }
    
    toReturn = c(list(colorsVector), plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    
    # print(plotTitle)
    # print(plotSubtitle)
    # print(plotCaption)
    # print(plotXLabel)
    # print(plotYLabel)
    
    return(toReturn)
    
  }
  
  # Get default text and color palettes for plots inits to NULL inputs
  # This version is for when you are not grouping an just have one numerical variable
  #
  # Used in the plots type:
  #
  #     - "Boxplots"
  #     - "Density"
  #     - "Histogram"
  #     - "QQ"
  #
  # You have the option to do Categorical Boxplots somewhere else
  getNumericalDefaults <- function(numericalName,
                                   colorsVector = NULL,
                                   plotTitle = NULL, plotSubtitle = NULL,
                                   plotCaption = NULL, plotXLabel = NULL,
                                   plotYLabel = NULL, fileType = NULL){
    
    # Prepare the defaults

    # ---- Strings for the plot
    # -------- Plot title
    if(is.null(plotTitle)){
      
      if(is.null(fileType))             plotTitle    = "Default plot title"
      if(fileType == "Boxplot")         plotTitle    = paste("Boxplot for ",              numericalName, sep = "")
      if(fileType == "Density")         plotTitle    = paste("Density distribution for ", numericalName, sep = "")
      if(fileType == "Histogram")       plotTitle    = paste("Histogram for ",            numericalName, sep = "")
      if(fileType == "QQ")              plotTitle    = paste("QQ-plot for  ",             numericalName, sep = "")
      
    }
    # -------- Plot subtitle
    if(is.null(plotSubtitle)){
      
      plotSubtitle = ""
      
    }
    # -------- Plot caption
    if(is.null(plotCaption)){
      
      plotCaption = ""
      
    }
    # -------- Plot X axys
    if(is.null(plotXLabel)){
      
      if(is.null(fileType))             plotXLabel = ""
      if(fileType == "Boxplot")         plotXLabel = numericalName
      if(fileType == "Density")         plotXLabel = numericalName
      if(fileType == "Histogram")       plotXLabel = numericalName
      if(fileType == "QQ")              plotXLabel = paste("Theoretical quantiles for  ", numericalName, sep = "")
      
    }
    # -------- Plot Y axys
    if(is.null(plotYLabel)){
      
      if(is.null(fileType))             plotYLabel = ""
      if(fileType == "Boxplot")         plotYLabel = "Absolute Frequency"
      if(fileType == "Density")         plotYLabel = "Relative Frequency"
      if(fileType == "Histogram")       plotYLabel = "Absolute Frequency"
      if(fileType == "QQ")              plotYLabel = paste("Actual quantiles for  ", numericalName, sep = "")

    }
    
    # ---- Color scheme
    if(is.null(colorsVector) || is.na(colorsVector)){
      
      #myPalette    = colorRampPalette(brewer.pal(5, "Spectral"))
      #colorsVector = myPalette(1)
      
      # Default to black
      colorsVector = "#000000"
      
    }
    
    
    
    toReturn = c(list(colorsVector), plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    
    return(toReturn)
    
  }
  
  # Get default text and color palettes for plots inits to NULL inputs
  # This version is for when you are not grouping an have two numerical variable
  #
  # Used in the plots type:
  #
  #     - "Scatterplot"
  #
  getBiNumericalDefaults <- function(numericalNameA, numericalNameB, 
                                     colorsVector = NULL,
                                     plotTitle = NULL, plotSubtitle = NULL,
                                     plotCaption = NULL, plotXLabel = NULL,
                                     plotYLabel = NULL,  fileType = NULL){
    
    # Prepare the defaults
    # ---- Strings for the plot
    # -------- Plot title
    if(is.null(plotTitle)){
      
      if(is.null(fileType))         plotTitle    = "Default plot title"
      if(fileType == "Scatterplot") plotTitle    = paste("Scatterplot for ", numericalNameA, " and ", numericalNameB,  sep = "")
      
    }
    # -------- Plot subtitle
    if(is.null(plotSubtitle)){
      
      plotSubtitle = ""
      
    }
    # -------- Plot caption
    if(is.null(plotCaption)){
      
      plotCaption = ""
      
    }
    # -------- Plot X axys
    if(is.null(plotXLabel)){
        
        if(is.null(fileType))             plotXLabel = ""
        if(fileType == "Scatterplot")     plotXLabel = numericalNameA
    }
    # -------- Plot Y axys
    if(is.null(plotYLabel)){
        
        if(is.null(fileType))             plotYLabel = ""
        if(fileType == "Scatterplot")     plotYLabel = numericalNameB
    }
    # ---- Color scheme
    if(is.null(colorsVector) || is.na(colorsVector)){

      myPalette    = colorRampPalette(brewer.pal(5, "Spectral"))
      colorsVector = myPalette(1)

    }
    
    
    toReturn = c(list(colorsVector), plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    
    return(toReturn)
    
  }
  
  # Get default text and color palettes for plots inits to NULL inputs
  # This version is for when you are grouping a categorie and a numerical
  getCategoricalNumericalDefaults <- function(groupingName, myCategories,
                                              numericalName,
                                              colorsVector = NULL,
                                              plotTitle = NULL, plotSubtitle = NULL,
                                              plotCaption = NULL, plotXLabel = NULL,
                                              plotYLabel = NULL, fileType = NULL){
    
    # Get the number of categories form the categories vector
    nCategories = length(myCategories)
    
    # Prepare the defaults
    # ---- Strings for the plot
    # -------- Plot title
    if(is.null(plotTitle)){
      
      if(is.null(fileType))                  plotTitle = "Default plot title"
      if(fileType == "BMIPlot")              plotTitle = paste("BMI for ",                  groupingName,  sep = "")
      if(fileType == "CategoricalBoxplot")   plotTitle = paste("Boxplot for ",              numericalName, sep = "")
      if(fileType == "CategoricalHistogram") plotTitle = paste("Histogram for ",            numericalName, sep = "")
      if(fileType == "CategoricalDensity")   plotTitle = paste("Density distribution for ", numericalName, sep = "")
      if(fileType == "pValuesHeatmap")       plotTitle = paste("P-Values heatmap for ",     groupingName,  sep = "")
      
      
    }
    # -------- Plot subtitle
    if(is.null(plotSubtitle)){
      
      if(is.null(fileType))                  plotSubtitle = ""
      if(fileType == "BMIPlot")              plotSubtitle = paste("Grouped by ", groupingName, sep = "")
      if(fileType == "CategoricalBoxplot")   plotSubtitle = paste("Grouped by ", groupingName, sep = "")
      if(fileType == "CategoricalHistogram") plotSubtitle = paste("Grouped by ", groupingName, sep = "")
      if(fileType == "CategoricalDensity")   plotSubtitle = paste("Grouped by ", groupingName, sep = "")
      # p-Values Heatmap nothing
      
      
    }
    # -------- Plot caption
    if(is.null(plotCaption)){
      
      plotCaption = ""
      
    }
    # -------- Plot X axys
    if(is.null(plotXLabel)){
      
      plotXLabel = groupingName
      
      if(is.null(fileType))                  plotXLabel = "X"
      if(fileType == "BMIPlot")              plotXLabel = "BMI"
      if(fileType == "CategoricalBoxplot")   plotXLabel = groupingName
      if(fileType == "CategoricalHistogram") plotXLabel = groupingName
      if(fileType == "CategoricalDensity")   plotXLabel = groupingName
      if(fileType == "pValuesHeatmap")       plotXLabel = groupingName
      
    }
    # -------- Plot Y axys
    if(is.null(plotYLabel)){
      
      if(is.null(fileType))                  plotYLabel = "Y"
      if(fileType == "BMIPlot")              plotYLabel = "Relative Frequency"
      if(fileType == "CategoricalBoxplot")   plotYLabel = numericalName
      if(fileType == "CategoricalHistogram") plotYLabel = "Absolute Frequency"
      if(fileType == "CategoricalDensity")   plotYLabel = "Relative Frequency"
      if(fileType == "pValuesHeatmap")       plotYLabel = groupingName
      
    }
    # ---- Color scheme
    if(is.null(colorsVector) || is.na(colorsVector)){
      
      myPalette    = colorRampPalette(brewer.pal(5, "Spectral"))
      colorsVector = myPalette(nCategories)
      
    }

    toReturn = c(list(colorsVector), plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    
    return(toReturn)
    
  }
  
  
    # Get default text and color palettes for plots inits to NULL inputs
    # This version is for when you are grouping 2 categories and one numerical
    getBiCategoricalNumericalDefaults <- function(groupingName, myGroupCategories,
                                                  fillingName, myFillCategories,
                                                  numericalName,
                                                  colorsVector = NULL,
                                                  plotTitle = NULL, plotSubtitle = NULL,
                                                  plotCaption = NULL, plotXLabel = NULL,
                                                  plotYLabel = NULL, fileType = NULL){
    
        # Get the number of categories form the categories vector
        nGroupCategories = length(myGroupCategories)
        nFillCategories  = length(myFillCategories)
    
        # Prepare the defaults
        # ---- Strings for the plot
        # -------- Plot title
        if(is.null(plotTitle)){
      
            if(is.null(fileType))                        plotTitle = "Default plot title"
            if(fileType == "DoubleCategoricalBoxplot")   plotTitle = paste0("Boxplots for ", fillingName, " grouped by ", groupingName)

        }
        # -------- Plot subtitle
        if(is.null(plotSubtitle)){
      
            if(is.null(fileType))                        plotSubtitle = ""
            if(fileType == "DoubleCategoricalBoxplot")   plotSubtitle = ""

        }
        # -------- Plot caption
        if(is.null(plotCaption)){
      
            plotCaption = ""
      
        }
        # -------- Plot X axys
        if(is.null(plotXLabel)){
      
            plotXLabel = groupingName
      
            if(is.null(fileType))                      plotXLabel = "X"
            if(fileType == "DoubleCategoricalBoxplot") plotXLabel = groupingName
      
        }
        # -------- Plot Y axys
        if(is.null(plotYLabel)){
      
            if(is.null(fileType))                      plotYLabel = "Y"
            if(fileType == "DoubleCategoricalBoxplot") plotYLabel = numericalName

        }
        # ---- Color scheme
        if(is.null(colorsVector) || is.na(colorsVector)){
      
            myPalette    = colorRampPalette(brewer.pal(5, "Spectral"))
            colorsVector = myPalette(nFillCategories)
      
        }

        toReturn = c(list(colorsVector), plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    
        return(toReturn)
    
    }
  
  
  
  # TODO REVISE THIS
  # Get the defaults text and color for the network plots
  getCategoricalNetworkDefaults   <- function(highlightName = NULL, rimName = NULL,
                                              nCategoriesHighlight = NULL, nCategoriesRim = NULL,
                                              colorsVectorHighlight = NULL, colorVectorRim = NULL,
                                              plotTitle = NULL, plotSubtitle = NULL,
                                              plotCaption = NULL, plotXLabel = NULL,
                                              plotYLabel = NULL, fileType = NULL){

    # Prepare the defaults
    # ---- Strings for the plot
    # -------- Plot title
    if(is.null(plotTitle)){
     
      if(is.null(highlightName)){
        plotTitle = ""
      }
      else{
        plotTitle = paste0("Plot of a network using ", highlightName)  
      }
    }
    # -------- Plot subtitle
    if(is.null(plotSubtitle)){

      if(is.null(rimName)){
        plotSubtitle = ""
      }
      else{
        plotSubtitle = paste0("Rim hightlight with ", rimName)
      }

    }
    # -------- Plot caption
    if(is.null(plotCaption)){
      
      plotCaption = ""
      
    }
    # -------- Plot X axys
    if(is.null(plotXLabel)){
      
      plotXLabel = ""
      
    }
    # -------- Plot Y axys
    if(is.null(plotYLabel)){
      
      plotYLabel = ""
      
    }
    
    # ---- Color scheme for both variables
    if(is.null(colorsVectorHighlight) || is.na(colorsVectorHighlight) ){

      myPalette             = colorRampPalette(brewer.pal(5, "Spectral"))
      colorsVectorHighlight = myPalette(nCategoriesHighlight)
      
    }
    if(is.null(colorVectorRim) || is.na(colorVectorRim)){
      
      myPalette             = colorRampPalette(brewer.pal(5, "Spectral"))
      colorVectorRim        = myPalette(nCategoriesRim)
      
    }
    
    toReturn = c(list(colorsVectorHighlight), list(colorVectorRim), plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)

    return(toReturn)
    
  }
  
  # Get parameters for the theme depending on your theme
  # - white   / (default)
  # - no-grid / same as white, without axys, lines, or numbers in the axys
  # - simple  / same as white without legend
  # - blank   / without anything
  # - ggplot  / Default ggplot2 theme
  getThemeParameters <- function(themeName){
    
    # Init the default theme (white)
    myTheme = list(rep(NA,10))
    
    myTheme[[1]] = element_blank()                  # 1: Background color
    myTheme[[2]] = element_line(colour = "black")   # 2: Axys X and Y Line color
    myTheme[[3]] = element_line(colour = "grey70")  # 3: Axys Y , major breaks
    myTheme[[4]] = element_blank()                  # 4: Axys X , major breaks
    myTheme[[5]] = "right"                          # 5: Legend
    myTheme[[6]] = element_line(size = 3)           # 6: Ticks in both X and Y
    myTheme[[7]] = element_text()                   # 7: Numbers in the X axys
    myTheme[[8]] = element_text()                   # 8: Numbers in the Y axys
    
    
    # If you didn't gave a NULL theme
    if(!is.null(themeName)){
      
      if(themeName == "white"){ # White theme is the default too
        myTheme[[1]] = element_blank()
        myTheme[[2]] = element_line(colour = "black")
        myTheme[[3]] = element_line(colour = "grey70")
        myTheme[[4]] = element_blank()
        myTheme[[5]] = "right"
      }
      
      if(themeName == "no-grid"){ # Bare theme with legend only
        myTheme[[1]] = element_blank()
        myTheme[[2]] = element_blank()
        myTheme[[3]] = element_blank()
        myTheme[[4]] = element_blank()
        myTheme[[5]] = "right"
        myTheme[[6]] = element_blank() # Ticks in both X and Y
        myTheme[[7]] = element_blank() # Numbers in the X axys
        myTheme[[8]] = element_blank() # Numbers in the Y axys
      }

      if(themeName == "blank"){
        myTheme[[1]] = element_blank()
        myTheme[[2]] = element_blank()
        myTheme[[3]] = element_blank()
        myTheme[[4]] = element_blank()
        myTheme[[5]] = "none"
        myTheme[[6]] = element_blank() 
        myTheme[[7]] = element_blank() 
        myTheme[[8]] = element_blank() 
        
      }
      
      if(themeName == "ggplot"){
        myTheme[[1]] = element_rect(colour = "grey")   # 1: Background color
        myTheme[[2]] = element_blank()                 # 2: Axys X and Y Line color
        myTheme[[3]] = element_line(colour = "white")  # 3: Axys Y , major breaks
        myTheme[[4]] = element_line(colour = "white")  # 4: Axys X , major breaks
        myTheme[[5]] = "right"                         # 5: Legend
      }
      
      if(themeName == "simple"){
        myTheme[[1]] = element_blank()
        myTheme[[2]] = element_line(colour = "black")
        myTheme[[3]] = element_line(colour = "grey70")
        myTheme[[4]] = element_blank()
        myTheme[[5]] = "none"
      }
      
    }
    
    return(myTheme)
    
  }
  
  
  # TODO: Unifiy getImageWidth for all plots here
  getImageWidth <- function(){
    
    print("sss")
    
  }
  
}

# ------------------------------------------------
# TOOLS FOR PLOTTING
# ------------------------------------------------
{
  # This section contain functions, that have no ggplot2 related code inside,
  # which are use one way or another to create plots.
  
  # Create a layout in the shape of circles, where each circle is a modality of a
  # categorical variable. This type of layout is not implemented in the igraph
  # package, so I did this myself.
  #
  # The function will create a summary of the categorical variable. This summary
  # is later returned to the user. The circles are not sorted in any particular
  # order and it will respect the factor order of that category. You can later on
  # check the order by which the circles were drawn by just reading this summary
  # which is returned by the function.
  #
  # The first circle is drawn at 0 radians, and goes counterclock wise and
  # equally spaced from each other until you reach all the categories.
  #
  # edgesDF: The dataframe with the edges
  #
  # nodesDF: The dataframe with the nodes
  #
  # categoricalIndex: Index of the column nodesDF with the categorical variable by
  #                   which you want to create the layout.
  #
  # directedGraph:    Whether or not you want the edges to have a pointing arrow
  #                   with the directed information. Default is FALSE.
  #
  # Return the layout [1]
  # And the summary so you can know the categories order [2]
  createCategoricalLayout <- function(edgesDF, nodesDF, categoricalIndex, directedGraph = FALSE){
    
    # Prepare a random layout so we have the datastructure ready
    {
      myGraph = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedGraph)
      myConstantLayoutA = create_layout(graph = myGraph, layout = "mds")
      myConstantLayoutB = subset(myConstantLayoutA, name %in% myConstantLayoutA$name, x:y)    
    }
    
    # Get all the categories stats
    {
      myCurrentSummary        = summarizeCategorical(nodesDF, categoricalIndex, sorted="none")
      uniqueCategories        = myCurrentSummary[,1]
      totalUniques            = length(uniqueCategories)          
    }
    
    # Prepare the vectors where we are going to put the first centers
    {
      centroidsX     = rep(0,totalUniques)
      centroidsY     = rep(0,totalUniques)
      radianDistance = 0
      radianChunks   = 0
      if(totalUniques>1){
        radianDistance = (2*pi)/totalUniques
        radianChunks   = seq(0,(totalUniques-1)) * radianDistance
      }
      minX  = min(myConstantLayoutB$x)
      maxX  = max(myConstantLayoutB$x)
      minY  = min(myConstantLayoutB$y)
      maxY  = max(myConstantLayoutB$y)
      centerX = (maxX + minX)/2
      centerY = (maxY + minY)/2
      diffX   = maxX - minX
      firstCircleRadious  = diffX/4               # Radious distance is arbitrary, it just looks nice with /4
      
      centroidsX = centerX + cos(radianChunks) * firstCircleRadious
      centroidsY = centerY + sin(radianChunks) * firstCircleRadious
    }
    
    # For each category, find the coordinates for each node
    {
      # -- The coordinates goes into this matrixes
      coordinatesX = matrix(NA, nrow = totalUniques, ncol = max(myCurrentSummary[,2]))
      coordinatesY = matrix(NA, nrow = totalUniques, ncol = max(myCurrentSummary[,2]))
      # -- The radious of each individual circle
      secondCircleRadious  = firstCircleRadious
      if(totalUniques>1) secondCircleRadious = sqrt((centroidsX[1]-centroidsX[2])^2 +
                                                      (centroidsY[1]-centroidsY[2])^2)/4 # /1 overlap
      # / 2 touch each other
      # / 4 there is a space in between 
      
      # -- Fill each row
      for (i in 1:totalUniques) {
        
        # Get how many are in this particular modality
        totalCurrentModality = myCurrentSummary[i,2]
        
        # Create the two vectors where you are going to write this
        minicentroidsX     = rep(0,totalCurrentModality)
        minicentroidsY     = rep(0,totalCurrentModality)
        radianDistance = 0
        radianChunks   = 0
        if(totalCurrentModality>1){
          radianDistance = (2*pi)/totalCurrentModality
          radianChunks   = seq(0,(totalCurrentModality-1)) * radianDistance
        }
        
        minicentroidsX = centroidsX[i] + cos(radianChunks) * secondCircleRadious
        minicentroidsY = centroidsY[i] + sin(radianChunks) * secondCircleRadious
        
        # Do this one by one , since R doesn't want to relpace the whole row ¬¬
        for(j in 1:totalCurrentModality){
          
          coordinatesX[i,j] = minicentroidsX[j]
          coordinatesY[i,j] = minicentroidsY[j]
          
        }
        
      }      
    }
    
    # Change the x,y coordinate of each node in the layout
    {
      myIndexes = rep(1,totalUniques)
      
      for (i in 1:nrow(myConstantLayoutB)) {
        
        # Get the category
        currentCategory = nodesDF[i,categoricalIndex]
        
        # Grab the index of this category      
        currentIndex = which(currentCategory == uniqueCategories)
        
        # Assign that corresponding coordinates
        myConstantLayoutB$x[i] = coordinatesX[currentIndex, myIndexes[currentIndex]]
        myConstantLayoutB$y[i] = coordinatesY[currentIndex, myIndexes[currentIndex]]
        
        # Increase the index for that modality
        myIndexes[currentIndex] = myIndexes[currentIndex] + 1
        
        
      }      
    }
    
    myReturn      = vector("list", length = 2)
    myReturn[[1]] = myConstantLayoutB
    myReturn[[2]] = myCurrentSummary
    
    return(myReturn)
    
  }
  
  
  
}

# ------------------------------------------------
# ACTUAL PLOTS
# ------------------------------------------------

  
  # This section contain all the code for generating the actual plots.
  #
  # These are the common rules for ALL the plots
  #
  # (String) plotFilePath: This is the string where you want to save your plot.
  #                        It can be an specific file path, such as:
  #
  #                        "/home/username/Desktop/myPlot.png"
  #
  #                        In this case the function respect the original path
  #                        and save the image in this location exactly.
  #
  #                        Or it could be a specific folder instead: 
  #
  #                        "/home/username/Desktop/"
  #
  #                        In this case, the function will try to the best of it
  #                        habilities to gives an automatic filename which is
  #                        human readable and save it in that folder. This could
  #                        be something like:
  #
  #                        "Barplot_womenOnlyTable_Smoke.png"
  #
  #                        Which means that the file created is a bar plot, that
  #                        is using the dataframe named "womenOnlyTable" and is
  #                        using the "Smoke" variable to create this bar plot.
  #
  # Variables related to the automatic naming system:
  #
  # (String) plotTitle = NULL,
  # plotSubtitle = NULL,
  # plotCaption = NULL,
  # plotXLabel = NULL,
  # plotYLabel = NULL,
  # plotTheme = NULL,
  # overrideTableName = NULL,
  # overrideCaption   = NULL
  #
  # Variables related to the log system:
  #
  # TODO: logFileDescriptor here
  #
  # (bool) supressWarnings. Boolean that indicates whether you want to display
  #                         warnings related to the function or not. Default is
  #                         FALSE. By default it also print them in the console
  #                         Unless you have a file descriptor where you are
  #                         dumping all the warnings to analysize later.
  #
  # Returning variables:
  #
  # The function always return a list with some results. Some plots return a
  # bigger list than other and you need to check the specific information of
  # each function. But as a general rule, this is true for all the functions:
  #
  # (list) returnVector:
  #
  #                     1st element: The ggplot2 object
  #                     2nd element: The image complete filepath
  #                     3rd elment:  The latex filepath with the latex code
  #                                  that can be use to create this image
  #
  #                     ...
  #
  #                     1st from last element: Number of warnings raised
  #                     Last element:          Error code.
  #                                                0 = Everything goes right
  #                                             0 <  = Some warnings happen, but
  #                                                    the function finish right
  #                                             0 >  = Some error happen, and
  #                                                    the function halted. The
  #                                                    rest of the return vector
  #                                                    is probably nonsense or
  #                                                    invalid.
  
    # ---------------------------------
    #     UNIQUE VARIABLE ANALYSIS
    # ---------------------------------
    {
    
    # ---------------------------------
    #     CATEGORICAL ANALYSIS
    # ---------------------------------
    {
      
      # -----------------------------------------------
      #     DESCRIPTIVE STATISTICS (Is there something that is NOT descriptive?)
      #     -- doBarPlot
      #     -- doLongBarPlot
      # -----------------------------------------------
      {
        
        # Do a simple bar plot
        # (ie: Men and Women)
        #
        # This includes whether you have an absolute count plot, or relative
        # count plot.
        # (ie: 120 Men and 80 Women , or, 60% Men and 40% Women)
        #
        # You can also create a piechart plot directly from here. You can select
        # the absolute or relative count no matter whether you are using a bar
        # or piechart.
        #
        # Later on you will find another function that creates a piechart. This
        # is done just for clarity in the code, but that function the only thing
        # that does is call this function with the proper parameters. (TODO)
        #
        # (bool) showNumbers. Whether you want numbers counting how many things
        #                     do you have for each modality to appear in the
        #                     plot. Default is TRUE. It doesn't matter whether
        #                     you do an absolute count or relative count. But
        #                     if you do a relative count, the number will have
        #                     a "%" character next to it. (TODO)
        #
        # (int) cropNumbers.  If modality is too small, it get annoying to have
        #                     the text on top of it. You can choose to crop the
        #                     labels if it falls bellow a certain threshold.
        #                     The default is 0 which means don't crop anything.
        #
        # (string) sort       If you want the plot to have some order
        #                       "descending" default, from high to low,
        #                       "asscending"          from low to high,
        #                       "none"                alphabetical
        #
        # (bool) plotRotate   If you want to flip the X and Y axis. Check
        #                     the longbarplot() function if you want to plot
        #                     a very long barplot; that one is prepared to look
        #                     nicer without height restrictions. Default FALSE.
        #  
        # (float) overrideImageX don't do automatic addjustment of image size
        #                        and use this instead
        #   
        #
        # Numbers appears just under the bar always (RIGHT NOW IS THIS)
        #
        # TODO:
        #
        # (int) minValue.     If a modality count is too low, delete it
        #                     from the plot if total count is strict lower than
        #                     this minimal value. Default is 0, meaning that you
        #                     don't delete anything          
        # crop = Cut the bars after some value
        #
        # top = Take only the top X bars sorted by value count
        #
        # (string) countingType. "count"     / normal bar plot
        #                        "identity" /  relative bar plot
        
        doBarPlot <- function(tableBase, countingIndex, plotFilePath,
                              colorsVector = NULL,
                              countingType = "count",
                              showNumbers  = TRUE,
                              cropNumbers  = 0,
                              minValue     = 0,
                              sort = "descending",
                              plotRotate = FALSE,
                              polarCoordinates = FALSE,
                              plotTitle = NULL, plotSubtitle = NULL,
                              plotCaption = NULL,
                              plotXLabel = NULL, plotYLabel = NULL,
                              plotTheme = NULL,
                              overrideTableName = NULL,
                              overrideCaption   = NULL,
                              logFileDescriptor = NULL,
                              supressWarnings = FALSE,
                              overrideImageWidth  = 0,
                              overrideImageHeight = 0){
          
          # Init variables
          {
            myPlotType = "AbsBarplot"
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            if(countingType == "identity"){
              myPlotType = "RelBarplot"  
            }          
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
          
            # If the filePath is not null, find the final filepath
            # If the filePath is null it means that this plot is part of a composite of another plot
            if(!is.null(plotFilePath)){
              
                myFileExtension = getFileExtension(plotFilePath)
              
                if(myFileExtension == ".png") imageFilePath = plotFilePath
              
                else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                       tableName = myTableName, fileType = myPlotType,
                                                       variableIndex1 = countingIndex)              
            }
            

            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Delete the data that we are not going to use, if any
          if(FALSE){
          if(minValue !=0){
          
              summaryResult      = summarizeCategorical(tableBase,countingIndex)
              modalitiesToDelete = summaryResult[summaryResult$Count<minValue,1]
              totalToDelete      = length(totalToDelete)
              
              for (i in 1:totalToDelete) {
              
                  tableBase = tableBase[tableBase[,countingIndex] != modalitiesToDelete[i],]
                      
              }
              
          }
          }
          
          # Get info about different categories
          myCategories = unique(tableBase[,countingIndex])
          nCategories  = length(myCategories)
          groupingName = colnames(tableBase)[countingIndex]
          imageWidth   = max(5, nCategories * 2)  # Minimum 5 , so we can at least see the legend if there are too little categories
                                                  # It sound counter intuitive that the minimum is found by the max(), but trust me.
          imageHeight  = 8                        # Default image height, usually 8 is perfect in all cases.
          
          
          # If you try to do a barplot with way too many categories, make a warning about it
          if(nCategories > 16){
            
            if(supressWarnings == FALSE){
              
              warningString = ""
              warningString = paste0(warningString , " ---------------"  , "\n")
              warningString = paste0(warningString , " -- WARNING!! --"  , "\n")
              warningString = paste0(warningString , " ---------------"  , "\n")
              warningString = paste0(warningString , ""                  , "\n")
              warningString = paste0(warningString , " You are trying to do a bar plot with more than 16 categories. Are you sure? "       , "\n")
              warningString = paste0(warningString , " Notice that you have the function 'doLongBarPlot()' which will be a better option." , "\n")
              warningString = paste0(warningString , ""                  , "\n")
              
            }
            
            if(is.null(logFileDescriptor)){
              print(warningString)
            }
            else{
              print("No log!")
            }
            

          }
          
          # Prepare the defaults
          {
            defaultVector = getCategoricalDefaults(groupingName, myCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
          }
          
          # Generate an empty ggplot2 object
          myBarPlot = NA
          
          # You can either count all the element one by one (classic)
          if(countingType == "count"){
            
            # Init the plot 
            #myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,countingIndex]))
            myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex]))
            
            # Create bars
            # Give it a sorting order
            if(sort == "ascending")
                myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge(), colour="black")
            if(sort == "descending")
                myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_infreq(tableBase[,countingIndex])),          position=position_dodge(), colour="black")
            if(sort == "none")
                myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_rev(tableBase[,countingIndex])),             position=position_dodge(), colour="black")        
                
            # Write the text in the bar
            # This change slightly on whether you are rotated or not
            if(plotRotate == FALSE)
                myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), vjust = 1.5, color = "white", fontface = "bold")
            else{
                myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), hjust = 1.3, vjust = 0.5, color = "white", fontface = "bold")    
            }
                
            
              # Create bars
              #geom_bar(stat = "count", position = position_dodge(), colour="black") +
              #scale_fill_manual(values = colorsVector, na.value = COLOR_NA) +
              
              
              
            
            
            
          }
          # Or have a table that is already summarized
          else{
            
            # Count the percentages
            currentSummary         = summarizeCategorical(tableBase, countingIndex, sorted="none")
            
            # Make the labels
            currentSummary$Label   = ""
            # -- If you don't want to crop any number, do the default 
            if(cropNumbers <= 0){
              currentSummary$Label   = paste0( round(currentSummary[,3]*100,2), "%")  
            }
            # -- Otherwise, check one by one, label only those above the given
            #    threshold.
            else{
              
              for (i in 1:nrow(currentSummary)) {
            
                if(currentSummary[i,3] >= cropNumbers){
                  currentSummary$Label[i] = paste0( round(currentSummary[i,3]*100,2), "%")    
                }
                
              }
              
            }
            
            currentSummary$Label_Y = cumsum(currentSummary[,3])
            
            # Init the plot 
            myBarPlot = ggplot(data = currentSummary, aes(x = 0, y = currentSummary[,3], fill = currentSummary[,1])) + 
              scale_fill_manual(values = colorsVector, na.value = COLOR_NA) +
              geom_bar(stat = "identity", color = "black") +
              
              geom_shadowtext(aes(label = Label), position = position_stack(vjust = 0.5)) +                 # Text on center
              # geom_shadowtext(aes(label = currentSummary[,4], y = currentSummary[,5]), color = "white", vjust = 1.5) + # Text on top
              scale_x_continuous(expand = c(0,0)) + 
              scale_y_continuous(labels = scales::percent) + 
              scale_fill_manual(values = colorsVector, na.value = COLOR_NA)
            
            
          }
          
          # Add the common parts for both options,
          # but both change slightly if the plot is rotated or not
          {

                            
              myBarPlot = myBarPlot +
              
              # Create titles and subtitles
              labs(title    = plotTitle,
                   subtitle = plotSubtitle,
                   caption  = plotCaption,
                   fill     = groupingName,
                   x = plotXLabel, y = plotYLabel)
                            
              if(plotRotate == FALSE){
                  
                  myBarPlot = myBarPlot +
                                  
                  # Apply the theme
                  theme(panel.background   = themeData[[1]],
                        axis.line          = themeData[[2]],
                        panel.grid.major.y = themeData[[3]],
                        panel.grid.major.x = themeData[[4]],
                        legend.position    = themeData[[5]],
                        axis.ticks         = themeData[[6]],
                        axis.text.x        = themeData[[7]],
                        axis.text.y        = themeData[[8]]) 
                  
              }
              else{
                  
                  myBarPlot = myBarPlot +
                                  
                  # Apply the theme
                  theme(panel.background   = themeData[[1]],
                        axis.line          = themeData[[2]],
                        panel.grid.major.y = themeData[[4]],
                        panel.grid.major.x = themeData[[3]],
                        legend.position    = themeData[[5]],
                        axis.ticks         = themeData[[6]],
                        axis.text.x        = themeData[[7]],
                        axis.text.y        = themeData[[8]]) 
                  
                  
               }


          }
          
          # Transform into a pie chart if you want to
          if(polarCoordinates == TRUE){
            
            myBarPlot =  myBarPlot + coord_polar("y", start=0)
            myBarPlot =  myBarPlot + theme(axis.text.x  = element_blank(),
                                           axis.text.y  = element_blank(),
                                           axis.line.x  = element_blank(),
                                           axis.line.y  = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.title.y = element_blank(),
                                           panel.border = element_blank(),
                                           axis.ticks   = element_blank(),
            )
            
          }
          
          # Turn the plot around if needed
          if(plotRotate == TRUE)  myBarPlot = myBarPlot + coord_flip()
          
          
          # Save the image and the txt files with the data
          # -- Check if the user want to override dimensions
          if(overrideImageWidth>0)  imageWidth  = overrideImageWidth
          if(overrideImageHeight>0) imageHeight = overrideImageHeight
          
          #latexFilePath = ""
          if(imageFilePath!=""){
            ggsave(imageFilePath, plot = myBarPlot, width = imageWidth, height = imageHeight)  
            #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)            
          }
          

          
          myReturn = vector("list", length = 2)
          myReturn[[1]] = myBarPlot
          myReturn[[2]] = imageFilePath
          #myReturn[[3]] = latexFilePath
          
          return (myReturn)
          
        }
        
        
        # Do a simple bar plot for variables with maaaaany categories
        # It doesn't apply any color and the labels are rotated 90º
        # Warning, no limit for image width/height
        #
        # crop = Cut the bars after some value
        #
        # top = Take only the top X bars sorted by value
        # 
        doLongBarPlot <- function(tableBase, countingIndex, plotFilePath,
                                  colorsVector = NULL,
                                  countingType = "count",
                                  plotTitle = NULL, plotSubtitle = NULL,
                                  plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                  plotTheme = NULL,
                                  barsFontSize = 2,
                                  overrideTableName = NULL,
                                  overrideCaption   = NULL,
                                  crop = 0, top = 0, sort = "descending",
                                  overrideHeigh = NULL){
          
          # Init variables
          {
            myPlotType = "LongAbsBarplot"
            if(countingType == "identity") myPlotType = "LongRelBarplot"
            
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = countingIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Get info about different categories
          myCategories = unique(tableBase[,countingIndex])
          nCategories  = length(myCategories)
          groupingName = colnames(tableBase)[countingIndex]
          
          # Prepare the defaults
          {
            defaultVector = getCategoricalDefaults(groupingName, myCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
          }
          
          # Crop the values if needed
          if(top > 0){
            
            mySummary = summarizeCategorical(tableBase, countingIndex, sorted = "top", crop = top)
            filterTheseCatOnly = mySummary[,1]
            tableBase = tableBase[(tableBase[,countingIndex] %in% filterTheseCatOnly),]
            
          }
          
          # Do the plot
          myPlot = ggplot(tableBase, aes(tableBase[,countingIndex]))
            
          # Give it a sorting order
          if(sort == "descending")
            myPlot = myPlot + geom_bar( aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge())
          if(sort == "asscending")
            myPlot = myPlot + geom_bar( aes(x = (fct_infreq(tableBase[,countingIndex]))), position=position_dodge())
          if(sort == "none")
            myPlot = myPlot + geom_bar( aes(x = fct_rev(tableBase[,countingIndex])), position=position_dodge())
            
          # Add the rest of the things
          myPlot = myPlot +
            # Create bars
            #geom_bar( aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge()) +
            
            # Write the text in the bar
            #geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
            
            #geom_text(stat='count', aes(label=..count..), position = position_dodge(0.9), vjust = 1.5, color = "white") +
            
            geom_shadowtext(stat='count', aes(label=..count..), position = position_dodge(0.9), hjust = 1.5, color = "white", fontface = "bold", size = barsFontSize) +
            #geom_shadowtext(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold", size = 2) +
            
            # Write the text in the bar
            #geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
            #geom_shadowtext(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
            
            #Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel,
                 
                 axis.text.x=element_text(size=rel(0.3))
                 
                 #axis.text.x=element_text(margin = margin(t = 0))
                 
            ) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]]) + 
            
            # Turn it around for easy reading
            coord_flip()
          
          # Save the image and the txt files with the data
          imageHeigh   = nCategories/5
          if(!is.null(overrideHeigh)) imageHeigh = overrideHeigh
          ggsave(imageFilePath, height = imageHeigh, plot = myPlot, limitsize = FALSE)
          latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          myReturn = vector("list", length = 3)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imageFilePath
          myReturn[[3]] = latexFilePath
          
          return (myReturn)
          
        }
        
      }
      
    }
    
    # -----------------------------------------------
    #     NUMERICAL ANALYSIS
    # -----------------------------------------------
    {
      
      # -----------------------------------------------
      #     HISTOGRAMS
      # -----------------------------------------------
      {
        doHistogramPlot2 <- function(tableBase, variableIndex, plotFilePath,
                                     colorsVector = NULL,
                                     
                                     totalBins = NULL, binsWidth = NULL, oneTickPerBin = TRUE,
                                     
                                     writeValues    = TRUE,
                                     binFontSize    = NULL,
                                     binBorderColor = NULL,
                                     
                                     normalizeYMaximum = NULL, normalizeXMaximum = NULL,
                                     
                                     plotTitle = NULL, plotSubtitle = NULL,
                                     plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                     plotTheme = NULL,
                                     
                                     titleFontSize = NULL,
                                     axysFontSize = NULL,
                                     
                                     generalFontSize = NULL,
                                     
                                     generalMargins = NULL,
                                     titleMargins = NULL,
                                     
                                     overrideTableName = NULL,
                                     overrideCaption   = NULL){
          
          # Init variables
          {
            
            # Plot type
            myPlotType = "Histogram"
            
            # Table name
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            # Variable name
            numericalName = colnames(tableBase)[variableIndex]
            
            # Bins variables
            {
              myTotalBins  = -1
              myBinsWidth  = -1
              
              minimumValue = min(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
              maximumValue = max(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
              deltaValue   = maximumValue - minimumValue
              
              upperValue   = max(maximumInteger(tableBase, variableIndex, variableIndex))
              lowerValue   = 0
              delta2Value  = upperValue - lowerValue 
            }
            
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, fileType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
            
            # Correct for NULLs
            if(plotTitle    == "") plotTitle    = NULL
            if(plotSubtitle == "") plotSubtitle = NULL
            if(plotCaption  == "") plotCaption  = NULL
            if(plotXLabel   == "") plotXLabel   = NULL
            if(plotYLabel   == "") plotYLabel   = NULL
            
            # Set the proper border for the bin color
            if(is.null(binBorderColor)) binBorderColor = "grey"
            
            
          }
          
          # What to do with the number of bins, and wide of each bin
          {
            # ---- If neither of then are not initialize
            if(is.null(totalBins) && is.null(binsWidth)){
              
              myTotalBins  = 30
              myBinsWidth  = deltaValue/myTotalBins
              
            }
            
            # ---- If at least one is initialize
            else{
              
              # ---- If BOTH are init, this is a ERROR from the user
              if(!is.null(totalBins) && !is.null(binsWidth)){
                
                print("----")
                print("WARNING!: In the function doing the histogram")
                print("----")
                print("You have choosen to initialize both the number of bins, and how wid each bin is.")
                print("I must choose only one of the two given values.")
                print("I default to the number of bins.")
                print("Please correct this in your code so you don't see this message again.")
                print("----")
                
                myTotalBins  = 30
                myBinsWidth  = deltaValue/myTotalBins
                
              }
              
              else{
                
                # ---- If only total number of bins is init.
                if(!is.null(totalBins)){
                  
                  myTotalBins = totalBins
                  myBinsWidth = deltaValue/myTotalBins
                  
                }
                
                else{
                  
                  # ---- If only bin width is init.
                  
                  myBinsWidth  = binsWidth
                  myTotalBins  = ceiling(deltaValue/myBinsWidth) + 1
                  
                }
                
              }
              
            }
            
          }
          
          # Figure it out special numbers of the plot
          {
            
            # Where to put the breaks
            xBreaksEvery = myBinsWidth
            
            # The coordinates where to pain the plot
            expandXPercent = 0.1
            leftLimit     = minimumValue - (deltaValue * expandXPercent)
            rightLimit    = maximumValue + (deltaValue * expandXPercent)
            
            expandYPercent = 0.1
            upperLimit    = upperValue + (delta2Value * expandYPercent)
            lowerLimit    = lowerValue # - (delta2Value * expandYPercent)
            
            if(!is.null(normalizeXMaximum)) rightLimit    = normalizeXMaximum
            if(!is.null(normalizeYMaximum)) upperLimit    = normalizeYMaximum
            
          }
          
          # Do the plot
          {
            myPlot = ggplot(data=tableBase, aes(tableBase[,variableIndex])) +
              
              # Do the histogram
              geom_histogram(binwidth = binsWidth, fill=colorsVector[1], col=binBorderColor) +

              # Limit the plot to the maximum and minimum
              coord_cartesian(xlim=c(leftLimit,rightLimit), ylim=c(lowerLimit,upperLimit))
              
            # Check if you are a number or a date
            if(isDate(tableBase[,variableIndex])){
              myPlot = myPlot + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                                             date_labels = "%B")
            }
            else{
              # Add each of the individual breaks
              myPlot = myPlot + scale_x_continuous(breaks = seq(minimumValue , maximumValue , xBreaksEvery))              
            }
            
            myPlot = myPlot + 

              # Create titles and subtitles
              labs(title    = plotTitle,
                   subtitle = plotSubtitle,
                   caption  = plotCaption,
                   x = plotXLabel, y = plotYLabel) +
              
              # Apply the theme
              theme(panel.background   = themeData[[1]],
                    axis.line          = themeData[[2]],
                    panel.grid.major.y = themeData[[3]],
                    panel.grid.major.x = themeData[[4]],
                    
              )
            
            # If you want to write the values on top of each bar (DEFAULT)
            if(writeValues == TRUE){
              
              # With black text on top of each column
              # If you want to specify font size
              if(is.null(binFontSize)){
                myPlot = myPlot + stat_bin(aes(y=..count.., label=..count..), binwidth = binsWidth, geom="text", vjust=-.5)  
              }
              else{
                myPlot = myPlot + stat_bin(aes(y=..count.., label=..count..), binwidth = binsWidth, geom="text", vjust=-.5, size = binFontSize)  
              }
              
            }
            
            # If you want to change the title font size
            if(!is.null(titleFontSize)){
              
              myPlot = myPlot + theme(plot.title = element_text(size = titleFontSize))  
              
            }
            
            # If you want to change the axys font size
            if(!is.null(axysFontSize)){
              
              myPlot = myPlot + theme(axis.text  = element_text(size = axysFontSize)  )  
              
            }
            
            # If you want to override ALL font sizes and declare a general font size
            if(!is.null(generalFontSize)){
              
              myPlot = myPlot + theme(text = element_text(size = generalFontSize))  
              
            }
            
            # If you want to give special margins
            if(!is.null(generalMargins)){
              
              myPlot = myPlot + theme(plot.margin = unit(generalMargins, "cm"))  
              
            }
            
            # If you want different margins around the title
            if(!is.null(titleMargins)){
              
              myPlot = myPlot + theme(plot.title       = element_text(margin = margin(titleMargins) ) )  
              myPlot = myPlot + theme(axis.title.y     = element_text(margin = margin(0,0,0,0)) )   
              myPlot = myPlot + theme(plot.background  = element_rect(fill  = "red", size = 0.1 ) )  
              myPlot = myPlot + theme(panel.background = element_rect(fill = "blue", size = 1 ) )  
              myPlot = myPlot + theme(plot.title.       = element_rect(fill = "yellow", size = 1 ) )  
              #myPlot = myPlot + theme(plot.margin      = margin(0.1, 0.2, 0.5, 2, "cm") )  
              
            }
            
          }
          
          # ---- Save the image
          
          imageWidth = ceiling(myTotalBins/2)
          ggsave(imageFilePath, plot = myPlot, width = imageWidth)  
          
          myReturn = vector("list", length = 2)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imageFilePath
          
          return(myReturn)
          
        }
        
      }
      
      # -----------------------------------------------
      #     DENSITY
      # -----------------------------------------------
      {
        
        doDensityPlot <- function(tableBase, variableIndex, plotFilePath,
                                  colorsVector = NULL,
                                  borderPlot = FALSE, rotatePlot = FALSE,
                                  plotTitle = NULL, plotSubtitle = NULL,
                                  plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                  plotTheme = NULL,
                                  overrideTableName = NULL,
                                  overrideCaption   = NULL){
          
          # Init variables
          {
            
            # Plot type
            myPlotType = "Density"
            
            # Table name
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            # Variable name
            numericalName = colnames(tableBase)[variableIndex]
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Get info about the variable
          numericalName = colnames(tableBase)[variableIndex]
          
          # Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, fileType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
            
          }
          
          # Do the plot
          densityPlot = ggplot(data = tableBase, aes(x = tableBase[,variableIndex], fill = colorsVector)) +
            
            # Add the density plot
            geom_density(alpha=0.8) +
            scale_fill_manual(values=colorsVector) +
            
            # Create titles and subtitles
            labs(title           = plotTitle,
                 subtitle        = plotSubtitle,
                 caption         = plotCaption,
                 x = plotXLabel, y = plotYLabel) +
            
            theme(legend.position="none") +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]])
          
          # Something about the border
          if(borderPlot == TRUE)
            densityPlot = densityPlot + border()
          
          # Rotate the plot if asked
          if(rotatePlot == TRUE )
            densityPlot = densityPlot + coord_flip()
          
          
          # Save the image
          imageWidth    = 8
          ggsave(imageFilePath, plot = densityPlot, width = imageWidth)  
          latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          
          myReturn = vector("list", length = 3)
          myReturn[[1]] = densityPlot
          myReturn[[2]] = imageFilePath
          myReturn[[3]] = latexFilePath
          
          return(myReturn)
          
        }
        
        
      }
      
      # -----------------------------------------------
      #     QQ
      # -----------------------------------------------
      {
        
        doQQPlot <- function(tableBase, variableIndex, plotFilePath,
                             colorsVector = NULL,
                             plotTitle = NULL, plotSubtitle = NULL,
                             plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                             plotTheme = NULL,
                             overrideTableName = NULL,
                             overrideCaption   = NULL){
          
          # Init variables
          {
            
            # Plot type
            myPlotType = "QQ"
            
            # Table name
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            # Variable name
            numericalName = colnames(tableBase)[variableIndex]
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, fileType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
            
          }
          
          # Run the normality test
          # (Shapiro-Wilk’s test., p-value > 0.05 => NORMALITY)
          # First we need to check if all the values are the same
          # In that case is not normally distributed
          normalValue   = 0
          areAllEqual   = sd(tableBase[,variableIndex], na.rm = TRUE)
          if(areAllEqual != 0)
            normalValue   = shapiro.test(tableBase[,variableIndex])$p.value
          
          plotLabelNormalValue = round(normalValue,2)
          
          # Do the plot
          myPlot = ggplot(tableBase, aes(sample = tableBase[,variableIndex] )) +
            
            # Add the qq related things  
            geom_qq_band(bandType = "pointwise", alpha = 0.3) +
            stat_qq_line(color="blue") +
            stat_qq_point() +
            
            # Add the normality text
            geom_text(aes(x=-Inf,y=Inf,hjust=0,vjust=1, label=plotLabelNormalValue), color="red", parse = TRUE) +
            
            # Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 color    = numericalName,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]])
          
          # ---- Save the image
          imageWidth = 8
          ggsave(imageFilePath, plot = myPlot, width = imageWidth)  
          latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          # ---- Return
          myReturn = vector("list", length = 3)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imageFilePath
          myReturn[[3]] = latexFilePath
          
          return(myReturn)
          
        }
        
      }
      
      # -----------------------------------------------
      #     BOXPLOT
      # -----------------------------------------------
      {
        doBoxPlot <- function(tableBase, variableIndex, plotFilePath,
                              outlierShape = 19,
                              colorsVector = NULL, showPValues = TRUE,
                              ymin = NULL, ymax = NULL,
                              plotTitle = NULL, plotSubtitle = NULL,
                              plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                              plotTheme = NULL,
                              overrideTableName = NULL,
                              overrideCaption   = NULL){
          
          # Define plot type
          myPlotType  = "Boxplot"
          myTableName = deparse(substitute(tableBase))
          
          # If you need to override your table name, then do it
          if(!is.null(overrideTableName)) myTableName = overrideTableName
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Prepare the Y axis limits
          if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
          if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)
          
          # Get info about different categories (there are none)
          groupingName = ""
          
          # Get info about the variable that is to be plotted
          numericalName = colnames(tableBase)[variableIndex]          
          
          # Count the number of characters in the title
          maximumTitlechars = nchar(plotTitle)
          
          # Set the imageWidth
          imageWidth = maximumTitlechars * 0.1
          
          # Init the plot object
          myBoxPlot               = NA
          
          # Init the centralities
          centralitiesDF           =  data.frame(matrix(NA, nrow = 1, ncol = 3))
          colnames(centralitiesDF) = c("ID", "Mean", "Median")
          centralitiesDF[,1]       = numericalName
          centralitiesDF[,2]       = mean(   tableBase[,variableIndex], na.rm = TRUE)
          centralitiesDF[,3]       = median( tableBase[,variableIndex], na.rm = TRUE)
          
          # Finally, do the actual plot
          # ---- Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName,
                                                 colorsVector = colorsVector, plotTitle   = plotTitle,
                                                 plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                 plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel)
            
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
          }
          
          # Do the plot
          myTempTableBase = tableBase
          myTempTableBase$Unique = numericalName
          colnames(myTempTableBase)[ncol(myTempTableBase)] = numericalName
          
          
          myBoxPlot = ggplot(data = tableBase, aes(x = myTempTableBase[,ncol(myTempTableBase)], y = myTempTableBase[,variableIndex], fill = myTempTableBase[,ncol(myTempTableBase)])) +
            
            # ---- Boxplot
            geom_boxplot(outlier.shape = outlierShape) +
            scale_fill_manual(values=colorsVector) +
            
            # ---- With tiny points for the samples
            geom_jitter(position = position_jitterdodge(), color="black", size=0.2, alpha=0.1) +
            
            # ---- Apply the Y limits
            scale_y_continuous(limits=c(ymin, ymax)) +
            
            # ---- Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]]) +
            
            # Override the theme and delete the name for the made up categorical
            theme(axis.text.x = element_blank())
          
          # Save everything
          ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth)
          #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          # Prepare the return vector
          myReturn = vector("list", length = 3)
          myReturn[[1]] = myBoxPlot
          myReturn[[2]] = centralitiesDF
          myReturn[[3]] = imageFilePath
          #myReturn[[4]] = latexFilePath
          
          return (myReturn)
          
        }
        
      }
      
      
      
      
    }
    
  }
  
    # ---------------------------------
    #     MULTIPLE VARIABLE ANALYSIS
    # ---------------------------------
    {
        
        # ---------------------------------
        #     TWO CATEGORICAL ANALYSIS
        # ---------------------------------
        {
         
            # -----------------------------------------------
            #     BAR PLOTS
            #
            #       -- Stacked relative bar plot
            #       -- Horizontal Stacked relative bar plot
            #       -- Horizontal absolute bar plot
            # -----------------------------------------------
            {
     
      # Do a combine bar plot
      # With bar stacked up to relative total
      # colorsVector should be the same as the groupIndex if you want it to make sense
      doBarRelativeCombinePlot <- function(tableBase, countingIndex, groupIndex, plotFilePath,
                                           colorsVector = NULL,
                                           plotTitle = NULL, plotSubtitle = NULL,
                                           plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                           plotTheme = NULL,
                                           overrideTableName = NULL,
                                           overrideCaption   = NULL,
                                           supressWarnings   = FALSE){

        
        # Init variables
        {
          
          # Plot type
          myPlotType = "CombinedRelBarplot"
          
          # Table name
          myTableName = deparse(substitute(tableBase))
          
          if(!is.null(overrideTableName)){
            myTableName = overrideTableName
            
          }
          
        }
        
        # Get an automatic imagePath name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = countingIndex,
                                                 variableIndex2 = groupIndex)
          
        }
        
        # Get the theme information
        themeData = getThemeParameters(plotTheme)
        
        # Convert the numerical in categories if needed
        countingVariableType = class(tableBase[,countingIndex])
        
        if(countingVariableType != "character" && countingVariableType != "factor") {
          
          # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
          myLevels = sort(unique(tableBase[,countingIndex]))
          myLevels = as.character(myLevels)
          
          tableBase[,countingIndex] = as.character(tableBase[,countingIndex])
          tableBase[,countingIndex] = factor(tableBase[,countingIndex], levels = myLevels)
          
          # Sometimes you know that you are casting categories on porpoise
          # So don't show me warnings if I say so
          if(supressWarnings == FALSE){

            print("WARNING!!")
            print("Doing the relative barplot for:")
            print(myTableName)
            print("With indexes")
            print(countingIndex)
            print(groupIndex)
            print("I found a numerical variable")
            print("I transformed into categorical automatically and did the plot anyway")
            print("Maybe you wanted to do something else?")
            
          }
          
        }

        # Get info about different categories
        {
          myCategoriesA = unique(tableBase[,countingIndex])
          nCategoriesA  = length(myCategoriesA)
          groupingNameA = colnames(tableBase)[countingIndex]
          
          myCategoriesB = unique(tableBase[,groupIndex])
          nCategoriesB  = length(myCategoriesB)
          groupingNameB = colnames(tableBase)[groupIndex]
        }

        # Prepare the defaults
        {

          defaultVector = getBiCategoricalDefaults(groupingNameB, myCategoriesB, 
                                                   groupingNameA, myCategoriesA, 
                                                   colorsVector = colorsVector,
                                                   plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                   plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                   plotYLabel = plotYLabel, plotType = myPlotType)
          
          
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # Transform into relative table
        # Mutate the table to do the cumulative sum easier
        # (I hate you dplyr!)
        {
          
          totalRows     = nrow(tableBase)
          
          copyTable   = data.frame(matrix(NA, nrow = totalRows, ncol = 2), stringsAsFactors = TRUE)
          
          colnames(copyTable) = c("G","C")
          copyTable$G = tableBase[,groupIndex]
          copyTable$C = tableBase[,countingIndex]
          
          proportionTable <-
            copyTable %>%
            group_by(G, C) %>%
            tally() %>%
            group_by(G) %>%
            mutate(pct = n / sum(n))
          
          proportionTable <- proportionTable %>%
            group_by(G) %>%
            mutate(label_y = cumsum(pct))
        }

        # Delete labels that are too little and are annoying in the plot
        {
          proportionTable$myLabel = paste(round(proportionTable$pct*100, 2),"%",sep = '')
          proportionTable$myLabel[proportionTable$pct < 0.05] = ""        
        }

        # Do the plot
        {
          
        
        myPlot = ggplot(proportionTable, aes(  x = G , y = pct , fill = fct_rev(proportionTable[,2][[1]]))) +
          
          geom_bar(stat = "identity", color = "black") +
          
          scale_fill_manual(values = rev(colorsVector)) +
          
          geom_shadowtext(aes(label = myLabel, y = label_y), vjust = 1.5, color = "white") +
          
          scale_y_continuous(labels = scales::percent) +
           
          # I'm trying to force all labels here, but I have no idea how     
          #scale_x_discrete(G, labels = G, breaks = G) + 
                

          # Create titles and subtitles
          labs(title    = plotTitle,
               subtitle = plotSubtitle,
               caption  = plotCaption,
               fill     = groupingNameA,
               x = plotXLabel, y = plotYLabel) +
          
          # Apply the theme
          theme(panel.background   = themeData[[1]],
                axis.line          = themeData[[2]],
                panel.grid.major.y = themeData[[3]],
                panel.grid.major.x = themeData[[4]],
                legend.position    = themeData[[5]])
        
        }
        
        # Save the image
        imageWidth    = max(5, nCategoriesB * 2)
        ggsave(imageFilePath, plot = myPlot, width = imageWidth)  
        # latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)

        myReturn = vector("list", length = 2)
        myReturn[[1]] = myPlot
        myReturn[[2]] = imageFilePath
        #myReturn[[3]] = latexFilePath

        return(myReturn)
        
      }
       
      # Same as before, but in horizontal
      doLongBarRelativeCombinePlot <- function(tableBase, countingIndex, groupIndex, plotFilePath,
                                               barsFontSize = 2,
                                               colorsVector = NULL,
                                               plotTitle = NULL, plotSubtitle = NULL,
                                               plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                               plotTheme = NULL,
                                               overrideTableName = NULL,
                                               overrideCaption   = NULL,
                                               supressWarnings   = FALSE, sort = "descending",
                                               imageHeight = NULL, imageWidth = NULL){
        
        
        # Init variables
        {
          
          # Plot type
          myPlotType = "CombinedRelBarplot"
          
          # Table name
          myTableName = deparse(substitute(tableBase))
          
          if(!is.null(overrideTableName)){
            myTableName = overrideTableName
            
          }
          
        }
        
        # Get an automatic imagePath name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = countingIndex,
                                                 variableIndex2 = groupIndex)
          
        }
        
        # Get the theme information
        themeData = getThemeParameters(plotTheme)
        
        # Convert the numerical in categories if needed
        countingVariableType = class(tableBase[,countingIndex])
        
        if(countingVariableType != "character" && countingVariableType != "factor") {
          
          # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
          myLevels = sort(unique(tableBase[,countingIndex]))
          myLevels = as.character(myLevels)
          
          tableBase[,countingIndex] = as.character(tableBase[,countingIndex])
          tableBase[,countingIndex] = factor(tableBase[,countingIndex], levels = myLevels)
          
          # Sometimes you know that you are casting categories on porpoise
          # So don't show me warnings if I say so
          if(supressWarnings == FALSE){
            
            print("WARNING!!")
            print("Doing the relative barplot for:")
            print(myTableName)
            print("With indexes")
            print(countingIndex)
            print(groupIndex)
            print("I found a numerical variable")
            print("I transformed into categorical automatically and did the plot anyway")
            print("Maybe you wanted to do something else?")
            
          }
          
        }
        
        # Get info about different categories
        {
          
            myCategoriesA = NA
            myCategoriesB = NA  
            
            if(is.factor(tableBase[,countingIndex])){
                myCategoriesA = levels(tableBase[,countingIndex])
            }
            else{
                myCategoriesA = unique(tableBase[,countingIndex])
            }
            
            nCategoriesA  = length(myCategoriesA)
            groupingNameA = colnames(tableBase)[countingIndex]
          
            if(is.factor(tableBase[,groupIndex])){
                myCategoriesB = levels(tableBase[,groupIndex])
            }
            else{
                myCategoriesB = unique(tableBase[,groupIndex])
            }
            
            nCategoriesB  = length(myCategoriesB)
            groupingNameB = colnames(tableBase)[groupIndex]
        }
        
        # Prepare the defaults
        {
          
          defaultVector = getBiCategoricalDefaults(groupingNameB, myCategoriesB, 
                                                   groupingNameA, myCategoriesA, 
                                                   colorsVector = colorsVector,
                                                   plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                   plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                   plotYLabel = plotYLabel, plotType = myPlotType)
          
          
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # Transform into relative table
        # Mutate the table to do the cumulative sum easier
        # (I hate you dplyr!)
        {
          
          totalRows     = nrow(tableBase)
          
          copyTable   = data.frame(matrix(NA, nrow = totalRows, ncol = 2), stringsAsFactors = TRUE)
          
          colnames(copyTable) = c("G","C")
          copyTable$G = tableBase[,groupIndex]
          copyTable$C = tableBase[,countingIndex]
          
          proportionTotalRows = nCategoriesA * nCategoriesB
          proportionTable = DF(proportionTotalRows,6)
          colnames(proportionTable) = c("G","C","n","pct","label_y","myLabel")
          
          currentIndex = 1
          
          preCumTotal  = 0
          cumTotal     = 0
          
          
          for(j in 1:nCategoriesB){
          
                preCumTotal  = 0  
                cumTotal     = 0 
              
                for(i in 1:nCategoriesA){
                    
                    myGCategory = as.character(myCategoriesB[j])
                    myCCategory = as.character(myCategoriesA[i])
                  
                    proportionTable[currentIndex,1] = myGCategory         
                    proportionTable[currentIndex,2] = myCCategory
                
                    currentSubtable    = copyTable[copyTable$G == myGCategory & copyTable$C == myCCategory,]
                    currentConditional = copyTable[copyTable$G == myGCategory,]
                  
                    proportionTable[currentIndex,3] = nrow(currentSubtable)
                    proportionTable[currentIndex,4] = nrow(currentSubtable)/nrow(currentConditional)
                  
                    cumTotal     = cumTotal + nrow(currentSubtable)/nrow(currentConditional)
                  
                    proportionTable[currentIndex,5] = (cumTotal + preCumTotal)/2
                  
                    preCumTotal = cumTotal
                  
                    currentIndex = currentIndex + 1
                  
              }  

          }
          
          # proportionTable <-
          #   copyTable %>%
          #   group_by(G, C) %>%
          #   #tally() %>%
          #   group_by(G) %>%
          #   mutate(pct = n / sum(n))
          # 
          # proportionTable <- proportionTable %>%
          #   group_by(G) %>%
          #   mutate(label_y = cumsum(pct))
          
        }
        
        # Delete labels that are too little and are annoying in the plot
        {
          proportionTable$myLabel = paste(round(proportionTable$pct*100, 2),"%",sep = '')
          proportionTable$myLabel[proportionTable$pct < 0.05] = ""        
        }
        
        # WHY IN FLYING SHIT, DOES NOT DATAFRAMES HAVE A SIMPLE REVERSE OPERATION!???
        # Really, look at this crap. There is no myDataframe.reverseRow()
        # You need to do it the supercomplicated way, do an apply rev,
        # and whatever the shit that function gives you need to be casted to dataframe again
        # FUCK R, and FUCK WHOEVER DECIDED ON THIS SYNTAX!
        
        # calculating reverse
        #proportionTable = as.data.frame(apply(proportionTable, 2, rev))
        
        
        # Do the plot
        {

          #myPlot = ggplot(proportionTable, aes(  x = G , y = pct , fill = fct_rev(proportionTable[,2][[1]])))
          
          
          #myPlot = ggplot(proportionTable, aes(  x = G , y = pct, fill = fct_rev(C)))
          
          myPlot = ggplot(proportionTable, aes(  x = fct_rev(G) , y = pct, fill = C))
            
          
          # Give it a sorting order
          if(sort == "descending")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
                                       aes(x = fct_rev(fct_infreq(proportionTable[,2][[1]]))))
          if(sort == "asscending")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
                                       aes(x = (fct_infreq(proportionTable[,2][[1]]))))
          if(sort == "none")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black")

          # Do the rest
          myPlot = myPlot +
          
            scale_fill_manual(values = rev(colorsVector)) +
            
            geom_shadowtext(aes(label = myLabel, y = label_y), hjust = 0.5, color = "white", fontface = "bold", size = barsFontSize) +

            scale_y_continuous(labels = scales::percent) +
            
            # Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingNameA,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]])
          
        }
        
        # Rotate the plot so the bars are horizontal and is easier
        # to read from top to bottom.
        myPlot = myPlot + coord_flip()
        
        # Save the image
        myImageHeight = nCategoriesB/20
        if(!is.null(imageHeight)) myImageHeight = imageHeight 
        
        myImageWidth  = 8
        if(!is.null(imageWidth))  myImageWidth = imageWidth
        
        ggsave(imageFilePath, width = myImageWidth, height = myImageWidth, plot = myPlot, limitsize = FALSE)
        #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return everything
        myReturn = vector("list", length = 3)
        myReturn[[1]] = myPlot
        myReturn[[2]] = imageFilePath
        #myReturn[[3]] = latexFilePath
        
        return(myReturn)
        
      }
      
      
      
      
                doLongBarAbsoluteCombinePlot <- function(tableBase, xIndex, yIndex, colorIndex,
                                                         plotFilePath,
                                                         barsFontSize = 2,
                                                         colorsVector = NULL,
                                                         plotTitle = NULL, plotSubtitle = NULL,
                                                         plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                                         plotTheme = NULL,
                                                         overrideTableName = NULL,
                                                         overrideCaption   = NULL,
                                                         supressWarnings   = FALSE, sort = "descending",
                                                         imageHeight = NULL, imageWidth = NULL){
          
          
                    # Init variables
                    {
                          
                        # Plot type
                        myPlotType = "CombinedAbsBarplot"
                          
                        # Table name
                        myTableName = deparse(substitute(tableBase))
                          
                        if(!is.null(overrideTableName)) myTableName = overrideTableName
                          
                     }
          
                    # Get an automatic imagePath name if you don't have a proper one
                    {
              
                        imageFilePath = ""
              
                        myFileExtension = getFileExtension(plotFilePath)
              
                        if(myFileExtension == ".png") imageFilePath = plotFilePath
                  
                        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                               tableName = myTableName, fileType = myPlotType,
                                                               variableIndex1 = xIndex,
                                                               variableIndex2 = colorIndex)
              
                    }
          
                    # Get the theme information
                    themeData = getThemeParameters(plotTheme)
          
                    # Convert the numerical in categories if needed
                    countingVariableType = class(tableBase[,xIndex])
          
                    if(countingVariableType != "character" && countingVariableType != "factor") {
              
                        # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
                        myLevels = sort(unique(tableBase[,xIndex]))
                        myLevels = as.character(myLevels)
              
                        tableBase[,xIndex] = as.character(tableBase[,xIndex])
                        tableBase[,xIndex] = factor(tableBase[,xIndex], levels = myLevels)
              
                        # Sometimes you know that you are casting categories on porpoise
                        # So don't show me warnings if I say so
                        if(supressWarnings == FALSE){
                  
                            print("WARNING!!")
                            print("Doing the relative barplot for:")
                            print(myTableName)
                            print("With indexes")
                            print(xIndex)
                            print(colorIndex)
                            print("I found a numerical variable")
                            print("I transformed into categorical automatically and did the plot anyway")
                            print("Maybe you wanted to do something else?")
                  
                        }
              
                    }
          
                    # Get info about different categories
                    {
                          
                        myCategoriesA = NA
                        myCategoriesB = NA  
                          
                        if(is.factor(tableBase[,xIndex])) myCategoriesA = levels(tableBase[,xIndex])
                        else myCategoriesA = unique(tableBase[,xIndex])
                          
                        nCategoriesA  = length(myCategoriesA)
                        groupingNameA = colnames(tableBase)[xIndex]
                          
                        if(is.factor(tableBase[,colorIndex])) myCategoriesB = levels(tableBase[,colorIndex])
                        else myCategoriesB = unique(tableBase[,colorIndex])
                        
                        nCategoriesB  = length(myCategoriesB)
                        groupingNameB = colnames(tableBase)[colorIndex]
                        
                    }
          
                    # Prepare the defaults
                    {
              
                        defaultVector = getBiCategoricalDefaults(groupingNameB, myCategoriesB, 
                                                                 groupingNameA, myCategoriesA, 
                                                                 colorsVector = colorsVector,
                                                                 plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                                 plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                                 plotYLabel = plotYLabel, plotType = myPlotType)
              
                        colorsVector  = defaultVector[[1]]
                        plotTitle     = defaultVector[[2]][1]
                        plotSubtitle  = defaultVector[[3]][1]
                        plotCaption   = defaultVector[[4]][1]
                        plotXLabel    = defaultVector[[5]][1]
                        plotYLabel    = defaultVector[[6]][1]
                        
                    }
          
                    # Create an standard table for the plot
                    copyTable = tableBase
                    colnames(copyTable)[xIndex]     = "myX"
                    colnames(copyTable)[yIndex]     = "myY"
                    colnames(copyTable)[colorIndex] = "myC"
          
                    # Do the plot
                    {
              
                        myPlot = ggplot(copyTable, aes(  x = fct_rev(myX) , y = myY, fill = myC))
              
                        # Give it a sorting order
                        if(sort == "descending")
                            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
                                                       aes(x = fct_rev(fct_infreq(proportionTable[,2][[1]]))))
                        if(sort == "asscending")
                            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
                                                       aes(x = (fct_infreq(proportionTable[,2][[1]]))))
                        if(sort == "none")
                            myPlot = myPlot + geom_bar(stat = "identity", color = "black")
              
                        # Do the rest
                        myPlot = myPlot +
                  
                        scale_fill_manual(values = rev(colorsVector)) +
                  
                        #geom_shadowtext(aes(label = myLabel, y = label_y), hjust = 0.5, color = "white", fontface = "bold", size = barsFontSize) +
                  
                        # Create titles and subtitles
                        labs(title    = plotTitle,
                             subtitle = plotSubtitle,
                             caption  = plotCaption,
                             fill     = groupingNameA,
                             x = plotXLabel, y = plotYLabel) +
                  
                        # Apply the theme
                        theme(panel.background   = themeData[[1]],
                              axis.line          = themeData[[2]],
                              panel.grid.major.y = themeData[[3]],
                              panel.grid.major.x = themeData[[4]],
                              legend.position    = themeData[[5]])
              
                    }
          
                    # Turn the plot 90º
                    myPlot = myPlot + coord_flip()
          
                    # Save the image
                    # -- Set the width and height automatically unless the user says otherwise
                    myImageHeight = nCategoriesB/20
                    if(!is.null(imageHeight)) myImageHeight = imageHeight 
          
                    myImageWidth  = 8
                    if(!is.null(imageWidth))  myImageWidth = imageWidth
          
                    ggsave(imageFilePath, width = myImageWidth, height = myImageWidth, plot = myPlot, limitsize = FALSE)
          
          
                    # Return everything
                    myReturn = vector("list", length = 2)
                    myReturn[[1]] = myPlot
                    myReturn[[2]] = imageFilePath
          
                    return(myReturn)
          
                }
      
      
      
    }
            
               
        }
        
        
        
        
        
        
        # ---------------------------------
        #          DENSITY PLOTS
        # ---------------------------------
        {
            doCategoricalDensityPlot <- function(tableBase, variableIndex, categoryIndex, plotFilePath = NULL,
                                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                                 colorsVector = NULL, plotTheme = NULL,
                                                 borderPlot = FALSE, rotatePlot = FALSE,
                                                 overrideTableName = NULL,
                                                 overrideCaption = NULL,
                                                 imageWidth = 8, imageHeight = 8){
    
                # Init variables
                {
                    myPlotType = "CategoricalDensity"
                    myTableName = deparse(substitute(tableBase))
              
                    if(!is.null(overrideTableName)){
                        myTableName = overrideTableName
                
                    }          
                }
    
                # Get an automatic name if you don't have a proper one
                {
              
                    imageFilePath = ""
              
                    myFileExtension = getFileExtension(plotFilePath)
              
                    if(myFileExtension == ".png") imageFilePath = plotFilePath
              
                    else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                           tableName = myTableName, fileType = myPlotType,
                                                           variableIndex1 = categoryIndex, variableIndex2 = variableIndex)
              
                }
    
                # Get the theme information
                themeData = getThemeParameters(plotTheme)
            
                # Get info about different categories
                {
                    myCategories  = unique(tableBase[,categoryIndex])
                    nCategories   = length(myCategories)
                    groupingName  = colnames(tableBase)[categoryIndex]
                    numericalName = colnames(tableBase)[variableIndex]
                    totalRows     = nrow(tableBase)          
                }
            
                # ---- Prepare the defaults
                {
              
                    defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                                    colorsVector = colorsVector, plotTitle   = plotTitle,
                                                                    plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                                    plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                                    fileType = myPlotType)
              
                    colorsVector  = defaultVector[[1]]
                    plotTitle     = defaultVector[[2]][1]
                    plotSubtitle  = defaultVector[[3]][1]
                    plotCaption   = defaultVector[[4]][1]
                    plotXLabel    = defaultVector[[5]][1]
                    plotYLabel    = defaultVector[[6]][1]
                    
                }
    
                # Do the plot
                {
              
                    densityPlot = ggplot(data = tableBase, aes(x = tableBase[,variableIndex], fill = tableBase[,categoryIndex])) +
                
                    # Add the density plot
                    geom_density(alpha=0.8) +
                    scale_fill_manual(values=colorsVector) +
                
                    # Create titles and subtitles
                    labs(title    = plotTitle,
                         subtitle = plotSubtitle,
                         caption  = plotCaption,
                         color    = groupingName,
                         fill     = groupingName,
                         x = plotXLabel, y = plotYLabel) +
                
                    # Apply the theme
                    theme(panel.background   = themeData[[1]],
                          axis.line          = themeData[[2]],
                          panel.grid.major.y = themeData[[3]],
                          panel.grid.major.x = themeData[[4]],
                          legend.position    = themeData[[5]])
    
                    # Something about the border
                    if(borderPlot == TRUE)
                        densityPlot = densityPlot + border()
              
                    # Rotate the plot if asked
                    if(rotatePlot == TRUE )
                        densityPlot = densityPlot + coord_flip()
              
                }
            
                # Save the image
                ggsave(imageFilePath, plot = densityPlot, width = imageWidth, height = imageWidth)
                #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
            
                # Return
                myReturn = vector("list", length = 3)
                myReturn[[1]] = densityPlot
                myReturn[[2]] = imageFilePath
                #myReturn[[3]] = latexFilePath
            
                return (myReturn)
            
          }  
      
        }
        
        # ---------------------------------
        #          BOX PLOTS
        # ---------------------------------
        {
            
        
        
        # Boxplot with optional p-value under it
        #
        # - variableIndex is the column index with the numerical values
        # - groupIndex    is the column index with the categorical values
        #
        # groupIndex can be NULL. Then, you won have any grouping of course and
        #                         you just have a boxplot of an univariable case
        doCategoricalBoxPlot <- function(tableBase, groupIndex, variableIndex, plotFilePath,
                                         outlierShape = 19,
                                         cutOffLine   = NULL,
                                         colorsVector = NULL, showPValues = TRUE,
                                         ymin = NULL, ymax = NULL,
                                         plotTitle = NULL, plotSubtitle = NULL,
                                         plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                         plotTheme = NULL,
                                         angleXLabels = 0,
                                         overrideImageWidth  = NULL,
                                         overrideImageHeight = NULL,
                                         overrideScaleToZero = FALSE,
                                         overrideTableName = NULL,
                                         overrideCaption   = NULL){

            # Define plot type
            myPlotType  = "CategoricalBoxplot"
            myTableName = deparse(substitute(tableBase))

            # If you need to override your table name, then do it
            if(!is.null(overrideTableName)) myTableName = overrideTableName

            # Get an automatic name if you don't have a proper one
            {

                imageFilePath = ""
          
                myFileExtension = getFileExtension(plotFilePath)
          
                if(myFileExtension == ".png") imageFilePath = plotFilePath
          
                else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                       tableName = myTableName, fileType = myPlotType,
                                                       variableIndex1 = groupIndex, variableIndex2 = variableIndex)

            }

            # Get the theme information
            themeData = getThemeParameters(plotTheme)

            # Prepare the Y axis limits
            if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
            if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)

            # This is going to be the return object, might be empty though
            # "2" signify a p-value that is impossible. Thus marking that value was impossible to find out
            # This might happen because we don't have more than 2 categories, all numbers are NA, all numbers are the same, and so on
            pValuesDF     = NULL

            # Get info about different categories
        
            # ---- Init variables FOR THE NO CATEGORIES - NUMERICAL CASE:
            nCategories   = 1
            groupingName  = NA
            numericalName = colnames(tableBase)[variableIndex]
            myCategories  = c(numericalName)
        
            # ---- For the CATEGORICAL - NUMERICAL CASE:
            if(!is.null(groupIndex)){
          
                myCategories  = unique(tableBase[,groupIndex])
                nCategories   = length(myCategories)
                groupingName  = colnames(tableBase)[groupIndex]
          
                # Gives the X label to grouping name if it exist
                if((str_trim(plotXLabel) != "")&&(!is.null(plotXLabel))){
                    groupingName  = plotXLabel
                }
          
                numericalName = colnames(tableBase)[variableIndex]          
          
            }
            # ---- FOR THE NO CATEGORIES - NUMERICAL CASE:
            #      We still need a column with the unique variable
            else{
         
                newGroupIndex = ncol(tableBase) + 1
                tableBase[,newGroupIndex] = numericalName
                groupIndex = newGroupIndex
           
            }

            # Count the number of characters in the categories in order to decide the image width factor
            maximumCategoryChars = max(nchar(as.character(myCategories)))
            maximumCategoryChars = max(maximumCategoryChars, 8) # Tiny names looks weird, let take 8 as minimum char size

            # Count the number of characters in the title
            maximumTitlechars    = nchar(plotTitle)
        
            # Init the plot object
            myBoxPlot            = NA

            # Init the pValues, even though it could be impossible to do them
            pValuesDF            = DF(nCategories, nCategories + 1)
            colnames(pValuesDF)  = c("ID", as.character(myCategories))
            pValuesDF[,1]        = myCategories

            # Init the centralities, even though it could be impossible to do them
            centralitiesDF           = DF(nCategories, 3)
            colnames(centralitiesDF) = c("ID", "Mean", "Median")
            centralitiesDF[,1]       = myCategories
     
            # Create as many subsets as there are categories, and put them into this list
            subsetsDF     = rep(list(data.frame(NULL)), nCategories)
            for(i in 1:nCategories){
            
                subsetsDF[[i]] = subset(tableBase, tableBase[,groupIndex] == as.character(myCategories[i]))
            
            }
        
            # For each category (start at 1 because we want to find all centralities)
            for(i in 1:nCategories){
          
                samplesI      = subsetsDF[[i]][,variableIndex]
          
                centralitiesDF[i,2] = mean(samplesI,   na.rm = TRUE)
                centralitiesDF[i,3] = median(samplesI, na.rm = TRUE)
          
            }

            # Check out if we have enough categories to do p-values
            # If we have more than two categories, do something, otherwise
            # print a warning about impossible to find p-values
            if(nCategories >= 2){
              
                tTestString = paste(" T-Test performed under these conditions:",
                                    "     + Assumed unequal variance (Welsh df)",
                                    "     + Two Sided",
                                    "     + Mu = 0",
                                    "",
                                    " - Whas your population sampled randomly properly?",
                                    "     If not try Resampling: http://finzi.psych.upenn.edu/R/library/coin/doc/coin.pdf",
                                    " - Whas your data non normally distributed?",
                                    "     For non-parametric test try Mann-Whitney U, Wilcoxon Signed Rank, Kruskal Wallis, and Friedman tests.",
                                    " - Are you doing a lot of T-Test at the same time?",
                                    "     Consider doing an ANOVA instead, and make sure you run a post-hoc if you don't.",
                                    sep="\n")
              
                # Find the p-values
                # ---- We have nCategories! pValues.
                # ---- We are going to put them into a triangular matrix of nCategories x nCategories.
                # ---- Also notice that the main diagonal means nothing here, as it doesn't make sense to find the p-value with yourself
                {
    
                    # ---- Now do all the possible combinations
                    # ---- Be aware that for many categories this is not advice, you might want to do an ANOVA instead
                    # ---- With some type of TukeySD analysis afterwards
                
                    # For each category (start at 2 because we don't do p-values with self)
                    for(i in 2:nCategories){
                      
                        # For each other category that we haven't done already
                        for(j in 1:(i-1)){
                        
                        # Count that you have enough data to do a T-Test
                        {
                          samplesI      = subsetsDF[[i]][,variableIndex]
                          samplesJ      = subsetsDF[[j]][,variableIndex]
                          samplesI      = samplesI[!is.na(samplesI)]
                          samplesJ      = samplesJ[!is.na(samplesJ)]
                          totalSamplesI = length(samplesI)
                          totalSamplesJ = length(samplesJ)
                          sdSamplesI    = sd(samplesI)
                          sdSamplesJ    = sd(samplesJ)
                        }
                        
                        # If you do...
                        if( (totalSamplesI >= 2) && (totalSamplesJ >= 2) ){
                          
                          # Count that not everything is the same
                          sdSamplesI    = sd(samplesI)
                          sdSamplesJ    = sd(samplesJ)
                          
                          # If you do, then you can run a T-Test
                          if((sdSamplesI > 0) && (sdSamplesJ > 0) ){
                            
                            myPValue = t.test(subsetsDF[[i]][,variableIndex], subsetsDF[[j]][,variableIndex])$p.value
                            pValuesDF[i,j+1] = myPValue # +1 because the first column is the category ID
                            
                          }
         
                        }
                      }
                    }
    
                }
              
                # Find the averages and medians
              
              
            }
            
            else{
              
                print( "WARNING!! Can't generate p-values")
                print( "          Only one categorie was found")
                print( "          To make a p-value I need at least two categories with two different values in each")
                print( "          This was the attemp: ")
                print( paste("          Table:              ", myTableName,                                                  sep=''))
                print( paste("          Category variable:  ", groupingName , " only found ", as.character(myCategories[1]), sep='' ))
                print( paste("          Numerical variable: ", numericalName,                                                sep=''))
              
                showPValues = FALSE
              
            }

            # Finally, do the actual plot
            # ---- Prepare the defaults
            {

                defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                                colorsVector = colorsVector, plotTitle   = plotTitle,
                                                                plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                                plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                                fileType = myPlotType)
          
                colorsVector  = defaultVector[[1]]
                plotTitle     = defaultVector[[2]][1]
                plotSubtitle  = defaultVector[[3]][1]
                plotCaption   = defaultVector[[4]][1]
                plotXLabel    = defaultVector[[5]][1]
                plotYLabel    = defaultVector[[6]][1]
            }

            # Prepare the dataframe with the horizontal lines
            horizontalDF = NA
        
            if(showPValues == TRUE){
          
                # Init the p-values labels and lines DF
                {
                    totalRows              = choose(nCategories,2)
                    horizontalDF           = data.frame(matrix(0, nrow = totalRows, ncol = 4))
                    colnames(horizontalDF) = c("StartX", "EndX", "StartY", "pvalue")
                }
        
                # ---- Find where we can plot the p-values labels
                {
                    yMinimum         = min(tableBase[,variableIndex], na.rm=T)
                    yMaximum         = max(tableBase[,variableIndex], na.rm=T)
                    distanceRelative = yMaximum - yMinimum
                    
                    constantSpace    = (distanceRelative*0.1) # Arbitrary number here
                    upperBound       = yMinimum
                    lowerBound       = upperBound - (constantSpace * totalRows * 1.2) # 1.2 because I say so, trying to making look nice with an arbitrary number
                    firstStep        = upperBound
                }

                # ---- Calculate the coordinates for each of the lines
                currentLine = 1
          
                # For each category
                for(i in 2:nCategories){
            
                    # For each other category
                    for(j in 1:(i-1)){

                      roundedPValue = signif(pValuesDF[i,j+1],3)
                      
                      horizontalDF$StartX[currentLine] = i
                      horizontalDF$EndX[currentLine]   = j
                      
                      horizontalDF$StartY[currentLine] = firstStep - ( currentLine * constantSpace )
                      horizontalDF$pvalue[currentLine] = roundedPValue
                      
                      currentLine = currentLine + 1
              
                    }
            
                }

                # If we are going to write the p-values, we need to lower the ymin limit where we draw the plot
                # Otherwise it won't show anything          
                ymin = min(horizontalDF$StartY) - constantSpace/3
          
            }

            # Change the yLimit if you want the scale to be forced to zero
            if(overrideScaleToZero == TRUE) ymin = 0

            # Do the plot
            myBoxPlot = ggplot()+
            
                # ---- Boxplot
                geom_boxplot(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,groupIndex]), outlier.shape = outlierShape) +
                scale_fill_manual(values=colorsVector) +
            
                # ---- With tiny points for the samples
                geom_jitter(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,groupIndex]), position = position_jitterdodge(), color="black", size=0.2, alpha=0.1) +
            
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +
            
                # ---- Create titles and subtitles
                labs(title    = plotTitle,
                     subtitle = plotSubtitle,
                     caption  = plotCaption,
                     fill     = groupingName,
                     x = plotXLabel, y = plotYLabel)
        
            # Apply the theme
            # The difference here is whether or not you want angled labels
            if(angleXLabels == 0){

                myBoxPlot = myBoxPlot +
            
                theme(panel.background   = themeData[[1]],
                      axis.line          = themeData[[2]],
                      panel.grid.major.y = themeData[[3]],
                      panel.grid.major.x = themeData[[4]],
                      legend.position    = themeData[[5]])
                        
            }
            else{
            
                myBoxPlot = myBoxPlot +

                theme(panel.background   = themeData[[1]],
                      axis.line          = themeData[[2]],
                      axis.text.x        = element_text(angle = angleXLabels, vjust = 0.95, hjust = 0.95),
                      panel.grid.major.y = themeData[[3]],
                      panel.grid.major.x = themeData[[4]],
                      legend.position    = themeData[[5]])
            
            }

            # The plot is finished here
            # We can add now optional stuff to it:
            if(showPValues == TRUE){
            
                myBoxPlot = myBoxPlot +
              
                # ---- With the actual p-values under it
                # -------- Add the relative segments
                geom_segment(data = horizontalDF, aes(x = StartX,  y = StartY, xend = EndX, yend = StartY) ) +
                # -------- Add nice accotation segments
                geom_segment(data = horizontalDF, aes(x = StartX,  y = StartY + constantSpace/5, xend = StartX, yend = StartY - constantSpace/5) ) +
                geom_segment(data = horizontalDF, aes(x = EndX,    y = StartY + constantSpace/5, xend = EndX,   yend = StartY - constantSpace/5) ) +
                # -------- Add the pvalues text
                geom_text(data = horizontalDF, aes(x = (StartX + EndX)/2 , y = StartY - constantSpace/3, label = pvalue) , color="black")
            
            }
        
            # -- Show the cut line
            if(!is.null(cutOffLine)){
        
                # The cutOffLine is a list of size 2, that contain a vector of size N in each square
                #
                # The first list is the intercept values
                # The second list is the variable names
                # The third list is the colors for each line (mandatory no default yet)
            
                cutOffData = data.frame(yintercept = cutOffLine[[1]], Lines = cutOffLine[[2]])
                myBoxPlot  = myBoxPlot + 
                
                             new_scale_color() +
                
                             geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines, linetype = Lines)) +
                
                             scale_colour_manual(values = cutOffLine[[3]])
                         
            }
        
            # Save the image
            {
                # Get the default values for with and height
                imageWidth  = maximumCategoryChars * 0.1 * nCategories + maximumCategoryChars * 0.2 # The multiplying number is arbitrary based on what I think look best
                imageWidth  = max(imageWidth, maximumTitlechars * 0.1) # + Add long title correction to image size                
            
                imageHeight = 8
            
                # But override if the user want to
                if(!is.null(overrideImageWidth))  imageWidth  = overrideImageWidth
                if(!is.null(overrideImageHeight)) imageHeight = overrideImageHeight
            }
            
            ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth, height = imageHeight)
            
            
            # Return
            myReturn = vector("list", length = 4)
            myReturn[[1]] = myBoxPlot
            myReturn[[2]] = pValuesDF
            myReturn[[3]] = centralitiesDF
            myReturn[[4]] = imageFilePath
        
            return (myReturn)
            
            
        }

  
        # Box plot for a categorical variable, grouped by several concept.
        # For tables such as this:
        # 
        # | Value | Sex | Concept  |
        #    3      M      Blood A
        #    2      M      Blood 0
        #    6      F      Blood A
        #          ...
        #
        # Which will give you a plot such as this one:
        #
        #                    |
        #       |  |         |  |
        #       |  |         | []
        #      [] []         | []
        #      [] |         [] []         
        #      |            |  |
        #
        #       M F          M F          M F           M F
        #
        #     Blood 0      Blood A      Blood B      Blood AB
        doBiCategoricalBoxPlot <- function(tableBase, groupIndex, variableIndex, fillIndex, plotFilePath,
                                           outlierShape = 19,
                                           cutOffLine   = NULL,
                                           colorsVector = NULL, showPValues = TRUE,
                                           ymin = NULL, ymax = NULL,
                                           plotTitle = NULL, plotSubtitle = NULL,
                                           plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                           plotTheme = NULL,
                                           angleXLabels = 0,
                                           overrideImageWidth  = NULL,
                                           overrideImageHeight = NULL,
                                           overrideScaleToZero = FALSE,
                                           overrideTableName = NULL,
                                           overrideCaption   = NULL,
                                           longPlot = FALSE){

            # Define plot type
            myPlotType  = "DoubleCategoricalBoxplot"
            myTableName = deparse(substitute(tableBase))

            # If you need to override your table name, then do it
            if(!is.null(overrideTableName)) myTableName = overrideTableName

            # Get an automatic name if you don't have a proper one
            {

                imageFilePath = ""
          
                myFileExtension = getFileExtension(plotFilePath)
          
                if(myFileExtension == ".png") imageFilePath = plotFilePath
          
                else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                       tableName = myTableName, fileType = myPlotType,
                                                       variableIndex1 = groupIndex, variableIndex2 = variableIndex, variableIndex3 = fillIndex)

            }

            # Get the theme information
            themeData = getThemeParameters(plotTheme)

            # Prepare the Y axis limits
            if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
            if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)

            # This is going to be the return object, might be empty though
            # "2" signify a p-value that is impossible. Thus marking that value was impossible to find out
            # This might happen because we don't have more than 2 categories, all numbers are NA, all numbers are the same, and so on
            pValuesDF     = NULL

            # Get info about different categories
            nGroupCategories  = 0
            nFillCategories   = 0
            myGroupCategories = ""
            myFillCategories  = ""
            groupingName      = colnames(tableBase)[groupIndex]
            fillingName       = colnames(tableBase)[fillIndex]
            numericalName     = colnames(tableBase)[variableIndex]
            
            # We are not going to check whether is numerical or not. Assume that everything is correct
        
            # Check whether we have levels or not
            # -- For the group by variable
            if(!is.null( levels(tableBase[,groupIndex]))){
             
                myGroupCategories = levels(tableBase[,groupIndex])
                nGroupCategories  = length(myGroupCategories)
                   
            }
            else{
                
                myGroupCategories = unique(tableBase[,groupIndex])
                nGroupCategories  = length(myGroupCategories)
                
            }
            # -- For the filling variable
            if(!is.null( levels(tableBase[,fillIndex]))){
             
                myFillCategories = levels(tableBase[,fillIndex])
                nFillCategories  = length(myFillCategories)
                   
            }
            else{
                
                myFillCategories = unique(tableBase[,fillIndex])
                nFillCategories  = length(myFillCategories)
                
            }

            # Count the number of characters in the categories in order to decide the image width factor
            maximumCategoryChars = max(nchar(as.character(myGroupCategories)))
            maximumCategoryChars = max(maximumCategoryChars, 8) # Tiny names looks weird, let take 8 as minimum char size

            # Count the number of characters in the title
            maximumTitlechars    = nchar(plotTitle)
        
            # Init the plot object
            myBoxPlot            = NA

            # Prepare the pValues dataframe
            # For this type of plot we are only going to make p-values labels if the fill by variable only have 2 categories
            pValuesDF           = DF(nGroupCategories, 2, defaultValue = "")
            colnames(pValuesDF) = c("Group", "Value")
            pValuesDF[,1]       = myGroupCategories
            
            # Prepare the averages dataframe, we are going to find the average only here
            averagesDF           = DF(nGroupCategories, (nFillCategories + 1), defaultValue = NA)
            colnames(averagesDF) = c("Group", myFillCategories)
            averagesDF[,1]       = myGroupCategories
            
            # Fill the p-values and avarages
            # For each group
            for(i in 1:nGroupCategories){
            
                # Get current Group
                currentGroup = as.character(myGroupCategories[i])
                
                # Subset by that group, we will find the p-values with this later
                subsetGroup  = subset(tableBase, tableBase[,groupIndex] == currentGroup)
                
                subsetA      = NA
                subsetB      = NA
                

                
                # For each fill (typically 2, but coded for several)
                # Find the avarages
                for(j in 1:nFillCategories){
                    
                    # Get which subgroup we are analyzing
                    currentFill  = as.character(myFillCategories[j])
                    
                    # Find the subset for this particular combination
                    subsetFill = subset(subsetGroup, subsetGroup[,fillIndex] == currentFill)
                    
                    # Count how many numbers that are not NA we have left, if more than 0, then find the average
                    if(sum(!is.na(subsetFill[,variableIndex])) > 0) averagesDF[i,(j+1)] = mean( subsetFill[,variableIndex] , na.rm = TRUE)
                    
                    # Prepare the subsets for the p-values
                    if(j == 1) subsetA = subsetFill
                    if(j == 2) subsetB = subsetFill
                }

                # Find the p-values if possible
                # TODO: Add the sd = 0, and n>2 condition
                
                
                #print(subsetA[,variableIndex])
                
                #print(subsetB[,variableIndex])
                
                # Check preconditions
                totalARows = length(subsetA[,variableIndex])
                totalBRows = length(subsetB[,variableIndex])
                
                if(totalARows > 2 & totalBRows > 2){
                
                    SDA = sd(subsetA[,variableIndex], na.rm = TRUE)
                    SDB = sd(subsetB[,variableIndex], na.rm = TRUE)
                    
                    if(SDA != 0 & SDB !=0){
                
                        myPValue       = t.test(subsetA[,variableIndex], subsetB[,variableIndex])$p.value
                        pValuesDF[i,2] = myPValue         
                        
                    }

                }
                
            }
    

            # Finally, do the actual plot
            # ---- Prepare the defaults
            {

                defaultVector = getBiCategoricalNumericalDefaults(groupingName, myGroupCategories,
                                                                  fillingName, myFillCategories,
                                                                  numericalName,
                                                                  colorsVector = colorsVector, plotTitle   = plotTitle,
                                                                  plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                                  plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                                  fileType = myPlotType)
                
                colorsVector  = defaultVector[[1]]
                plotTitle     = defaultVector[[2]][1]
                plotSubtitle  = defaultVector[[3]][1]
                plotCaption   = defaultVector[[4]][1]
                plotXLabel    = defaultVector[[5]][1]
                plotYLabel    = defaultVector[[6]][1]
                
              }

            # Prepare the Pvalues labels
            # Prepare the dataframe with the horizontal lines??
            horizontalDF = NA
        
            if(FALSE){
          
                  # Init the p-values labels and lines DF
                  {
                    totalRows              = choose(nCategories,2)
                    horizontalDF           = data.frame(matrix(0, nrow = totalRows, ncol = 4))
                    colnames(horizontalDF) = c("StartX", "EndX", "StartY", "pvalue")
                }

                  # ---- Find where we can plot the p-values labels
                  {
                    yMinimum         = min(tableBase[,variableIndex], na.rm=T)
                    yMaximum         = max(tableBase[,variableIndex], na.rm=T)
                    distanceRelative = yMaximum - yMinimum
            
                    constantSpace    = (distanceRelative*0.1) # Arbitrary number here
                    upperBound       = yMinimum
                    lowerBound       = upperBound - (constantSpace * totalRows * 1.2) # 1.2 because I say so, trying to making look nice with an arbitrary number
                    firstStep        = upperBound
                }

                  # ---- Calculate the coordinates for each of the lines
                  currentLine = 1
          
                  # For each category
                  for(i in 2:myGroupCategories){
            
                    # For each other category
                    for(j in 1:(i-1)){
              
              #roundedPValue = signif(pValuesMatrix[j,i],3)
              #horizontalDF$StartX[currentLine] = j
              #horizontalDF$EndX[currentLine]   = i

              roundedPValue = signif(pValuesDF[i,j+1],3)
              
              horizontalDF$StartX[currentLine] = i
              horizontalDF$EndX[currentLine]   = j
              
              horizontalDF$StartY[currentLine] = firstStep - ( currentLine * constantSpace )
              horizontalDF$pvalue[currentLine] = roundedPValue
              
              currentLine = currentLine + 1
              
            }
            
          }

                  # If we are going to write the p-values, we need to lower the ymin limit where we draw the plot
                  # Otherwise it won't show anything          
                  ymin = min(horizontalDF$StartY) - constantSpace/3
          
               }

            # Change the yLimit if you want the scale to be forced to zero
            if(overrideScaleToZero == TRUE) ymin = 0
            
            # Do the plot
            {
                myBoxPlot = ggplot()+
            
                # ---- Boxplot
                geom_boxplot(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,fillIndex]), outlier.shape = outlierShape,
                    width = 0.6)  +
                scale_fill_manual(values=colorsVector) +
            
                    
                    
                    
                # ---- With tiny points for the samples
                geom_jitter(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,fillIndex]), position = position_jitterdodge(), color="black", size=0.2, alpha=0.1) +
            
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +
            
                # ---- Create titles and subtitles
                labs(title    = plotTitle,
                     subtitle = plotSubtitle,
                     caption  = plotCaption,
                     fill     = groupingName,
                     x = plotXLabel, y = plotYLabel)
                
            }

            # Apply the theme
            # The difference here is whether or not you want angled labels
            if(angleXLabels == 0){

                    myBoxPlot = myBoxPlot +
            
                    
                    theme(panel.background   = themeData[[1]],
                          axis.line          = themeData[[2]],
                          panel.grid.major.y = themeData[[3]],
                          panel.grid.major.x = themeData[[4]],
                          legend.position    = themeData[[5]])
                        
                }
            else{
            
                    myBoxPlot = myBoxPlot +

                    theme(panel.background   = themeData[[1]],
                          axis.line          = themeData[[2]],
                          axis.text.x        = element_text(angle = angleXLabels, vjust = 0.95, hjust = 0.95),
                          panel.grid.major.y = themeData[[3]],
                          panel.grid.major.x = themeData[[4]],
                          legend.position    = themeData[[5]])
            
                }

            # The plot is finished here
            # We can add now optional stuff to it:
            showPValues = FALSE
            if(showPValues == TRUE){
            
                    myBoxPlot = myBoxPlot +
              
                    # ---- With the actual p-values under it
                    # -------- Add the relative segments
                    geom_segment(data = horizontalDF, aes(x = StartX,  y = StartY, xend = EndX, yend = StartY) ) +
                    # -------- Add nice accotation segments
                    geom_segment(data = horizontalDF, aes(x = StartX,  y = StartY + constantSpace/5, xend = StartX, yend = StartY - constantSpace/5) ) +
                    geom_segment(data = horizontalDF, aes(x = EndX,    y = StartY + constantSpace/5, xend = EndX,   yend = StartY - constantSpace/5) ) +
                    # -------- Add the pvalues text
                    geom_text(data = horizontalDF, aes(x = (StartX + EndX)/2 , y = StartY - constantSpace/3, label = pvalue) , color="black")
            
                }
        
            # -- Show the cut line
            if(!is.null(cutOffLine)){
        
                    # The cutOffLine is a list of size 2, that contain a vector of size N in each square
                    #
                    # The first list is the intercept values
                    # The second list is the variable names
                    # The third list is the colors for each line (mandatory no default yet)
            
                    cutOffData = data.frame(yintercept = cutOffLine[[1]], Lines = cutOffLine[[2]])
                    myBoxPlot  = myBoxPlot + 
                
                                 new_scale_color() +
                
                                 geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines, linetype = Lines)) +
                
                                 scale_colour_manual(values = cutOffLine[[3]])
                         
            
            
            
                }
        
            # Save the image
            {
                  
                # Get the default values for with and height
                imageWidth  = maximumCategoryChars * 0.1 * nGroupCategories + maximumCategoryChars * 0.2 # The multiplying number is arbitrary based on what I think look best
                imageWidth  = max(imageWidth, maximumTitlechars * 0.1) # + Add long title correction to image size                
            
                imageHeight = 8
            
                # But override if the user want to
                if(!is.null(overrideImageWidth)){
                    imageWidth  = overrideImageWidth
                }
                if(!is.null(overrideImageHeight)){
                    imageHeight = overrideImageHeight
                }

                if(longPlot == TRUE){
                
                    myBoxPlot = myBoxPlot + coord_flip()
                        
                    tempVariable = imageWidth
                    imageWidth   = imageHeight
                    imageHeight  = tempVariable
                }
                
                ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth, height = imageHeight, limitsize = FALSE)
                #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
            
             }

             # Return
             myReturn = vector("list", length = 4)
             myReturn[[1]] = myBoxPlot
             myReturn[[2]] = pValuesDF
             myReturn[[3]] = averagesDF
             myReturn[[4]] = imageFilePath
        
             return (myReturn)

      }
        
   
        
        }

        # ---------------------------------
        #          HEATMAPS
        # ---------------------------------
        {
        
              doCategoricalHeatmap <- function(tableBase, categoryXIndex, categoryYIndex, plotFilePath,
                                   roundDecimal = 0, addMarginals = TRUE,
                                   plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                   plotTheme = "no-grid",
                                   overrideTableName = NULL,
                                   overrideCaption   = NULL,
                                   logFileDescriptor = NULL,
                                   supressWarnings = FALSE){
    
    # Define plot type
    myPlotType  = "CategoricalHeatmap"
    myTableName = deparse(substitute(tableBase))
    
    # If you need to override your table name, then do it
    if(!is.null(overrideTableName)){
      myTableName = overrideTableName
      
    }

    # Get an automatic imagePath name if you don't have a proper one
    {
      
      imageFilePath   = ""
      myFileExtension = getFileExtension(plotFilePath)
      
      if(myFileExtension == ".png") imageFilePath = plotFilePath
      
      else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                             tableName = myTableName, fileType = myPlotType,
                                             variableIndex1 = categoryXIndex,
                                             variableIndex2 = categoryYIndex)
      
    }
    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)

    # Get info about the categories
    categoryAName    = colnames(tableBase)[categoryXIndex]
    categoryBName    = colnames(tableBase)[categoryYIndex]
        
    # Get info about the modalities of each category
    modalitiesXNames = levels(tableBase[,categoryXIndex])
    if(is.null(modalitiesXNames)) modalitiesXNames = sort(unique(tableBase[,categoryXIndex]))
    modalitiesYNames = levels(tableBase[,categoryYIndex])
    if(is.null(modalitiesYNames)) modalitiesYNames = sort(unique(tableBase[,categoryYIndex]))
    totalXModalities = length(modalitiesXNames)
    totalYModalities = length(modalitiesYNames)
    
    # Init a matrix with the total for each combination
    matrixCount = matrix(0, nrow = totalYModalities, ncol = totalXModalities)
    
    # Make a new dataframe so we can draw the heatmap
    # Category X / Category Y / Total / X coordinate / Y coordinate / Color of tile / Size for text
    totalRows              = totalXModalities * totalYModalities
    heatmapTable           = data.frame(matrix(NA, nrow = totalRows, ncol = 8))
    colnames(heatmapTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size", "Label" )
    
    
    # Fill the dataframe
    for(i in 1:totalXModalities){
      
      for(j in 1:totalYModalities){
        
        # Get the current line and categories
        currentLine = (i-1)*totalYModalities + j
        
        currentXModality = modalitiesXNames[i]
        currentYModality = modalitiesYNames[j]
        
        # Init the row in the dataframe
        heatmapTable$CategoryX[currentLine] = currentXModality
        heatmapTable$CategoryY[currentLine] = currentYModality
        
        # Count how many we have
        heatmapTable$Total[currentLine]     = sum(tableBase[tableBase[,categoryXIndex] == currentXModality, categoryYIndex ] == currentYModality)
        
        # Set the proper X / Y coordinates
        heatmapTable$Xcoordinate[currentLine] = i
        heatmapTable$Ycoordinate[currentLine] = j
        
        # Set the manual color and size for the text (not in use)
        heatmapTable$myColor[currentLine] = "Red"
        heatmapTable$Size[currentLine]    = 1
        
        # Set the label
        heatmapTable$Label[currentLine] = round(heatmapTable$Total[currentLine],roundDecimal)
        
        
      }
      
    }
    
    # cols = brewer.pal(n = 5, name = "RdBu")
    # 
    # blueColors = brewer.pal(n = 3, name = "PuBu")
    # redColors  = brewer.pal(n = 3, name = "OrRd")
    # lastBlue   = blueColors[3]
    # lastRed    = redColors[3]
    # whiteColor = "#FFFFFF"
    # blackColor = "#000000"
    
    #joinColors = c(whiteColor, whiteColor, redColors, lastRed, blackColor, lastBlue, rev(blueColors), whiteColor, whiteColor)
    
    # joinColors = c(rev(redColors), whiteColor, whiteColor, whiteColor, blueColors)
    
    #print(joinColors)
    
    heatmapPlot = ggplot( heatmapTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total )) +

                          scale_fill_gradient2("Combination", limits = c(0, max(heatmapTable$Total)), 
                                               mid = "#132b43", high = "#54adf2") +
            
                          # The background rectangle
                          geom_tile(color = "black") +

      
                          # What is written inside the rectangle
                          geom_shadowtext( aes(label= Label), color = "white", fontface = "bold") +

                          # What is written in the X axys
                          geom_text(aes(label = CategoryX, y = 0.3), angle = 90, hjust = 1) +
                          # What is written in the Y axys
                          geom_text(aes(label = CategoryY, x = 0.3), hjust = 1) +
      
                          # Give an extra space so we can see the labels clearly  
                          coord_cartesian( xlim = c(-1, totalXModalities + 0.5),
                                           ylim = c(-1, totalYModalities + 0.5)) +

                          # Create titles and subtitles
                          labs(title    = plotTitle,
                               subtitle = plotSubtitle,
                               caption  = plotCaption,
                               fill     = "Total",
                               x = plotXLabel, y = plotYLabel) +
      
                          # Apply the theme
                          theme(panel.background   = themeData[[1]],
                                axis.line          = themeData[[2]],
                                panel.grid.major.y = themeData[[3]],
                                panel.grid.major.x = themeData[[4]],
                                legend.position    = themeData[[5]],
                                axis.ticks         = themeData[[6]],
                                axis.text.x        = themeData[[7]],
                                axis.text.y        = themeData[[8]])

    if(addMarginals==TRUE){

      # Get the summaries for each categorical variable      
      XSummary = summarizeCategorical(tableBase, categoryXIndex, sorted="none")
      YSummary = summarizeCategorical(tableBase, categoryYIndex, sorted="none")

      # Prepare the dataframe with the marginal total
      totalRows              = totalXModalities + totalYModalities
      marginalTable          = data.frame(matrix(NA, nrow = totalRows, ncol = 8))
      colnames(marginalTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size", "Label" )
      
      
      # Fill the dataframe
      for(i in 1:totalXModalities){

        # Get the current line and categories
        currentLine = i
        currentXModality = modalitiesXNames[i]

        # Init the row in the dataframe
        marginalTable$CategoryX[currentLine] = currentXModality
        marginalTable$CategoryY[currentLine] = ""
        
        # Count how many we have
        marginalTable$Total[currentLine]     = XSummary$Count[i]
        
        # Set the proper X / Y coordinates
        marginalTable$Xcoordinate[currentLine] = i
        marginalTable$Ycoordinate[currentLine] = totalYModalities + 1
        
        # Set the label
        marginalTable$Label[currentLine] = round(marginalTable$Total[currentLine],roundDecimal)
        
      }
        
      for(i in 1:totalYModalities){
          
        # Get the current line and categories
        currentLine = i + totalXModalities
        currentYModality = modalitiesXNames[i]
          
        # Init the row in the dataframe
        marginalTable$CategoryX[currentLine] = ""
        marginalTable$CategoryY[currentLine] = currentYModality
          
        # Count how many we have
        marginalTable$Total[currentLine]     = YSummary$Count[i]
          
        # Set the proper X / Y coordinates
        marginalTable$Xcoordinate[currentLine] = totalXModalities + 1
        marginalTable$Ycoordinate[currentLine] = i
          
        # Set the label
        marginalTable$Label[currentLine] = round(marginalTable$Total[currentLine],roundDecimal)
          
      }
        
      #print(marginalTable)
      
      
      # Add the marginal tiles
      heatmapPlot = heatmapPlot +
        
                    new_scale_fill() +
                    # The background rectangle
                    geom_tile(data = marginalTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total), color = "black") +
                    # What is written inside the rectangle
                    geom_shadowtext(data = marginalTable, aes(label= Label), color = "white", fontface = "bold") +
                    # Add a different gradient
                    scale_fill_gradient2("Marginal", 
                                         low = "black", high = "red") +
        
                    # Give an extra space so we can see the labels clearly  
                    coord_cartesian( xlim = c(-1, totalXModalities + 1.5),
                                     ylim = c(-1, totalYModalities + 1.5))
      
      
       

    }
    
    # Save the image and the txt files with the data
    # ---- Get the dimensions
    imageWidth = totalXModalities * 1.4
    if(addMarginals == TRUE) imageWidth = imageWidth   + 1
    imageHeight = totalYModalities * 1.4
    if(addMarginals == TRUE) imageHeight = imageHeight + 1
    # ---- Keep the ratio
    imageWidth  = max(imageWidth, imageHeight)
    imageHeight = max(imageWidth, imageHeight)
    # ---- Correct the ratio, for some reasons looks better this way
    imageWidth  = imageWidth * 1.1
    
    ggsave(imageFilePath, plot = heatmapPlot, width = imageWidth, height=imageHeight, scale=1.2)  
    latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
    
    myReturn = vector("list", length = 3)
    myReturn[[1]] = heatmapPlot
    myReturn[[2]] = imageFilePath
    myReturn[[3]] = latexFilePath
    
    return (myReturn)
    
  }
                
        }
        
        
        # ---------------------------------
        #          SPECIALS
        #
        #          -- BMI density with BMI values pre-filled
        # ---------------------------------
        {
            # ---- BMI for each grouping
            doBMIPlot <- function(tableBase, BMIindex, groupIndex, plotFilePath,
                                  colorsVector = NULL,
                                  plotTitle = NULL, plotSubtitle = NULL,
                                  plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                  plotTheme = NULL,
                                  overrideTableName = NULL,
                                  overrideCaption   = NULL){

        # Init variables
        myPlotType = "BMIPlot"
        myTableName = deparse(substitute(tableBase))

        if(!is.null(overrideTableName)){
          myTableName = overrideTableName

        }

        # Get an automatic name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = groupIndex, variableIndex2 = BMIindex)
          
        }

        # Get the theme information
        themeData = getThemeParameters(plotTheme)

        # Get info about different categories
        {
          myCategories  = unique(tableBase[,groupIndex])
          nCategories   = length(myCategories)
          groupingName  = colnames(tableBase)[groupIndex]
          numericalName = "BMI"
          totalRows     = nrow(tableBase)          
        }


        # ---- Prepare the defaults
        {

          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # ---- Find the maximum density
        myMaxDensity = max(density(tableBase[,BMIIndex], na.rm=TRUE)$y)

        for (i in 1:nCategories) {

          subTable      = tableBase[tableBase[,groupIndex] == myCategories[i],]
          subMaxDensity = max(density(subTable[,BMIIndex], na.rm=TRUE)$y)
          myMaxDensity  = max(myMaxDensity, subMaxDensity)

        }

        lowerCoordinate = myMaxDensity * -0.02
        middleCoordinate = lowerCoordinate/2

        # Do the plot
        {
          myBMIPlot = ggplot(tableBase, aes(x = tableBase[,BMIIndex],  fill = tableBase[,groupIndex]), colour = "Black") +

                             # Background rectangles
                             geom_rect(xmin=0,    xmax=18.5, ymin=-Inf, ymax=Inf, fill = "#ffffbb", color="black", alpha=0.05) +
                             geom_rect(xmin=18.5, xmax=25,   ymin=-Inf, ymax=Inf, fill = "#bbff9e", color="black", alpha=0.05) +
                             geom_rect(xmin=25,   xmax=30,   ymin=-Inf, ymax=Inf, fill = "#ffe3bb", color="black", alpha=0.05) +
                             geom_rect(xmin=30,   xmax=Inf,  ymin=-Inf, ymax=Inf, fill = "#ffbbbb", color="black", alpha=0.05) +

                             # Labels for the rectangles
                             geom_text(x = 14.5, y = middleCoordinate, label="Underweight", size=4, color="black", vjust = 1.5) +
                             geom_text(x = 21.5, y = middleCoordinate, label="Healthy",     size=4, color="black", vjust = 1.5) +
                             geom_text(x = 27.5, y = middleCoordinate, label="Overweight",  size=4, color="black", vjust = 1.5) +
                             geom_text(x = 33,   y = middleCoordinate, label="Obese",       size=4, color="black", vjust = 1.5) +

                             # Do a density plot
                             geom_density(alpha = 0.6) +

                             # Specify colors
                             scale_color_manual(values = colorsVector, na.value = COLOR_NA) +
                             scale_fill_manual(values  = colorsVector, na.value = COLOR_NA) +

                             # Set the limits for the IMC
                             xlim(12, 35) +
                             ylim(lowerCoordinate , myMaxDensity * 1.1) +

                             # Create titles and subtitles
                             labs(title    = plotTitle,
                                  subtitle = plotSubtitle,
                                  caption  = plotCaption,
                                  color    = groupingName,
                                  fill     = groupingName,
                                  x = plotXLabel, y = plotYLabel) +

                            # Apply the theme
                            theme(panel.background   = themeData[[1]],
                                  axis.line          = themeData[[2]],
                                  panel.grid.major.y = themeData[[3]],
                                  panel.grid.major.x = themeData[[4]])
        }

        # Save the image
        imageWidth    = 8
        ggsave(imageFilePath, plot = myBMIPlot, width = imageWidth)
        latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return
        myReturn = vector("list", length = 3)
        myReturn[[1]] = myBMIPlot
        myReturn[[2]] = imageFilePath
        myReturn[[3]] = latexFilePath
        
        return (myReturn)

      }
        }
    
  
}


# ------------------------------------------------
# PLOTS FOR NETWORKS RELATED IMAGES
# ------------------------------------------------

# edgesDF - A dataframe for the edges. It has this form:
#           
#                 from to value Category 0 ... Category N
#           315   315  1     1        TRUE     Modality_A
#           475   475  1     1       FALSE     Modality_B
#           1042  24   2     1       FALSE             NA
#           1094  76   2     1        TRUE     No modality
#           1687  669  2     1       FALSE     Modality_A
#           2474  438  3     1       FALSE     Modality_C
#
#           from and to is the ID of the nodes that you can find in nodesDF
#           All the from to to edges IDs must be in the nodesDF, but you might
#           have nodes without edges (which is perfectly normal)
#
#           Everything else is not mandatory, but is necessary if you want to
#           highlight something special about the edges (see later)
#
# nodesDF - A dataframe with the nodes. It has this shape:
#
#              ID  Age    Sex    BMI     School 
#           1  1    16  Woman  20.18 Vocational
#           2  2    16  Woman  18.08 Vocational
#           3  3    17    Man  18.10    General
#           4  4    16  Woman  21.82    General
#           5  5    16  Woman  24.26    General
#           6  6    16  Woman  18.84 Vocational
#
#           Only the ID is mandatory, the rest you only need it if you want
#           to highlight something special about the nodes (see later)
#
# (string) folderPath: A string to the folder where you want to save the image
#
# (int)    highlightVariable: The index column of the nodesDF that you want to
#                             use to color the nodes in the graph plot. If you
#                             leave to NULL, the dots will be black by default.
#
# List<String>  colorVectorHighlight: If you choose a highlight variable, you
#                                     might choose the color vector for that
#                                     variable. If you choose a categorical
#                                     variable the color vector must have a size
#                                     equal to the number of categories.
#
#                                     ie: c("#FFFFBF", "#ABDDA4", "#FDAE61",
#                                           "#D7191C", "#7F7F7F")
#
#                                     This is a color vector for BMI, for
#                                     categories "Underweight", "Healthy",
#                                     "Overweight", "Obese" and "Unknown".
#
#                                     If your nodesDF follows a factor scheme
#                                     the color vector will also follow that
#                                     color scheme order.
#
# (int) sizeVariableIndex: Which column from nodesDF shall be use as a size for
#                          each of the nodes in the plot. For example, you
#                          could use the number of connection to that node.
#
#                          If nothing is given, the default size is 0, and all
#                          the nodes are plotted as dots.
#
# (int) rimVariable: Each node in the plot has a black rim around it. You can
#                    change that and color the rim based on any variable you
#                    want. Is not recommended as the plot tend to get very
#                    convoluted, but you can use it nevertheless.
#
# List<String> colorVectorRim: The list of color for the rim.
#
# (int) edgesHighlight: Which column from edgesDF do you want to use to
#                       highlight the edges.
#
# List<String>  colorVectorEdge The list of color for the edges.

# (bool) directedPlot: If you want your plot to be directed. FALSE by default.
#                      If you do, expect arrows marker in each edge.
#
# (String) selectedLayout: Which layout do you want to use. The default is
#                         'mds'. The possible layouts are:
# 
#                         'manual' - Mark this if you want to use your own
#                                    layout (see next)
#
#                         'gem',
#                         'dh',
#                         'graphopt',
#                         'mds' = multidimensional scaling, it tries to keep a balance in between everything.
#                         'fr' = Fruchterman - Reingold , it keeps related vertices toguether
#                         'kk' = Kawai - Emphases distance as information
#                         'drl'
#                         'lgl'
#
#
# (dataFrame) manualLayout: If you want to give your own layout for the nodes.
#                           First you need to select 'manual' in selectedLayout
#                           argument, otherwise this variable will be ignored
#                           and do a MDS by default.
#
#                           The dataframe has this shape, where each row
#                           correspond to each node in nodeDF
#                           
#                                 x        y
#                           1     23.87218 2.307090
#                           2     23.87183 2.322370
#                           3     22.63146 2.821012
#                           4     20.87682 1.066371
#                           5     22.63123 2.833566
#                           6     23.87079 2.337620
#
# (bool) suppressAloneNode: If you want to not show nodes that aren't connecting
#                           to anything. Default is FALSE.

doGraphPlot <- function(edgesDF, nodesDF, folderPath,
                        highlightVariable = NULL, colorVectorHighlight = NULL,
                        sizeVariableIndex = NULL,
                        rimVariable = NULL, colorVectorRim = NULL,
                        edgesHighlight = NULL, colorVectorEdge = NULL,
                        edgesThickness = 1, edgesAlpha = 0.2,
                        directedPlot = FALSE,
                        selectedLayouts = NULL, manualLayout = NULL,
                        plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                        overrideLegendSize = NULL,
                        overrideLegendPosition = NULL,
                        overrideTableName = NULL,
                        overrideCaption = NULL,
                        suppressAloneNode = FALSE){
  
      # Get the name of the variable where the edges and nodes come from
      edgesNames    = deparse(substitute(edgesDF))
      nodesNames    = deparse(substitute(nodesDF))
    
      # Init file name
      {
        myPlotType  = "GraphPlot"
        myTableName = ""
        if(is.null(overrideTableName)){
          myTableName = paste(edgesNames,"_",nodesNames, sep="")
        }
        else{
          myTableName = overrideTableName
        } 
      }
    
      # Add a column where we are going to store the size information
      # If we have a size variable, init to that instead.
      {
        nodesDF$FinalSize = 0
        if(!is.null(sizeVariableIndex)){
          
          nodesDF$FinalSize = nodesDF[,sizeVariableIndex]/3
          
        }        
      }

      # You might have edges between nodes that are not in the node list
      # Delete the edges that doesn't appear in the nodes
      # Also give a warning to the user
      {
        notInNodesEdgesSet = union( setdiff(edgesDF$from, nodesDF$ID) , setdiff(edgesDF$to, nodesDF$ID) )
        totalMissingNodes  = length(notInNodesEdgesSet)
        # ---- The warning
        if(totalMissingNodes > 0){
          
          print("WARNING: Some of the provided edges are not in the provided nodes")
    
          totalEdges    = nrow(edgesDF)
          keepTheseRows = rep(TRUE, totalEdges)
          
          for (i in 1:totalEdges) {
            
            fromHere = edgesDF$from[i]
            toHere   = edgesDF$to[i]
            
            keepTheseRows[i] = !( (sum(fromHere == notInNodesEdgesSet) > 0)  ||  (sum(toHere == notInNodesEdgesSet) > 0) )
            
          }
          
          print("Previous total")
          print(totalEdges)
          
          edgesDF = edgesDF[keepTheseRows,]
          
          print("New total")
          totalEdges    = nrow(edgesDF)
          print(totalEdges)
          
        }    
      }
    
      # If you want to not show independent nodes
      if(suppressAloneNode == TRUE){
        
        # Get all the nodes ID
        nodesIDs = nodesDF$ID
        
        # Get all nodes that connects to something
        fromEdgesID    = unique(edgesDF$from)
        toEdgesID      = unique(edgesDF$to)
        nodesConnected = unique(append(fromEdgesID,toEdgesID))
        
        # Find the difference and delete
        validNodes     = nodesIDs %in% nodesConnected
        
        # Delete what is not valid
        nodesDF = nodesDF[validNodes,]
        
        print(paste("Not showing", sum(!validNodes), "nodes"))
        
      }

      # Grab the variables names from the nodes dataframe
      {
        myVariables   = colnames(nodesDF)
        
        highlitedName = NULL
        rimName       = NULL
        edgesName     = NULL
        
        if(!is.null(highlightVariable)) highlitedName = myVariables[highlightVariable]
        if(!is.null(rimVariable))       rimName       = myVariables[rimVariable]
        if(!is.null(edgesHighlight))    edgesName     = colnames(edgesDF)[edgesHighlight]
        
        highlighColumn = nodesDF[,highlightVariable]
        rimColumn      = nodesDF[,rimVariable]
        sizeColumn     = nodesDF$FinalSize
        edgeColumn     = edgesDF[,edgesHighlight]
        
        nCategoriesHighlight = length(unique(nodesDF[,highlightVariable]))
        nCategoriesRim       = length(unique(nodesDF[,rimVariable]))     
        nCategoriesEdges     = length(unique(edgesDF[,edgesHighlight]))
      }
    
      # Prepare the defaults text and color schemes
      {
        defaultVector         = getCategoricalNetworkDefaults(highlitedName, rimName, nCategoriesHighlight, nCategoriesRim, colorVectorHighlight, colorVectorRim,
                                                              plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
        
        colorVectorHighlight  = defaultVector[[1]]
        colorVectorRim        = defaultVector[[2]]
        plotTitle             = defaultVector[[3]][1]
        plotSubtitle          = defaultVector[[4]][1]
        plotCaption           = defaultVector[[5]][1]
        plotXLabel            = defaultVector[[6]][1]
        plotYLabel            = defaultVector[[7]][1]    
      
        # If you have edges highlighted by something, but you don't give a color for
        # them, init the vector color to standard palette and give a warning to the
        # user.
        if(!is.null(edgesHighlight) &&  is.null(colorVectorEdge) ){
    
          print("WARNING! Doing a graph plot, you told me to highlight by edge")
          print("but you didn't gave me the colors. I'm initializing the colors")
          print("to something, but you should check this manually.")
          
          myPalette       = colorRampPalette(brewer.pal(5, "Spectral"))
          colorVectorEdge = myPalette(nCategoriesEdges)
          
        }
          
      }

      # Check which layouts are you going to use
      {
        # Default inits
        doingManual  = FALSE  
        graphLayouts = 'mds'
        # Check that we have a selected layout
        if( !is.null(selectedLayouts) ){
          
          # If you selected manual, check that we have an actual layout
          if(selectedLayouts == 'manual'){
              
              # If you are suppose to do manual, but you don't give me anything
              # give a warning and do mds instead
              if( is.null(manualLayout) ){
                
                print(" WARNING!! ")
                print(" You selected a manual layout but didn't gave any coordinates")
                print(" I'm doing the graph anyway but with a mds layout instead")
                
              }
              # If you selected manual, and give an actual valid layout, flag that
              # we are going to do a manual layout.
              else{
                graphLayouts = 'manual'
                doingManual  = TRUE
              }
            
          }
          
          # If you selected something else, do nothing and trust (lol) that the user
          # has selected a valid layout option. If not, the function will gives an
          # error on running time.
          else{
            graphLayouts = selectedLayouts
          }
          
        }
        # If the selected layout is null, check that we don't have a manual layout
        else{
          # If it is not null, then we default to manual
          if(!is.null(manualLayout)){
              graphLayouts = 'manual'
              doingManual  = TRUE
          }
          
        }
    
      }

      # Prepare the filenames, paths, and so on
      {
       
        # Add GRAPH + highlight name, rim name, edge name, layout name (if any of these)
        fileName = paste0("Graph_",  myTableName, "_")
        if(!is.null(highlitedName)) fileName = paste0(fileName, highlitedName, "_")
        if(!is.null(highlitedName)) fileName = paste0(fileName, rimName,       "_")
        if(!is.null(highlitedName)) fileName = paste0(fileName, edgesName,     "_")
        fileName = paste0(fileName, graphLayouts)
        
        # Get the filenames for the plot and the latex
        imgFileName   = paste0(fileName, ".png")
        latexFileName = paste0(fileName, ".tex")
        
        # Remove all the spaces from the names if any, latex don't like that
        imgFileName   = str_replace_all(string=imgFileName,   pattern=" ", repl="")
        latexFileName = str_replace_all(string=latexFileName, pattern=" ", repl="")
        
        # Make the final file path.
        imgFilePath   = file.path(paste(folderPath, imgFileName,   sep = ""))
        latexFilePath = file.path(paste(folderPath, latexFileName, sep = ""))
        
      }

      # Create the graph object and start plotting
      {
        # Graph object
        myGraph = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)    
        
        # Base plot object
        myPlot = NA
        
        # Init the plot depending on whether you are doing a manual or normal layout
        # manualLayout is given as an argument and we trust the user that everything is right
        if(doingManual == FALSE) myPlot = ggraph(myGraph, layout = graphLayouts)
        if(doingManual == TRUE)  myPlot = ggraph(manualLayout)
        
        # Add the edges
        # -- If you didn't gave any hightlight variable
        if( is.null(edgesHighlight)) {
          
          myPlot = myPlot + geom_edge_link0(edge_alpha = edgesAlpha)
          
        }
        # -- If you want a highlight variable
        else{
    
          
          edge_legend_title = edgesName
          
          myPlot = myPlot + geom_edge_link(alpha      = edgesAlpha,
                                           width      = edgesThickness,
                                           aes(colour = edgesDF[,edgesHighlight])) +
            
                            scale_edge_color_manual(edge_legend_title, values = colorVectorEdge)
          
        }
        
        # Add the nodes and the rim of the nodes.
        # This part is a bit weird because you can't do that independent from each
        # other, so you have 4 possible combinations
        {
          
          # If you have a fill, you may or may not have a rim
          if(!is.null(highlightVariable)){
            
            # -- Fill, no rim
            if(is.null(rimVariable)){
              
              myPlot = myPlot +
                geom_node_point(aes(fill = highlighColumn), size = sizeColumn, stroke = 1, shape = 21) +
                scale_fill_manual(values = colorVectorHighlight)
              
            }
            
            # -- Fill, and rim
            else{
              
              myPlot = myPlot +
                geom_node_point(aes(fill = highlighColumn, color = rimColumn), size = sizeColumn, stroke = 2, shape = 21) +
                scale_color_manual(values=colorVectorRim) +
                scale_fill_manual(values=colorVectorHighlight)
            }
            
          }
          
          # If you don't have a fill, you may or may not have a rim
          else{
            
            # -- No fill, no rim
            if(is.null(rimVariable)){
              
              myPlot = myPlot +
                geom_node_point(aes(fill = "grey"), size = sizeColumn, stroke = 1, shape = 21) +
                theme(legend.position="none")
              
            }
            
            # -- No fill, with rim
            else{
              
              myPlot = myPlot +
                geom_node_point(aes(fill = "grey", color = rimColumn), size = sizeColumn, stroke = 2, shape = 21) +
                scale_color_manual(values=colorVectorRim)
              
              
            }
            
            
            
          }
          
          
        }
        
        # Add Theme and Labs
        {
          myPlot = myPlot +
            
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line        = element_blank(),
                  axis.text.x      = element_blank(),
                  axis.text.y      = element_blank(),
                  axis.ticks       = element_blank()) +
            
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 color    = rimName,
                 fill     = highlitedName,
                 x = plotXLabel, y = plotYLabel)
          
        }
        
        # If you want a bigger legend
        if(!is.null(overrideLegendSize)){
            myPlot = myPlot +
        
                guides(colour = guide_legend(override.aes = list(size=overrideLegendSize))) +
                guides(fill   = guide_legend(override.aes = list(size=overrideLegendSize)))
                
                #theme(legend.key.size = unit(overrideLegendSize, 'cm'))        
        
        }
        
        # If you want the legend somewhere else
        if(!is.null(overrideLegendPosition)){
            
            myPlot = myPlot + theme(legend.position= overrideLegendPosition )
            
        }
        
        
    
        
        
      }
      
      # Save the image and the .tex files to generate the image
      {
        imageWidth = 8
        ggsave(imgFilePath, plot = myPlot, width = imageWidth, height = imageWidth)

        
        myReturn      = vector("list", length = 2)
        myReturn[[1]] = myPlot
        myReturn[[2]] = imgFilePath
        
      }
  
    return (myReturn)
    
}
