# -----------------------------------------------------------------------------
#
# This script load basic tools needed for analysis of data.
#
# This script itself doesn't have any functions, and is just a recopilation of
# several sources into one file.
#
# -----------------------------------------------------------------------------

source("toolsBasic.R",       encoding="utf-8")
source("toolsNetwork.R",     encoding="utf-8")
source("toolsLatex.R",       encoding="utf-8")
source("toolsPlotting.R",    encoding="utf-8")
source("toolsSummarizers.R", encoding="utf-8")


# Add the needed libraries
# ---- Basics
library(ggplot2)       # Basic ggplot2 library
library(ggcorrplot)    # Correlogram
library(ggnewscale)    # Allows for multiple color scales in one plot
library(RColorBrewer)  # Color settins and palettes
library(shadowtext)    # Drop shadows in text for better viewing
library(latex2exp)     # Allows latex expressions in ggplot
library(qqplotr)       # QQ plots with bands
library(forcats)       # Reverse order of factors


#library(plyr)          # map_values()  , also plyr needs to load before dplyr
library(dplyr)         # Mutate stuff in several dataframes for several functions


# ---- I don't know if these are use within this library
library(reshape2)


#library(lubridate)     # as_datetime for Sys.time() mostly
library(ggpubr)
library(stringr)
library(scales)
library(gtable)
library(grid)
library(gridExtra)     # grid.arrange()



# ---- For the network (check which one is for drawing)
library(network)
library(sna)
library(GGally)



# ------------------------------------------------
# GET DATA INTO TXT FILES
# ------------------------------------------------
{



}

# ------------------------------------------------
# PLOTTING FUNCTIONS
# ------------------------------------------------

# Common functions?
{




}


# ---- BARPLOTS

{



  doIdentityBarPlot <- function(tableBase, groupingIndex, countingIndex, plotFilePath, colorsVector = NULL,
                        plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL, coloringIndex = groupingIndex){

    # Init variables
    myPlotType = "IdentityBarplot"
    myTableName = deparse(substitute(tableBase))

    # Get an automatic name if you don't have a proper one
    genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName=myTableName, plotType = myPlotType, variableIndex1 = countingIndex, variableIndex2= groupingIndex )
    imageFilePath   = paste(genericFilePath,".png",sep="")
    tableFilePath   = paste(genericFilePath,".txt",sep="")
    latexFilePath   = paste(genericFilePath,".tex",sep="")

    # Get info about different categories
    myCategories = unique(tableBase[,coloringIndex])
    nCategories  = length(myCategories)
    groupingName = colnames(tableBase)[coloringIndex]
    imageWidth   = max(5, nCategories * 2)  # Minimum 5 , so we can at least see the leyend if there are too little categories
    # It sound counter intuitive that the minimum is found by the max(), but trust me.


    # If you try to do a barplot with way too many categories, make a warning about it
    if(nCategories > 16){

      print("")
      print(" ---------------")
      print(" -- WARNING!! --")
      print(" ---------------")
      print("")
      print(" You are trying to do a bar plot with more than 16 categories. Are you sure? ")
      print(" Notice that you have the function 'doLongBarPlot()' which will be a better option.")
      print("")

    }


    # Prepare the defaults
    defaultVector = getCategoricalDefaults(groupingName, myCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = myPlotType)

    colorsVector  = defaultVector[[1]]
    plotTitle     = defaultVector[[2]][1]
    plotSubtitle  = defaultVector[[3]][1]
    plotCaption   = defaultVector[[4]][1]
    plotXLabel    = defaultVector[[5]][1]
    plotYLabel    = defaultVector[[6]][1]

    # # You can either count al the element one by one (classic)
    # if(countingType == "count"){
    #
    # }
    # # Or have a table that is already summarized
    # else{
    #
    # }

    # Do the plot
    ggplot(tableBase, aes(x = tableBase[,groupingIndex] , y = tableBase[,countingIndex], fill = tableBase[,coloringIndex])) +

      # Create bars
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      #scale_fill_manual(values = colorsVector, na.value = colorNA) +

      # Write the text in the bar
      #geom_shadowtext(stat="identity", aes(label=paste( round(tableBase[,countingIndex]*100,2), "%", sep='')), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
      # TODO: There is not a constant space in between the text and the top of the bar when dealing with absolute values

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           fill     = groupingName,
           x = plotXLabel, y = plotYLabel)

    # Save the image and the txt file with the data
    ggsave(imageFilePath, width = imageWidth)
    #writeAbsBarplotData(     tableBase, countingIndex, tableFilePath)
    #writeAbsBarplotDataLATEX(tableBase, countingIndex, latexFilePath)

  }






  # Do a simple bar plot stacked


  # Do a combine bar plot
  # With bar next to each other
  # (ie: Men and Women by Type of Sugar consummed)
  # colorsVector should be the same as the groupIndex if you want it to make sense
  doBarDodgeCombinePlot <- function(tableBase, countingIndex, groupIndex, colorsVector, plotFilePath,
                                    plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    # Init variables
    myPlotType = "AbsDodgeBarplot"
    myTableName = deparse(substitute(tableBase))

    # Get an automatic name if you don't have a proper one
    genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName=myTableName, plotType = myPlotType, variableIndex1 = countingIndex, variableIndex2 = groupIndex )
    imageFilePath   = paste(genericFilePath,".png",sep="")
    tableFilePath   = paste(genericFilePath,".txt",sep="")
    latexFilePath   = paste(genericFilePath,".tex",sep="")

    # Get info about different categories
    myCategoriesA = unique(tableBase[,countingIndex])
    nCategoriesA  = length(myCategoriesA)
    groupingNameA = colnames(tableBase)[countingIndex]

    myCategoriesB = unique(tableBase[,groupIndex])
    nCategoriesB  = length(myCategoriesB)
    groupingNameB = colnames(tableBase)[groupIndex]

    #imageWidth    = nCategoriesA * nCategoriesB

    imageWidth   = max(5, nCategoriesA * nCategoriesB)

    # Prepare the defaults
    defaultVector = getBiCategoricalDefaults(groupingNameA, myCategoriesA, nCategoriesA,
                                             groupingNameB, myCategoriesB, nCategoriesB,
                                             colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    colorsVector  = defaultVector[[1]]
    plotTitle     = defaultVector[[2]][1]
    plotSubtitle  = defaultVector[[3]][1]
    plotCaption   = defaultVector[[4]][1]
    plotXLabel    = defaultVector[[5]][1]
    plotYLabel    = defaultVector[[6]][1]

    # Do the plot
    ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,groupIndex])) +

      # Create bars
      geom_bar(position=position_dodge(), colour="black") +
      scale_fill_manual(values = colorsVector) +

      # Write the text in the bar
      #geom_text(stat='count', aes(label=..count..), position = position_dodge(width=0.90), color = "white", fontface = "bold", vjust = 2) +
      geom_shadowtext(stat='count', aes(label=..count..), position = position_dodge(width=0.90), color = "white", fontface = "bold", vjust = 1.5) +
      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           fill     = groupingNameB,
           x = plotXLabel, y = plotYLabel)

    ggsave(imageFilePath, width = imageWidth)

  }

  # Do a combine bar plot
  # With bar stacked
  # (ie: Men and Women by Type of Sugar consummed)
  # colorsVector should be the same as the groupIndex if you want it to make sense
  doBarStackCombinePlot <- function(tableBase, countingIndex, groupIndex, colorsVector, plotFilePath,
                                    plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    # Get info about different categories
    myCategoriesA = unique(tableBase[,countingIndex])
    nCategoriesA  = length(myCategoriesA)
    groupingNameA = colnames(tableBase)[countingIndex]

    myCategoriesB = unique(tableBase[,groupIndex])
    nCategoriesB  = length(myCategoriesB)
    groupingNameB = colnames(tableBase)[groupIndex]

    imageWidth    = nCategoriesA * 2

    # Prepare the defaults
    defaultVector = getBiCategoricalDefaults(groupingNameA, myCategoriesA, nCategoriesA,
                                             groupingNameB, myCategoriesB, nCategoriesB,
                                             colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    colorsVector  = defaultVector[[1]]
    plotTitle     = defaultVector[[2]][1]
    plotSubtitle  = defaultVector[[3]][1]
    plotCaption   = defaultVector[[4]][1]
    plotXLabel    = defaultVector[[5]][1]
    plotYLabel    = defaultVector[[6]][1]

    ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,groupIndex])) +

      # Create bars
      geom_bar(colour="black") +
      scale_fill_manual(values = colorsVector) +

      # Write the text in the bar
      #geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
      geom_shadowtext(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           fill     = groupingNameB,
           x = plotXLabel, y = plotYLabel)

    ggsave(plotFilePath, width = imageWidth)

  }





  # Do a combine bar plot
  # With any possible bar scheme
  # (ie: Men and Women by Type of Sugar consummed)
  # colorsVector should be the same as the groupIndex if you want it to make sense
  #
  # TODO: ADD LIST OF NAMES TO PLOTS
  #
  doAllCombinePlots <- function(tableBase, countingIndex, groupIndex, colorsVector, plotFilePath,
                                plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    doBarDodgeCombinePlot(tableBase, countingIndex, groupIndex, colorsVector, plotFilePath, plotTitle = plotTitle, plotSubtitle = plotSubtitle, plotCaption = plotCaption, plotXLabel = plotXLabel, plotYLabel = plotYLabel)
    doBarStackCombinePlot(tableBase, countingIndex, groupIndex, colorsVector, plotFilePath, plotTitle = plotTitle, plotSubtitle = plotSubtitle, plotCaption = plotCaption, plotXLabel = plotXLabel, plotYLabel = plotYLabel)

  }


}


# ---- BOXPLOTS

{
  # Boxplot with no grouping at all
  do1Boxplot <- function(tableBase, variableIndex, colorsVector, plotFilePath,
                         plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    # First, get rid of the NA values
    tableBase = tableBase[!is.na(tableBase[,variableIndex]),]

    # Get info about the variable
    numericalName = colnames(tableBase)[variableIndex]
    imageWidth    = 3

    # Prepare the defaults
    defaultVector = getNumericalDefaults(numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    colorsVector  = defaultVector[[1]]
    plotTitle     = defaultVector[[2]][1]
    plotSubtitle  = defaultVector[[3]][1]
    plotCaption   = defaultVector[[4]][1]
    plotXLabel    = defaultVector[[5]][1]
    plotYLabel    = defaultVector[[6]][1]

    ggplot(tableBase, aes(x = tableBase[,variableIndex], y=tableBase[,variableIndex]))+

      geom_boxplot() +
      geom_jitter(color="black", size=0.3, alpha=0.2) +

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           x = plotXLabel, y = plotYLabel)

    ggsave(plotFilePath, width = imageWidth)

  }

  # Boxplot by categorical with numerical Y axys
  do2Boxplot <- function(tableBase, groupIndex, variableIndex, colorsVector, plotFilePath,
                         plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    # Get info about different categories
    myCategories  = unique(tableBase[,groupIndex])
    nCategories   = length(myCategories)
    groupingName  = colnames(tableBase)[groupIndex]
    numericalName = colnames(tableBase)[variableIndex]
    imageWidth    = nCategories * 2

    # Prepare the defaults
    defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, nCategories, numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
    colorsVector  = defaultVector[[1]]
    plotTitle     = defaultVector[[2]][1]
    plotSubtitle  = defaultVector[[3]][1]
    plotCaption   = defaultVector[[4]][1]
    plotXLabel    = defaultVector[[5]][1]
    plotYLabel    = defaultVector[[6]][1]

    # Do the plot
    ggplot(tableBase, aes(x = tableBase[,groupIndex], y =tableBase[,variableIndex], fill = tableBase[,groupIndex]))+

      # ---- Boxplot
      geom_boxplot() +
      scale_fill_manual(values=colorsVector) +

      # ---- With tiny points for the samples
      geom_jitter(position = position_jitterdodge(), color="black", size=0.3, alpha=0.2) +

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           fill     = groupingName,
           x = plotXLabel, y = plotYLabel)

    # Save the plot
    ggsave(plotFilePath, width = nCategories * 3)

  }


  # Boxplot subsetted by one category only, but one plot for each category
  # The selected columns are transformed and melted into categories
  #
  doLongBoxPlotPValues <- function(tableBase, groupIndex, listVariableIndex, colorsVector, folderFilePath,
                                   rootPlotTitle = NULL, rootPlotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    # Init variables
    myPlotType = "LongBoxPlot"
    myTableName = deparse(substitute(tableBase))

    # Get info about different categories
    myCategories  = unique(tableBase[,groupIndex])
    nCategories   = length(myCategories)
    groupingName  = colnames(tableBase)[groupIndex]
    numericalNames = colnames(tableBase)[listVariableIndex]

    # Do a list with many subset, one for each category
    subsetsDF     = rep(list(data.frame(NULL)), nCategories)

    # For each of these subsets...
    for(i in 1:nCategories){

      # Get the name of the category
      currentCategoryName = myCategories[i]

      # Populate it
      subsetsDF[[i]]  = subset(tableBase, tableBase[,groupIndex] == myCategories[i])

      # Get rid of the columns that we don't want
      subsetsDF[[i]] = subsetsDF[[i]][,listVariableIndex]

      # Get the melted table
      currentMeltedTable = melt(subsetsDF[[i]])

      # This table is now a DF with two columns
      # The first column is a categorical with the variable name, and the second some values
      # Named "variable" and "value" by the way
      # So you just need to do a normal boxplot with that
      # But one for each of the original category
      # Hence the root for some of the variables here.

      #filePathForThisPlot = paste(rootPlotFilePath, "_", currentCategoryName, ".png", sep='')
      titleForThisPlot    = paste("Boxplot for ", groupingName, " with " , currentCategoryName, " only", sep='')
      subtitleForThisPlot = paste("Using the given numerical columns as categories",                     sep='')
      overrideName        = paste(myTableName, currentCategoryName,                                      sep="")


      # print(titleForThisPlot)
      # print("In this folder")
      # print(folderFilePath)

      aa = doBoxPlotPValues(currentMeltedTable, 1, 2, NULL, folderFilePath,
                            plotTitle = titleForThisPlot, plotSubtitle = subtitleForThisPlot,
                            plotCaption = plotCaption, plotXLabel = plotXLabel, plotYLabel = plotYLabel,
                            overrideTableName = overrideName)

      # print(aa)

    }

  }

}



# ---- Box plot with upper and lower limits
#coord_cartesian(ylim = c(lowerBound, upperBound)) +

# ---- Violin plots


# ---- PROBABILITY DISTRIBUTIONS
{
  # ---- Histograms (NOT WORKING; I HATE YOU HISTOGRAMS !!!!!! ) Working now?
  {



  # TODO: Delete, depecrated, fix the categorical too!
  doHistogramPlot <- function(tableBase, variableIndex, plotFilePath, totalBins = NULL, binsWidth = NULL,
                              plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                              overrideTableName = NULL){

    # Init variables
    myPlotType = "Histogram"
    myTableName = deparse(substitute(tableBase))

    if(!is.null(overrideTableName)){
      myTableName = overrideTableName

    }

    # Get an automatic name if you don't have a proper one
    genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName=myTableName, plotType = myPlotType, variableIndex1 = variableIndex )
    imageFilePath   = paste(genericFilePath,".png",sep="")
    tableFilePath   = paste(genericFilePath,".txt",sep="")
    latexFilePath   = paste(genericFilePath,".tex",sep="")

    # Get info about different categories and variables
    groupingName   = colnames(tableBase)[variableIndex]
    #isIntegerData  = all( myIsInteger(tableBase[,variableIndex]) , na.rm= TRUE)

    # What to do with the number of bins, and wide of each bin
    # ---- If neither of then are not initialize
    if(is.null(totalBins) && is.null(binsWidth)){

      totalBins  = 30
      binsWidth  = 0

    }

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

        totalBins  = 30
        binsWidth  = 0

      }

      else{

        # ---- If only total number of bins is init.
        if(!is.null(totalBins)){

          binsWidth = 0

        }

        else{

          # ---- If only bin width is init.
          totalBins  = 0

        }

      }

    }


    # Figure it out where to put the breaks for the X axys
    minimumValue = min(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
    maximumValue = max(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
    deltaValue   = maximumValue - minimumValue

    if(binsWidth ==0){
      binsWidth = deltaValue/totalBins
    }

    # xBreaksEvery = 0

    leftLimit      = minimumValue - deltaValue * 0.1
    rightLimit     = maximumValue + deltaValue * 0.1

    # extraDelta     = rightLimit - leftLimit
    # totalExtraBins = ceiling(extraDelta/binsWidth)

    if(totalBins!=0){

      # If we only have one value repeated over and over, we have only 1 bin
      if(deltaValue == 0){

        breakList    = c(minimumValue)

      }
      else{
        #totalBreaks  = totalBins + 1
        #totalBreaks  = totalExtraBins - 12
        #xBreaksEvery = deltaValue/totalBreaks
        breakList     = round( c ( seq(minimumValue, maximumValue , by = deltaValue/(totalBins - 1) ) ) ,2)

        #binLength    = deltaValue/totalBreaks
        #breakList    = round(seq(minimumValue, maximumValue , xBreaksEvery),2)
        #breakList    = round(seq(leftLimit, rightLimit , xBreaksEvery),2)

        # print("0 -- MIN -----")
        # print(minimumValue)
        # print(maximumValue)
        # print("A -- LIMIT -----")
        # print(leftLimit)
        # print(rightLimit)
        # # print("B -- DELTA -----")
        # # print(deltaValue)
        # # print(extraDelta)
        # print("C -- TOTAL BINS -----")
        # print(totalBins)
        # # print(totalExtraBins)
        # # print("D -- BREAKS -----")
        # # totalBreaks
        # print("D -------")
        # print(breakList)
        # print(length(breakList))

        # # Override if you have integer data
        # if( (isIntegerData==TRUE) && (xBreaksEvery < 1)){
        #   xBreaksEvery = 1
        # }

      }

    }
    else{

      xBreaksEvery = binsWidth

    }


    # Histogram if you give total of bins
    if(totalBins !=0){

      print("Histogram for totalbins")

      ggplot(data=tableBase, aes(as.integer(tableBase[,variableIndex]))) +

        #geom_histogram(breaks = breakList, bins = totalBins, fill="black", col="grey") +
        geom_histogram(bins = totalBins, fill="black", col="grey") +

        #coord_cartesian(xlim=c(leftLimit,rightLimit)) +
        coord_cartesian(xlim=c(minimumValue,maximumValue)) +

        stat_bin(aes(y=..count.., label=..count..), bins = totalBins, geom="text", vjust=-.5) +

        #scale_y_continuous(breaks = seq(0, 15, by = 1)) +

        # Create titles and subtitles
        labs(title    = plotTitle,
             subtitle = plotSubtitle,
             caption  = plotCaption,
             color    = groupingName,
             x = plotXLabel, y = plotYLabel)

    }

    # Histogram if you give binwidth
    else{

      totalBins = ceiling(deltaValue/binsWidth) + 1

      ggplot(data=tableBase, aes(as.integer(tableBase[,variableIndex]))) +

        geom_histogram(binwidth = binsWidth, fill="black", col="grey") +

        scale_x_continuous(breaks = seq(minimumValue - 1 , maximumValue , xBreaksEvery), lim = c(minimumValue - 1 , maximumValue)) +

        #stat_bin(aes(y=..count.., label=..count..), bins = totalBins, geom="text", vjust=-.5) +

        #scale_y_continuous(breaks = seq(0, 15, by = 1)) +

        # Create titles and subtitles
        labs(title    = plotTitle,
             subtitle = plotSubtitle,
             caption  = plotCaption,
             color    = groupingName,
             x = plotXLabel, y = plotYLabel)


    }




    # Save the image and the txt files with the data
    # ---- Get the image width
    imageWidth = 8
    if(totalBins > 0){
      imageWidth = totalBins/2
    }

    # ---- Save the image
    ggsave(imageFilePath, width = imageWidth)
    writeHistogramPlotDataLATEX(tableBase, variableIndex, latexFilePath)


  }

  }
  # ---- QQ plots
  # (include shapiro test? in the plot)

  # QQ-plot




  # ---- Density plots
  {





  }
}
# ---- REGRESSIONS
{
# TODO: Add density plots in each axis
# TODO: Add the shapiro test? for normality in each density

# Get two numerical columns and make the regression between then
doSimpleRegression <- function(tableBase, independentColumnIndex, dependentColumnIndex, plotFilePath,
                               plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Get info about the variable
  numericalNameA = colnames(tableBase)[independentColumnIndex]
  numericalNameB = colnames(tableBase)[dependentColumnIndex]
  imageWidth     = 3

  # Prepare the defaults
  defaultVector  = getBiNumericalDefaults(numericalNameA, numericalNameB, 1, NULL, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
  colorsVector   = defaultVector[[1]]
  plotTitle      = defaultVector[[2]][1]
  plotSubtitle   = defaultVector[[3]][1]
  plotCaption    = defaultVector[[4]][1]
  plotXLabel     = defaultVector[[5]][1]
  plotYLabel     = defaultVector[[6]][1]

  # Init basic variables
  returnR2 = 0
  myModel  = 0

  # Find how many samples we have
  totalSampleIndependent = sum(!is.na(tableBase[,independentColumnIndex]))
  totalSampleDependent   = sum(!is.na(tableBase[,dependentColumnIndex]))

  # If we have enought do the plot, otherwise, skip the whole process
  if( totalSampleIndependent >= 2 && totalSampleDependent >= 2){

    # Check that the values match in the same position of the vector
    tableComplete <- sum((!is.na(tableBase[,independentColumnIndex])) & (!is.na(tableBase[,dependentColumnIndex])))

    if(tableComplete >=2 ){

      doPlots = TRUE

    }
    else{
      doPlots = FALSE

      print("-------------------------------------------------------")
      print("I can't make the regression of these two variables")
      print("There are enought samples in each variable group, but they don't coincide in the same row")
      print("")
      print("For example:")
      print("  A  |  B  |")
      print(" Na  |  1  |")
      print(" Na  |  2  |")
      print("  3  |  Na  |")
      print("  4  |  Na  |")
      print("-------------------------------------------------------")
      print("Columns:")
      print(paste("independent:",independentColumnIndex))
      print(paste("dependent:"  ,dependentColumnIndex))
      print("Total data found:")
      print(totalSampleIndependent)
      print(totalSampleDependent)
      print("-------------------------------------------------------")

    }


  }
  else{
    doPlots <- FALSE

    print("-------------------------------------------------------")
    print("I can't make the regression of these two variables")
    print("I don't have enough samples, need at least 2 for each variable")
    print("-------------------------------------------------------")
    print("Columns:")
    print(paste("independent:",independentColumnIndex))
    print(paste("dependent:"  ,dependentColumnIndex))
    print("Total data found:")
    print(totalSampleIndependent)
    print(totalSampleDependent)
    print("-------------------------------------------------------")
  }

  # If we have enought points...
  if(doPlots){

    # Make the X² + X + C model
    myModel = lm(tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex] + I(tableBase[,independentColumnIndex]^2), tableBase)

    # Get the R2 value
    myR2value = summary(myModel)$r.squared

    # The returning value have all the digits
    returnR2  = myR2value

    # The value that we are going to print in the plot has only 2 decimals (ie R^2 = 0.94)
    myR2value = format(myR2value, digits = 2)
    r2String  = paste("R2: ", myR2value, sep = " ")


    # Do a scatter plot
    #--------------------------------------------------------------------
    ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex])) +

      # With hollow points
      geom_point(shape=1) +

      # With confidence interval of 95%
      geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +

      # Add the R2 text
      geom_text(aes(x=-Inf,y=Inf,hjust=0,vjust=1, label= r2String), color="red") +

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           x = plotXLabel, y = plotYLabel)

    ggsave(plotFilePath, width = 8)

  }

  return (myModel)

}

# Get two numerical columns and make the regression between then
# But also highlights the points with a third given variable that needs to be categorical
doSimpleColorRegression <- function(tableBase, independentColumnIndex, dependentColumnIndex, colorCodeIndex, colorsVector,  plotFilePath,
                                    plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Get info about the variable
  numericalNameA = colnames(tableBase)[independentColumnIndex]
  numericalNameB = colnames(tableBase)[dependentColumnIndex]
  groupingName   = colnames(tableBase)[colorCodeIndex]
  myCategories   = unique(as.character(tableBase[,groupIndex]))
  nCategories    = length(myCategories)

  imageWidth     = 3

  # Prepare the defaults
  defaultVector = getBiNumericalDefaults(numericalNameA, numericalNameB, nCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
  colorsVector  = defaultVector[[1]]
  plotTitle     = defaultVector[[2]][1]
  plotSubtitle  = defaultVector[[3]][1]
  plotCaption   = defaultVector[[4]][1]
  plotXLabel    = defaultVector[[5]][1]
  plotYLabel    = defaultVector[[6]][1]

  # Init basic variables
  returnR2 = 0
  myModel  = 0

  # Find how many samples we have
  totalSampleIndependent <- sum(!is.na(tableBase[,independentColumnIndex]))
  totalSampleDependent   <- sum(!is.na(tableBase[,dependentColumnIndex]))

  # If we have enought do the plot, otherwise, skip the whole process
  if( totalSampleIndependent >= 2 && totalSampleDependent >= 2){

    # Check that the values match in the same position of the vector
    tableComplete <- sum((!is.na(tableBase[,independentColumnIndex])) & (!is.na(tableBase[,dependentColumnIndex])))

    if(tableComplete >=2 ){

      doPlots <- TRUE

    }
    else{
      doPlots <- FALSE

      print("-------------------------------------------------------")
      print("I can't make the regression of these two variables")
      print("There are enought samples in each variable group, but they don't coincide in the same row")
      print("")
      print("For example:")
      print("  A  |  B  |")
      print(" Na  |  1  |")
      print(" Na  |  2  |")
      print("  3  |  Na  |")
      print("  4  |  Na  |")
      print("-------------------------------------------------------")
      print("Columns:")
      print(paste("independent:",independentColumnIndex))
      print(paste("dependent:"  ,dependentColumnIndex))
      print("Total data found:")
      print(totalSampleIndependent)
      print(totalSampleDependent)
      print("-------------------------------------------------------")

    }


  }
  else{
    doPlots <- FALSE

    print("-------------------------------------------------------")
    print("I can't make the regression of these two variables")
    print("I don't have enough samples, need at least 2 for each variable")
    print("-------------------------------------------------------")
    print("Columns:")
    print(paste("independent:",independentColumnIndex))
    print(paste("dependent:"  ,dependentColumnIndex))
    print("Total data found:")
    print(totalSampleIndependent)
    print(totalSampleDependent)
    print("-------------------------------------------------------")
  }

  # If we have enought points...
  if(doPlots){

    # Make the X² + X + C model
    myModel <- lm(tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex] + I(tableBase[,independentColumnIndex]^2), tableBase)

    # Get the R2 value
    myR2value = summary(myModel)$r.squared

    # The returning value have all the digits
    returnR2  = myR2value

    # The value that we are going to print in the plot has only 2 decimals (ie R^2 = 0.94)
    myR2value = format(myR2value, digits = 2)
    r2String  = paste("R2: ", myR2value, sep = " ")

    # Do a scatter plot
    #--------------------------------------------------------------------
    ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex])) +

      # With hollow points
      #geom_point(shape=1) +
      geom_point(aes(color = tableBase[,colorCodeIndex])) +

      # With confidence interval of 95%
      geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
      scale_color_brewer(palette="Set1") +

      # Add the R2 text
      geom_text(aes(x=-Inf,y=Inf,hjust=0,vjust=1, label= r2String), color="red") +

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           color    = groupingName,
           fill     = groupingName,
           x = plotXLabel, y = plotYLabel)


    ggsave(plotFilePath, width = 8)

  }

  return (myModel)

}

# Get two numerical columns and make the regression between then
# But also divides the data into groups, given by a third given variable that needs to be categorical
#
# (str) borders. Gives you the option to put a summary of the variables in each side of the plot
#           - density
#           - histogram TODO:
#           - boxplot   TODO:
doCombineRegression <- function(tableBase, independentColumnIndex, dependentColumnIndex, groupIndex, colorsVector, plotFilePath, borders = NULL,
                                plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Init variables
  myPlotType = "CombineRegression"
  myTableName = deparse(substitute(tableBase))

  # Get an automatic name if you don't have a proper one
  genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName=myTableName, plotType = myPlotType, variableIndex1 = independentColumnIndex , variableIndex2 = dependentColumnIndex, variableIndex3 = groupIndex )
  imageFilePath   = paste(genericFilePath,".png",sep="")
  tableFilePath   = paste(genericFilePath,".txt",sep="")
  latexFilePath   = paste(genericFilePath,".tex",sep="")


  # This is what you are going to return at the end of the fuction
  # Init to -2 means that the analysis was not performed due not having enought categories
  rStringVector = -2

  # First, get rid of the NA values
  #tableBase = tableBase[!is.na(tableBase[,groupIndex]),]

  # Get info about different categories and variables
  myCategories   = unique(as.character(tableBase[,groupIndex]))
  nCategories    = length(myCategories)
  numericalNameA = colnames(tableBase)[independentColumnIndex]
  numericalNameB = colnames(tableBase)[dependentColumnIndex]
  groupingName   = colnames(tableBase)[groupIndex]
  imageWidth     = 3

  # print("------ COLORS ------")
  # print(colorsVector)
  # print("------------")

  # Prepare the defaults
  defaultVector = getBiNumericalDefaults(numericalNameA, numericalNameB, nCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
  colorsVector  = defaultVector[[1]]
  plotTitle     = defaultVector[[2]][1]
  plotSubtitle  = defaultVector[[3]][1]
  plotCaption   = defaultVector[[4]][1]
  plotXLabel    = defaultVector[[5]][1]
  plotYLabel    = defaultVector[[6]][1]

  print("bb")

  # Check out if we have enought categories to do a combine regression
  if(nCategories < 2){

    print( "WARNING!! Can't create a regression with different groups")
    print( "Only one categorie was found")
    print( "To make a regression with only one category, use the doSimpleRegression() function instead")
    print( "Conflicting column name: ")
    print( groupingName )

  }
  else{

    # Create as many substets as there are categories, and put them into this list
    subsetsDF     = rep(list(data.frame(NULL)), nCategories)

    for(i in 1:nCategories){

      subsetsDF[[i]] = subset(tableBase, tableBase[,groupIndex] == as.character(myCategories[i]))

    }

    # Find the regression models
    # ---- We have nCategories regression models.
    # ---- We are going to put them into a vector of nCategories
    rValuesVector = rep(0,       nCategories)
    rStringVector = rep(2,       nCategories)
    rModelsVector = rep(list(0), nCategories)

    # ---- Now do all the possible combinations
    doPlots = TRUE

    # print("----------")
    # print(nCategories)
    # print("----------")

    for(i in 1:nCategories){

      # Only continue, if we didn't have troubles with any other variable
      if(doPlots == TRUE){

        # Find how many samples we have
        totalSampleIndependent = sum(!is.na(tableBase[,independentColumnIndex]))
        totalSampleDependent   = sum(!is.na(tableBase[,dependentColumnIndex]))

        # print("----------")
        # print(i)
        # print(totalSampleIndependent)
        # print(totalSampleDependent)
        # print("----------")

        # If we have enought do the plot, otherwise, skip the whole process
        if( totalSampleIndependent >= 2 && totalSampleDependent >= 2){

          # Check that the values match in the same position of the vector
          tableComplete = sum((!is.na(subsetsDF[[i]][,independentColumnIndex])) & (!is.na(subsetsDF[[i]][,dependentColumnIndex])))

          # print(tableComplete)
          # print("----------")
          # print(!is.na(tableBase[,independentColumnIndex]))
          # print((!is.na(tableBase[,dependentColumnIndex])))
          # print("----------")
          # print(tableBase[,independentColumnIndex])
          # print(tableBase[,dependentColumnIndex])


          if(tableComplete >=2 ){

            # print(head(subsetsDF[[i]][,dependentColumnIndex]))

            # Make the X² + X + C model
            myModel            = lm(subsetsDF[[i]][,dependentColumnIndex] ~ subsetsDF[[i]][,independentColumnIndex] + I(subsetsDF[[i]][,independentColumnIndex]^2), subsetsDF[[i]])
            rModelsVector[[i]] = myModel

            # Get the R2 value
            myR2value = summary(myModel)$r.squared

            # The returning value have all the digits
            rValuesVector[i]  = myR2value

            # The value that we are going to print in the plot has only 2 decimals (ie R^2 = 0.94)
            myR2value        = format(myR2value, digits = 2)
            rStringVector[i] = paste("R2: ", myR2value, sep = " ")

            # print("------ R2 ----")
            # print(myR2value)
            # print("--------------")

          }
          else{
            doPlots = FALSE

            print("-------------------------------------------------------")
            print("I can't make the regression of this variable")
            print(colnames(tableBase)[groupIndex])
            print("For this category")
            print(myCategories[i])
            print("There are enought samples in each variable group... \n
                   but they don't coincide in the same row")
            print("")
            print("For example:")
            print("  A  |  B  |")
            print(" Na  |  1  |")
            print(" Na  |  2  |")
            print("  3  |  Na  |")
            print("  4  |  Na  |")
            print("-------------------------------------------------------")
            print("Columns:")
            print(paste("independent column:"  ,independentColumnIndex))
            print(paste("dependent column:  "  ,dependentColumnIndex))
            print(paste("grouping by index:"  ,groupIndex))
            print("Total data found:")
            print(totalSampleIndependent)
            print(totalSampleDependent)
            print("-------------------------------------------------------")

          }


        }
        else{
          doPlots <- FALSE

          print("-------------------------------------------------------")
          print("I can't make the regression of these two variables")
          print("I don't have enough samples, need at least 2 for each variable")
          print("-------------------------------------------------------")
          print("Columns:")
          print(paste("independent:"  ,independentColumnIndex))
          print(paste("dependent:  "  ,dependentColumnIndex))
          print(paste("grouping by:"  ,groupIndex))
          print("Total data found:")
          print(totalSampleIndependent)
          print(totalSampleDependent)
          print("-------------------------------------------------------")
        }




      }

    }

    # Add the R2 text
    #geom_text(aes(x=-Inf,y=Inf,hjust=0,vjust=1, label=r2StringAlpha), color="red", parse = TRUE) +
     # geom_text(aes(x=-Inf,y=Inf,hjust=0,vjust=3, label=r2StringBeta), color="blue", parse = TRUE) +

    # At this point, if none of the categories failed, we have all the R2 values and models ready
    if(doPlots == TRUE){

      # Prepare the dataframe with the horizontal lines
      totalRows              = nCategories
      horizontalDF           = data.frame(matrix(NA, nrow = totalRows, ncol = 3))
      colnames(horizontalDF) = c("Level", "R2String", "Category")

      for(i in 1:totalRows){

        horizontalDF$Level[i]    = 2*i - 1
        horizontalDF$R2String[i] = rStringVector[i]
        horizontalDF$Category[i] = myCategories[i]

      }

      # print("Before plot")
      # print("-----------")
      # print(colorsVector)
      # print(nCategories)

      print("aa")

      # Do the basic plot
      regressionPlot = ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex], color=tableBase[,groupIndex])) +

             scale_color_manual(values=colorsVector) +

             # With hollow points
             geom_point(shape=1) +
             # With confidence interval of 95%
             geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +

             # Add the R2 strings
             geom_text(data = horizontalDF, aes(x=-Inf,y=Inf,hjust=0,vjust=Level, label=R2String, color = Category )) +

             # Create titles and subtitles
             labs(title    = plotTitle,
                  subtitle = plotSubtitle,
                  caption  = plotCaption,
                  color    = groupingName,
                  x = plotXLabel, y = plotYLabel)


      print("cc")


      # print("Before extras")
      # print("-----------")

      # If the user want to display extra info
      if(!is.null(borders)){

        validOption = FALSE

        # If the user want to plot a density plot in each side
        if(borders=="density"){

          xAxysPlot = doCategoricalDensityPlot(tableBase, groupIndex, independentColumnIndex, colorsVector, borderPlot = TRUE,
                                               plotTitle = "", plotSubtitle = "", plotCaption = "", plotXLabel = "", plotYLabel = "")

          yAxysPlot = doCategoricalDensityPlot(tableBase, groupIndex, dependentColumnIndex,   colorsVector, rotatePlot = TRUE,
                                               plotTitle = "", plotSubtitle = "", plotCaption = "", plotXLabel = "", plotYLabel = "")

          validOption = TRUE
        }

        # If we don't understand what the user want, then put a warning about it
        else{

          print("WARNING!: Doing the regression plot")
          print("You choose this option to add information to the regression plot:")
          print(borders)
          print("I don't know what this option means, you can use these options:")
          print("- density ")
          print("- histogram ")
          print("- boxplot ")
          print("For now, I'm going to make the plot as if you didn't choose any border option")


        }

        # If whatever the user want to display is actually valid
        # Build the plot and save
        if(validOption == TRUE){

          # print("Arranging all")
          # print("-----------")

          ggarrange( xAxysPlot, NULL,
                     regressionPlot, yAxysPlot,

                     ncol = 2, nrow = 2,  align = "hv",
                     widths = c(2, 1), heights = c(1, 2),
                     common.legend = TRUE)


          # print("After Arranging all")
          # print("-----------")

        }


      }

      print("Before saving")
      print("-----------")

      ggsave(imageFilePath)

    }

  }

  return(rStringVector)

}

}
# Do combine simple numerical plots (WHAT?)
# ----

# Do special plots
{



# ---- Supersummary plot (weird one)
doTableplot <- function(tableBase, plotFilePath, plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Init variables
  myPlotType   = "TablePlot"
  myTableName  = deparse(substitute(tableBase))
  totalColumns = length(colnames(tableBase))
  imageWidth   = totalColumns #* 2
  imageHeight  = 7                     # This equals 2000px aprox, we don't really need for the image to be high

  # Get an automatic name if you don't have a proper one
  genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName = myTableName, plotType = myPlotType)

  if(genericFilePath != plotFilePath){ # If you got a new file path, it means that you inputed a folder, thus you need to add extensions

    imageFilePath   = paste(genericFilePath,".png",sep="")
    tableFilePath   = paste(genericFilePath,".txt",sep="")
    latexFilePath   = paste(genericFilePath,".tex",sep="")

  }

  myTablePlot  = tableplot(tableBase)

  print(imageFilePath)

  writeTablePlotDataLATEX(myTableName, latexFilePath)
  tableSave(myTablePlot, filename = imageFilePath, width = imageWidth, height = imageHeight)

}



# ---- Correlogram

# Parameters:
#
# (str) shape:
#
#           - square
#           - circle
#
# (str) triangular:
#
#           - full
#           - lower
#           - upper
#
doCorrelogram <- function(tableBase, plotFilePath, clusterOption = TRUE, shape = "square", triangular = "full", plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  correlationMatrix = round(cor(tableBase), 1)

  clusteringTest = sum(correlationMatrix)

  if(is.na(clusteringTest) && clusterOption == TRUE){

    print("WARNING!: In correlogram")
    print("You told me to cluster the results, but some of the values")
    print("in the resulting correlation matrix are NA/NaN/Infinity")
    print("I can't cluster with those numbers")
    print("So I'm turning off the clustering option")

    clusterOption = FALSE

  }

  ggcorrplot(correlationMatrix, hc.order = clusterOption, method = shape, type = triangular,
             lab = TRUE, colors = c("#E46726", "white", "#6D9EC1"))



  ggsave(plotFilePath, width = 8)

}

}

# ---- NETWORKS
# -------- Option that does everything:
#            Network plot (all usefull combination)
#            Histogram with the number of incoming edges
#            Histogram with the number of outcoming edges
#            Density (histogram?) with the out/in ratio
#            Density (histogram?) with the in/out ratio
#            Reciprocity (both nodes connected) (histogram)
#            Centrality (how many steps to reach every other node from one particular node)
#                        (density with the averages of all connections)

# https://kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf
# Do multiple plots with the same layout and nodes, but different edges:
# Page 45
# The proportion of present edges from all possible edges in the network.
# edge_density(myGraph)


# Add information about the degree of each node to the nodes dataframe
# -- All degree
# -- In degree
# -- Out degree
# -- Reciprocity
# -- In/All ratio
# -- Out/All ratio
# -- In/Out ration
updateDegrees <- function(edgesDF, nodesDF){

  # Create the updated object
  newNodesDF = nodesDF

  # Create the plot with the info we have right now
  myGraph = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = TRUE)

  # Count the different degrees and put it into the new dataframe
  newNodesDF$allDegree   = degree(myGraph, mode = "all")
  newNodesDF$inDegree    = degree(myGraph, mode = "in")
  newNodesDF$outDegree   = degree(myGraph, mode = "out")
  newNodesDF$InAllRatio  = newNodesDF$inDegree  / newNodesDF$allDegree
  newNodesDF$OutAllRatio = newNodesDF$outDegree / newNodesDF$allDegree
  newNodesDF$InOutRatio  = newNodesDF$inDegree  / newNodesDF$outDegree

  # Finally, find the reciprocity (we do it by hand, apparently there is no function for this in igraph yet)
  newNodesDF$ReciprocityAbs   = -1 # How many reciprocal relationship you have
                                   # if you have 0 outbounds, this is set to -1
                                   # if you have 0 inbounds, this is set to -2
  newNodesDF$ReciprocityRatio = -1 # How many reciprocal relationships you have in comparison with all your relationships
                                   # if you have 0 relationships in total, this is set to -1

  totalNodes = nrow(nodesDF)

  # For each nodes
  for (i in 1:totalNodes) {

    myNodeID = nodesDF$name[i]

    # Find the list of out-friends for this node
    myOutFriends    = subset(edgesDF, from == myNodeID)
    totalOutFriends = nrow(myOutFriends)

    # You have selected no friends
    if(totalOutFriends == 0){

      newNodesDF$ReciprocityAbs[i] = -1 # Default, number, so you don't need to actually do anything
                                        # I'm just leaving this code here in case we need to update that someday


    }

    # In any other case
    else{

      # Find the list of in-friends for this node
      myInFriends    = subset(edgesDF, to == myNodeID)
      totalInFriends = nrow(myInFriends)

      # Init the common friends variable, as in right now, you don't have any reciprocication
      # This is done like this to optimize code
      totalCommon = 0

      # You don't have any friends :-/
      if(totalInFriends == 0){

        newNodesDF$ReciprocityAbs[i] = -2

      }

      # You have out friends, and in friends, so now we intersect both sets, and find the number
      else{

        setA = myOutFriends$to
        setB = myInFriends$from

        myCommonFriends    = intersect(setA,setB)
        totalCommon        = length(myCommonFriends)

        newNodesDF$ReciprocityAbs[i] = totalCommon

      }

      # Finally, we find the ratio of reciprocity with the rest of your relationships
      totalRelationships = newNodesDF$allDegree[i]

      if(totalRelationships>0){
        newNodesDF$ReciprocityRatio[i]  = totalCommon/totalRelationships
      }

    }

  }

  return(newNodesDF)

}



# Do all the histograms that analyze the degree of nodes
doDegreePlots <- function(edgesDF, nodesDF, folderPath, categoricalVariableIndex = NULL, colorsVector = NULL,
                          plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Get info about different categories
  myCategories = unique(nodesDF[,categoricalVariableIndex])
  nCategories  = length(myCategories)
  groupingName = colnames(nodesDF)[categoricalVariableIndex]
  imageWidth   = 8

  # Prepare the defaults
  defaultVector = getCategoricalDefaults(groupingName, myCategories, nCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
  colorsVector  = defaultVector[[1]]
  plotTitle     = defaultVector[[2]][1]
  plotSubtitle  = defaultVector[[3]][1]
  plotCaption   = defaultVector[[4]][1]
  plotXLabel    = defaultVector[[5]][1]
  plotYLabel    = defaultVector[[6]][1]

  # Make the plot

  # Add the information about the several degrees variables into the nodes DF
  nodesDF$All = degree()




  # If the categorical variable is null, we plot just plot the histograms with the summary
  # This is done in all the cases



  # If we do have  a categorical variable, plot the histograms and density plots


}



# given a list of from IDs and to IDs, analyze how many of them
# are nominations, receivers, and reciprocal
#
# Return the 3 analysis dataframes
getTotalConnections <- function(edgesDF, listOfFroms, listOfTos){

  # Create the needed dataframes
  {
    totalFroms = length(listOfFroms)
    totalTos   = length(listOfTos)
    
    # Dataframe with the FROMS
    fromDF       = data.frame(matrix(NA, nrow = totalFroms, ncol = 2))
    colnames(fromDF) = c("From","Total")
    
    # Dataframe with the TOS
    toDF         = data.frame(matrix(NA, nrow = totalTos, ncol = 2))
    colnames(toDF) = c("To","Total")
    
    # Dataframe with the reciprocals FROMS TO
    reciprocalDF = data.frame(matrix(NA, nrow = (totalFroms * totalTos), ncol = 3))
    colnames(reciprocalDF) = c("From", "To","Total")
  }
  
  # Analyze stuff
  # ---- FROMS
  for(i in 1:totalFroms){
    
    currentFrom = listOfFroms[i]
    
    # Add the row to the data frame
    fromDF[i,1] = currentFrom
    
    # Count
    fromDF[i,2] = sum(edgesDF[,1] == currentFrom)
    
  }
  # ---- TOS
  for(i in 1:totalTos){
    
    currentTo = listOfTos[i]
    
    # Add the row to the data frame
    toDF[i,1] = currentTo
    
    # Count
    toDF[i,2] = sum(edgesDF[,2] == currentTo)
    
  }
  # ---- RECIPROCALS
  currentIndex = 1
  for(i in 1:totalFroms){
    
    currentFrom = listOfFroms[i]
    
    for(j in 1:totalTos){
      
      currentTo   = listOfTos[j]
      
      reciprocalDF[currentIndex,1] = currentFrom
      reciprocalDF[currentIndex,2] = currentTo
        
      # Select the Froms
      temp = edgesDF[edgesDF[,1] == currentFrom,]
      # From those Froms, select the Tos and add
      reciprocalDF[currentIndex,3] = sum(temp[,2] == currentTo)
      
      currentIndex = currentIndex + 1
      
    }
  }
  
  
  # Return the dataframes
  myReturn = vector("list", length = 3)
  myReturn[[1]] = fromDF
  myReturn[[2]] = toDF
  myReturn[[3]] = reciprocalDF
  
  return (myReturn)
  
}







new_homophily <- function(graph, vertex.attr) {
  V(graph)$name <- vertex_attr(graph, vertex.attr)
  edges <- get.data.frame(graph)

  # heterophilous ties where vertices have different `"group"` attributes
  external <- length(which(edges$from != edges$to))

  # homophilous ties where vertices have the same `"group"` attributes
  internal <- length(which(edges$from == edges$to))

  list(
    n_external = external,
    n_internal = internal,
    prop_external = external / nrow(edges), # proportion of ties that are heterophilous
    prop_internal = internal / nrow(edges), # proportion of ties that are homophilous (the results of your initial function)
    ei_index = (external - internal) / nrow(edges) # (EL - IL) / (EL + IL)
  )
}






# ERGM
# https://youtu.be/3mwkO4u0yKE?t=3832
# https://bookdown.org/markhoff/social_network_analysis/homophily-and-exponential-random-graphs-ergm.html

ERGMAnalysys <- function( myGraph ){

  myNetwork    = asNetwork(myGraph)
  random_graph = ergm(myNetwork ~ edges)
  theta        = random_graph$coef

  set.seed(1234)
  hundred_simulations = simulate(random_graph,
                                  coef = theta,
                                  nsim = 100,
                                  control = control.simulate.ergm(MCMC.burnin   = 1000,
                                                                  MCMC.interval = 1000))



  #par(mfrow = c(3, 3))
  #sapply(hundred_simulations[1:9], plot, vertex.cex = 1, vertex.col = "tomato")

  net_densities = unlist(lapply(hundred_simulations, network.density))

  hist(net_densities, xlab = "Density", main = "", col = "lightgray")
  abline(v = network.density(myNetwork), col = "red", lwd = 3, lty = 2)
  abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 1)

  gof_stats <- gof(random_graph,
                   control.gof.formula(nsim = 100),
                   coef = theta)

  par(mfrow = c(2, 3))
  plot(gof_stats, main = '')

  model1 = ergm(myNetwork ~ edges +
                nodematch("NasalCarrier") + nodematch("ThroatCarrier"),
                control = control.ergm(seed=1,MCMC.samplesize=4096,MCMC.interval=8192),verbose=T)



  summary(model1)

  #mcmc.diagnostics(model1)

}


ERGMAnalysys2 <- function( myGraph ){

  myNetwork    = asNetwork(myGraph)
  random_graph = ergm(myNetwork ~ edges)
  theta        = random_graph$coef

  set.seed(1234)
  hundred_simulations = simulate(random_graph,
                                 coef = theta,
                                 nsim = 100,
                                 control = control.simulate.ergm(MCMC.burnin   = 1000,
                                                                 MCMC.interval = 1000))

  net_densities = unlist(lapply(hundred_simulations, network.density))

  hist(net_densities, xlab = "Density", main = "", col = "lightgray")
  abline(v = network.density(myNetwork), col = "red", lwd = 3, lty = 2)
  abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 1)

  gof_stats <- gof(random_graph,
                   control.gof.formula(nsim = 100),
                   coef = theta)

  par(mfrow = c(2, 3))
  plot(gof_stats, main = '')

  model1 = ergm(myNetwork ~ edges +
                  nodematch("Snuff", diff = T) +
                  nodematch("Sex", diff = T)
                )



  summary(model1)


# model2 <- ergm(statnet37 ~ edges +
#                  nodematch("race", diff = T, levels = c("1", "2", "5")) +
#                  nodematch("sex", diff = T, levels = c("1", "2")) +
#                  nodematch("grade", diff = T, levels = as.character(c(7:12))))

}
# Check this , chapter 8, for network vis http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html


# ---- TIME SERIES
# -------- Line plots
doTimeSerie <- function(tableBase, timeIndex, variablesIndexList, colorVector, plotFilePath,
                        plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

    # Get info about different variables
    nVariables   = length(variablesIndexList)
    groupingName = colnames(tableBase)[variablesIndexList]
    imageWidth   = 8

    # Prepare the defaults
    defaultVector = getMultiNumericalDefaults(NULL, nVariables, colorVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)

    colorsVector  = defaultVector[[1]]
    plotTitle     = defaultVector[[2]][1]
    plotSubtitle  = defaultVector[[3]][1]
    plotCaption   = defaultVector[[4]][1]
    plotXLabel    = defaultVector[[5]][1]
    plotYLabel    = defaultVector[[6]][1]

    # Make a subset of the given dataframe with the given columns and melt it
    subTable    = tableBase[c(timeIndex,variablesIndexList)]
    idVariable  = colnames(subTable)[1]
    meltedTable = melt(subTable, id.vars = idVariable)

    print(meltedTable)
    print(idVariable)

    # Do the base plot
    basePlot = ggplot(meltedTable, aes(x =  meltedTable[,1], y = meltedTable[,3], color = meltedTable[,2])) +

               geom_line() +

               # Create titles and subtitles
               labs(title    = plotTitle,
                    subtitle = plotSubtitle,
                    caption  = plotCaption,
                    color    = groupingName,
                    fill     = groupingName,
                    x = plotXLabel, y = plotYLabel)



    # Save everything
    ggsave(plotFilePath, width = imageWidth)


}

# -------- Bar plots with option for sorting
doBarSerie  <- function(tableBase, timeIndex, variablesIndexList, colorVector, plotFilePath, sortIndex = NULL, decreasingOrder = FALSE,
                        plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Get info about different variables
  nVariables   = length(variablesIndexList)
  groupingName = colnames(tableBase)[variablesIndexList]
  imageWidth   = 10

  # Prepare the defaults
  defaultVector = getMultiNumericalDefaults(NULL, nVariables, colorVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)

  colorsVector  = defaultVector[[1]]
  plotTitle     = defaultVector[[2]][1]
  plotSubtitle  = defaultVector[[3]][1]
  plotCaption   = defaultVector[[4]][1]
  plotXLabel    = defaultVector[[5]][1]
  plotYLabel    = defaultVector[[6]][1]


  # Reorder by given variable if any
  if(!is.null(sortIndex)){

    tableBase[,timeIndex] = factor(tableBase[,timeIndex], levels = tableBase[order(tableBase[,sortIndex], decreasing = decreasingOrder), timeIndex])

  }


  # Make a subset of the given dataframe with the given columns and melt it
  subTable    = tableBase[c(timeIndex,variablesIndexList)]
  idVariable  = colnames(subTable)[1]
  meltedTable = melt(subTable, id.vars = idVariable)


  # Do the base plot
  basePlot = ggplot(meltedTable, aes(x =  meltedTable[,1], y = meltedTable[,3], fill = meltedTable[,2])) +

  #basePlot = ggplot(meltedTable, aes(x =  reorder(meltedTable[,1], meltedTable[,1], order(tableBase[,firstVariable])), y = meltedTable[,3], fill = meltedTable[,2])) +

    # Columns
    geom_col(position=position_dodge()) +

    # Create titles and subtitles
    labs(title    = plotTitle,
         subtitle = plotSubtitle,
         caption  = plotCaption,
         color    = groupingName,
         fill     = groupingName,
         x = plotXLabel, y = plotYLabel)



  # Save everything
  ggsave(plotFilePath, width = imageWidth)


}



# HEATMAPS

# ---- Categories vs Categories count
doCategoriesHeatmap <- function(tableBase, xIndex, yIndex, plotFilePath = NULL,
                                plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL){

  # Init image Width
  imageWidth = 8

  # Get the X/Y Categories and how many are they
  # -- First get the levels, where ARE NO NA VALUES, but a logical order is maintained
  xLevels = levels(tableBase[,xIndex])
  yLevels = levels(tableBase[,yIndex])
  # -- Now get the uniques, where MAY BE SOME NA VALUES
  xUniques = unique(tableBase[,xIndex])
  yUniques = unique(tableBase[,yIndex])
  # -- Overwrite the uniques with the levels, plus whatever is left in the unique vector.
  #    This way we keep the original order, and add any other weird things line NA, NaN, infinities, or whatever
  xUniques = union(xLevels, xUniques)
  yUniques = union(yLevels, yUniques)

  totalX = length(xUniques)
  totalY = length(yUniques)

  # Init a matrix with the total for each combination
  matrixCount = matrix(0, nrow = totalY, ncol = totalX)

  # Make a new dataframe so we can draw the heatmap
  # Category X / Category Y / Total / X coordinate / Y coordinate / Color of tile / Size for text
  totalRows              = totalX * totalY
  heatmapTable           = data.frame(matrix(NA, nrow = totalRows, ncol = 7))
  colnames(heatmapTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size" )
  absoluteCount          = nrow(tableBase)

  # Fill the dataframe
  for(i in 1:totalX){

    for(j in 1:totalY){

        # Get the current line and categories
        currentLine = (i-1)*totalY + j

        currentXCategory = xUniques[i]
        currentYCategory = yUniques[j]

        # Init the row in the dataframe
        heatmapTable$CategoryX[currentLine] = currentXCategory
        heatmapTable$CategoryY[currentLine] = currentYCategory

        # Count how many we have
        currentSubset = subset(tableBase, tableBase[,xIndex] == currentXCategory & tableBase[,yIndex] == currentYCategory)

        currentSubset = filter(tableBase, tableBase[,xIndex] == currentXCategory & tableBase[,yIndex] == currentYCategory)

        heatmapTable$Total[currentLine]     = nrow(currentSubset)

        # Set the proper X / Y coordinates
        heatmapTable$Xcoordinate[currentLine] = i
        heatmapTable$Ycoordinate[currentLine] = j

        # Set the manual color and size for the text
        heatmapTable$myColor[currentLine] = "Red"
        heatmapTable$Size[currentLine]    = 1

    }

  }

  ggplot( heatmapTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total )) +

      # The background rectangle
      geom_tile(color = "black") +
      # What is written inside the rectangle
      geom_shadowtext( aes(label= Total), color = "white", fontface = "bold") +

      # What is written in the X axys
      geom_text(aes(label = CategoryX, y = 0.3), angle = 90, hjust = 1) +
      # What is written in the Y axys
      geom_text(aes(label = CategoryY, x = 0.3), hjust = 1) +

      # Give an extra space so we can see the labels clearly
      coord_cartesian( xlim = c(-1, totalX + 1),
                       ylim = c(-1, totalY + 1)) +


      # Remove the background default lines and grey panel
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +

      # Create titles and subtitles
      labs(title    = plotTitle,
           subtitle = plotSubtitle,
           caption  = plotCaption,
           color    = "Total",
           x = plotXLabel, y = plotYLabel)

  # Save everything
  ggsave(plotFilePath)


}
