
# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)


source("load.R",    encoding="utf-8")

# Individual indexes
{
  sexIndex     = grep("Sex",             colnames(phenotypeTable))
  painIndex    = grep("Pain_Resistance", colnames(phenotypeTable))
  smokeIndex   = grep("Smoke",           colnames(phenotypeTable))
  ageIndex     = grep("Age",             colnames(phenotypeTable))
  sys1Index    = grep("SYS1",            colnames(phenotypeTable))
  sys2Index    = grep("SYS2",            colnames(phenotypeTable))
  sys3Index    = grep("SYS3",            colnames(phenotypeTable))
  bloodIndex   = grep("Blood.Test",      colnames(phenotypeTable))
  schooldIndex = grep("Shool.Program",   colnames(phenotypeTable))
  BMIIndex     = grep("BMI",             colnames(phenotypeTable))
  IDIndex      = grep("ID",              colnames(phenotypeTable))

  painNetworkIndex = grep("Pain", colnames(nodesDF))
  sexNetworkIndex  = grep("Sex",  colnames(nodesDF))  
}

# Collective indexes
numericalCollection = c(sys1Index, sys2Index, sys3Index)

# ---- BARPLOTS
# ---------------------------------

# -------- SIMPLE bar plots
# ---------------------------------
{
  # ------------ Frequency of men and women
  doBarPlot(phenotypeTable, sexIndex,  colorVectorSex,       filepath_sexAbsFrequency, 
            plotTitle = "Abs Frequency men and women",       plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Sex",        plotYLabel = "Absolute frequency")      
  
  # ------------ Frequency by type of pain
  doBarPlot(phenotypeTable, painIndex, colorVectorPain,      filepath_painResistanceAbsFrequency,
            plotTitle = "Abs Frequency of pain and no pain", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Pain group", plotYLabel = "Absolute frequency")      
  
  # ------------ Frequency of smoking and no smoking
  doBarPlot(phenotypeTable, smokeIndex, colorVectorGeneric2, filepath_smokeAbsFrequency,
            plotTitle = "Abs Frequency of smokers group", plotSubtitle = "(smoking is bad!)", plotCaption = "Source: Tromsø 7", plotXLabel = "Smoke group", plotYLabel = "Absolute frequency")
  
}

# -------- COMBINED bar plots
# ---------------------------------
{
  # ------------ Frequency of men and women and pain type side by side
  doBarDodgeCombinePlot(phenotypeTable, sexIndex, painIndex, colorVectorPain, filepath_painBySexAbsFrequency,
                        plotTitle = "Abs Frequency of pain and no pain", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Pain group", plotYLabel = "Absolute frequency")
  
  # ------------ Frequency of men and women and pain type stacked
  doBarStackCombinePlot(phenotypeTable, sexIndex, painIndex, colorVectorPain, filepath_painBySexAbsFrequency,
                        plotTitle = "Abs Frequency of pain and no pain", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Pain group", plotYLabel = "Absolute frequency")
  
}


# -------- For boxplots
{
  # -------------- 1D (This doesn't make too much sense, better use a density plot, histogram, or whatever else)
  
  # -------------- Grouped by category
  doBoxPlotPValues(phenotypeTable, sexIndex, ageIndex, colorVectorSex, filepath_ageSexAgeBoxplot,
                   plotTitle = "Something something p-values", plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL)
  
  
  # -------- For violinplots
  # -------------- 1D
  
  # -------------- Grouped by category
  
  
}

# -------- For histograms
{
  doHistogramPlot(phenotypeTable, ageIndex, filepath_ageHistogram, totalBins = 10,
                  plotTitle = "Histagram for age", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Absolute frequency")
  
  doHistogramPlot(phenotypeTable, ageIndex, filepath_ageHistogram, binsWidth = 1,
                  plotTitle = "Histagram for age", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Absolute frequency")
  
}
  
# -------- For density plots
{
  doDensityPlot(phenotypeTable, bloodIndex, ageIndex, NULL, filepath_ageBloodDensity,
                plotTitle = "Density for age by blood", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Relative frequency")
  
}

# -------- For QQ plots
# -------------- 1D

# -------------- Grouped by category



# -------- For regression
{
  # -------------- Simple 2D
  doSimpleRegression(phenotypeTable, ageIndex, sys1Index, filepath_ageSYS1Regression,
                     plotTitle = "Regression simple", plotSubtitle = "", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Absolute frequency")
  
  # -------------- Simple 2D with points divided by categories
  doSimpleColorRegression(phenotypeTable, ageIndex, sys1Index, bloodIndex, NULL,  filepath_ageSYS1BloodRegression,
                          plotTitle = "Regression with categories", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Absolute frequency")
  
  # -------------- Complex 2D Grouped by category
  doCombineRegression(phenotypeTable, ageIndex, sys1Index, bloodIndex, NULL,  filepath_ageSYS1BloodComplexRegression,
                      plotTitle = "Regression with categories", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Absolute frequency")
  
  # -------------- Complex 2D Grouped by category adding density plots in axys
  doCombineRegression(phenotypeTable, ageIndex, sys1Index, bloodIndex, NULL,  filepath_ageSYS1BloodComplexRegressionExtraDensity,
                      plotTitle = "Regression with categories and extra plots", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Something",
                      borders = "density")
  
  
}

# -------- For special plots
# ------------- BMI
doBMI(phenotypeTable, BMIIndex, sexIndex, colorVectorSex, filepath_BMIplot, plotTitle = "BMI Plot", plotSubtitle = "BMI = Mass(kg)/Height(m)^2", plotCaption = NULL, plotXLabel = "BMI", plotYLabel = "Frequency")

# ------------- Population piramid
# ------------- City population and area
# ------------- PCAs

# ------------- Summary
doTableplot(phenotypeTable, filepath_summaryPlot, plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL)

# ------------- Correlogram
doCorrelogram(continuousTable, filepath_correlogram,
              plotTitle = "Regression with categories and extra plots", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Something")

doCorrelogram(continuousTable, filepath_correlogram,
              clusterOption = FALSE, shape = "circle", triangular = "lower",
              plotTitle = "Regression with categories and extra plots", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "Age", plotYLabel = "Something")

# -------- For networks
nodesDF = updateDegrees(edgesTruncated, nodesDF)

# ------------- Get a bunch of new indexes
sizeNetworkIndex  = grep("allDegree",  colnames(nodesDF))

doGraphPlot(edgesTruncated, nodesDF, networkFolder, highlightVariable = painNetworkIndex)
doGraphPlot(edgesTruncated, nodesDF, networkFolder, highlightVariable = painNetworkIndex, rimVariable = sexNetworkIndex)
doGraphPlot(edgesTruncated, nodesDF, networkFolder)
doGraphPlot(edgesTruncated, nodesDF, networkFolder, rimVariable = sexNetworkIndex)


doGraphPlot(edgesTruncated, nodesDF, networkFolder, highlightVariable = painNetworkIndex, sizeVariableIndex = sizeNetworkIndex)



doDensityPlot(nodesDF, 3, 5, NULL, filepath_ageBloodDensity)
doHistogramPlot(nodesDF, 5, filepath_ageHistogram, binsWidth = 1)

#doDegreePlots(edgesTruncated, nodesDF, networkFolder)


# -------- For time series
# ------------ Lines plots
doTimeSerie(phenotypeTable, IDIndex, numericalCollection, NULL, filepath_timeseries,
            plotTitle = "Something that looks like a time series but isn't", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "ID", plotYLabel = "Something")
# ------------ Bars with sorting option
doBarSerie(phenotypeTable, IDIndex, numericalCollection, NULL, filepath_barsorted,
            plotTitle = "Something that looks like a time series but isn't", plotSubtitle = "A random subtitle", plotCaption = "Source: Tromsø 7", plotXLabel = "ID", plotYLabel = "Something")