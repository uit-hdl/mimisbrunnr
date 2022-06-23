# Copypaste code, don't delete yet

    # This function does a bunch of simulations and return a vector of size
    # total simulations. Each element of the vector contain how many same to
    # same relationships we have in that simulation.
    #
    # This function doesn't tell you whether the relationships are bias or not
    # it only generate the vector, later on you have to analyize that.
    #
    # There are 3 ways to run the bootstrap vector:
    #
    #     A)
    #
    #        Get the general prevalence. Whatever you give in the categoricalIndex
    #        variable has a prevalence (ie Positive 60% Negative 40%), and that
    #        frequency is apply to everyone in the nodes table.
    #
    #     B1)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which has an specific
    #        modality given by overrideFrequencyCategory (ie Sex -> Woman).
    #        This modality has a prevalence likely different from the general
    #        population (ie Positive 60% Negative 40% was the original, but 
    #        women has a Positive 20% and Negative 80%). Then this frequency
    #        is apply to everyone in the table.
    #
    #     B2)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which doesn't has an
    #        specific modality given by overrideFrequencyCategory (NULL/NA).
    #
    #        This means that you are going to take each modality (Men, Women)
    #        And apply that prevalence to each element of the node table.
    #        (ie Men Positive 80% Negative 20%, Women Positive 20% and
    #        Negative 80%). Then this frequency is apply to everyone in the table.
    #
    #     C)
    #
    #        Maintain the original frequency for some modalities but not for an
    #        specific one. This mean that you have an special categorical variable
    #        given by overrideFrequenciesIndex, which has a bunch of modalities,
    #        but one is special given by overrideFrequencyCategory 
    #        (ie BMI -> Underweight, Healthy, Overweight, Obese ; and we keep
    #        healthy as the special one). Instead of doing B) to Obese, we are
    #        going to filter by a third variable given by IndexLockVariable and
    #        ModalityLock. So the results would be:
    #
    #            - Men keep the men prevalence, not the original (ie: Positive
    #              70% Negative 30%)
    #
    #            - Women don't keep the women prevalence (20/80), instead they
    #              keep the prevalence of Obese Women (ie 40/60).
    #
    #
    #
    # tableBase                = which DF do you want
    #
    # tableEdges               = edges with the real relationships
    #                            with "from" , "to" variables, "value" optional
    #
    # categoricalIndex         = For the given DF in tableBase, which column do you
    #                            want to analyze. The column must contain a
    #                            categorical type of variable.
    #                            (ie: SPAType or Carrier Status)
    #
    #                            If you have a numerical variable, you need to
    #                            convert it to categorical first.
    #
    # totalSimulations         = How many simulations do you want to do. As a
    #                            general rule, 1000 simulations is good enough.
    #
    # simulateRelationships    = Whether to use original relationships or simulate
    #                            new random ones. (DEFAULT = FALSE)
    #
    #                            Keeping the same relationships maintain the network
    #                            topology, so you are analyzing if there is a bias
    #                            in the relationships (ie: Do same type of carrier
    #                            are more likely to be friends with each others)
    #
    #                            If you don't keep the relationship, the results
    #                            will have several different meanings depending of
    #                            what you are doing. Consult your local statician
    #                            for more information.
    #                           
    #
    # overrideFrequenciesIndex = Which column are you going to use to build the
    #                            frequency table. The default is the same as
    #                            categoricalIndex, so nothing change by default.
    #
    #                            (ie: Index for Sex, BMI, Smoking, or whatever)
    #
    #                            You might want to do, for example, the analysis
    #                            for smoking, but using the frequency of Sex: MEN
    #                            and Sex: WOMEN, to check whether men or women have
    #                            some sort of bias, higher risk, or whatever.
    #
    # overrideFrequencyCategory = If you want to use the previous variable, I need
    #                             a category to filter by (ie: "Woman", "Yes", "40")
    #                             If you give me an index in the previous variable,
    #                             but this is still NULL (default) I will run
    #                             B2 instead of B1.
    #
    # IndexLockVariable         = (Int) If you want to lock the frequency table
    #                             for other categories. By default is NULL.
    #
    #                             This means that if you DON'T lock it (default)
    #                             you take the frequency table of ie women, and apply
    #                             that frequency to everybody, including men.
    #                             This is usefull to check whether men or women
    #                             are more affected by something.
    #
    #                             If you DO LOCK it, you take the frequency table
    #                             for women, based on another filter, and you keep
    #                             the frequency for men as it is.
    #
    # ModalityLock              = (String) See IndexLock. This variable select
    #                             The modality to use for the locking.
    #
    #
    # showProgressBar         =  (String) If not NULL (default), the console will show
    #                            a little progress bar from 0% to 100% telling
    #                            how far we are doing the simulations.
    #
    #                            Beware that the progress bar will clear the console
    #                            of all text.
    #
    #                            If not NULL, you need to add a string here that
    #                            will show up later in console. Recomendation is
    #                            that you give something meaningfull like
    #
    #                            "Doing simulations for school: "
    #
    #                            Is useful to set it to NULL if you use this inside
    #                            another function, of if you don't want to loose the
    #                            text in the console for whatever reason.
  
    # Return: A vector of size totalSimulations, which how many same to same relationship
    #         where found in that particular simulation
  
    getBootstrapVector2 <- function(tableBase, tableEdges, categoricalIndex, totalSimulations,
                                   simulateRelationships = FALSE,
                                   overrideFrequenciesIndex = NULL, overrideFrequencyCategory = NULL,
                                   indexLockVariable = NULL, modalityLock = NULL,
                                   showProgressBar = NULL){
    
    
        # Prepare the vector with the bootsraps results
        bootstrapVector      = rep(0,totalSimulations)
    
        # Get the basic statistics from the nodes table
        frequencyTable          = summarizeCategorical(tableBase, categoricalIndex)
        totalNodes              = nrow(tableBase)
        
        # Get the basic statistics from the edge table
        totalRelationships      = nrow(tableEdges)
    
        # If you want to override the Frequency table it means that you want
        # to use the categorical index but restricted to only a particular
        # category of another column.
        
        # Cases B and C
        if(!is.null(overrideFrequenciesIndex)){
      
            # Check that you have a valid category to filter
            #
            # B1, or C
            if(!is.null(overrideFrequencyCategory)){
                
                # There is also the option for locking the variable
                # By default, is null, and we do nothing else
                #
                # B1
                if(is.null(indexLockVariable)){

                    filterTableBase = tableBase[tableBase[,overrideFrequenciesIndex] == overrideFrequencyCategory,]
                    frequencyTable  = summarizeCategorical(filterTableBase, categoricalIndex)                    
                    
                }
                # If not null, we need a modality. If we don't have such modality
                # we give a warning and use the defaults.
                #
                # C
                else{
                    
                    if(!is.null(modalityLock)){

                        # This frequency is for the special case
                        # (ie: Women who take high Estradiol)
                        # Any other frequency we will find out later
                        
                        filterTableBase = tableBase[tableBase[,overrideFrequenciesIndex] == overrideFrequencyCategory,]
                        filterLockBase  = filterTableBase[filterTableBase[,indexLockVariable] == modalityLock,]
                        frequencyTable  = summarizeCategorical(filterLockBase, categoricalIndex)                    

                    }
                    
                    # You wanted to do C but I'm missing information, so I do
                    # A instead.
                    else{

                        print("WARNING: You ask to override the simulation frequency
                               table based on another categorical variable, and locking
                               the category using a third modality. You gave me the index
                               for locking but not the modality. So I'm ignoring your request
                               and using the default 'A' mode.")                  
                        
                    }
                                        
                }
                
            }
            
            
            # B2. There is nothing to do here, we will fix stuff later.
            #else{
            
                #print("WARNING: You ask to override the simulation frequency
                 #      table based on another categorical variable, but you
                  #     didn't gave me a category. So I'm ignoring your request
                   #    and using the defaults.")    
                
            #}

        }
    
        # Dataframes we are going to use to run the simulations. In each
        # simulation, we fill the patient table and the relationship table.
        # -- People
        totalPatients           = totalNodes
        patientsTable           = DF(totalPatients, 3)
        colnames(patientsTable) = c("ID", "TargetVariable", "ModalityOverride")
        # -- Relationships
        frienshipDF             = DF(totalRelationships, 4)
        colnames(frienshipDF)   = c("from","to","value","SameRelationship")
    
    
        # For each simulation:
        #
        # ---- Generate the random patient table with their random attributes which 
        #      follows the given frequency rules.
        #
        # ---- Generate the friendship table (or not depending of your function call)
        #
        # ---- Count how many patients that are friends share the same attribute
        
        for (j in 1:totalSimulations) {

            # Print feedback of simulation progress to the user
            if(!is.null(showProgressBar)){

                cat("\014")
                print(showProgressBar)
                print("")
                print(getProgressCharacters((100*j)/totalSimulations))
                                
            }
            
            # Init the patients
            # -- Init the IDs
            patientsTable[,1] = tableBase[,1] 
            # -- Init the random categorical variable by sampling from the pool of
            #    frequencies that you choose in the frequency table
            #    This depends on whether you are in case A, B or C
            #
            #    A and B, we use the frequency table that we already found out
            #    These are the simple case, and the case in which we use the
            #    conditional probability.
            #
            #    C is when we use a frequency table for one modality only, and
            #    the rest of modalities stay the same
            #
            
            #    A and B
            if(is.null(indexLockVariable)){
            
                # A
                if(is.null(overrideFrequenciesIndex)){
                
                    patientsTable[,2] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)    
                        
                }
                
                # B
                else{
                
                    # B1
                    if(!is.null(overrideFrequencyCategory)){
                        
                        patientsTable[,2] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)    
                        
                    }
                    
                    # B2
                    # In this case we need to find the modality for each person
                    # and apply the correct prevalence, similar to C (next)
                    else{
                        
                        # Give each patient it original modality for the override
                        # (ie, same sex, same categorical BMI, whatever)
                        patientsTable[,3] = tableBase[,overrideFrequenciesIndex] 
                
                        # Find out the modalities of the override index
                        myModalities    = getModalities(tableBase, overrideFrequenciesIndex)
                        totalModalities = length(myModalities)
                
                        # Here we create a matrix of samples.
                        # There is one row for each modality of the target category
                        sampleMatrix    = newList(totalModalities)
                        for(i in 1:totalModalities){
                        
                            # Init the row
                            sampleMatrix[[i]] = rep(NA, totalPatients)
                            
                            # Get the modality we are studying
                            currentModality = myModalities[i]
                            
                            specialFilterTable    = tableBase[tableBase[,overrideFrequenciesIndex] == currentModality,]
                            specialFrequencyTable = summarizeCategorical(specialFilterTable, categoricalIndex)                    
                                    
                            sampleMatrix[[i]] = sample(specialFrequencyTable$Modality, size = totalPatients, replace = TRUE, prob = specialFrequencyTable$Relative)     
                             
                        }
                
                        # For each patient, check the modality and apply the
                        # proper sample.
                        for(i in 1:totalPatients){
                            
                            # Get the patient modality
                            currentPatientModality = patientsTable[i,3]
                            
                            # Find that modality in the sample matrix
                            currentModalityIndex   = grep(TRUE, (currentPatientModality == myModalities))
                            
                            # Assign the sample value from the sample matrix
                            patientsTable[i,2] = sampleMatrix[[currentModalityIndex]][i]
                            
                        }
                        
                    }
                        
                }
                
            }
            #   C 
            else{

                # Give each patient it original modality for the override
                # (ie, same sex, same categorical BMI, whatever)
                patientsTable[,3] = tableBase[,overrideFrequenciesIndex] 
                
                # Find out the modalities of the locking variable
                myModalities    = getModalities(tableBase, overrideFrequenciesIndex)
                totalModalities = length(myModalities)
                
                # Here we create a matrix of samples.
                # There is one row for each modality of the target category
                # We have the special case of locking (this case), we already
                # found out the frequency table for the locking case, we need
                # to find out the rest.
                sampleMatrix    = newList(totalModalities)
                for(i in 1:totalModalities){
                
                    # Init the row
                    sampleMatrix[[i]] = rep(NA, totalPatients)
                    
                    # Get the modality we are studying
                    currentModality = myModalities[i]

                    # Special case, already done
                    if(currentModality == overrideFrequencyCategory){
                    
                        sampleMatrix[[i]] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)     
                        
                    }
                    # Everything else
                    else{
                        
                        specialFilterTable    = tableBase[tableBase[,overrideFrequenciesIndex] == currentModality,]
                        specialFrequencyTable = summarizeCategorical(specialFilterTable, categoricalIndex)                    
                            
                        sampleMatrix[[i]] = sample(specialFrequencyTable$Modality, size = totalPatients, replace = TRUE, prob = specialFrequencyTable$Relative)     
                        
                    }

                }
                
                # For each patient, check the modality.
                # if it is the lock modality, use the sampling we have already
                # found out for each case.
                for(i in 1:totalPatients){
                    
                    # Get the patient modality
                    currentPatientModality = patientsTable[i,3]
                    
                    # Find that modality in the sample matrix
                    currentModalityIndex   = grep(TRUE, (currentPatientModality == myModalities))
                    
                    # Assign the sample value from the sample matrix
                    patientsTable[i,2] = sampleMatrix[[currentModalityIndex]][i]
                    
                }
                
            }
            
            # For each relationship
            # -- Simulate a new relationship if needed
            # -- Check if the have the same category.
            for (i in 1:totalRelationships) {
        
                # Get the real relationship From and To values
                # Right here, despise variable name, they are not random yet
                randomFrom = tableEdges[i,1]
                randomTo   = tableEdges[i,2]
        
                # If you need the simulation of relationship, simulate random from and to
                if(simulateRelationships == TRUE){
          
                    # Take a random from and to, that are not the same, and are not already in the list
          
                    # -- Pick the first one
                    candidatesList = patientsTable$ID
                    randomFrom     = sample(candidatesList, 1)
          
                    # -- Take away the first one from the candidates list as you are not suppose to have a relationship with yourself
                    candidatesList = candidatesList[candidatesList != randomFrom]
          
                    # -- Pick the second one
                    # ---- You already have a list of relationships that start with FROM
                    # ---- This list could be empty though
                    # ---- In any case, grab the TOs from that list
                    # ---- Those are the forbidden numbers that you need to take away from the candidate list
                    forbiddenTos = frienshipDF[frienshipDF$from == randomFrom,2]
          
                    # ---- Update the candidate list if there is one or more forbiddens
                    if (length(forbiddenTos) > 0){
            
                        candidatesList = candidatesList[!(candidatesList %in% forbiddenTos)]
            
                    }
          
                    # ---- Finally pick the second one
                    randomTo = sample(candidatesList, 1)
          
                }
        
                # Set this particular relationship into our friendship DF
                frienshipDF$from[i]  = randomFrom
                frienshipDF$to[i]    = randomTo
                frienshipDF$value[i] = 1
        
                # Check if they share same target variable
                targetTypeFrom = patientsTable[patientsTable[,1] == randomFrom,2]
                targetTypeTo   = patientsTable[patientsTable[,1] == randomTo,  2]
            
                # Sometimes you get a random number that doesn't work, is very weird but the bug is there, I'm trying to catch it with this
                # (but It haven't show in sometime so I think is fixed)
                if(is.na((targetTypeFrom == targetTypeTo))){
          
                    print("ALERT!")
                    print(targetTypeFrom)
                    print(targetTypeTo)
                    print(randomFrom)
                    print(randomTo)
                    print(i)
          
          
                }
        
                # Label if both targets have the same category
                frienshipDF$SameRelationship[i] = (targetTypeFrom == targetTypeTo)
        
            }
      
            # Add how many have the same relationship to the result of this simulation
            bootstrapVector[j] = sum(frienshipDF$SameRelationship)
      
        }
    
        return(bootstrapVector)
    
  }


    # This function tells you if your relationships are bias or not
    # Only for categorical variables. It can run the version A, B1, B2 and C,
    # of the Bootstrap vector.
    #
    # In order to select each version, fill the input variables accordingly.
    #
    # nodesTable    = Dataframe with the information about your nodes
    #                 It can have any structure you want, the only restriction is
    #                 the first column must be the ID column, and it must be
    #                 a numerical ID
    #
    #
    # listOfEdgesDF = List of Dataframes. Each dataframe have a different network
    #                 The dataframe structure goes like this:
    #
    #                 from     to     value 
    #                    1      2         3
    #                    2      3         5
    #                          ...
    #
    #                The "value" column is irrelevant for this analysis, but
    #                is the standard way to save the edges in a network.
    #
    # listOfNetworkNames = List of Strings with the name of each network. The
    #                      default value is NULL and it will be named from 1 to 
    #                      X. Otherwise, if you want proper names, give them to
    #                      the function in this paremeter.
    #
    #
    # listOfConsequenceIndexes = List of Indexes with the variables that you want
    #                            to study. The variables must be categorical, and
    #                            the indexes must be a valid index contain within
    #                            the nodeTable.
    #
    # totalSimulations         = How many simulations you want to run
    #                            There is no default, 10 is good for testing
    #                            And 1000 is good for getting results.
    # 
    # Return:
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Network"               Name of the Network
    #     "Total Relationships",  How many relationships we have
    #     "Equal Relationships",  How many same to same relationships we have
    #  
    #     "MIN",                  Minimum same to same relationships count that we found in the simulation
    #     "Q1",                   Percentile 25 of same to same relationships count that we found in the simulation
    #     "Median",               Median same to same relationships count that we found in the simulation
    #     "Average",              Average same to same relationships count that we found in the simulation
    #     "Q3",                   Percentile 75 same to same relationships count that we found in the simulation
    #     "MAX",                  Maximum same to same relationships count that we found in the simulation
    #     "SD",                   Standard Deviation same to same relationships count that we found in the simulation
    #
    #     "ConsequenceIndex"      The actual name of this column change depending of the name of each consequence index.
    #                             In here we have the actual p-value for each network, on whether your relationship
    #                             is bias or not.
    # 
    doCategoricalBiasAnalysis2 <- function(nodesTable, listOfEdgesDF,
                                          listOfConsequenceIndexes,
                                          totalSimulations,
                                          #overrideFrequenciesIndex = NULL, overrideFrequencyCategory = NULL,
                                          indexLockVariable = NULL, modalityLock = NULL,
                                          listOfNetworksNames = NULL){

        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
        }
        
        # Prepare the blank base DF where we write the results.
        # We will have one of this for each consequence index.
        biasSimulationsDF           =  data.frame(matrix(NA, nrow = totalNetworks, ncol = 10 + 1 ))
        colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Average", "Q3", "MAX", "SD",  "ConsequenceIndex"  )
        for(i in 1:totalNetworks){
            biasSimulationsDF[i,1]  = myNetworkNames[i]
        }
        
        # We have a list of result for each of the consequence index
        # So in here we prepare such list, and give a blank DF to each
        biasResultsList =  newList(totalConsequenceIndexes)
        for( i in 1:totalConsequenceIndexes){
      
            # Init the DF to empty
            biasResultsList[[i]] = biasSimulationsDF

            # Change the name of the variable we are interested in for this DF
            colnames(biasResultsList[[i]])[11] = consequenceNames[i]

        }

        # For each of the consequence index, we do these 1000-ish simulation for
        # each of the networks that you have.
        for (i in 1:totalConsequenceIndexes){
      
            # Get the DF where we save the results for this variable
      
            # R is stupid, why can't I pass a reference? why do I need to use an index
            # here when then an alias to the variable would make everything more
            # readable and efficient??? >:[
            # currentDF = biasResultsList[[i]]
      
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[i]
                currentName  = consequenceNames[i]        
            }

            # For each of the network, do the proper bias analysis
            {
                
                # For each edges DF
                for(j in 1:totalNetworks){
                    
                    # Get the current edges
                    currentEdges = listOfEdgesDF[[j]]
                    
                    # Find out the if "from" - "to" have the same relationship
                    currentEdges$SameRelationship = addEdgeRelationship(currentEdges,  nodesTable, currentIndex)
                    
                    # Find out how many relationships we have
                    currentTotalRelationships          = nrow(currentEdges)
                    
                    # Find out how many same to same relationships we have
                    # This is the real value that we use in the p-value calculation
                    currentTotalSameRelationships      = sum(currentEdges$SameRelationship == TRUE)
                 
                    # Check if carrier have bias friendship towards people with the same carrier status
                    #
                    # -- Prepare the custom message 
                    currentWaitingMessage = paste0( "Doing simulations for ", myNetworkNames[j], " please wait..." )

                    # -- Do the bias analysis
                    #currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentIndex, totalSimulations,
                    #                                       showProgressBar = currentWaitingMessage)
                    
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = overrideFrequenciesIndex,
                                                           overrideFrequencyCategory = overrideFrequencyCategory,
                                                           indexLockVariable         = indexLockVariable,
                                                           modalityLock              = modalityLock,
                                                           showProgressBar           = currentWaitingMessage)
                    
                    
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write all this info in the appropriate part of the results
                    biasResultsList[[i]][j,  2 ] = currentTotalRelationships
                    biasResultsList[[i]][j,  3 ] = currentTotalSameRelationships
                    biasResultsList[[i]][j,  4 ] = min(currentBiasResult)
                    biasResultsList[[i]][j,  5 ] = as.integer(summary(currentBiasResult)[2])
                    biasResultsList[[i]][j,  6 ] = median(currentBiasResult)
                    biasResultsList[[i]][j,  7 ] = currentAverage
                    biasResultsList[[i]][j,  8 ] = as.integer(summary(currentBiasResult)[5])
                    biasResultsList[[i]][j,  9 ] = max(currentBiasResult)
                    biasResultsList[[i]][j, 10 ] = currentSD
                    biasResultsList[[i]][j, 11 ] = currentPValue
                       
                }

            }


        }
        
                
        # Everything is finish, give back the list of biases and close.
        return(biasResultsList)
        
        
    }

    
    # This function tells you if your relationships are bias or not with respect
    # each modality of each categorical variable.
    #
    # It also find the confident interval
    #
    # Only for categorical variables (obviously). It can run versions B and C
    # of the bootstrap vector function.
    #
    # The combinations are:
    #
    #     ALPHA:
    #
    #     indexLockVariable  = NULL and modalityLock  = NULL
    #
    #     Run the classical analysis with the list of all modalities without
    #     any preconditioned bias
    #
    #     BETA:
    #
    #     indexLockVariable != NULL and modalityLock != NULL
    #
    #     For each of the variables, we divided assign two different frequencies
    #     in the node table, by whatever you are giving me in these two variables.
    #     Anything that is not this variables, keeps it original frequency, but
    #     anything that coincide with this variable keep the conditional frequency
    #     to each modalities of each of the consequence indexes given.
    #
    #
    # Return
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Variable"
    #     "Modality"
    #     "Index"
    #     "Network"
    #     "Real Relationships"
    #     "Real Same to Same"
    #     "Simulated Unbias Average Same"
    #     "Simulated Unbias Minimum Same"
    #     "Simulated Bias Average Same"
    #     "Simulated Bias SD"
    #     "Target Variable"
    #     "Base Risk"
    #     "Low CI"
    #     "High CI"
    
    doModalityBiasAnalysis2 <-function(nodesTable, listOfEdgesDF,
                                      listOfConsequenceIndexes,
                                      listOfExplanatoryIndexes,
                                      totalSimulations,
                                      listOfNetworksNames       = NULL,
                                      indexLockVariable         = NULL,
                                      modalityLock              = NULL,
                                      confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalCurrentModalities = length(currentModalities)
        
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }
        

        # For each consequence variable and each network, we are going to need
        # to Simulate the bias analysis, and get the bias average. We do that
        # now, and save it for later
        #
        #
        biasResultsDF = NA
        #
        # If we run B1 or B2, we need to compare with A
        #
        if(is.null(indexLockVariable)){
            
                biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                           listOfConsequenceIndexes,
                                                           totalSimulations,
                                                           listOfNetworksNames = myNetworkNames)
        }
        #
        # If we run C, we need to compare with B2
        # 
        #
        else{

                biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                           listOfConsequenceIndexes,
                                                           totalSimulations,
                                                           overrideFrequenciesIndex = indexLockVariable,
                                                           listOfNetworksNames      = myNetworkNames)
            
        }


        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100*i / totalModalities ,2), "%" ))
                    print("--------------------")

                    # Get the bootstrap vector:
                    
                    currentBiasResult = NA
                    
                    # ALPHA:
                    if(is.null(indexLockVariable)){

                        currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                               overrideFrequenciesIndex  = currentVariableIndex,
                                                               overrideFrequencyCategory = currentModalityName)
                                                
                    }
                    

                    # BETA:
                    else{
                        
                                            
                        currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                               overrideFrequenciesIndex  = currentVariableIndex,
                                                               overrideFrequencyCategory = currentModalityName,
                                                               indexLockVariable = indexLockVariable,
                                                               modalityLock = modalityLock)
                        
                    }
                    
                    
                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }
                
                
                


    #1     "Variable"
    # 2    "Modality"
    #  3   "Index"
    #   4  "Network"
    #5     "Real Relationships"
    # 6    "Real Same to Same"
    #  7   "Simulated Unbias Average Same"
    #   8  "Simulated Unbias Minimum Same"
    #9     "Simulated Bias Average Same"
    # 0    "Simulated Bias SD"
    #  1   "Target Variable"
    #   2  "Base Risk"
    #3     "Low CI"
    # 4    "High CI"
                    
            }
            
            
        }        
        
        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][i,13] = (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9] + simulationByVariablesDFList[[j]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][i,14] = (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9] + simulationByVariablesDFList[[j]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...
                    if(i < totalExplanatoryIndexes){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
        }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }    
    

    
    doModalityBiasAnalysis2 <-function(nodesTable, listOfEdgesDF,
                                      listOfConsequenceIndexes,
                                      listOfExplanatoryIndexes,
                                      totalSimulations,
                                      listOfNetworksNames       = NULL,
                                      indexLockVariable         = NULL,
                                      modalityLock              = NULL,
                                      confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalCurrentModalities = length(currentModalities)
        
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }
        

        # For each consequence variable and each network, we are going to need
        # to Simulate the bias analysis, and get the bias average. We do that
        # now, and save it for later
        #
        #
        biasResultsDF = NA
        #
        # If we run B1 or B2, we need to compare with A
        #
        if(is.null(indexLockVariable)){
            
                biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                           listOfConsequenceIndexes,
                                                           totalSimulations,
                                                           listOfNetworksNames = myNetworkNames)
        }
        #
        # If we run C, we need to compare with B2
        # 
        #
        else{

                biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                           listOfConsequenceIndexes,
                                                           totalSimulations,
                                                           overrideFrequenciesIndex = indexLockVariable,
                                                           listOfNetworksNames      = myNetworkNames)
            
        }


        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100*i / totalModalities ,2), "%" ))
                    print("--------------------")

                    # Get the bootstrap vector:
                    
                    currentBiasResult = NA
                    
                    # ALPHA:
                    if(is.null(indexLockVariable)){

                        currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                               overrideFrequenciesIndex  = currentVariableIndex,
                                                               overrideFrequencyCategory = currentModalityName)
                                                
                    }
                    

                    # BETA:
                    else{
                        
                                            
                        currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                               overrideFrequenciesIndex  = currentVariableIndex,
                                                               overrideFrequencyCategory = currentModalityName,
                                                               indexLockVariable = indexLockVariable,
                                                               modalityLock = modalityLock)
                        
                    }
                    
                    
                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }
                
                
                


    #1     "Variable"
    # 2    "Modality"
    #  3   "Index"
    #   4  "Network"
    #5     "Real Relationships"
    # 6    "Real Same to Same"
    #  7   "Simulated Unbias Average Same"
    #   8  "Simulated Unbias Minimum Same"
    #9     "Simulated Bias Average Same"
    # 0    "Simulated Bias SD"
    #  1   "Target Variable"
    #   2  "Base Risk"
    #3     "Low CI"
    # 4    "High CI"
                    
            }
            
            
        }        
        
        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][i,13] = (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9] + simulationByVariablesDFList[[j]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][i,14] = (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9] + simulationByVariablesDFList[[j]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...
                    if(i < totalExplanatoryIndexes){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
        }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }
        
    
    
    
 # Medicine
    {
    """
    ################################################################################
    Medicine:
    ################################################################################
    
    ----------------------------------------------------------------------------
      When did the medicine questionare was filled and burocracy
    ----------------------------------------------------------------------------
      
      MEDICATION_DATE_FF1
    MEDICATION_TIME_FF1
    MEDICATION_SIGNATURE_FF1
    
    
    ----------------------------------------------------------------------------
      List of medication to be transformed in proper relational DB
    ----------------------------------------------------------------------------
      
      MEDICATION_BRAND1_FF1
    MEDICATION_ATC1_FF1
    MEDICATION_REGULAR1_FF1
    
    MEDICATION_BRAND2_FF1
    MEDICATION_ATC2_FF1
    MEDICATION_REGULAR2_FF1
    
    MEDICATION_BRAND3_FF1
    MEDICATION_ATC3_FF1
    MEDICATION_REGULAR3_FF1
    
    MEDICATION_BRAND4_FF1
    MEDICATION_ATC4_FF1
    MEDICATION_REGULAR4_FF1
    
    MEDICATION_BRAND5_FF1
    MEDICATION_ATC5_FF1
    MEDICATION_REGULAR5_FF1
    
    MEDICATION_OTHER_FF1
    MEDICATION_OTHER_DESC_FF1
    
    ANALGETICS_BRAND1_FF1
    ANALGETICS_ATC1_FF1
    ANALGETICS_HOURS1_FF1
    ANALGETICS_LAST_NUMBER1_FF1
    
    ANALGETICS_BRAND2_FF1
    ANALGETICS_ATC2_FF1
    ANALGETICS_HOURS2_FF1
    ANALGETICS_LAST_NUMBER2_FF1
    
    ANALGETICS_BRAND3_FF1
    ANALGETICS_ATC3_FF1
    ANALGETICS_HOURS3_FF1
    ANALGETICS_LAST_NUMBER3_FF1
    
    ANALGETICS_BRAND4_FF1
    ANALGETICS_ATC4_FF1
    ANALGETICS_HOURS4_FF1
    ANALGETICS_LAST_NUMBER4_FF1
    
    ANTIBIOTICS_BRAND1_FF1
    ANTIBIOTICS_ATC1_FF1
    
    ANTIBIOTICS_BRAND2_FF1
    ANTIBIOTICS_ATC2_FF1
    
    ANTIBIOTICS_BRAND3_FF1
    ANTIBIOTICS_ATC3_FF1
    
    ANTIBIOTICS_BRAND1_FF12
    ANTIBIOTICS_ATC1_FF12
    
    ANTIBIOTICS_BRAND2_FF12
    ANTIBIOTICS_ATC2_FF12
    
    ANTIBIOTICS_BRAND3_FF12
    ANTIBIOTICS_ATC3_FF12
    
    ----------------------------------------------------------------------------
      List of YES/NO question shortcuts
    ----------------------------------------------------------------------------
      
      MEDICATION_DAILY_FF1
    
    ANALGETICS_FF1
    
    ANTIBIOTICS_FF1
    ANTIBIOTICS_FF12
    
    PAINKILLERS_PRESC_4WEEKS_FF1
    
    PAINKILLERS_NOPRESC_4WEEKS_FF1
    
    SLEEPING_PILLS_4WEEKS_FF1
    
    ANTIDEPRESSANTS_4WEEKS_FF1
    
    ADHD_MEDICATION_4WEEKS_FF1
    
    TRANQUILIZERS_4WEEKS_FF1	
    
    ----------------------------------------------------------------------------
      Other things
    ----------------------------------------------------------------------------
      
      TIME_LAST_MEAL_FF1
    
    MEDIC0
    MEDIC1
    MEDIC2
    MEDIC3
    MEDIC4
    MEDIC5
    MEDIC6
    MEDIC7
    MEDIC8
    MEDIC9
    MEDICA
    MEDICB
    MEDICC
    MEDICD
    MEDICE
    MEDICF
    
    ANALG0	
    ANALG1
    ANALG2
    ANALG3
    
    ANTIB0
    ANTIB1
    ANTIB2
    ANTIB3
    ANTIB4
    ANTIB5
    """
        
    }
  
    # Contraceptives
    {
       
        """
        
        
    ################################################################################
    Menstruation and Contraceptives:
      ################################################################################
    
    This data is for women only, as such you have those weird WOMEN_DATE
    variables that has nothing to do with women by itself, is just that the
    variables that are related to women need to have a register date,
    signatures, and so on. And the  person that designed these variable thought
    it was a good idea to summarize all with WOMEN_X variable names
    
    ----------------------------------------------------------------------------
      When did the women only questionare was filled and burocracy
    ----------------------------------------------------------------------------
      
      WOMEN_DATE_FF1
    WOMEN_TIME_FF1
    WOMEN_SIGNATURE_FF1	
    
    CHANCE_PREGNANT_FF1
    PREGNANCY_TEST_CONSENT_FF1
    PREGNANCY_TEST_RESULT_FF1	
    DEXA_APPROVED_FF1 (No dexa info, just approved dexa, just no dexa for you if you are pregnant)
    
    ----------------------------------------------------------------------------
      Menstruation information
    ----------------------------------------------------------------------------
      
      MENSES_FF1
    MENSES_REGULARITY_FF1
    MENSES_CYCLE_LENGTH_FF1
    MENSES_START_DATE_CERTAIN_FF1
    MENSES_START_DATE_FF1
    
    ----------------------------------------------------------------------------
      Contraceptives shortcuts. Note that in the final table, we don't mark yes/no
    but rather the type. If the type is none, that means no, and if it is
    something else, that means yes.
    ----------------------------------------------------------------------------

    CONTRACEPTIVES_FF1
    CONTRACEPTIVES_TYPE_FF1


    ----------------------------------------------------------------------------
    Contraceptives DB list
    ----------------------------------------------------------------------------

    ORAL_CONTRACEPT_NAME_FF1
    ORAL_0
	INJECTED_CONTRACEPT_NAME_FF1
    INJEC0
    SUBDERMAL_CONTRACEPT_NAME_FF1
    SUBDE0
    CONTRACEP_SKIN_PATCH_NAME_FF1
    CONTR0
    VAGINAL_CONTRACEPT_NAME_FF1
    VAGIN0
    ORAL_CONTRACEPT_ATC_FF1
    INJECTED_CONTRACEPT_ATC_FF1
    SUBDERMAL_CONTRACEPT_ATC_FF1   
    CONTRACEP_SKIN_PATCH_ATC_FF1
    VAGINAL_CONTRACEPT_ATC_FF1

        
        """
        
         
        
    }
    
    # Social Network
    {
    
    """
        
################################################################################
Social Network:
################################################################################
    ----------------------------------------------------------------------------
    Other comments
    ----------------------------------------------------------------------------

	NETWO0
	NETWO1
	NETWO2
	NETWO3
	NETWO4
	NETWO5
	NETWO6
	NETWO7
	NETWO8
	NETWO9
	NETWOA

      """      
    }
    
    # Aureus
    

################################################################################
Aureus:
################################################################################
    ----------------------------------------------------------------------------
    All of these variables are comments and labels which are useful to keep
    track of the samples, but doesn't have any use for the analysis itself.
    ----------------------------------------------------------------------------
      
      NASAL_SAMPLE_OK_FF1
    THROAT_SAMPLE_OK_FF1
    LAB_COMMENTS_DAY0_FF1
    LAB_COMMENTS_DAY2_FF1
    LAB_COMMENTS_ENRICH_FF1
    
    COMMENTS_STAPH_FF1
    COMME0
    COMME1
    
    FREEZE_NUMBER_STAPH_NASAL_FF1
    FREEZE_NUMBER_STAPH_THROAT_FF1
    freeze_number_staph_nasal_ff11
    freeze_number_staph_throat_ff11
    
    event_swab_ff1
    event_swab_med_ff1
    event_swab_med_comment_ff1
    event_swab_tech_ff1
    event_swab_tech_comment_ff1
    event_swab_abort_ff1
    event_swab_abort_comment_ff1
    event_swab_other_ff1
    event_swab_other_comment_ff1
    
    event_swab_ff12
    event_swab_med_ff12
    event_swab_med_cmnt_ff12
    event_swab_tech_ff12
    event_swab_tech_cmnt_ff12
    event_swab_abort_ff12
    event_swab_abort_cmnt_ff12
    event_swab_other_ff12
    event_swab_other_cmnt_ff12


Blood
    ----------------------------------------------------------------------------
    Above/Bellow limit booleans. Not in use since we define our own dynamic
    table telling which limit we have for each depending on sex, age, and so on
    ----------------------------------------------------------------------------

    S_E2_BELOW_LIMIT_FF1
    S_PROG_BELOW_LIMIT_FF1
    S_SHBG_ABOVE_LIMIT_FF1
    S_LH_BELOW_LIMIT_FF1
    S_FSH_BELOW_LIMIT_FF1
	S_PROG_BELOW_LMT_LCMSMS_FF1
	S_ESTR_BELOW_LMT_LCMSMS_FF1

################################################################################
Questionaries:
################################################################################

    ----------------------------------------------------------------------------
    When was the questionare filled
    ----------------------------------------------------------------------------

	DATE_QUESTBACK_FF1

################################################################################
Sociology:
################################################################################

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------


	LEFT_HOME_WHEN_FF1

    HOUSHOLD_MOTHER_FF1
    HOUSHOLD_FATHER_FF1
    HOUSHOLD_SIBS1TO2_FF1
    HOUSHOLD_SIBS3PLUS_FF1
    HOUSHOLD_STEPFATHER_FF1
    HOUSHOLD_STEPMOTHER_FF1
    HOUSHOLD_FOSTERPARENT_FF1
   	HOUSHOLD_ADOPTIVEPARENT_FF1
	HOUSHOLD_GRANDPARENTS_FF1
	HOUSHOLD_FRIENDS_FF1
	HOUSHOLD_ALONE_FF1
	HOUSHOLD_INSTITUTION_FF1
	HOUSHOLD_OTHER_FF1

	MOTHER_WORK_FULL_TIME_FF1
	MOTHER_WORK_PART_TIME_FF1
	MOTHER_WORK_UNEMPLOYED_FF1
	MOTHER_WORK_DISABLED_FF1
	MOTHER_WORK_DOMESTIC_FF1
	MOTHER_WORK_SCHOOL_FF1
	MOTHER_WORK_PENSIONED_FF1
	MOTHER_WORK_DECEASED_FF1
	MOTHER_WORK_DONT_KNOW_FF1
	MOTHER_WORK_OTHER_FF1

	FATHER_WORK_FULL_TIME_FF1
	FATHER_WORK_PART_TIME_FF1
	FATHER_WORK_UNEMPLOYED_FF1
	FATHER_WORK_DISABLED_FF1
	FATHER_WORK_DOMESTIC_FF1
	FATHER_WORK_SCHOOL_FF1
	FATHER_WORK_PENSIONED_FF1
	FATHER_WORK_DECEASED_FF1
	FATHER_WORK_DONT_KNOW_FF1
	FATHER_WORK_OTHER_FF1

    MOTHER_EDUCATION_FF1
	FATHER_EDUCATION_FF1

	ETHNICITY_NORWEGIAN_FF1
	ETHNICITY_SAMI_FF1
	ETHNICITY_KVENFINNISH_FF1
	ETHNICITY_OTHER_FF1
	ETHNICITY_OTHER_SPEC_FF1

	ETHNI0
	ETHNI1
	ETHNI2
	ETHNI3
	ETHNI4
	ETHNI5
	ETHNI6
	ETHNI7
	ETHNI8
	ETHNI9
	ETHNIA


################################################################################
Frienships overview:

    This has nothing to do with the social network, is about how relationships
    forms and how well or bad you make friends. We dont have any interesting
    variable anyway.
    
################################################################################
    
    
    NUMBER_SMS_FF1
    
    
    ################################################################################
    Puberty:
      ################################################################################
    MENARCHE_FF1
    MENARCHE_AGE_YEAR_FF1
    MENARCHE_AGE_MONTH_FF1
    
    PUBIC_HAIR_FEMALE_FF1
    BREASTS_FEMALE_FF1
    
    PUBIC_HAIR_MALE_FF1
    PUBIC_HAIR_AGE_MALE_FF1
    PUBERTY_BOYS_HEIGHT_FF1
    PUBERTY_BOYS_HAIR_BODY_FF1
    PUBERTY_BOYS_VOICE_FF1
    PUBERTY_BOYS_HAIR_FACE_FF1
    

    ################################################################################
    Sports:
      ################################################################################
    TRANSPORT_TYPE_SUMMER_FF1
    TRANSPORT_DURATION_SUMMER_FF1
    TRANSPORT_TYPE_WINTER_FF1
    TRANSPORT_DURATION_WINTER_FF1
    
    PHYS_ACT_LEISURE_FF1
    PHYS_ACT_OUTSIDE_SCHOOL_FF1
    PHYS_ACT_FREQ_FF1
    PHYS_ACT_HOURS_FF1
    PHYS_ACT_INTENSITY_FF1
    
    ################################################################################
    Hygiene:
      ################################################################################
    SHOWER_BATH_FREQUENCY_FF1
    HANDWASH_FREQUENCY_FF1
    BODY_LOTION_FREQUENCY_FF1
    SKIN_COLOUR_SUNBATHING_FF1
    HOLIDAY_SUNBATHING_FF1
    SOLARIUM_LAST_4WEEKS_FF1
    
    ################################################################################
    Hospital History:
      ################################################################################
    HOSPITAL_LAST_12M_FF1
    HOUSEHOLD_HEALTHSERVICES_FF1
    TONSILLECTOMY_FF1
    
    ################################################################################
    Inflamatory Biomarkers:
      LOD = late onset development ??
      NDL = ???????????
      
      The breaks inbetween biomarkers is just to divide by alphabetic order, so I
    can check that we have the same LOD and NDL
    ################################################################################
    
    OLKINF_BATCH_NUMBER_FF1
    OLKINF_FLAGGED_FF1
    
    ADA_LOD_OLKINF_FF1
    ARTN_LOD_OLKINF_FF1
    AXIN1_LOD_OLKINF_FF1
    
    BDNF_LOD_OLKINF_FF1
    BNGF_LOD_OLKINF_FF1
    
    CASP8_LOD_OLKINF_FF1
    CCL11_LOD_OLKINF_FF1
    CCL19_LOD_OLKINF_FF1
    CCL20_LOD_OLKINF_FF1
    CCL23_LOD_OLKINF_FF1
    CCL25_LOD_OLKINF_FF1
    CCL28_LOD_OLKINF_FF1
    CCL3_LOD_OLKINF_FF1
    CCL4_LOD_OLKINF_FF1
    CD244_LOD_OLKINF_FF1
    CD40_LOD_OLKINF_FF1
    CD5_LOD_OLKINF_FF1
    CD6_LOD_OLKINF_FF1
    CDCP1_LOD_OLKINF_FF1
    CSF1_LOD_OLKINF_FF1
    CST5_LOD_OLKINF_FF1
    CX3CL1_LOD_OLKINF_FF1
    CXCL1_LOD_OLKINF_FF1
    CXCL10_LOD_OLKINF_FF1
    CXCL11_LOD_OLKINF_FF1
    CXCL5_LOD_OLKINF_FF1
    CXCL6_LOD_OLKINF_FF1
    CXCL9_LOD_OLKINF_FF1
    
    DNER_LOD_OLKINF_FF1
    
    EIF4EBP1_LOD_OLKINF_FF1
    ENRAGE_LOD_OLKINF_FF1
    
    FGF19_LOD_OLKINF_FF1
    FGF21_LOD_OLKINF_FF1
    FGF23_LOD_OLKINF_FF1
    FGF5_LOD_OLKINF_FF1
    FLT3L_LOD_OLKINF_FF1
    
    GDNF_LOD_OLKINF_FF1
    
    HGF_LOD_OLKINF_FF1
    
    IFNG_LOD_OLKINF_FF1
    IL10_LOD_OLKINF_FF1
    IL10RA_LOD_OLKINF_FF1
    IL10RB_LOD_OLKINF_FF1
    IL12B_LOD_OLKINF_FF1
    IL13_LOD_OLKINF_FF1
    IL15RA_LOD_OLKINF_FF1
    IL17A_LOD_OLKINF_FF1
    IL17C_LOD_OLKINF_FF1
    IL18_LOD_OLKINF_FF1
    IL18R1_LOD_OLKINF_FF1
    IL1A_LOD_OLKINF_FF1
    IL2_LOD_OLKINF_FF1
    IL20_LOD_OLKINF_FF1
    IL20RA_LOD_OLKINF_FF1
    IL22RA1_LOD_OLKINF_FF1
    IL24_LOD_OLKINF_FF1
    IL2RB_LOD_OLKINF_FF1
    IL33_LOD_OLKINF_FF1
    IL4_LOD_OLKINF_FF1
    IL5_LOD_OLKINF_FF1
    IL6_LOD_OLKINF_FF1
    IL7_LOD_OLKINF_FF1
    IL8_LOD_OLKINF_FF1
    
    LIF_LOD_OLKINF_FF1
    LIFR_LOD_OLKINF_FF1
    
    MCP1_LOD_OLKINF_FF1
    MCP2_LOD_OLKINF_FF1
    MCP3_LOD_OLKINF_FF1
    MCP4_LOD_OLKINF_FF1
    MMP1_LOD_OLKINF_FF1
    MMP10_LOD_OLKINF_FF1
    
    NRTN_LOD_OLKINF_FF1
    NT3_LOD_OLKINF_FF1
    
    OPG_LOD_OLKINF_FF1
    OSM_LOD_OLKINF_FF1
    
    PDL1_LOD_OLKINF_FF1
    
    SCF_LOD_OLKINF_FF1
    SIRT2_LOD_OLKINF_FF1
    SLAMF1_LOD_OLKINF_FF1
    ST1A1_LOD_OLKINF_FF1	
    STAMBP_LOD_OLKINF_FF1
    
    TGFA_LOD_OLKINF_FF1
    TGFB1_LOD_OLKINF_FF1
    TNF_LOD_OLKINF_FF1
    TNFB_LOD_OLKINF_FF1
    TNFRSF9_LOD_OLKINF_FF1
    TNFSF14_LOD_OLKINF_FF1
    TRAIL_LOD_OLKINF_FF1
    TRANCE_LOD_OLKINF_FF1
    TSLP_LOD_OLKINF_FF1
    TWEAK_LOD_OLKINF_FF1
    
    UPA_LOD_OLKINF_FF1
    
    VEGFA_LOD_OLKINF_FF1
    
    ADA_NLD_OLKINF_FF1
    ARTN_NLD_OLKINF_FF1
    AXIN1_NLD_OLKINF_FF1
    
    BDNF_NLD_OLKINF_FF1
    BNGF_NLD_OLKINF_FF1
    
    CASP8_NLD_OLKINF_FF1
    CCL11_NLD_OLKINF_FF1
    CCL19_NLD_OLKINF_FF1
    CCL20_NLD_OLKINF_FF1
    CCL23_NLD_OLKINF_FF1
    CCL25_NLD_OLKINF_FF1
    CCL28_NLD_OLKINF_FF1
    CCL3_NLD_OLKINF_FF1
    CCL4_NLD_OLKINF_FF1
    CD244_NLD_OLKINF_FF1
    CD40_NLD_OLKINF_FF1
    CD5_NLD_OLKINF_FF1
    CD6_NLD_OLKINF_FF1
    CDCP1_NLD_OLKINF_FF1
    CSF1_NLD_OLKINF_FF1
    CST5_NLD_OLKINF_FF1
    CX3CL1_NLD_OLKINF_FF1
    CXCL1_NLD_OLKINF_FF1
    CXCL10_NLD_OLKINF_FF1
    CXCL11_NLD_OLKINF_FF1
    CXCL5_NLD_OLKINF_FF1
    CXCL6_NLD_OLKINF_FF1
    CXCL9_NLD_OLKINF_FF1
    
    DNER_NLD_OLKINF_FF1
    
    EIF4EBP1_NLD_OLKINF_FF1
    ENRAGE_NLD_OLKINF_FF1
    
    FGF19_NLD_OLKINF_FF1
    FGF21_NLD_OLKINF_FF1
    FGF23_NLD_OLKINF_FF1
    FGF5_NLD_OLKINF_FF1
    FLT3L_NLD_OLKINF_FF1
    
    GDNF_NLD_OLKINF_FF1
    
    HGF_NLD_OLKINF_FF1
    
    IFNG_NLD_OLKINF_FF1
    IL10_NLD_OLKINF_FF1
    IL10RA_NLD_OLKINF_FF1
    IL10RB_NLD_OLKINF_FF1
    IL12B_NLD_OLKINF_FF1
    IL13_NLD_OLKINF_FF1
    IL15RA_NLD_OLKINF_FF1
    IL17A_NLD_OLKINF_FF1
    IL17C_NLD_OLKINF_FF1
    IL18_NLD_OLKINF_FF1
    IL18R1_NLD_OLKINF_FF1
    IL1A_NLD_OLKINF_FF1	
    IL2_NLD_OLKINF_FF1
    IL20_NLD_OLKINF_FF1
    IL20RA_NLD_OLKINF_FF1
    IL22RA1_NLD_OLKINF_FF1
    IL24_NLD_OLKINF_FF1
    IL2RB_NLD_OLKINF_FF1
    IL33_NLD_OLKINF_FF1
    IL4_NLD_OLKINF_FF1
    IL5_NLD_OLKINF_FF1
    IL6_NLD_OLKINF_FF1
    IL7_NLD_OLKINF_FF1
    IL8_NLD_OLKINF_FF1
    
    LIF_NLD_OLKINF_FF1
    LIFR_NLD_OLKINF_FF1
    
    MCP1_NLD_OLKINF_FF1
    MCP2_NLD_OLKINF_FF1
    MCP3_NLD_OLKINF_FF1
    MCP4_NLD_OLKINF_FF1
    MMP1_NLD_OLKINF_FF1
    MMP10_NLD_OLKINF_FF1
    
    NRTN_NLD_OLKINF_FF1
    NT3_NLD_OLKINF_FF1
    
    OPG_NLD_OLKINF_FF1
    OSM_NLD_OLKINF_FF1
    
    PDL1_NLD_OLKINF_FF1
    
    SCF_NLD_OLKINF_FF1
    SIRT2_NLD_OLKINF_FF1
    SLAMF1_NLD_OLKINF_FF1
    ST1A1_NLD_OLKINF_FF1
    STAMBP_NLD_OLKINF_FF1
    
    TGFA_NLD_OLKINF_FF1
    TGFB1_NLD_OLKINF_FF1
    TNF_NLD_OLKINF_FF1
    TNFB_NLD_OLKINF_FF1
    TNFRSF9_NLD_OLKINF_FF1
    TNFSF14_NLD_OLKINF_FF1
    TRAIL_NLD_OLKINF_FF1
    TRANCE_NLD_OLKINF_FF1
    TSLP_NLD_OLKINF_FF1
    TWEAK_NLD_OLKINF_FF1
    
    UPA_NLD_OLKINF_FF1
    
    VEGFA_NLD_OLKINF_FF1
    
    ################################################################################
    Diet:
      ################################################################################
    fat_fish_ff1
    lean_fish_ff1
    seagull_eggs_ff1
    reindeer_ff1	
    