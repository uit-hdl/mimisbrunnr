############################
# GET NETWORK INFORMATION
# -- getFrienshipTypes()   For a given ID and Frienships Matrix, return the
#                          friends IDs and Total numbers of relationships for
#                          the given ID.
#
# -- addEdgeRelationship() For a given edges DF, check if the nodes share some
#                          common value. Doesn't modify the original DF.
#
# -- deleteConnections()   For a given edges DF and list of IDS, tells you which
#                          rows from the given edgesDF you should delete.
#                          Doesn't modify the original DF.
############################
{
  # For a given ID and a given friendship matrix (not graph), return:
  #
  # How many people this ID nominated and their IDs
  # How many people nominated this ID and their IDs (popularity)
  # How many frienships are reciprocal and their IDs
  #
  # The friendship matrix must be of the type 0/1 and have IDs coincide
  # with the rows and column indexes.
  #
  # Return a list
  #
  # 1: total key popularity
  # 2: total key nominations
  # 3: total key reciprocals
  # 4: IDs that nominated this key
  # 5: IDs that this key nominated
  # 6: IDs that are reciprocal
  #
  getFrienshipTypes <- function(key, frienshipMatrix){
    
    totalRows = nrow(frienshipMatrix)
    
    # -- Popularity
    myPopularity     = sum(frienshipMatrix[,key])
    myPopularityIDs  = grep(1, frienshipMatrix[,key])
    # -- Nominations
    myNominations    = sum(frienshipMatrix[key,1:totalRows])
    myNominationsIDs = grep(1,frienshipMatrix[key,1:totalRows])
    # -- Reciprocals
    myReciprocalIDs  = myPopularityIDs[myPopularityIDs %in% myNominationsIDs]
    myReciprocal     = length(myReciprocalIDs)
    
    myReturn = vector("list", length = 6)
    myReturn[[1]] = myPopularity
    myReturn[[2]] = myNominations
    myReturn[[3]] = myReciprocal
    myReturn[[4]] = myPopularityIDs
    myReturn[[5]] = myNominationsIDs
    myReturn[[6]] = myReciprocalIDs
    
    return (myReturn)
    
  }
  
  # Shortcut for the function above, gives the IDs of all undirected friends
  # of a given ID.
  getUndirectedFriends <- function(key, frienshipMatrix){

    myFriends =  unique(c(getFrienshipTypes(key, friendshipMatrix)[[4]],
                          getFrienshipTypes(key, friendshipMatrix)[[5]]))
            
      
  }
  
  # For a given list of edges, and a given table, check whether the FROM and TO
  # nodes have the same value for a giving index referencing a category.
  #
  # The function does not modify the original DF, you need to asign the returned
  # vector to a new column of your edgesDF.
  #
  # Return a list of either: 
  #     TRUE/FALSE for each edge, if they have the same category (1)
  #     Category name / NOT_QUAL, if they have the same category (2)
  #
  # (bool) keepName. If FALSE (default) , it will return case (1), otherwise (2)
  #
  # Examples:
  #
  # edgesDF
  
  # From     To
  #   1       2
  #   1       3
  #   3       1
  #   3       4
  #   4       2
  #
  # tableBase
  # 1   2     3        4
  #
  # 1   Man   Healthy  Never
  # 2   Man   Obese    Never
  # 3   Woman Obese    Daily
  # 4   Woman Healthy  Sometimes
  #
  # ComparingIndex: 4
  #
  # KeepName = FALSE
  # From     To         Returned Vector:
  #   1       2         TRUE
  #   1       3         FALSE
  #   3       1         FALSE
  #   3       4         FALSE
  #   4       2         FALSE
  #
  # KeepName = TRUE
  # From     To         Returned Vector:
  #   1       2         Never
  #   1       3         NOT_EQUAL
  #   3       1         NOT_EQUAL
  #   3       4         NOT_EQUAL
  #   4       2         NOT_EQUAL
  #
  addEdgeRelationship <- function(edgesDF, tableBase, comparingIndex, keepName = FALSE){
    
    totalEdges   = nrow(edgesDF)
    resultVector = rep(FALSE, totalEdges)
    
    # Add the same/different relationship to the edges list
    for(i in 1:totalEdges){
      
      # Get the From and To
      myFromID = edgesDF[i,1]
      myToID   = edgesDF[i,2]
      
      # Find them in the table
      modalityFrom = tableBase[tableBase$ID == myFromID, comparingIndex]
      modalityTo   = tableBase[tableBase$ID == myToID,   comparingIndex]
      
      # Check if they are the same or not
      resultVector[i] = (modalityFrom == modalityTo)
      
      # Add the modality if necesary
      if(keepName == TRUE){
        if(resultVector[i] == TRUE)  resultVector[i] = as.character(modalityFrom)
        else                         resultVector[i] = "NOT_EQUAL"
      }
      
    }
    
    return(resultVector)
    
  }
  
  # This function is useful to delete relationships in an edges dataframe.
  #
  # You might want to delete all edges that source or arrives to a particular ID,
  # or you might want to delete all edges that preceselly from a given ID to a 
  # given ID.
  #
  # Typically, you always want the first case, and you want to delete IDs {3,5,7}
  # for example. So you want to delete all FROMS 3,5,7, and all TOS 3,5,7. So
  # you will almost always use the first column of the returned DF to modify your
  # original edges table
  #
  # Given a list of from_IDs and to_IDs, return a list with:
  # -- All the edges that have any FROM or TO IDs
  # -- All the edges that have both FROM or TO IDs
  #
  # ie:
  #
  # Edges:
  # From To
  # 1    2
  # 1    3
  # 2    1
  # 2    4
  # 4    1
  # 1    4
  #
  # Delete FROM{1,4} TO{1}
  #
  # Will return
  #
  # OR AND
  # T  F
  # T  F
  # F  F
  # T  F
  # F  F
  # T  T
  #
  #
  # Return two list of TRUE/FALSE rows that satisfy the OR/AND relationship
  deleteConnections <- function(edgesDF, listOfFroms, listOfTos){
    
    totalFroms         = length(listOfFroms)
    totalTos           = length(listOfTos)
    totalRelationships = nrow(edgesDF)
    
    candidatesOR  = rep(FALSE, totalRelationships)
    candidatesAND = rep(FALSE, totalRelationships)
    
    # Analyze stuff
    # ---- FROMS
    for(i in 1:totalFroms){
      
      currentFrom   = listOfFroms[i]
      
      candidatesOR  = candidatesOR | (edgesDF[,1] == currentFrom)
      candidatesAND = candidatesOR
    }
    # ---- TOS
    for(i in 1:totalTos){
      
      currentTo = listOfTos[i]
      
      candidatesOR  = candidatesOR  | (edgesDF[,2] == currentTo)
      candidatesAND = candidatesAND & candidatesOR
      
    }
    
    # Return the dataframes
    myReturn = vector("list", length = 2)
    myReturn[[1]] = candidatesOR
    myReturn[[2]] = candidatesAND
    
    return (myReturn)
    
  }
  
}


# From an edge dataframe, generate the friendship matrix. I also need the total
# number of people since there are people who don't nominate or get nominated.
getFriendshipMatrix <- function(edgesDF, totalPeople){

    friendshipMatrix = matrix(0, nrow = totalPeople, ncol = totalPeople)
    
    # Now you just need to fill the matrix edge by edge
    for(i in 1:nrow(edgesDF)){
    
        currentFrom  = as.numeric(edgesDF[i,1])
        currentTo    = as.numeric(edgesDF[i,2])
        currentValue = as.numeric(edgesDF[i,3])
        
        friendshipMatrix[currentFrom, currentTo] = currentValue
        
        
    }
    
    return(friendshipMatrix)
    
        
    
}


