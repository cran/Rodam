require(RCurl)
#' API layer for the ODAM web services
#'
#' @author Daniel Jacob - INRA UMR 1332 BFP (C) 2016
#'
#' @description the class that implements the API layer for the ODAM (Open Data for Access and Mining) web services.
#'
#' Provides functions to allow you to retrieve online data using ODAM Web Services. This obviously requires that data are implemented according the ODAM approach (Open Data for Access and Mining), namely that the data subsets were deposited in the suitable data repository in the form of TSV files associated with  their metadata also described  in TSV files.
#' 
#' @rdname odamws
#' @field wsURL defines the URL of the webservice - Must be specify when creating a new instance of the odamws object.
#' @field dsname specifies the name of the Dataset to query - Must be specify when creating a new instance of the odamws object.
#' @field auth specifies the authentication code to access to the Dataset by this webservice (if required)
#' @field subsets a data.frame object containing metadata related to the data subsets - Initialized during the instantiation step
#' @field subsetNames a list of the data subset names - Initialized during the instantiation step
#' @field connectList a matrix of the connection graph between data subsets (i.e. the links between each subset with the subset at its origin, so that links can be interpreted as 'obtained from'). The data subsets are referred by their subset number. (corresponding to the 'SetID' column in the 'subsets' field)  - Initialized during the instantiation step.
#' @examples
#'\dontrun{
#' dh <- new("odamws", "http://www.bordeaux.inra.fr/pmb/getdata/", "frim1")
#' dn <- show(dh)
#' # Get data from 'samples' subset with a constraint
#' data <- dh$getDataByName('samples','sample/365')
#'  # Get 'activome' data subset
#' ds <- dh$getSubsetByName('activome')
#' # Get the merged data of both data subsets based on their common identifier
#' refID <- "SampleID"
#' setName1 <- "activome"
#' setName2 <- "qNMR_metabo"
#' dsMerged <- dh$getMerged(refID, setName1, setName2)
#'}
#' @import RCurl
#' @importFrom methods setRefClass
#' @importFrom methods new
#' @export
odamws <- setRefClass("odamws", 
   fields = list(
      wsURL       = "character",
      dsname      = "character",
      delimiter   = "character",
      auth        = "numeric",
      subsets     = "data.frame",
      subsetNames = "character",
      connectList = "matrix"
   ), 
   methods = list(
   
      # Initialize the attributes
      initialize = function(wsURL, dsname, auth=1)
      {
         wsURL <<- wsURL
         dsname <<- dsname
         auth <<- auth

         # Get subsets information
         subsets <<- getWS('subset')
         subsets <<- subsets[order(subsets$SetID),]
         subsetNames <<- as.vector(subsets[,'Subset'])
         connectList <<- cbind( subsets[subsets$LinkID>0, ]$LinkID , subsets[subsets$LinkID>0, ]$SetID )
         L<-NULL; for( s in subsets$Subset ) { L <- c(L, dim(getWS(s))[1]) }
         subsets$Count <<- L
      },

      getDataTree = function() {
      "Returns a data.tree object filled up  according to the data subset metadata"
         fillDN = function( dn, indx) {
             L <- as.vector(connectList[ connectList[,1]==indx, 2])
             if (length(L)>0) {
                for (i in 1:length(L)) {
                    dn$AddChild( subsets[subsets$SetID==L[i],]$Subset )
                    dn$children[[i]] <- fillDN( dn$children[[i]], L[i])
                    dn$children[[i]]$Description <- subsets[subsets$SetID==L[i],]$Description
                    dn$children[[i]]$Identifier <- subsets[subsets$SetID==L[i],]$Identifier
                    dn$children[[i]]$WSEntry <- subsets[subsets$SetID==L[i],]$WSEntry
                    dn$children[[i]]$SetID <- subsets[subsets$SetID==L[i],]$SetID
                    dn$children[[i]]$Count <- subsets[subsets$SetID==L[i],]$Count
                }
             }
             dn
         }
         indx <- min(connectList[,1])
         if (requireNamespace("data.tree", quietly = TRUE)) {
             dn <- data.tree::Node$new( subsets[subsets$SetID==indx,]$Subset )
             dn$Description <- subsets[subsets$SetID==indx,]$Description
             dn$Identifier <- subsets[subsets$SetID==indx,]$Identifier
             dn$WSEntry <- subsets[subsets$SetID==indx,]$WSEntry
             dn$SetID <- subsets[subsets$SetID==indx,]$SetID
             dn$Count <- subsets[subsets$SetID==indx,]$Count
             dn <- fillDN(dn,indx)
         } else {
             dn <- subsets
         }
         dn
      },

      # Show subsets list by default
      show = function()
      {
      "Show the hierarchical tree of data subsets. Returns a data.tree object. See 'getDataTree' method."
         if (requireNamespace("data.tree", quietly = TRUE)) {
             dn <- getDataTree()
             print(dn, "SetID", "Identifier", "WSEntry", "Description", "Count")
         } else {
             print(subsets[ , c("SetID", "LinkID", "Identifier", "WSEntry", "Description", "Count") ])
         }
      },

      trim = function(x) gsub("^\\s+|\\s+$", "", x),
      NUM = function(x) as.numeric(as.vector(x)),
      CHAR = function(x) as.vector(x),
      dateToStr = function(x) as.Date(x, origin = "1899-12-30"),
      timeToStr = function(x) paste0(round(x*24,0),'h',round((x*24-round(x*24,0))*60,0)),

      getWS = function (query='')
      {
      "Low level routine allowing to retrieve data or metadata from  a query formatted according the ODAM framework specifications - Returns a data.frame object. By default, i.e. with an empty query, a data.frame object containing metadata related to the data subsets is returned."

         myurl <- paste(wsURL,'/tsv/', dsname, '/', query,"?auth=",auth,sep="");
         read.csv(textConnection(RCurl::getURL(myurl)), head=TRUE, sep="\t");
      },

      getWSEntryByName = function(setName) {
         getWS(paste('(',setName,')/entry',sep=''))
      },

      getDataByID = function(setID,condition='')
      {
         getDataByName(subsetNames[setID],condition)
      },

      getDataByName = function(setName,condition='')
      {
      "Returns the data of the 'setName' subset as a data.frame"
         slash <- ifelse ( nchar(condition)==0 || substr(condition,1,1)=='/', '', '/' )
         getWS(paste('(',setName,')',slash, condition,sep=''))
      },

      getSubsetByID = function(setID,condition='') {
         getDataSetByName(subsetNames[setID],condition)
      },

      getSubsetByName = function(setName,condition='') {
      "Returns both data and metadate of the 'setName' subset as an  object list:

		data - a data.frame object containing the data. The column names of this data.frame are gathered according their categories and avaivalble in embedded lists, and described below.

		varnames, facnames, qualnames,  - Return lists containing  the 'quantitative' variables, the 'factor' variables,  the 'qualitative ' variables  respectively.

		samplename - a data.frame containing the metadata about the data subset, namely its name (Subset), its identifier (Attribute), its description (Description), its type (Type), and its CVTerm (CV_Term_ID, CV_Term_Name)

		LABELS - a data.frame containing the metadata about all attributes - its format is the same as the 'samplename' data.frame.

		WSEntry - a data.frame containing the correspondance between some attributes and their alias name, these latter serving within a query to put a constraint a or selection on this attribute. Note: a 'WSEntry' is an alias name associated with an attribute that allows user to query the data subset by putting a filter condition (i.e. a selection constraint) on the corresponding attribute. Not all attributes have a WSEntry but only few ones, especially the attributes within the identifier and factor categories. For instance, the WSEntry of the 'SampleID' attribute is 'sample'. Thus, if you want to select only samples with their ID equal to 365, you have to specify the filter condition as  'sample/365'."

         setID <- which(subsetNames==setName)
         # Get DATA
         slash <- ifelse ( nchar(condition)==0 || substr(condition,1,1)=='/', '', '/' )
         data <- getWS(paste('(',setName,')',slash, condition,sep=''))

         # Get Samples: attribute features, list of identifiers
         I <- getWS(paste('(',setName,')/identifier',sep=''))
         samplename <- I[I$Subset == setName, ]
         samples <- CHAR(samplename$Attribute)

         # Get quantitative variable features
         Q <- getWS(paste('(',setName,')/quantitative',sep=''))
         varnames <- Q[Q$Subset == setName, ]

         # Get qualitative variable features
         Q <- getWS(paste('(',setName,')/qualitative',sep=''))
         qualnames <- Q

         # Get factor features
         facnames <- getWS(paste('(',setName,')/factor',sep=''))

         # Get all qualitative features
         features <- rbind(I, facnames, qualnames)

         # Get WSEntries 
         entries <- getWS(paste('(',setName,')/entry',sep=''))

         # Merge all labels
         LABELS <- rbind( 
            matrix( c( as.matrix(samplename)[,c(2:3)], 'Identifier', as.matrix(samplename)[,c(5:6)]), ncol=5, byrow=FALSE  ),
            matrix( c( as.matrix(facnames)[,c(2:3)], replicate(dim(facnames)[1],'Factor'  ), as.matrix(facnames)[,c(5:6)] ), ncol=5, byrow=FALSE  ),
            matrix( c( as.matrix(varnames)[,c(2:3)], replicate(dim(varnames)[1],'Variable'), as.matrix(varnames)[,c(5:6)] ), ncol=5, byrow=FALSE  )
         )
         if (dim(as.matrix(qualnames))[1]>0 ) { LABELS <- rbind ( LABELS, 
            matrix( c( as.matrix(qualnames)[,c(2:3)], replicate(dim(qualnames)[1],'Feature'), as.matrix(qualnames)[,c(5:6)] ), ncol=5, byrow=FALSE )
         )}
         colnames(LABELS) <- c( 'Attribute', 'Description', 'Type', 'CV_Term_ID ', 'CV_Term_Name' )
         LABELS[,4] <- sapply(CHAR(LABELS[,4]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
         LABELS[,5] <- sapply(CHAR(LABELS[,5]), function(x) { ifelse( ! is.na(x), x, "NA" ); })

         for( i in 1:dim(varnames)[1]) { if (CHAR(varnames$Type[i]) == 'numeric') data[,CHAR(varnames$Attribute[i])] <- NUM(data[,CHAR(varnames$Attribute[i])]); }
         for( i in 1:dim(samplename)[1]) { if (CHAR(samplename$Type[i]) == 'numeric') data[,CHAR(samplename$Attribute[i])] <- NUM(data[,CHAR(samplename$Attribute[i])]); }

         list( setID=setID, setName=setName, data=data, 
               samplename=samplename, samples=samples, varnames=CHAR(varnames$Attribute), facnames=CHAR(facnames$Attribute), 
                                                       qualnames=CHAR(qualnames$Attribute), features=CHAR(features$Attribute), 
                                                       WSEntry = entries, LABELS=as.data.frame(LABELS) )
      },

      getCommonID = function(refID, setName1, setName2)
      {
      "Returns the list of  identifiers (defined by refID as an identifier attribute label) that are in common between two subsets (defined by the attribute label of the setName1 and setName2 subsets) i.e. resulting in the intersection of the two identifier sets."
         ds1 <- getSubsetByName(setName1)
         ds2 <- getSubsetByName(setName2)
         unique(ds1$data[ ds1$data[, refID ] %in% ds2$data[, refID ], refID ])
      },

      getMerged = function(refID, setName1, setName2)
      {
      "Returns a data.frame containing data obtained by merging two subsets (defined by the attribute label of the setName1 and setName2 subsets) that have the same identifiers in common (defined by refID as an identifier attribute label)  i.e. resulting in the intersection of the two identifier sets."
         ds1 <- getSubsetByName(setName1)
         ds2 <- getSubsetByName(setName2)
         CommonID <- unique(ds1$data[ ds1$data[, refID ] %in% ds2$data[, refID ], refID ])
         subds1 <- unique(ds1$data[ ds1$data[, refID] %in% CommonID, c(refID, ds2$facnames, ds1$varnames) ])
         subds1 <- subds1[ order(subds1[ ,refID ]), ]
         subds2 <- unique(ds2$data[ ds2$data[, refID] %in% CommonID, c(refID, ds2$varnames) ])
         subds2 <- subds2[ order(subds2[ ,refID ]), ]
         cbind( subds1, subds2[ , -1 ] )
      }
   )
)
