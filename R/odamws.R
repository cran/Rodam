require(httr)
#' API layer for the ODAM web services
#'
#' @author Daniel Jacob - INRAE UMR 1332 BFP (C) 2021
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
#' @field msgError contains an error message if an error occurs
#' @field maxtime defines the maximum request time
#' @field ssl_verifypeer defines if the peer's SSL certificate is verified
#' @examples
#'\dontrun{
#' dh <- new("odamws", "https://pmb-bordeaux.fr/getdata/", "frim1")
#' dn <- show(dh)
#' # Get data from 'samples' subset with a constraint
#' data <- dh$getDataByName('samples','sample/365')
#' # Get 'activome' data subset
#' ds <- dh$getSubsetByName('activome')
#' # Get the merged data of both data subsets based on their common identifiers
#' setNameList <- c("activome", "qNMR_metabo" )
#' dsMerged <- dh$getSubsetByName(setNameList)
#'}
#' @import httr
#' @importFrom methods setRefClass
#' @importFrom methods new
#' @export
odamws <- setRefClass("odamws",
   fields = list(
      wsURL       = "character",
      dsname      = "character",
      auth        = "character",
      subsets     = "data.frame",
      subsetNames = "character",
      connectList = "matrix",
      msgError    = "character",
      maxtime     = "numeric",
      ssl_verifypeer = "logical"
   ),

   methods = list(

      # Initialize the attributes
      initialize = function(wsURL, dsname, auth='', maxtime=30, ssl_verifypeer = TRUE)
      {
         options(stringsAsFactors=FALSE)
         options(warn=-1)
         wsURL <<- wsURL
         dsname <<- dsname
         auth <<- auth
         msgError <<- ''
         maxtime <<- maxtime
         ssl_verifypeer <<- ssl_verifypeer

         # Get subsets information
         subsets <<- getWS('subset')
         if (nchar(msgError)==0) {
             subsets <<- subsets[order(subsets$SetID),]
             subsetNames <<- as.vector(subsets[,'Subset'])
             connectList <<- cbind( subsets[subsets$LinkID>0, ]$LinkID , subsets[subsets$LinkID>0, ]$SetID )
             L<-NULL; for( s in subsets$Subset ) { L <- c(L, dim(getWS(s))[1]) }
             subsets$Count <<- L
         }
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
      VEC = function(x) as.vector(x),
      dateToStr = function(x) as.Date(x, origin = "1899-12-30"),
      timeToStr = function(x) paste0(round(x*24,0),'h',round((x*24-round(x*24,0))*60,0)),

      getWS = function (query='')
      {
      "Low level routine allowing to retrieve data or metadata from  a query formatted according the ODAM framework specifications - Returns a data.frame object. By default, i.e. with an empty query, a data.frame object containing metadata related to the data subsets is returned."

         myurl <- paste(wsURL, '/tsv/', dsname, '/', query, sep="")
         headers <-  c();
         if ( nchar(auth)>0 ) { headers <- c('x-api-key' = auth) }
         out <- data.frame()
         tryCatch({
             resp <- httr::GET(myurl, config = httr::config(ssl_verifypeer = ssl_verifypeer),
                                      add_headers(.headers = headers), timeout(maxtime))
             T <- simplify2array(strsplit(httr::content(resp, as='text'),"\n"))
             if (length(grep("(DOCTYPE|html)", T[1]))) {
                 msgError <<- "ERROR: the query-path is not valid"
             } else {
                 out <- read.csv(textConnection(T), head=TRUE, sep="\t")
                 if (nrow(out)==0) { msgError <<- gsub("\\.", " ", colnames(out))[1] }
             }
             if(nchar(msgError)>0) { cat(msgError) }
         }, error=function(e) {
             msgError <<- "ERROR : the API host is not responding; it is either not found or does not exist"
             cat(msgError)
         })
         out
      },

      getWSEntryByName = function(setName) {
         getWS(paste('(',setName,')/entry',sep=''))
      },

      getDataByID = function(setID, condition='')
      {
         getDataByName(subsetNames[setID],condition)
      },

      getDataByName = function(setName, condition='')
      {
      "Returns the data of the 'setName' subset as a data.frame"
         slash <- ifelse ( nchar(condition)==0 || substr(condition,1,1)=='/', '', '/' )
         getWS(paste('(',setName,')',slash, condition,sep=''))
      },

      getSubsetByID = function(setID, condition='', rmvars=FALSE)
      {
         getSubsetByName(subsetNames[setID],condition)
      },

      getSubsetByName = function(setNameList, condition='', rmvars=FALSE)
      {
      "Returns both data and metadatas of the subsets defined by 'setNameList' as a list of objects. 'setNameList' can contain one or more subset names. If 'setNameList' contains two or more subset names, the returned data set will correspond to the merged data subsets based on the identifiers of the first common data subset :

        data - a data.frame object containing the data. The column names of this data.frame are gathered according their categories and avaivalble in embedded lists, and described below.

        varnames, facnames, qualnames,  - Return lists containing  the 'quantitative' variables, the 'factor' variables,  the 'qualitative ' variables  respectively.

        varsBySubset - a list containing the 'quantitative' variables by subset.

        idSet - a data.frame containing the metadata about the common identifier, namely the subset name it belongings (Subset), the identifier name (Attribute), the description (Description), the type (Type), and the CVTerm (CV_Term_ID, CV_Term_Name).

        idName - the identifier name (Attribute) of the first common data subset.

        LABELS - a data.frame containing the metadata about all attributes - its format is the same as the 'samplename' data.frame.

        WSEntry - a data.frame containing the correspondance between some attributes and their alias name, these latter serving within a query to put a constraint a or selection on this attribute. Note: a 'WSEntry' is an alias name associated with an attribute that allows user to query the data subset by putting a filter condition (i.e. a selection constraint) on the corresponding attribute. Not all attributes have a WSEntry but only few ones, especially the attributes within the identifier and factor categories. For instance, the WSEntry of the 'SampleID' attribute is 'sample'. Thus, if you want to select only samples with their ID equal to 365, you have to specify the filter condition as  'sample/365'."

         # Get SetIDs
         setIDList <- subsets[ subsets$Subset %in% setNameList, ]$SetID
         strNameList <- paste(setNameList, collapse=',')

         # Get DATA
         slash <- ifelse ( nchar(condition)==0 || substr(condition,1,1)=='/', '', '/' )
         data <- getWS(paste('(',strNameList,')',slash, condition,sep=''))

         # Get quantitative variable features
         varnames <- NULL
         Q <- getWS(paste('(',strNameList,')/quantitative',sep=''))
         for( i in 1:length(setNameList) ) varnames <- rbind(varnames,  Q[Q$Subset == setNameList[i], ])

         # Get qualitative variable features
         qualnames <- getWS(paste('(',strNameList,')/qualitative',sep=''))

         # Get factor features
         facnames <- getWS(paste('(',strNameList,')/factor',sep=''))

         # Get factor features
         identifiers <- getWS(paste('(',strNameList,')/identifier',sep=''))

         # Get WSEntries
         entries <- getWS(paste('(',strNameList,')/entry',sep=''))

         # Gather all qualitative features
         features <- rbind(identifiers, facnames, qualnames)

         # Get Samples: attribute features, list of identifiers
         L <- NULL
         if (length(setNameList)==1) {
             L[1] <- subsets[ subsets$Subset==setNameList[1], ]$SetID
         } else for( i in 1:length(setNameList) ) {
             l <- c( subsets[ subsets$Subset==setNameList[i], ]$SetID )
             while( l[length(l)]>0 ) l <- c(l, subsets[ subsets$SetID==l[length(l)], ]$LinkID )
             if (i==1) {
                L <- l
             } else {
                L <- l[l %in% L]
             }
         }

         setName <- subsets[ subsets$SetID==L[1], ]$Subset
         Q <- getWS(paste('(',setName,')/identifier',sep=''))
         samplename <- Q[Q$Subset %in% setName, ]

         # Merge all labels
         LABELS <- rbind(
            matrix( c( as.matrix(identifiers)[,c(1:4)], replicate(nrow(identifiers),'Identifier' ), as.matrix(identifiers)[,c(6:7)]), ncol=7, byrow=FALSE  ),
            matrix( c( as.matrix(facnames)[,c(1:4)], replicate(dim(facnames)[1],'Factor'  ), as.matrix(facnames)[,c(6:7)] ), ncol=7, byrow=FALSE  ),
            matrix( c( as.matrix(varnames)[,c(1:4)], replicate(dim(varnames)[1],'Variable'), as.matrix(varnames)[,c(6:7)] ), ncol=7, byrow=FALSE  )
         )
         if (dim(as.matrix(qualnames))[1]>0 ) { LABELS <- rbind ( LABELS,
            matrix( c( as.matrix(qualnames)[,c(1:4)], replicate(dim(qualnames)[1],'Feature'), as.matrix(qualnames)[,c(6:7)] ), ncol=7, byrow=FALSE )
         )}
         colnames(LABELS) <- c( 'Subset', 'Attribute', 'WSEntry', 'Description', 'Type', 'CV_Term_ID ', 'CV_Term_Name' )
         LABELS[,6] <- sapply(VEC(LABELS[,6]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
         LABELS[,7] <- sapply(VEC(LABELS[,7]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
         LABELS <- as.data.frame(LABELS)

         varsBySubset <- list()
         for(setName in setNameList)
              varsBySubset[[setName]] <- VEC(varnames$Attribute[ varnames$Attribute %in% LABELS[ LABELS$Subset==setName, ]$Attribute ])

         for( i in 1:dim(varnames)[1]) { if (VEC(varnames$Type[i]) == 'numeric') data[,VEC(varnames$Attribute[i])] <- NUM(data[,VEC(varnames$Attribute[i])]); }
         for( i in 1:dim(samplename)[1]) { if (VEC(samplename$Type[i]) == 'numeric') data[,VEC(samplename$Attribute[i])] <- NUM(data[,VEC(samplename$Attribute[i])]); }

         # Remove quantitative variables with all values at zero
         if (rmvars) {
            V <- simplify2array( lapply(varnames$Attribute, function(v) { sum( which(data[, VEC(v)]!=0) ) }) )
            if (length(which(V==0))>0) {
               data <- data[, ! colnames(data) %in% VEC(varnames$Attribute[ c(which(V==0))]) ]
               LABELS <- LABELS[! LABELS[,1] %in% VEC(varnames$Attribute[c(which(V==0))]), ]
               varnames <- varnames[ -c(which(V==0)), ]
            }
         }

         list( setID=setIDList, setName=setNameList, data=data[ , unique(LABELS$Attribute) ],
               samplename=VEC(samplename$Attribute), identifiers=unique(VEC(identifiers$Attribute)),
               varnames=VEC(varnames$Attribute), varsBySubset=varsBySubset, facnames=VEC(facnames$Attribute),
               qualnames=VEC(qualnames$Attribute), features=VEC(unique(features$Attribute)),
               WSEntry = entries, LABELS=LABELS )
      },

      getCommonID = function(refID, setName1, setName2)
      {
      "Returns the list of  identifiers (defined by refID as an identifier attribute label) that are in common between two subsets (defined by the attribute label of the setName1 and setName2 subsets) i.e. resulting in the intersection of the two identifier sets."
         ds1 <- getSubsetByName(setName1)
         ds2 <- getSubsetByName(setName2)
         unique(ds1$data[ ds1$data[, refID ] %in% ds2$data[, refID ], refID ])
      },

      getSetInCommon = function(setNameList)
      {
      "Get the data subset in common with the data subset list 'setNameList'. Returns a list containing the elements :

          *  refID: Main Keyname serving as reference ID along with all data subsets defined in setNameList,

          *  setName : the data subset name corresponding to the refID"
         setlines <- which(subsets$Subset %in% setNameList)
         setIDS <- unique(sort(subsets[setlines, ]$Identifier))
         if (length(setIDS)==1) {
           refID <- setIDS
           setName <- subsets$Subset[ subsets$SetID == min(subsets$SetID[subsets$Identifier==setIDS]) ]
         } else {
           setName <- subsets$Subset[ subsets$SetID == min(subsets$LinkID[ subsets$Identifier %in% setIDS ]) ]
           refID <- subsets$Identifier[subsets$Subset==setName]
         }
         list(refID=refID, setName=setName)
      },

      getUpSetTable = function(setNameList)
      {
      " Return an encoded dataframe in binary and set up so that columns represent data subsets present in 'setNameList', and each row represents an element (ID). If an element (ID) is in the data subset it is represented as a 1 in that position, otherwise it is represented as a 0. Useful for use with the R package UpSetR (https://cran.r-project.org/package=UpSetR)"
           # Get the data subset in common
           R <- getSetInCommon(setNameList)
           # Get the reference data subset
           ds1 <- getSubsetByName(R$setName)
           g0 <- as.vector(unique(ds1$data[,R$refID]))

           # For data subset in setNameList, compute the count of common ids
           input  <-list()
           for( k in 1:length(setNameList) ) {
               gk <- getCommonID(R$refID, R$setName ,setNameList[k])
               input[[k]] <- which(g0 %in% gk)
           }
           nsets <- length(setNameList)
           M <- NULL; for(i in 1:nsets) M <- c(M, input[[i]]);
           M <- unique(sort(M))
           V <- NULL; for(i in 1:nsets) V <- cbind(V, (M %in% input[[i]])*1)
           rownames(V) <- M
           colnames(V) <- setNameList
           as.data.frame(V)
      },

      getMerged = function(refID, setName1, setName2)
      {
      "[DEPRECATED] Returns a data.frame containing data obtained by merging two subsets (defined by the attribute label of the setName1 and setName2 subsets) that have the same identifiers in common (defined by refID as an identifier attribute label)  i.e. resulting in the intersection of the two identifier sets."
         ds1 <- getSubsetByName(setName1)
         ds2 <- getSubsetByName(setName2)
         CommonID <- unique(ds1$data[ ds1$data[, refID ] %in% ds2$data[, refID ], refID ])
         subds1 <- unique( ds1$data[ ds1$data[, refID] %in% CommonID, c(refID, ds1$facnames, ds1$varnames) ])
         subds1 <- subds1[ order(subds1[ ,refID ]), ]
         subds2 <- unique(ds2$data[ ds2$data[, refID] %in% CommonID, c(refID, ds2$facnames, ds2$varnames) ])
         subds2 <- subds2[ order(subds2[ ,refID ]), ]
         cbind( subds1, subds2[ , ! colnames(subds2) %in% colnames(subds1) ] )
      }
   )
)
