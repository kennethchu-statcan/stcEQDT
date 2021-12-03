
getReferenceIDToElementID <- function(
    DF.nested           = NULL,
    DF.QGuid.to.QNumber = NULL,
    DF.PGuid.to.PNumber = NULL
    ) {

    # thisFunctionName <- "getReferenceIDToElementID";
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.referenceID.to.referentID <- DF.nested[DF.nested[,'key2'] == 'References',];
    DF.referenceID.to.referentID <- DF.referenceID.to.referentID[DF.referenceID.to.referentID[,'key4'] %in% base::c('referentId'),base::c('key3','value')];
    base::colnames(DF.referenceID.to.referentID) <- base::gsub(x = base::colnames(DF.referenceID.to.referentID), pattern = "^key3$",  replacement = "referenceID");
    base::colnames(DF.referenceID.to.referentID) <- base::gsub(x = base::colnames(DF.referenceID.to.referentID), pattern = "^value$", replacement = "referentID" );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.referentID.to.elementID <- DF.nested[DF.nested[,'key2'] == 'Referents',];
    DF.referentID.to.elementID <- DF.referentID.to.elementID[DF.referentID.to.elementID[,'key4'] %in% base::c('id'),base::c('key3','value')];
    base::colnames(DF.referentID.to.elementID) <- base::gsub(x = base::colnames(DF.referentID.to.elementID), pattern = "^key3$",  replacement = "referentID");
    base::colnames(DF.referentID.to.elementID) <- base::gsub(x = base::colnames(DF.referentID.to.elementID), pattern = "^value$", replacement = "elementID" );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.referentID.to.elementID[,'elementID'] <- base::apply(
        X      = DF.referentID.to.elementID[,base::c('referentID','elementID')],
        MARGIN = 1,
        FUN    = function(x) {
            if ( x[2] %in% DF.QGuid.to.QNumber[,'guid'] ) {
                temp.string <- DF.QGuid.to.QNumber[DF.QGuid.to.QNumber[,'guid'] == x[2],'questionNumber'];
                temp.string <- base::paste0("questionNumber ",temp.string);
                return( temp.string );
            } else {
                return( x[2] );
                }
            }
        );

    DF.referentID.to.elementID[,'elementID'] <- base::apply(
        X      = DF.referentID.to.elementID[,base::c('referentID','elementID')],
        MARGIN = 1,
        FUN    = function(x) {
            if ( x[2] %in% DF.PGuid.to.PNumber[,'guid'] ) {
                temp.string <- DF.PGuid.to.PNumber[DF.PGuid.to.PNumber[,'guid'] == x[2],'pageNumber'];
                temp.string <- base::paste0("pageNumber ",temp.string);
                return( temp.string );
            } else {
                return( x[2] );
                }
            }
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.referenceID.to.elementID <- base::merge(
        x     = DF.referenceID.to.referentID,
        y     = DF.referentID.to.elementID,
        all.x = TRUE,
        by    = "referentID"
        );

    # base::cat("\n# utils::str(DF.referenceID.to.elementID)\n");
    # base::print(   utils::str(DF.referenceID.to.elementID)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- base::list(
        referenceID.to.elementID = DF.referenceID.to.elementID,
        referentID.to.elementID  = DF.referentID.to.elementID
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat(base::paste0("\n",thisFunctionName,"() quits."));
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

##################################################
