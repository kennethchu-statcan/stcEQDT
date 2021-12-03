
getElementToLocalization <- function(
    DF.nested                   = NULL,
    DF.referenceID.to.elementID = NULL,
    DF.localization             = NULL,
    element.types               = c('datapointValue','displayTarget','gotoTarget','setTarget')
    ) {

    thisFunctionName <- "getElementToLocalization";
    base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- DF.nested[DF.nested[,'key1'] != 'Reference',];
    DF.output <- DF.output[DF.output[,'key4'] %in% element.types,c('key4','value')];
    base::colnames(DF.output) <- base::gsub(x = base::colnames(DF.output), pattern = "^value$", replacement = "referenceID");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- base::merge(
        x  = DF.output,
        y  = DF.referenceID.to.elementID[,c('referenceID','referentID')],
        by = "referenceID"
        );
    DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),"key4")];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- base::merge(
        x     = DF.output,
        y     = DF.nested[DF.nested[,'key4'] == 'id',c('value','key2')],
        all.x = TRUE,
        by.x  = 'referentID',
        by.y  = 'value'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- base::merge(
        x     = DF.output,
        y     = DF.nested[DF.nested[,'key4'] == 'text',c('key2','value')],
        all.x = TRUE,
        by    = 'key2'
        );
    DF.output <- DF.output[,c('referenceID','referentID','key2','value')];
    base::colnames(DF.output) <- base::gsub(x = base::colnames(DF.output), pattern = "^key2$",  replacement = "guid"          );
    base::colnames(DF.output) <- base::gsub(x = base::colnames(DF.output), pattern = "^value$", replacement = "localizationID");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- base::merge(
        x     = DF.output,
        y     = DF.localization,
        all.x = TRUE,
        by.x  = 'localizationID',
        by.y  = 'localizationID'
        );
    DF.output <- DF.output[,c('referenceID','referentID','guid','localizationID','english','french')];

    base::cat("\n# DF.output\n");
    base::print(   DF.output   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::cat(base::paste0("\n",thisFunctionName,"() quits."));
    base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
