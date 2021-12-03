
getPGuidToPNumber <- function(
    DF.nested = NULL
    ) {

    # thisFunctionName <- "getPGuidToPNumber";
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- DF.nested;
    DF.output <- DF.output[DF.output[,'key1'] == 'Page',      ];
    DF.output <- DF.output[DF.output[,'key3'] == 'properties',];
    DF.output <- DF.output[DF.output[,'key4'] == 'pageNumber',];
    DF.output <- DF.output[,c('key2','value')];
    base::colnames(DF.output) <- base::c('guid','pageNumber');
    DF.output <- base::unique(DF.output);

    # base::cat("\n# utils::str(DF.output)\n");
    # base::print(   utils::str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat(base::paste0("\n",thisFunctionName,"() quits."));
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
