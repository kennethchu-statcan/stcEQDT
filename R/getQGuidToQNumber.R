
getQGuidToQNumber <- function(
    DF.nested = NULL
    ) {

    # thisFunctionName <- "getQGuidToQNumber";
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- DF.nested;
    DF.output <- DF.output[DF.output[,'key1'] == 'Question',      ];
    DF.output <- DF.output[DF.output[,'key3'] == 'properties',    ];
    DF.output <- DF.output[DF.output[,'key4'] == 'questionNumber',];
    DF.output <- DF.output[,base::c('key2','value')];
    base::colnames(DF.output) <- base::c('guid','questionNumber');
    DF.output <- unique(DF.output);

    # base::cat("\n# utils::str(DF.output)\n");
    # base::print(   utils::str(DF.output)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat(base::paste0("\n",thisFunctionName,"() quits."));
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }

##################################################
