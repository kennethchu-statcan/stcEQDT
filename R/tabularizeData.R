
tabularizeData <- function(
    list.input       = NULL,
    csv.nested       = "table-nested.csv",
    csv.non.nested   = "table-non-nested.csv",
    csv.reference    = "table-reference.csv",
    csv.localization = "table-localization.csv"
    ) {

    # thisFunctionName <- "tabularizeData";
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat("\nbase::names(list.input)\n");
    # base::print( base::names(list.input)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.diagnostics <- tabularizeData_list.names(list.input = list.input);

    # utils::write.csv(
    #     x         = DF.diagnostics,
    #     file      = "table-diagnostics.csv",
    #     row.names = FALSE
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # temp.names <- c('Reference','Localization');
    # for ( temp.name in temp.names ) {
    #     tabularizeData_utils::str(list.input = list.input, temp.name = temp.name);
    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.nested <- tabularizeData_nested(
        list.input = list.input
        );

    # base::cat("\nutils::str(DF.nested)\n");
    # base::print( utils::str(DF.nested)   );
    #
    # base::cat("\nbase::unique(DF.nested[,'key1'])\n");
    # base::print( base::unique(DF.nested[,'key1'])   );
    #
    # base::cat("\nbase::table(DF.nested[DF.nested[,'key1'] != 'Reference',c('key1','key3')])\n");
    # base::print( base::table(DF.nested[DF.nested[,'key1'] != 'Reference',c('key1','key3')])   );
    #
    # utils::write.csv(
    #     x         = DF.nested,
    #     file      = csv.nested,
    #     row.names = FALSE
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.non.nested <- tabularizeData_non.nested(
        list.input = list.input
        );

    # base::cat("\nutils::str(DF.non.nested)\n");
    # base::print( utils::str(DF.non.nested)   );
    #
    # utils::write.csv(
    #     x         = DF.non.nested,
    #     file      = csv.non.nested,
    #     row.names = FALSE
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.localization <- tabularizeData_localization(
        list.input = list.input
        );

    # base::cat("\nutils::str(DF.localization)\n");
    # base::print( utils::str(DF.localization)   );
    #
    # utils::write.csv(
    #     x         = DF.localization,
    #     file      = csv.localization,
    #     row.names = FALSE
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.output <- base::list(
        DF.diagnostics  = DF.diagnostics,
        DF.nested       = DF.nested,
        DF.non.nested   = DF.non.nested,
        DF.localization = DF.localization
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat(base::paste0("\n",thisFunctionName,"() quits."));
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( list.output );

    }

##################################################
tabularizeData_localization <- function(
    list.input = NULL
    ) {

    DF.english <- list.input[['Localization']][['English']];
    DF.english <- base::data.frame(
        ID      = base::names(DF.english),
        english = base::as.character(DF.english)
        );
    DF.english[,'index'] <- base::seq(1,base::nrow(DF.english));

    DF.french   <- list.input[['Localization']][['French']];
    temp.french <- base::as.character(DF.french);
    base::Encoding(temp.french) <- "UTF-8";
    DF.french <- base::data.frame(
        ID     = base::names(DF.french),
        french = temp.french
        );

    DF.output <- base::merge(
        x   = DF.english,
        y   = DF.french,
        by  = "ID",
        all = TRUE
        );

    base::colnames(DF.output) <- base::gsub(
        x           = base::colnames(DF.output),
        pattern     = "^ID$",
        replacement = "localizationID"
        );

    DF.output <- DF.output[base::order(DF.output[,'index']),];
    DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),'index')];
    return( DF.output );

    }

tabularizeData_nested <- function(
    list.input = NULL
    ) {
    DF.output   <- data.frame();
    temp.groups <- base::names(list.input);
    temp.groups <- base::setdiff(temp.groups,c('Localization'));
    for ( temp.group in temp.groups ) {

        list.entities <- list.input[[temp.group]];
        for ( entityID in base::names(list.entities) ) {
            list.attributes <- list.entities[[entityID]];
            temp.attributes <- base::names(list.attributes);
            for ( temp.attribute in temp.attributes ) {
                if ( temp.attribute %in% base::c('properties','parent') ) {
                    if ( base::length(list.attributes[[temp.attribute]]) > 0 ) {
                        temp.keys <- base::setdiff(base::names(list.attributes[[temp.attribute]]),"type");
                        for ( temp.key in temp.keys ) {
                            DF.temp <- base::data.frame(
                                key1  = temp.group,
                                key2  = entityID,
                                key3  = temp.attribute,
                                index = 1,
                                key4  = temp.key,
                                value = ifelse(base::is.null(list.attributes[[temp.attribute]][[temp.key]]),"NULL",list.attributes[[temp.attribute]][[temp.key]])
                                );
                            DF.output <- base::rbind(DF.output,DF.temp);
                            }
                        }
                } else {

                    if ( 'Reference' == temp.group ) {
                        temp.keys <- base::setdiff(base::names(list.attributes[[temp.attribute]]),"type");
                        for ( temp.key in temp.keys ) {
                            DF.temp <- data.frame(
                                key1  = temp.group,
                                key2  = entityID,
                                key3  = temp.attribute,
                                index = temp.index,
                                key4  = temp.key,
                                value = ifelse(is.null(list.attributes[[temp.attribute]][[temp.key]]),"NULL",list.attributes[[temp.attribute]][[temp.key]])
                                );
                            DF.output <- base::rbind(DF.output,DF.temp);
                            }
                    } else {
                        temp.length <- base::length(list.attributes[[temp.attribute]]);
                        if ( temp.length > 0 ) {
                            for ( temp.index in seq(1,temp.length) ) {
                                temp.keys <- base::setdiff(base::names(list.attributes[[temp.attribute]][[temp.index]]),"type");
                                for ( temp.key in temp.keys ) {
                                    DF.temp <- base::data.frame(
                                        key1  = temp.group,
                                        key2  = entityID,
                                        key3  = temp.attribute,
                                        index = temp.index,
                                        key4  = temp.key,
                                        value = ifelse(base::is.null(list.attributes[[temp.attribute]][[temp.index]][[temp.key]]),"NULL",list.attributes[[temp.attribute]][[temp.index]][[temp.key]])
                                        );
                                    DF.output <- base::rbind(DF.output,DF.temp);
                                    }
                                }
                            }
                        }

                    }
                }
            }

        }
    return( DF.output );
    }

tabularizeData_non.nested <- function(
    list.input = NULL
    ) {
    DF.output   <- base::data.frame();
    temp.groups <- base::names(list.input);
    # temp.groups <- setdiff(temp.groups,c('Reference','Localization'))
    temp.groups <- setdiff(temp.groups,c('Localization'))
    for ( temp.group in temp.groups ) {
        if ( base::is.list(list.input[[temp.group]]) ) {
            if ( 0 == base::length(list.input[[temp.group]]) ) {
                DF.temp <- data.frame(
                    key1  = temp.group,
                    value = NA
                    );
                DF.output <- base::rbind(DF.output,DF.temp);
                }
        } else {
            DF.temp <- base::data.frame(
                key1  = temp.group,
                value = list.input[[temp.group]]
                );
            DF.output <- base::rbind(DF.output,DF.temp);
            }
        }
    return( DF.output );
    }

tabularizeData_str <- function(
    list.input = NULL,
    temp.name  = NULL
    ) {
    base::cat(base::paste0("\nutils::str(list.input[[",temp.name,"]])\n"));
    base::print(utils::str( list.input[[temp.name]] ));
    }

tabularizeData_list.names <- function(
    list.input = NULL
    ) {
    DF.output <- base::data.frame();
    for ( temp.name in base::names(list.input) ) {
        temp.components <- lapply( X = list.input[[temp.name]], FUN = function(x) return(base::paste(base::names(x),collapse=",")) );
        temp.components <- base::paste(base::unique(base::as.character(temp.components)),collapse=" # ");
        # temp.components <- ifelse(temp.name %in% c('Localization'), "", temp.components);
        DF.temp <- base::data.frame(
            key1             = temp.name,
            typeof           = base::typeof(list.input[[temp.name]]),
            length           = base::length(list.input[[temp.name]]),
            components       = temp.components,
            stringsAsFactors = FALSE
            );
        DF.output <- base::rbind(DF.output,DF.temp);
        }
    return( DF.output );
    }
