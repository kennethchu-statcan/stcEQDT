
getListOfNodes <- function(
    list.input                  = NULL,
    DF.localization             = NULL,
    DF.referenceID.to.elementID = NULL,
    DF.referentID.to.elementID  = NULL,
    attribute.types             = c('children','variables','rows','columns','initLogic','displayLogic','enterLogic','exitLogic','validationLogic','condition.if','condition.then','condition.else')
    ) {

    # thisFunctionName <- "getListOfNodes";
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat("\nutils::str(DF.localization)\n");
    # base::print( utils::str(DF.localization)   );

    # base::cat("\nutils::str(DF.referenceID.to.elementID)\n");
    # base::print( utils::str(DF.referenceID.to.elementID)   );

    # base::cat("\nutils::str(DF.referentID.to.elementID)\n");
    # base::print( utils::str(DF.referentID.to.elementID)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.nested <- tabularizeData_nested(
        list.input = list.input
        );

    DF.nested <- DF.nested[DF.nested[,'key1'] != 'Reference',];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.nested[DF.nested[,'key3'] == 'if',  'key3'] <- 'condition.if';
    DF.nested[DF.nested[,'key3'] == 'then','key3'] <- 'condition.then';
    DF.nested[DF.nested[,'key3'] == 'else','key3'] <- 'condition.else';

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat("\nutils::str(DF.nested)\n");
    # base::print( utils::str(DF.nested)   );
    #
    # base::cat("\nbase::unique(DF.nested[,'key1'])\n");
    # base::print( base::unique(DF.nested[,'key1'])   );
    #
    # base::cat("\ntable(DF.nested[,c('key1','key3')])\n");
    # base::print( table(DF.nested[,c('key1','key3')])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    current.depth <- 0;
    current.guid  <- 'Root';

    DF.nested[,'depth'] <- NA;
    DF.nested[DF.nested[,'key2'] == current.guid,'depth'] <- current.depth;

    list.nodes <- base::list();
    list.attributes <- getListOfNodes_get.attributes(
        DF.input                    = DF.nested[DF.nested[,'key2'] == current.guid,],
        DF.localization             = DF.localization,
        DF.referenceID.to.elementID = DF.referenceID.to.elementID,
        DF.referentID.to.elementID  = DF.referentID.to.elementID,
        attribute.types             = attribute.types
        );
    list.nodes[[ current.guid ]] <- node$new(
        guid            = current.guid,
        depth           = current.depth,
        type            = list.attributes[['type'           ]],
        properties      = list.attributes[['properties'     ]],
        parent          = list.attributes[['parent'         ]],
        children        = list.attributes[['children'       ]],
        variables       = list.attributes[['variables'      ]],
        rows            = list.attributes[['rows'           ]],
        columns         = list.attributes[['columns'        ]],
        initLogic       = list.attributes[['initLogic'      ]],
        displayLogic    = list.attributes[['displayLogic'   ]],
        enterLogic      = list.attributes[['enterLogic'     ]],
        exitLogic       = list.attributes[['exitLogic'      ]],
        validationLogic = list.attributes[['validationLogic']],
        condition.if    = list.attributes[['condition.if'   ]],
        condition.then  = list.attributes[['condition.then' ]],
        condition.else  = list.attributes[['condition.else' ]]
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    current.guids <- base::setdiff(base::unique(DF.nested[,'key2']),current.guid);
    while ( base::length(current.guids) > 0 & sum(DF.nested[!base::is.na(DF.nested[,'depth']) & DF.nested[,'depth'] == current.depth,'key3'] == 'children') > 0 ) {

        DF.temp        <- DF.nested[!base::is.na(DF.nested[,'depth']) & DF.nested[,'depth'] == current.depth,];
        children.IDs   <- base::unique(DF.temp[DF.temp[,'key3'] %in% attribute.types,'value']);
        current.guids  <- base::setdiff(current.guids,children.IDs);

        current.depth <- current.depth + 1;
        DF.nested[DF.nested[,'key2'] %in% children.IDs,'depth'] <- current.depth;

        for ( guid in children.IDs ) {
            list.attributes <- getListOfNodes_get.attributes(
                DF.input                    = DF.nested[DF.nested[,'key2'] == guid,],
                DF.localization             = DF.localization,
                DF.referenceID.to.elementID = DF.referenceID.to.elementID,
                DF.referentID.to.elementID  = DF.referentID.to.elementID,
                attribute.types             = attribute.types
                );
            list.nodes[[ guid ]] <- node$new(
                guid            = guid,
                depth           = current.depth,
                type            = list.attributes[['type'           ]],
                properties      = list.attributes[['properties'     ]],
                parent          = list.attributes[['parent'         ]],
                children        = list.attributes[['children'       ]],
                variables       = list.attributes[['variables'      ]],
                rows            = list.attributes[['rows'           ]],
                columns         = list.attributes[['columns'        ]],
                initLogic       = list.attributes[['initLogic'      ]],
                displayLogic    = list.attributes[['displayLogic'   ]],
                enterLogic      = list.attributes[['enterLogic'     ]],
                exitLogic       = list.attributes[['exitLogic'      ]],
                validationLogic = list.attributes[['validationLogic']],
                condition.if    = list.attributes[['condition.if'   ]],
                condition.then  = list.attributes[['condition.then' ]],
                condition.else  = list.attributes[['condition.else' ]]
                );
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # base::cat(base::paste0("\n",thisFunctionName,"() quits."));
    # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( base::list(DF.nodes = DF.nested , list.nodes = list.nodes) );

    }

##################################################
getListOfNodes_get.attributes <- function(
    DF.input                    = NULL,
    DF.localization             = NULL,
    DF.referentID.to.elementID  = NULL,
    DF.referenceID.to.elementID = NULL,
    attribute.types             = NULL
    ) {

    list.attributes <- base::list();
    list.attributes[[ 'type' ]] <- DF.input[1,'key1'];

    attribute.types <- c(c('parent','properties'),attribute.types);
    for ( attribute.type in attribute.types ) {

        list.attributes[[ attribute.type ]] <- NULL;

        DF.attributes <- DF.input[DF.input[,'key3'] == attribute.type,];

        temp.indexes <- base::unique(DF.attributes[,'index']);
        list.indexes <- base::list();
        for ( temp.index in temp.indexes ) {

            DF.temp <- DF.attributes[DF.attributes[,'index'] == temp.index,];

            temp.key4s <- base::unique(DF.temp[,'key4']);
            list.key4s <- base::list();
            for ( temp.key4 in temp.key4s ) {

                temp.value  <- DF.temp[DF.temp[,'key4'] == temp.key4,'value'];
                temp.string <- temp.value;

                if ( temp.value %in% DF.referenceID.to.elementID[,"referenceID"] ) {
                    DF.one.row  <- DF.referenceID.to.elementID[DF.referenceID.to.elementID[,"referenceID"] == temp.value,];
                    DF.one.row  <- DF.one.row[,base::setdiff(base::colnames(DF.one.row),"referenceID")];
                    temp.string <- base::paste0(temp.string," (",base::paste(DF.one.row,collapse=", "),")");
                    }
                if ( temp.value %in% DF.referentID.to.elementID[,"referentID"] ) {
                    DF.one.row  <- DF.referentID.to.elementID[DF.referentID.to.elementID[,"referentID"] == temp.value,];
                    DF.one.row  <- DF.one.row[,base::setdiff(base::colnames(DF.one.row),"referentID")];
                    temp.string <- base::paste0(temp.string," (",base::paste(DF.one.row,collapse=", "),")");
                    }
                if ( temp.value %in% DF.localization[,"localizationID"] ) {
                    DF.one.row  <- DF.localization[DF.localization[,"localizationID"] == temp.value,];
                    # temp.string <- base::paste0(temp.string," (",DF.one.row[,"english"],", ",DF.one.row[,"french" ],")");
                    temp.string <- base::paste0("(",DF.one.row[,"english"],", ",DF.one.row[,"french" ],")");
                    }

                list.key4s[[ temp.key4 ]] <- temp.string;

                }
            list.indexes[[ temp.index ]] <- list.key4s;

            }
        list.attributes[[ attribute.type ]] <- list.indexes;

        }

    return( list.attributes );

    }
