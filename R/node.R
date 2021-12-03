
require(R6);

node <- R6::R6Class(

    classname = "node",

    public = list(

        # instantiation attributes
        guid            = NULL,
        depth           = NULL,
        type            = NULL,
        properties      = NULL,
        parent          = NULL,
        children        = NULL,
        variables       = NULL,
        rows            = NULL,
        columns         = NULL,
        initLogic       = NULL,
        displayLogic    = NULL,
        enterLogic      = NULL,
        exitLogic       = NULL,
        validationLogic = NULL,
        condition.if    = NULL,
        condition.then  = NULL,
        condition.else  = NULL,

        # derived attributes

        # methods
        initialize = function(
            guid            = NULL,
            depth           = NULL,
            type            = NULL,
            properties      = NULL,
            parent          = NULL,
            children        = NULL,
            variables       = NULL,
            rows            = NULL,
            columns         = NULL,
            initLogic       = NULL,
            displayLogic    = NULL,
            enterLogic      = NULL,
            exitLogic       = NULL,
            validationLogic = NULL,
            condition.if    = NULL,
            condition.then  = NULL,
            condition.else  = NULL
            ) {
                self$guid            <- guid;
                self$depth           <- depth;
                self$type            <- type;
                self$properties      <- properties;
                self$parent          <- parent;
                self$children        <- children;
                self$variables       <- variables;
                self$rows            <- rows;
                self$columns         <- columns;
                self$initLogic       <- initLogic;
                self$displayLogic    <- displayLogic;
                self$enterLogic      <- enterLogic;
                self$exitLogic       <- exitLogic;
                self$validationLogic <- validationLogic;
                self$condition.if    <- condition.if;
                self$condition.then  <- condition.then;
                self$condition.else  <- condition.else;
                private$node.type    <- base::gsub(x = self$guid, pattern = "_[A-Za-z0-9]+$", replacement = "");
                private$generate_properties();
            },

        print_node = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            if (base::grepl(x = self$guid, pattern = "^Conditional_")) {
                private$print_node_Conditional(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^ConditionGroup_")) {
                private$print_node_ConditionGroup(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^Condition_"     )) {
                private$print_node_Condition(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^ValueGroup_"    )) {
                private$print_node_ValueGroup(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^Value_"         )) {
                private$print_node_Value(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^Comparison_"    )) {
                private$print_node_Comparison(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^Connective_"    )) {
                private$print_node_Connective(indent = indent);
            } else if (base::grepl(x = self$guid, pattern = "^Action_"        )) {
                private$print_node_Action(indent = indent);
            } else {
                private$print_node_Other(indent = indent);
                }
            },

        get_attribute_IDs = function() {
            vector.output <- base::unique(base::c(
                base::as.vector(base::unlist(self$children       )),
                base::as.vector(base::unlist(self$variables      )),
                base::as.vector(base::unlist(self$rows           )),
                base::as.vector(base::unlist(self$columns        )),
                base::as.vector(base::unlist(self$initLogic      )),
                base::as.vector(base::unlist(self$displayLogic   )),
                base::as.vector(base::unlist(self$enterLogic     )),
                base::as.vector(base::unlist(self$exitLogic      )),
                base::as.vector(base::unlist(self$validationLogic)),
                base::as.vector(base::unlist(self$condition.if   )),
                base::as.vector(base::unlist(self$condition.then )),
                base::as.vector(base::unlist(self$condition.else ))
                ));
            return(vector.output);
            }

        ), # public = list()

    private = list(

        node.type = NULL,

        format.referentID            =      "[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+",
        pattern.referentID.elementID = "id = [a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+ \\([a-zA-Z0-9_ \\.]+\\)",
      # pattern.referentID.elementID = "id = [a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+ \\([a-zA-Z0-9_]+\\)",
      # pattern.referentID.elementID = "(; ){,1}id = [a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+ \\([a-zA-Z0-9_ ]+\\)",
        pattern.referentID           = "id = [a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+-[a-z0-9]+ ",

        properties.list   = NULL,
        properties.vector = NULL,
        properties.string = NULL,

        guid.substitute = function(properties.string = private$properties.string) {
            match.result <- base::gregexpr(pattern = private$pattern.referentID.elementID, text = properties.string);
            if ( base::as.integer(match.result) < 0 ) {
              # return( self$guid );
                return( private$node.type );
            } else {
                output.string <- base::substr(x = properties.string, start = match.result, stop = -1 + base::as.integer(match.result) + attr(match.result[[1]],"match.length"));
                output.string <- base::gsub(x = output.string, pattern = private$pattern.referentID, replacement = "");
                output.string <- base::gsub(x = output.string, pattern = "(\\(|\\))", replacement = "");
                if ( output.string == self$guid ) {
                    output.string <- private$node.type;
                } else {
                  # output.string <- base::paste0(self$guid,", ",output.string);
                    output.string <- base::paste0(private$node.type,", ",output.string);
                    }
                return(output.string);
                }
            },

        extract_substr_by_pattern = function(text = "", pattern = "") {
            match.result <- base::gregexpr(pattern = pattern, text = text);
            if ( base::as.integer(match.result) < 0 ) {
                return("");
            } else {
                output.string <- base::substr(x = text, start = match.result, stop = -1 + base::as.integer(match.result) + attr(match.result[[1]],"match.length"));
                return(output.string);
                }
            },

        generate_properties = function() {
            if ( (!base::is.null(self$properties)) & base::length(self$properties) > 0 ) {
                private$properties.list   <- list();
                private$properties.vector <- c();
                for ( i in base::seq(1,base::length(self$properties)) ) {
                    for ( temp.key in base::names(self$properties[[i]]) ) {
                        temp.value  <- self$properties[[i]][[temp.key]];
                        temp.string <- base::paste0(temp.value,collapse=", ");
                        private$properties.list[[temp.key]] <- temp.string;
                        private$properties.vector <- base::c(private$properties.vector,base::paste0(temp.key," = ",temp.string));
                        # if ( temp.key != 'id' ) {
                        #     private$properties.vector <- c(private$properties.vector,base::paste0(temp.key," = ",temp.string));
                        #     }
                        }
                    }
                private$properties.string <- base::paste0(private$properties.vector, collapse = "; ");
                }
            },

        print_node_Conditional = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            base::cat("\n");
            base::cat(base::paste0(rep(indent,self$depth),collapse=""));
          # base::cat(base::paste0("(",self$guid,") "));
            base::cat(base::paste0("(",private$node.type,") "));
            if ( any(base::grepl(self$get_attribute_IDs(), pattern = "^ConditionGroup_")) ) {
                base::cat("\n");
                base::cat(base::paste0(rep(indent,1+self$depth),collapse=""));
            } else {
                base::cat("\n");
                base::cat(base::paste0(rep(indent,1+self$depth),collapse=""), "( NO CONDITION )", sep = "");
                }
            },

        print_node_ConditionGroup = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            if ( 'negation' %in% base::names(private$properties.list) ) {
                if (private$properties.list[['negation']] == 'TRUE') {
                    base::cat("!( ");
                } else {
                    base::cat("( ");
                    }
                }
            },

        print_node_Condition = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            base::cat("(");
            },

        print_node_ValueGroup = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {},

        print_node_Action = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            base::cat("\n");
            base::cat(base::paste0(rep(indent,self$depth),collapse="") );
            if ( base::is.null(self$properties) | base::length(self$properties) == 0 ) {
              # base::cat(base::paste0("(",self$guid,") "));
                base::cat(base::paste0("(",private$node.type,") "));
            } else { # if ( base::length(self$properties) > 0 )
                temp.string <- base::gsub(x = private$properties.string, pattern = private$format.referentID, replacement = "");
                temp.string <- base::gsub(x = temp.string, pattern = "[\\(,]", replacement = "");
                if ('setTarget' %in% base::names(private$properties.list)) {
                    temp.string <- base::gsub(x = temp.string, pattern = "\\)", replacement = " <- ");
                } else if ('gotoTarget' %in% base::names(private$properties.list)) {
                    temp.string <- base::gsub(x = temp.string, pattern = "\\)", replacement = "");
                } else if ('displayTarget' %in% base::names(private$properties.list)) {
                    temp.string <- base::gsub(x = temp.string, pattern = "\\)", replacement = "");
                    }
                temp.string <- base::gsub(x = temp.string, pattern = " {2,}", replacement = " ");
                base::cat(base::paste0("(",private$guid.substitute(),") : ",temp.string));
                }
            },

        print_node_Value = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            if ('datapointValue' %in% base::names(private$properties.list)) {
                output.string <- private$properties.list[['datapointValue']];
                output.string <- base::gsub(x = output.string, pattern = private$format.referentID, replacement = "");
                output.string <- base::gsub(x = output.string, pattern = "[ \\(\\),]",              replacement = "");
                base::cat(output.string);
            } else if ('numberValue' %in% base::names(private$properties.list)) {
                output.string <- private$properties.list[['numberValue']];
                base::cat(output.string);
            } else if ('specialVarValue' %in% base::names(private$properties.list)) {
                output.string <- private$properties.list[['specialVarValue']];
                base::cat(output.string);
            } else if ('stringValue' %in% base::names(private$properties.list)) {
                output.string <- private$properties.list[['stringValue']];
                base::cat(output.string);
                }
            },

        print_node_Comparison = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            if ( 'value' %in% base::names(private$properties.list) ) {
                output.string <- private$properties.list[['value']];
                output.string <- base::paste0(" ",output.string," ");
                base::cat(output.string);
                }
            },

        print_node_Connective = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            if ( 'value' %in% base::names(private$properties.list) ) {
                output.string <- private$properties.list[['value']];
                output.string <- base::paste0(" ",output.string," ");
                base::cat(output.string);
                }
            },

        print_node_Other = function(
            indent     = '  ',
            FUN.format = function(x) { return(x) }
            ) {
            base::cat("\n");
            base::cat(base::paste0(rep(indent,self$depth),collapse="") );
            if ( base::is.null(self$properties) | base::length(self$properties) == 0 ) {
              # base::cat(base::paste0("(",self$guid,") "));
                base::cat(base::paste0("(",private$node.type,") "));
            } else { # if ( base::length(self$properties) > 0 )
              # temp.string <- base::gsub(x = private$properties.string, pattern = base::paste0("[; ]{,1}",private$pattern.referentID.elementID), replacement = "");
                temp.string <- base::gsub(x = private$properties.string, pattern = base::paste0(private$pattern.referentID.elementID,"(; )*"), replacement = "");
                base::cat(base::paste0("(",private$guid.substitute(),") : ",temp.string));
                }
            }

        ) # private = list()

    );
