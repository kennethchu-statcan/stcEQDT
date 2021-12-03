#' json2tree
#'
#' This function accepts a EQDT JSON file of an electronic questionnaire, and
#' visualizes the logic structure of the electronic questionnaire in a tree
#' structure, and prints that tree structure to file.
#'
#' @docType class
#'
#' @import dplyr
#' @import R6
#' @import rjson
#'
#' @return NULL
#'
#' @examples
#' # See the vignette 'json2tree-usage' for more details, by executing the command: vignette("stcEQDT-usage")
#' library(stcEQDT)
#'
#' json2tree(
#'     eqdt.json = system.file("extdata", "OID-exit.json", package = "stcEQDT"),
#'     file.output = 'EQtree-OID-exit.txt'
#'     )
#'
#' @param eqdt.json character vector of length 1, containing path to EQDT JSON file
#' @param file.output character vector of length 1, containing path to output file
#' @param output.directory character vector of length 1, path to output directory. Default : base::paste0("json2tree.",base::gsub(x=base::Sys.time(),pattern="( |:)",replacement="-")). If NULL, output.directory will be set to working directory.
#'
#' @export

json2tree <- function(
    eqdt.json        = NULL,
    file.output      = 'EQtree.txt',
    output.directory = base::paste0("json2tree.",base::gsub(x=base::Sys.time(),pattern="( |:)",replacement="-"))
    ) {

    this.function.name <- "json2tree";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.directory <- base::normalizePath(base::getwd());
    if ( base::is.null(output.directory) ) {
        base::cat(base::paste0('\n\n# ',this.function.name,'(): All output and log files are written to: ',base::normalizePath(base::getwd()),'\n\n'));
    } else {
        if ( !base::dir.exists(output.directory) ) { base::dir.create(path = output.directory, recursive = TRUE); }
        output.directory <- base::normalizePath(output.directory);
        base::cat(base::paste0('\n\n# ',this.function.name,'(): All output and log files are written to: ',output.directory,'\n\n'));
        base::setwd(output.directory);
        }

    ### ####################################### ###
    list.json        <- getData(input.file = eqdt.json);
    list.data.frames <- tabularizeData(list.input = list.json);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.QGuid.to.QNumber <- getQGuidToQNumber(
        DF.nested = list.data.frames[['DF.nested']]
        );
    # utils::write.csv(file = "DF-QGuid-to-QNumber.csv",   x      = DF.QGuid.to.QNumber, row.names = FALSE);
    # base::saveRDS(   file = "DF-QGuid-to-QNumber.RData", object = DF.QGuid.to.QNumber);

    DF.PGuid.to.PNumber <- getPGuidToPNumber(
        DF.nested = list.data.frames[['DF.nested']]
        );
    # utils::write.csv(file = "DF-PGuid-to-PNumber.csv",   x      = DF.PGuid.to.PNumber, row.names = FALSE);
    # base::saveRDS(   file = "DF-PGuid-to-PNumber.RData", object = DF.PGuid.to.PNumber);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.referenceID.to.elementID <- getReferenceIDToElementID(
        DF.nested           = list.data.frames[['DF.nested']],
        DF.QGuid.to.QNumber = DF.QGuid.to.QNumber,
        DF.PGuid.to.PNumber = DF.PGuid.to.PNumber
        );
    DF.referenceID.to.elementID <- list.referenceID.to.elementID[['referenceID.to.elementID']];
    DF.referentID.to.elementID  <- list.referenceID.to.elementID[[ 'referentID.to.elementID']];

    # utils::write.csv(file = "DF-referenceID-to-elementID.csv",   x      = DF.referenceID.to.elementID, row.names = FALSE);
    # base::saveRDS(   file = "DF-referenceID-to-elementID.RData", object = DF.referenceID.to.elementID);

    # utils::write.csv(file = "DF-referentID-to-elementID.csv",   x      = DF.referentID.to.elementID, row.names = FALSE);
    # base::saveRDS(   file = "DF-referentID-to-elementID.RData", object = DF.referentID.to.elementID);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.getListOfNodes <- getListOfNodes(
        list.input                  = list.json,
        DF.localization             = list.data.frames[['DF.localization']],
        DF.referenceID.to.elementID = DF.referenceID.to.elementID,
        DF.referentID.to.elementID  = DF.referentID.to.elementID
        );

    DF.nodes   <- results.getListOfNodes[[  'DF.nodes']];
    list.nodes <- results.getListOfNodes[['list.nodes']];

    # utils::write.csv(file = "DF-nodes.csv",     x      =   DF.nodes);
    # base::saveRDS(   file = "DF-nodes.RData",   object =   DF.nodes);
    # base::saveRDS(   file = "list-nodes.RData", object = list.nodes);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    printListOfNodes(
        list.nodes = list.nodes,
        txt.output = file.output
        );

    ### ####################################### ###
    base::setwd(original.directory);
    base::return( NULL );

    }
