
getData <- function(
    input.file = NULL
    ) {

    list.fromJSON <- rjson::fromJSON(file = input.file);
    return( list.fromJSON );

    }
