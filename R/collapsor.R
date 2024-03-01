#' collapse a single lineage
#' @export
#' @param compressed_lineage GISAID credentials.
#' @param potential_parents parental lineages - get this from XXXX
#' @param strict strictly match collapsed definition? will return NA if not matching
#' @return str of collapsed lineage
collapse <- function(compressed_lineage, potential_parents, strict=FALSE) {
    if (compressed_lineage %in% potential_parents) {
        return(compressed_lineage)
    }
    
    uncompressed_lineage <- uncompress(compressed_lineage)
    parts <- unlist(strsplit(uncompressed_lineage, "\\."))
    
    for (i in 1:(length(parts) - 1)) {
        compressed_parent_lineage <- compress(paste(parts[1:(length(parts) - i)], collapse = "."))
        if (compressed_parent_lineage %in% potential_parents) {
            return(compressed_parent_lineage)
        }
    }
    
    if (grepl("^X", uncompressed_lineage) && "Recombinant" %in% potential_parents) {
        return("Recombinant")
    }
    
    if (strict) {
        return(NULL)
    }
    
    return(compressed_lineage)
}

collapse_column <- function(array_of_uncompress_lineages, potential_parents, strict=FALSE) {
    sapply(array_of_uncompress_lineages, function(compressed_lineage) {
        if (!is.na(compressed_lineage)) {
            collapse(compressed_lineage, potential_parents, strict=strict)
        } else {
            NULL
        }
    })
}

uncompress_column <- function(array_of_compressed_lineages) {
    sapply(array_of_compressed_lineages, function(compressed_lineage) {
        if (!is.na(compressed_lineage)) {
            uncompress(compressed_lineage)
        } else {
            NULL
        }
    })
}

expand <- function(lineage, delimiter=":") {
    uncompressed_lineage <- uncompress(lineage)
    parts <- unlist(strsplit(uncompressed_lineage, "\\."))
    
    levels <- length(parts) - 1
    indirections <- ifelse(levels %% 3 != 0, levels %% 3, 3)
    
    expanded_lineage <- c(compress(uncompressed_lineage))
    for (i in seq(indirections, levels, by=3)) {
        compressed_parent_lineage <- compress(paste(parts[1:(length(parts) - i)], collapse = "."))
        expanded_lineage <- c(expanded_lineage, compressed_parent_lineage)
    }
    expanded_lineage = expanded_lineage[-length(expanded_lineage)]
    return(paste(rev(expanded_lineage), collapse = delimiter))
}

expand_column <- function(array_of_lineages, delimiter=":") {
    sapply(array_of_lineages, function(lineage) {
        if (!is.na(lineage)) {
            expand(lineage, delimiter=delimiter)
        } else {
            NULL
        }
    })
}

load_potential_parents_from_url <- function(url) {
    potential_parents <- character()
    con <- url(url, "r")
    potential_parents <- c(potential_parents, readLines(con))
    close(con)
    potential_parents <- trimws(potential_parents)
    potential_parents <- potential_parents[potential_parents != ""]
    potential_parents <- potential_parents[!startsWith(potential_parents, "#")]
    return(potential_parents)
}

load_potential_parents_from_file <- function(collapse_file) {
    potential_parents <- character()
    con <- file(collapse_file, "r")
    potential_parents <- c(potential_parents, readLines(con))
    close(con)
    potential_parents <- trimws(potential_parents)
    potential_parents <- potential_parents[potential_parents != ""]
    potential_parents <- potential_parents[!startsWith(potential_parents, "#")]
    return(potential_parents)
}
