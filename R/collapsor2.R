library(jsonlite)

CollapsorR6 <- R6Class(
   "Collapsor",
   public = list(
       load_potential_parents_from_url = function(url) {
           potential_parents <- character()
           con <- url(url, "r")
           potential_parents <- c(potential_parents, readLines(con))
           close(con)
           potential_parents <- trimws(potential_parents)
           potential_parents <- potential_parents[potential_parents != ""]
           potential_parents <- potential_parents[!startsWith(potential_parents, "#")]
           return(potential_parents)
       },
       load_potential_parents_from_file = function(collapse_file) {
           potential_parents <- character()
           con <- file(collapse_file, "r")
           potential_parents <- c(potential_parents, readLines(con))
           close(con)
           potential_parents <- trimws(potential_parents)
           potential_parents <- potential_parents[potential_parents != ""]
           potential_parents <- potential_parents[!startsWith(potential_parents, "#")]
           return(potential_parents)
       },
       initialize = function(file = NULL, url = NULL) {
           if (is.null(file) && is.null(url)) {
               stop("Must specify alias file or URL")
           }
           
           if (!is.null(file)) {
               self$p <- self$load_potential_parents_from_file(file)
           }
           
           if (!is.null(url)) {
               self$p <- self$load_potential_parents_from_url(url)
           }
       },
       collapse = function(lineage, strict = FALSE) {
           if (lineage == ""){
               return("")
           }
           
           if (lineage %in% self$p) {
               return(lineage)
           }
           
           uncompressed_lineage <- self$uncompress(lineage)
           parts <- unlist(strsplit(uncompressed_lineage, "\\."))
           
           for (i in 1:(length(parts) - 1)) {
               compressed_parent_lineage <- self$compress(paste(parts[1:(length(parts) - i)], collapse = "."))
               if (compressed_parent_lineage %in% self$p) {
                   return(compressed_parent_lineage)
               }
           }
           
           if (grepl("^X", uncompressed_lineage) && "Recombinant" %in% self$p) {
               return("Recombinant")
           }
           
           if (strict) {
               return(NA)
           }
           
           return(compressed_parent_lineage)
       },
       collapse_column = function(array_of_uncompress_lineages, strict = FALSE) {
           sapply(array_of_uncompress_lineages, function(compressed_lineage) {
               if (!is.na(compressed_lineage)) {
                   self$collapse(compressed_lineage, strict = strict)
               } else {
                   NA
               }
           },
           USE.NAMES = FALSE)
       },
       uncompress_column = function(array_of_compressed_lineages) {
           sapply(array_of_compressed_lineages, function(compressed_lineage) {
               if (!is.na(compressed_lineage)) {
                   self$uncompress(compressed_lineage)
               } else {
                   NA
               }
           },
           USE.NAMES = FALSE)
       },
       expand = function(lineage, delimiter = ":") {
           uncompressed_lineage <- self$uncompress(lineage)
           parts <- unlist(strsplit(uncompressed_lineage, "\\."))
           
           levels <- length(parts) - 1
           indirections <- ifelse(levels %% 3 != 0, levels %% 3, 3)
           
           expanded_lineage <- c(self$compress(uncompressed_lineage))
           for (i in seq(indirections, levels, by=3)) {
               compressed_parent_lineage <- self$compress(paste(parts[1:(length(parts) - i)], collapse = "."))
               expanded_lineage <- c(expanded_lineage, compressed_parent_lineage)
           }
           expanded_lineage <- expanded_lineage[-length(expanded_lineage)]
           return(paste(rev(expanded_lineage), collapse = delimiter))
       },
       expand_column = function(array_of_lineages, delimiter = ":") {
           sapply(array_of_lineages, function(lineage) {
               if (!is.na(lineage)) {
                   self$expand(lineage, delimiter=delimiter)
               } else {
                   NA
               }
           },
           USE.NAMES = FALSE)
       }
   )
)