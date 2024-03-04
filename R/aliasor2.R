AliasorR6 <- R6::R6Class(
    "Aliasor",
     public = list(
         alias_dict = NULL,
         realias_dict = NULL,
         
         initialize = function(alias_file = NULL) {
             if (is.null(alias_file)) {
                 file <- jsonlite::fromJSON("https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json")
             } else {
                 file <- jsonlite::fromJSON(alias_file)
             }

             self$alias_dict <- list()
             for (column in names(file)) {
                 if (length(file[[column]]) > 1 || file[[column]] == "") {
                     self$alias_dict[[column]] <- column
                 } else {
                     self$alias_dict[[column]] <- file[[column]]
                 }
             }

             inverse <- function(x) {
                 value <- names(x)
                 name <- x[[1]]
                 return(setNames(value, name))
             }
             
             self$realias_dict <- list()
             for (i in seq_along(self$alias_dict)) {
                 lineage <- self$alias_dict[[i]]
                 alias <- names(self$alias_dict[i])
                 new <- setNames(alias, lineage)
                 self$realias_dict <- append(self$realias_dict, new)
             }

         },
         compress = function(name) {
             
             name_split <- unlist(strsplit(name, "\\."))
             levels <- length(name_split) - 1
             num_indirections <- floor((levels - 1) / 3)
             if (num_indirections <= 0) {
                 return(name)
             }
             alias <- paste(name_split[1:(3 * num_indirections + 1)], collapse = ".")
             ending <- paste(name_split[(3 * num_indirections + 2):length(name_split)], collapse = ".")
             return(paste(self$realias_dict[[alias]], ending, collapse = ".", sep = "."))
         },
         uncompress = function(name) {
             name_split <- unlist(strsplit(name, "\\."))
             letter <- name_split[1]
             unaliased <- tryCatch(self$alias_dict[[letter]], error = function(e) NULL)
             if (is.null(unaliased) || length(name_split) == 1) {
                 return(name)
             }
             if (length(name_split) == 2) {
                 return(paste(unaliased, name_split[2], sep = "."))
             } else {
                 return(paste(unaliased, paste(name_split[2:length(name_split)], collapse = "."), sep = "."))
             }
         },
         parent = function(name) {
             name <- uncompress(name)
             name_split <- unlist(strsplit(name, "\\."))
             if (length(name_split) <= 1) {
                 return("")
             } else {
                 return(compress(paste(name_split[1:(length(name_split) - 1)], collapse = ".")))
             }
         },
         partial_compress = function(name, up_to = 0, accepted_aliases = list()) {
             name_split <- unlist(strsplit(name, "\\."))
             levels <- length(name_split) - 1
             indirections <- floor((levels - 1) / 3)
             
             alias <- name_split[1]
             
             if (up_to > 0) {
                 if (indirections <= up_to) {
                     return(compress(name))
                 }
                 to_alias <- paste(name_split[1:(3 * up_to + 1)], collapse = ".")
                 alias <- paste(unlist(self$realias_dict[[to_alias]]), collapse = ".")
             }
             
             if (any(is.na(name_split[(3 * up_to + 2):length(name_split)]))) {
                 return(alias)
             }
             
             if (length(accepted_aliases) > 0) {
                 for (level in indirections:up_to) {
                     to_alias <- paste(name_split[1:(3 * level + 1)], collapse = ".")
                     if (to_alias %in% names(self$realias_dict)) {
                         if (self$realias_dict[[to_alias]] %in% accepted_aliases) {
                             alias <- paste(unlist(self$realias_dict[[to_alias]]), collapse = ".")
                             return(paste(alias, paste(name_split[(3 * level + 2):length(name_split)], collapse = "."), sep = "."))
                         }
                     }
                 }
             }
             
             return(paste(alias, paste(name_split[(3 * up_to + 2):length(name_split)], collapse = "."), sep = "."))
         }
     )
)

# Usage
aliasor <- AliasorR6$new()
compressed_name <- aliasor$compress("B.1.1.529.1")
uncompressed_name <- aliasor$uncompress(compressed_name)
parent_name <- aliasor$parent("some.name")
partial_compressed_name <- aliasor$partial_compress("some.name", up_to = 1)
