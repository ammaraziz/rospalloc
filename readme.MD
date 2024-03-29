# Rospalloc

The unholy creation of ChatGPT being fed the two (awesome) python packages `pango_aliasor` and `pango-collapse`. All credit, citations and kudos should go to the authors.

### Usage & Installation

Don't. Go use the python packages above. This package is experimental, written for a super narrow usecase. No support will be given.

### Actual Usage & Installation

Install:
```
devtools::install_github("https://github.com/ammaraziz/rospalloc")
```

Use:
```
library(rospalloc)
Aliasor()
potential_parents = Collapsor(file="combined.txt")
collapse_column(c("XBB.1.16.1", "JN.1.1.1"), potential_parents)
[1] "XBB.1.16" "JN.1.1"
```

🔥Alternative usage (R6)🔥:
```
library(rospalloc)
c = CollapsorR6$new(file="combined.txt")
c$collapse_column(c("XBB.1.16.1", "JN.1.1.1"))
[1] "XBB.1.16" "JN.1.1"
```

### Naming

> Prompt: combine aliasor and collapsor into one word, make it up with a defintion

>Combining `aliasor` and `collapsor` into one word could result in something like `Aliasollapse`, which could be defined as:

>"Aliasollapse: (noun) A versatile entity adept at both aliasing and collapsing operations. It seamlessly bridges the realms of lineage aliasing and collapsing, facilitating efficient manipulation and management of hierarchical lineage data."

This was too long so it was shortened to `allapsor` which was too confusing so I decided to keep it simple. `Rospalloc` is the reverse of `Collapsor`.