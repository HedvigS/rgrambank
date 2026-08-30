library(glue)

meta <- read.dcf("DESCRIPTION", keep.white = TRUE)
meta <- as.list(meta[1,])
# Parse Authors@R which is stored as a string in the DCF
meta$`Authors@R` <- trimws(meta$`Authors@R`)

# Get SHA
sha <- tryCatch(
  substr(Sys.getenv("GITHUB_SHA"), 1, 7),
  error = function(e) NULL
)
# Build note
note <- if (!is.null(sha)) {
  paste0("R package dev version commit ", sha)
} else {
  paste0("R package dev version ", meta$Version)
}

# Build authors
authors <- eval(parse(text = meta$`Authors@R`))

# Build each person() call separately
author_lines <- sapply(authors, function(a) {
  glue('    person("{a$given}", "{a$family}")')
})

# Collapse into a c() call
author_block <- paste0(
  "c(\n",
  paste(author_lines, collapse = ",\n"),
  "\n  )"
)

# Plain text version
author_string <- paste(
  sapply(authors, function(a) paste(a$given, a$family)),
  collapse = ", "
)

citation_content <- glue('
bibentry(
  bibtype      = "Manual",
  title        = "rgrambank: {meta$Title}",
  author       = {author_block},
  year         = "{format(Sys.Date(), "%Y")}",
  note         = "{note}",
  url          = "https://github.com/HedvigS/rgrambank",
  textVersion  = "{author_string}. rgrambank: {meta$Title}. {note}. https://github.com/HedvigS/rgrambank"
)
')

writeLines(citation_content, con = "../inst/CITATION")