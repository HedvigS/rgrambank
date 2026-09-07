library(glue)

meta <- read.dcf("../DESCRIPTION", keep.white = TRUE)
meta <- as.list(meta[1,])
# Parse Authors@R which is stored as a string in the DCF
meta$`Authors@R` <- trimws(meta$`Authors@R`)

# Get SHA
sha <- if (nzchar(Sys.getenv("GITHUB_SHA"))) {
  # Running in GitHub Actions
  substr(Sys.getenv("GITHUB_SHA"), 1, 7)
} else {
  # Running locally - try git directly
  tryCatch(
    {
      result <- system("git rev-parse HEAD", intern = TRUE)
      if (length(result) > 0 && nzchar(result)) substr(result, 1, 7) else NULL
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )
}

commit_date <- if (nzchar(Sys.getenv("GITHUB_SHA"))) {
  # In GitHub Actions, use the current date
  format(Sys.Date(), "%Y-%m-%d")
} else {
  # Running locally - try git directly
  tryCatch(
    {
      result <- system("git log -1 --format=%ci HEAD", intern = TRUE)
      if (length(result) > 0 && nzchar(result)) substr(result, 1, 10) else format(Sys.Date(), "%Y-%m-%d")
    },
    error = function(e) format(Sys.Date(), "%Y-%m-%d"),
    warning = function(w) format(Sys.Date(), "%Y-%m-%d")
  )
}


# Build note
note <- if (!is.null(sha)) {
  paste0("R package dev version commit ", sha, " (", commit_date, ")")
} else {
  paste0("R package dev version ", meta$Version, " (", commit_date, ")")
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
  url          = "https://github.com/HedvigS/rgrambank, https://zenodo.org/records/16915290",
  textVersion  = "{author_string} ({format(Sys.Date(), "%Y")}) rgrambank: {meta$Title}. {note}. https://github.com/HedvigS/rgrambank"
)
')

writeLines(citation_content, con = "../inst/CITATION")