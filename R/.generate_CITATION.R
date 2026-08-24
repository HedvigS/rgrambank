meta <- utils::packageDescription("rgrambank")

# Get SHA
sha <- tryCatch(
  substr(meta$RemoteSha, 1, 7),
  error = function(e) NULL
)

# Build note
note <- if (!is.null(sha)) {
  paste0("R package dev version commit ", sha)
} else {
  paste0("R package dev version ", meta$Version)
}

# Build author string by collapsing all authors
authors <- eval(parse(text = meta$`Authors@R`))
author_string <- paste(
  sapply(authors, function(a) paste(a$given, a$family)),
  collapse = ", "
)

# Write the static CITATION file with values filled in
citation_content <- paste0(
  'bibentry(
  bibtype  = "Manual",
  title    = "', meta$Title, '",
  author   = person("', author_string, '"),
  year     = "', format(Sys.Date(), "%Y"), '",
  note     = "', note, '",
  url      = "https://github.com/HedvigS/rgrambank",
  textVersion = "', author_string, '. rgrambank: ', meta$Title, '. ', note, '. ', meta$URL, '"
)'
)

writeLines(citation_content, con = "../inst/CITATION")
