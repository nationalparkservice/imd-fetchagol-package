% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_to_csv.R
\name{generateMetadataCSVs}
\alias{generateMetadataCSVs}
\title{Write data dictionaries to files}
\usage{
generateMetadataCSVs(
  data,
  dictionary_dir = here::here("data", "dictionary"),
  dictionary_filenames = c(tables = "data_dictionary_tables.txt", attributes =
    "data_dictionary_attributes.txt", categories = "data_dictionary_categories.txt"),
  lookup_dir = NA,
  verbose = FALSE,
  missing_value_dict = hash::hash(keys = c("string", "integer", "decimal", "datetime",
    "date", "time"), values = c("N/D", "-999", "-999", "", "", ""))
)
}
\arguments{
\item{data}{output of \code{fetchRawData()}}

\item{dictionary_dir}{Folder to store data dictionaries in}

\item{dictionary_filenames}{Named list with names \code{c("tables", "attributes", "categories")} indicating what to name the tables, attributes, and categories data dictionaries. You are encouraged to keep the default names unless you have a good reason to change them.}

\item{lookup_dir}{Optional folder to store lookup tables in. If left as \code{NA}, lookups won't be exported.}

\item{verbose}{Output feedback to console?}

\item{missing_value_dict}{a dictionary containing key-value pairs where the key is the attribute class and the value is the code for a missing value}
}
\description{
Write data dictionaries to files
}
