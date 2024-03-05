

#' Write data and data dictionaries to files
#'
#' You shouldn't need to call this function directly unless you are using it to write a data export function for another R package. If you are using this package on its own, you will usually want to call `fetchFromAccess(save_to_files = TRUE)`.
#'
#' @param all_tables Output of `fetchRaw Data()`
#' @param data_dir Folder to store data csv's in
#' @param dictionary_dir Folder to store data dictionaries in
#' @param lookup_dir Optional folder to store lookup tables in. If left as `NA`, lookups won't be exported.
#' @param dictionary_filenames Named list with names `c("tables", "attributes", "categories")` indicating what to name the tables, attributes, and categories data dictionaries. You are encouraged to keep the default names unless you have a good reason to change them.
#' @param verbose Output feedback to console?
#'
#' @export
#'
writeToFiles <- function(all_data, data_dir = here::here("data", "final"), dictionary_dir = here::here("data", "dictionary"), dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                                                                                                                                         attributes = "data_dictionary_attributes.txt",
                                                                                                                                                         categories = "data_dictionary_categories.txt"),
                         lookup_dir = NA, verbose = FALSE, cols_to_remove = c("Editor", "Creator")) {


  data <- all_data$data
  metadata <- all_data$metadata



  # Write data to csv
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  if (verbose) {message(paste0("Writing data to ", data_dir, "..."))}
  lapply(names(data), function(tbl_name) {
    if (verbose) {message(paste0("\t\t", tbl_name, ".csv"))}
    tbl <- data[[tbl_name]]
    # Convert dates & times back to character before writing so that they stay in correct format
    # col_types <- col_spec[[tbl_name]]
    # date_cols <- names(col_types)[col_types == "D"]
    # datetime_cols <- names(col_types)[col_types == "T"]
    # time_cols <- names(col_types)[col_types == "t"]
    # for (col in c(date_cols, datetime_cols, time_cols)) {
    #   format_string <- fields_dict$dateTimeFormatString[fields_dict$tableName == tbl_name & fields_dict$attributeName == col]
    #   if (is.na(format_string)) {
    #     stop(paste("No datetime format provided for column", col, "in table", tbl_name))
    #   }
    #   tbl <- tbl %>%
    #     dplyr::mutate(across(col, ~format(.x, format = QCkit::convert_datetime_format(format_string, convert_z = FALSE))))
    # }
    readr::write_csv(tbl,
                     here::here(data_dir, paste0(tbl_name, ".csv")),
                     na = "")
  })

  # Write metadata to CSVs
  #generateMetadataCSVs(metadata)
}

#' Write data dictionaries to files
#'
#' @param metadata Metadata part of output of `fetchRawData()`
#'
#' @export
#'
generateMetadataCSVs <- function(metadata, dictionary_dir = here::here("data", "dictionary"), dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                                                                                                       attributes = "data_dictionary_attributes.txt",
                                                                                                                       categories = "data_dictionary_categories.txt"),
                                 verbose = FALSE){
  # Create empty data frames to hold metadata
  dict <- list(
  tables_dict = data.frame(tableName = character(),
                            fileName = character(),
                            tableDescription = character()),

  attributes_dict = data.frame(tableName = character(),
                                attributeName = character(),
                                attributeDefinition = character(),
                                class = character(),
                                unit = character(),
                                dateTimeFormatString = character(),
                                missingValueCode = character(),
                                missingValueCodeExplanation = character(),
                                lookup = character(),
                                rClass= character()),

  categories_dict = data.frame(attributeName = character(),
                                code = character(),
                                definition = character()))



    dict <- lapply(names(metadata), function(table) {

      # print(table)
      # print(metadata[[table]]$table_name)
      # print('hi')
    dict$tables_dict <- dict$tables_dict %>%
      tibble::add_row(tableName = metadata[[table]]$table_name,
                      fileName = paste0(metadata[[table]]$table_name, ".csv"),
                      tableDescription = metadata[[table]]$table_description)

         })


  # TODO change this to apply
  for (i in 1:length(data$metadata)) {

    # Add information about each table to table metadata
    tables_dict <- tables_dict %>%
      tibble::add_row(tableName = data$metadata[[i]]$table_name,  tableDescription = data$metadata[[i]]$table_description)

    for(j in 1:length(data$metadata[[i]]$fields)) {

      # Add information about each attribute to attribute metadata
      attributes_dict <- attributes_dict %>%
        tibble::add_row(tableName = data$metadata[[i]]$table_name,
                        attributeName = names(data$metadata[[i]]$fields[j]),
                        attributeDefinition = data$metadata[[i]]$fields[[j]]$description,
                        class = data$metadata[[i]]$fields[[j]]$attributes$class,
                        unit = ifelse(!is.null(data$metadata[[i]]$fields[[j]]$attributes$unit), data$metadata[[i]]$fields[[j]]$attributes$unit, NA),
                        lookup = ifelse(length(data$metadata[[i]]$fields[[j]]$lookup$lookup_name) != 0, data$metadata[[i]]$fields[[j]]$lookup$lookup_name, NA))

      # If there is a lookup table associated with the attribute add it to categories table
      if (length(data$metadata[[i]]$fields[[j]]$lookup$lookup_name) != 0){

        lookupTable <- data$metadata[[i]]$fields[[j]]$lookup$lookup_df %>%
          # Add column containing attribute name and rename columns to be able to bind to categories table
          dplyr::mutate(attributeName = names(data$metadata[[i]]$fields[j]), code = name, definition = label) %>%
          dplyr::select(code, definition, attributeName)

        categories_dict <- rbind(categories_dict, lookupTable)
      }

    }
  }

  # Write dictionaries to file
  if (!dir.exists(dictionary_dir)) {
    dir.create(dictionary_dir, recursive = TRUE)
  }
  if (verbose) {message(paste0("\nWriting metadata to ", dictionary_dir, "..."))}
  if (verbose) {message(paste0("\t\t", dictionary_filenames["tables"]))}
  readr::write_tsv(tables_dict, here::here(dictionary_dir, dictionary_filenames["tables"]), na = "", append = FALSE)
  if (verbose) {message(paste0("\t\t", dictionary_filenames["attributes"]))}
  readr::write_tsv(attributes_dict, here::here(dictionary_dir, dictionary_filenames["attributes"]), na = "", append = FALSE)
  if (verbose) {message(paste0("\t\t", dictionary_filenames["categories"]))}
  readr::write_tsv(categories_dict, here::here(dictionary_dir, dictionary_filenames["categories"]), na = "", append = FALSE)
}

#' Generate column spec from data dictionary
#'
#' Given a fields data dictionary, create a list of column specifications that can be used in [readr::read_csv()] or [vroom::vroom()]
#'
#' @param fields Fields data dictionary, as returned by [fetchFromAccess]
#'
#' @return A list of lists
#' @export
#'
makeColSpec <- function(fields) {
  fields %<>%
    dplyr::mutate(colObject = dplyr::case_when(rClass == "character" ~ "c",
                                               rClass == "logical" ~ "l",
                                               rClass == "integer" ~ "i",
                                               rClass == "numeric" ~ "d",
                                               rClass == "Date" ~ "D",
                                               rClass == "POSIXct" ~ "T",
                                               rClass == "POSIXlt" ~ "T",
                                               rClass == "hms" ~ "t",
                                               rClass == "difftime" ~ "t",
                                               rClass == "factor" ~ "f",
                                               TRUE ~ "?")) %>%
    split(~tableName)

  col_spec <- lapply(fields, function(table) {
    spec <- split(table$colObject, table$attributeName)
    return(spec)
  })

  names(col_spec) <- names(fields)

  return(col_spec)
}

