

#' Write data and data dictionaries to files
#'
#' @param all_data Output of `fetchRawData()`
#' @param data_dir Folder to store data csv's in
#' @param dictionary_dir Folder to store data dictionaries in
#' @param lookup_dir Optional folder to store lookup tables in. If left as `NA`, lookups won't be exported.
#' @param dictionary_filenames Named list with names `c("tables", "attributes", "categories")` indicating what to name the tables, attributes, and categories data dictionaries. You are encouraged to keep the default names unless you have a good reason to change them.
#' @param verbose Output feedback to console?
#' @param removeColumns Should columns be removed?
#' @param cols_to_remove Columns that should be removed, `c("Editor", "Creator")` is the default because they can contain personally identifiable information
#' @param missing_value_dict a dictionary containing key-value pairs where the key is the attribute class and the value is the code for a missing value
#'
#' @export
#'
writeToFiles <- function(all_data, data_dir = here::here("data", "final"), dictionary_dir = here::here("data", "dictionary"), dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                                                                                                                                         attributes = "data_dictionary_attributes.txt",
                                                                                                                                                         categories = "data_dictionary_categories.txt"),
                         lookup_dir = NA, verbose = FALSE, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"),
                         missing_value_dict = hash::hash(keys = c("string", "integer", "decimal", "dateTime"), values = c("N/D", "-999", "-999", ""))) {

  if (removeColumns) {


    all_data <- removeCols(all_data)

    # # Remove specified attributes from data tables (default is creator and editor columns)
    # all_data$data <- lapply(all_data$data, function(table){
    #   table <- table %>% dplyr::select(-any_of(cols_to_remove))
    # })
    #
    # # Remove specified attributes from metadata info (default is creator and editor columns)
    # all_data$metadata <- lapply(all_data$metadata, function(table){
    #   table$fields <- table$fields[names(table$fields) %in% cols_to_remove == FALSE]
    #   return(table)
    # })
  }


  # Write metadata to csv's
  dict <- generateMetadataCSVs(data = all_data, missing_value_dict = missing_value_dict)

  col_spec <- makeColSpec(dict$attributes_dict)

  # Write data to csv
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  if (verbose) {message(paste0("Writing data to ", data_dir, "..."))}
  lapply(names(all_data$data), function(tbl_name) {
    if (verbose) {message(paste0("\t\t", tbl_name, ".csv"))}
    tbl <- all_data$data[[tbl_name]]
    # Convert dates & times back to character before writing so that they stay in correct format
    col_types <- col_spec[[tbl_name]]
    date_cols <- names(col_types)[col_types == "D"]
    datetime_cols <- names(col_types)[col_types == "T"]
    time_cols <- names(col_types)[col_types == "t"]
    for (col in c(date_cols, datetime_cols, time_cols)) {
      format_string <- dict$attributes_dict$dateTimeFormatString[dict$attributes_dict$tableName == tbl_name & dict$attributes_dict$attributeName == col]
      if (is.na(format_string)) {
        stop(paste("No datetime format provided for column", col, "in table", tbl_name))
      }
      tbl <- tbl %>%
        dplyr::mutate(across(col, ~format(.x, format = QCkit::convert_datetime_format(format_string, convert_z = FALSE))))
    }
    readr::write_csv(tbl,
                     here::here(data_dir, paste0(tbl_name, ".csv")),
                     na = "")
  })


}

#' Write data dictionaries to files
#'
#' @param data output of `fetchRawData()`
#' @param dictionary_dir Folder to store data dictionaries in
#' @param dictionary_filenames Named list with names `c("tables", "attributes", "categories")` indicating what to name the tables, attributes, and categories data dictionaries. You are encouraged to keep the default names unless you have a good reason to change them.
#' @param verbose Output feedback to console?
#' @param missing_value_dict a dictionary containing key-value pairs where the key is the attribute class and the value is the code for a missing value
#'
#' @export
#'
#'
# TODO: make sure that the metadata is correct even if variables are out of order
# TODO make it so the missing value dict doesn't break things if something is null/na
# TODO re-assign POSIXct/POSIXt variables so they aren't printed twice in data dict
generateMetadataCSVs <- function(data, dictionary_dir = here::here("data", "dictionary"), dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                                                                                                       attributes = "data_dictionary_attributes.txt",
                                                                                                                       categories = "data_dictionary_categories.txt"),
                                 verbose = FALSE, missing_value_dict = hash::hash(keys = c("string", "integer", "decimal", "dateTime", "date", "time"), values = c("N/D", "-999", "-999", "", "", ""))){

  # Create empty data frames to hold metadata
  dict <- list(tables_dict = data.frame(tableName = character(),
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



    # dict <- lapply(names(metadata), function(table) {
    #
    #   # print(table)
    #   # print(metadata[[table]]$table_name)
    #   # print('hi')
    # dict$tables_dict <- dict$tables_dict %>%
    #   tibble::add_row(tableName = metadata[[table]]$table_name,
    #                   fileName = paste0(metadata[[table]]$table_name, ".csv"),
    #                   tableDescription = metadata[[table]]$table_description)
    #
    #      })


  # TODO change this to apply
  for (i in 1:length(data$metadata)) {

    # Add information about each table to table metadata
    dict$tables_dict <- dict$tables_dict %>%
      tibble::add_row(tableName = data$metadata[[i]]$table_name,
                      fileName = paste0(data$metadata[[i]]$table_name, ".csv"),
                      tableDescription = data$metadata[[i]]$table_description)

    for(j in 1:length(data$metadata[[i]]$fields)) {

      # Add information about each attribute to attribute metadata
      dict$attributes_dict <- dict$attributes_dict %>%
        tibble::add_row(tableName = data$metadata[[i]]$table_name,
                        attributeName = names(data$metadata[[i]]$fields[j]),
                        attributeDefinition = data$metadata[[i]]$fields[[j]]$description,
                        class = data$metadata[[i]]$fields[[j]]$attributes$class,
                        # If attribute has a unit add to new metadata
                        unit = ifelse(!is.null(data$metadata[[i]]$fields[[j]]$attributes$unit), data$metadata[[i]]$fields[[j]]$attributes$unit, NA),
                        # If attribute is date/dateTime/time add format to new metadata
                        dateTimeFormatString = dplyr::case_when(
                          grepl("^date$", data$metadata[[i]]$fields[[j]]$attributes$class, ignore.case = TRUE) ~ "YYYY-MM-DD",
                          grepl("^dateTime$", data$metadata[[i]]$fields[[j]]$attributes$class, ignore.case = TRUE) ~ "YYYY-MM-DDThh:mm:ss",
                          grepl("^time$", data$metadata[[i]]$fields[[j]]$attributes$class, ignore.case = TRUE) ~ "hh:mm:ss",
                          .default = NA),
                        # Add the corresponding missing value code from the missing value dictionary based on the attribute class
                        missingValueCode = missing_value_dict[[data$metadata[[i]]$fields[[j]]$attributes$class]],
                        # If there is a lookup table for attribute add name to new metadata
                        lookup = ifelse(length(data$metadata[[i]]$fields[[j]]$lookup$lookup_name) != 0, data$metadata[[i]]$fields[[j]]$lookup$lookup_name, NA),
                        rClass = class(data$data[[names(data$metadata)[i]]][[j]]))

      # If there is a lookup table associated with the attribute add it to categories table
      if (length(data$metadata[[i]]$fields[[j]]$lookup$lookup_name) != 0){

        lookupTable <- data$metadata[[i]]$fields[[j]]$lookup$lookup_df %>%
          dplyr::rename_with(~tolower(.x)) %>%
          # Add column containing attribute name and rename columns to be able to bind to categories table
          dplyr::mutate(attributeName = names(data$metadata[[i]]$fields[j]), code = name, definition = label) %>%
          dplyr::select(code, definition, attributeName)

        dict$categories_dict <- rbind(dict$categories_dict, lookupTable)
      }

    }
  }

  # Write dictionaries to file
  if (!dir.exists(dictionary_dir)) {
    dir.create(dictionary_dir, recursive = TRUE)
  }
  if (verbose) {message(paste0("\nWriting metadata to ", dictionary_dir, "..."))}
  if (verbose) {message(paste0("\t\t", dictionary_filenames["tables"]))}
  readr::write_tsv(dict$tables_dict, here::here(dictionary_dir, dictionary_filenames["tables"]), na = "", append = FALSE)
  if (verbose) {message(paste0("\t\t", dictionary_filenames["attributes"]))}
  readr::write_tsv(dict$attributes_dict, here::here(dictionary_dir, dictionary_filenames["attributes"]), na = "", append = FALSE)
  if (verbose) {message(paste0("\t\t", dictionary_filenames["categories"]))}
  readr::write_tsv(dict$categories_dict, here::here(dictionary_dir, dictionary_filenames["categories"]), na = "", append = FALSE)

  return(dict)
  }

#' Generate column spec from data dictionary
#'
#' Given a fields data dictionary, create a list of column specifications that can be used in [readr::read_csv()] or [vroom::vroom()]
#'
#' @param fields Fields data dictionary, as returned by [fetchRawData]
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

# classes <- data.frame(tableName = character(),
#                       variable = character(),
#                       class = character(),
#                       type = character())
#
# for (i in 1:length(data$metadata)) {
#   for(j in 1:length(data$metadata[[i]]$fields)) {
#     classes <- classes %>%
#       add_row(
#         tableName = data$metadata[[i]]$table_name,
#         variable = names(data$metadata[[i]]$fields)[[j]],
#         class = data$metadata[[i]]$fields[[j]]$attributes$class,
#         type = class(data$metadata[[i]]$fields[[j]]$attributes$class)
#       )
#     # append(classes, data$metadata[[i]]$fields[[j]]$attributes$class)
#   }
# }
