#' @importFrom magrittr %>% %<>%
#' @importFrom keyring key_get


pkg_globals <- new.env(parent = emptyenv())

#' Get AGOL authentication token
#'
#' Treat this token as you would a password: don't hard-code it in your scripts or save it to a file. It will expire after 60 minutes.
#'
#' @param agol_username AGOL headless account username
#' @param agol_password AGOL headless account password (do not hard code this into your scripts!)
#' @param root NPS users should keep the default. See <https://developers.arcgis.com/rest/users-groups-and-items/root.htm> for more information.
#' @param referer NPS users should keep the default. See <https://developers.arcgis.com/rest/users-groups-and-items/generate-token.htm> for more information.
#'
#' @return An AGOL authentication token
#' @export
#'
fetchAGOLToken <- function(agol_username, agol_password = keyring::key_get(service = "AGOL", username = agol_username), root = "nps.maps.arcgis.com", referer = "https://irma.nps.gov") {

  url <- paste0("https://", root, "/sharing/rest/generateToken")

  # Get a token with a headless account
  token_resp <- httr::POST(url,
                           body = list(username = agol_username,
                                       password = agol_password,
                                       expiration = 60,
                                       referer = referer,
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))

  return(agol_token)
}

#' Fetch tabular data from AGOL
#'
#' Retrieves tabular data from AGOL layers and tables, even when number of rows exceeds maximum record count.
#'
#' @param data_path Feature service URL
#' @param layer_number Layer number
#' @param token Authentication token (not needed for public layers)
#' @param geometry Include spatial data columns? Works with points, not tested with other geometry types
#' @param where Query clause specifying a subset of rows (optional; defaults to all rows). See AGOL REST API documentation.
#' @param outFields String indicating which fields to return (optional; defaults to all fields). See AGOL REST API documentation.
#'
#' @return A tibble
#' @export
#'
fetchAllRecords <- function(data_path, layer_number, token, geometry = FALSE, where = "1=1", outFields = "*") {
  result <- tibble::tibble()
  exc_transfer <- TRUE
  offset <- nrow(result)

  qry <- list(where = where,
              outFields = outFields,
              f = "JSON",
              resultOffset = offset)

  if (!missing(token)) {
    qry$token <- token$token
  }

  while(exc_transfer) {
    resp <- httr::GET(paste0(data_path, "/", layer_number, "/query"),
                      query = qry)

    content <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))

    if ("error" %in% names(content)) {
      message <- glue::glue("Error code {content$error$code}: {content$error$message}")
      if ((content$error$message != content$error$details) && (content$error$details != '')) {
        message <- c(message, glue::glue("Details: {content$error$details}"))
      }
      names(message) <- rep("x", length(message))
      cli::cli_abort(message)
    }

    if ("exceededTransferLimit" %in% names(content)) {
      exc_transfer <- content$exceededTransferLimit
    } else {
      exc_transfer <- FALSE
    }

    # TODO: make it so line and polygon data are imported correctly with geometry data
    if (geometry) {
      partial_result <- cbind(content$features$attributes, content$features$geometry) %>%
      dplyr::mutate(wkid = content$spatialReference$wkid) %>%
      tibble::as_tibble()
    } else {
      partial_result <- tibble::as_tibble(content$features$attributes)
    }
    result <- rbind(result, partial_result)
    offset <- nrow(result)
    qry$resultOffset <- offset
  }
  return(result)
}

#' Fetch metadata from AGOL
#'
#' Retrieves metadata from AGOL layers and tables.
#'
#' @param url Feature service URL
#' @param layer_number Optional layer ID
#' @param token Authentication token (not needed for public layers)
#'
#' @return A list
#' @export
#'
fetchMetadata <- function(url, token, layer_number) {

  if (!missing(layer_number)) {
    url <- paste0(url, "/", layer_number, "/metadata")
  } else {
    url <- paste0(url, "/info/metadata")
  }

  # Get metadata
  if (!missing(token)) {
    resp <- httr::GET(url,
                      query = list(token = token$token,
                                   format = "fgdc",
                                   f = "xml"))
  } else {
    resp <- httr::GET(url)
  }
  content <- httr::content(resp, type = "text/xml", encoding = "UTF-8")
  metadata <- xml2::as_list(content)
  metadata <- wrangleLayerMetadata(metadata$metadata, token)

  return(metadata)
}

wrangleMetadata <- function(raw_meta) {
  meta <- lapply(raw_meta$eainfo, function(entity) {
    table_name <- entity$detailed$enttyp$enttypl[[1]]
    item_meta <- list(table_name = list(table_description = entity$detailed$enttyp$enttypd[[1]]))
  })
}

wrangleLayerMetadata <- function(raw_meta, token) {
  # Field level metadata
  fields <- lapply(raw_meta$eainfo$detailed[2:length(raw_meta$eainfo$detailed)], function(field) {
    field_name <- field$attrlabl[[1]]
    desc <- parseAttrDef(field$attrdef[[1]])
    try({
      lookup_name <- trimws(field$attrdomv$codesetd$codesetn[[1]])
      lookup_url <- trimws(field$attrdomv$codesetd$codesets[[1]])
      lookup_df <- fetchHostedCSV(stringr::str_remove(lookup_url, "^.*?id="), token)
      desc$lookup <- list(lookup_name = lookup_name,
                          lookup_url = lookup_url,
                          lookup_df = lookup_df)
    }, silent = TRUE)
    item_meta <- list()
    item_meta[[field_name]] <- desc
    return(item_meta)
  })

  # simplify list
  fields <- purrr::flatten(fields)

  # Table level metadata
  table_name <- trimws(raw_meta$eainfo$detailed[1]$enttyp$enttypl[[1]])
  table_desc <- trimws(raw_meta$eainfo$detailed[1]$enttyp$enttypd[[1]])

  meta <- list(table_name = table_name,
               table_description = table_desc,
               fields = fields)

  return(meta)
}

parseAttrDef <- function(def) {
  attrs <- list()
  if (!is.null(def)) {
    description <- trimws(stringr::str_remove_all(def, "\\{.*\\}"))
  } else {
    description <- NA
  }

  if (any(grepl("\\{.*\\}", def))) {
    starts <- stringr::str_locate_all(def, "\\{")[[1]][, 1]
    ends <- stringr::str_locate_all(def, "\\}")[[1]][, 1]

    for (i in 1:length(starts)) {
      start <- starts[i] + 1
      end <- ends[i] - 1
      name_value <- trimws(strsplit(substr(def, start, end), ":")[[1]])
      attrs[[name_value[1]]] <- name_value[2]
    }
  }
  def_list <- list(description = description,
                   attributes = attrs)

  return(def_list)
}

#' Fetch feature service info from AGOL
#'
#' Retrieves metadata from AGOL layers and tables.
#'
#' @param url Feature service URL
#' @param token Authentication token (not needed for public layers)
#'
#' @return A list
#' @export
#'
fetchLayerAndTableList <- function(url, token) {

  qry <- list(f = "json")

  # Get feature service info
  if (!missing(token)) {
    qry$token <- token$token
  }

  resp <- httr::GET(url,
                    query = qry)

  content <- httr::content(resp, type = "text/json", encoding = "UTF-8")
  feature_service <- jsonlite::fromJSON(content)

  # Get layer id's and names
  if (utils::hasName(feature_service, "layers") & length(feature_service$layers) > 0) {
    layers <- dplyr::select(feature_service$layers, id, name)
  } else {
    layers <- tibble::tibble(.rows = 0)
  }

  # Get table id's and names
  if (utils::hasName(feature_service, "tables") & length(feature_service$tables) > 0) {
    tables <- dplyr::select(feature_service$tables, id, name)
  } else {
    tables <- tibble::tibble(.rows = 0)
  }

  layers_tables <- rbind(layers, tables)

  return(layers_tables)
}


#' Fetch tabular data and metadata from AGOL
#
#' @param database_url Feature service URL
#' @param agol_username AGOL headless account username
#' @param agol_password AGOL headless account password (do not hard code this into your scripts!)
#'
#' @return A list containing tabular data and metadata
#' @export
#'
fetchRawData <- function(database_url, agol_username, agol_password = keyring::key_get(service = "AGOL", username = agol_username)) {
  token <- fetchAGOLToken(agol_username, agol_password)
  layers_tables <- fetchLayerAndTableList(database_url, token)
  ids <- layers_tables$id
  names(ids) <- layers_tables$name

  # Catches errors when metadata can't be imported
  metadata <- tryCatch({
    # Import metadata
    metadata <- sapply(ids, function(id) {
      meta <- fetchMetadata(database_url, token, id)
      meta[["table_id"]] <- id
      return(meta)
    }, simplify = FALSE, USE.NAMES = TRUE)

    # If metadata was unable to import
  }, error = function(e) {
    message("Unable to import metadata - make sure it is filled out")
    print(paste0("Error message: ", e))

    metadata = list()})


  # Catches errors from incorrect queries
  data <- tryCatch({
    # If metadata didn't import - import data tables without using metadata
    if(length(metadata) == 0){
      message("Importing data tables with no metadata")

      # Import data tables without using metadata info
      data <- sapply(layers_tables$id, function(id){
        data_table <- fetchAllRecords(database_url, id, token)
        return(data_table)
      }, simplify = FALSE, USE.NAMES = TRUE)

      # Assign data layers correct names
      names(data) <- layers_tables$name

      data
    } else{
    # If metadata did import - try to import data using metadata info
    data <- sapply(metadata, function(meta){
      data_table <- fetchAllRecords(database_url, meta$table_id, token, outFields = paste(names(meta$fields), collapse = ",")) %>%
        dplyr::select(dplyr::any_of(names(meta$fields)))
      return(data_table)
    }, simplify = FALSE, USE.NAMES = TRUE)}

  }, error = function(e){

    message("Querying data using metadata failed - try calling troubleshootMetadata() to find problem")
    print(paste0("Error message: ", e))
    message("Importing data tables without using metadata")

    # Import data tables without using metadata info
    data <- sapply(layers_tables$id, function(id){
      data_table <- fetchAllRecords(database_url, id, token)
      return(data_table)
    }, simplify = FALSE, USE.NAMES = TRUE)

    # Assign data layers correct names
    names(data) <- layers_tables$name

    return(data)
  })

  raw_data <- list(data = data,
                   metadata = metadata)

  return(raw_data)
}


#' Set data types based on AGOL metadata
#
#' @param raw_data list of tabular data and metadata
#'
#' @return A list containing tabular data and metadata
#' @export
#'
setDataTypesFromMetadata <- function(raw_data) {
  data <- sapply(names(raw_data$data), function(tbl_name) {
    tbl <- raw_data$data[[tbl_name]]
    meta <- raw_data$metadata[[tbl_name]]$fields
    col_types <- sapply(meta, function(field) {
      return(field$attributes$class)
    }, simplify = TRUE, USE.NAMES = TRUE)
    col_types <- unlist(col_types)
    decimal <- names(col_types[col_types == "decimal"])
    integer <- names(col_types[col_types == "integer"])
    date <- names(col_types[col_types == "date"])
    dateTime <- names(col_types[col_types == "dateTime"])
    time <- names(col_types[col_types == "time"])
    string <- names(col_types[col_types == "string"])
    if (nrow(tbl) > 0) {
      tbl <- dplyr::mutate(tbl,
                           dplyr::across(decimal, as.double),
                           dplyr::across(integer, as.integer),
                           dplyr::across(date, function(x) {as.POSIXct(x/1000, origin = "1970-01-01")}),
                           dplyr::across(dateTime, function(x) {as.POSIXct(x/1000, origin = "1970-01-01")}),
                           dplyr::across(time, function(x) {as.POSIXct(x/1000, origin = "1970-01-01")}),
                           dplyr::across(string, as.character))
    }
    return(tbl)
  }, simplify = FALSE, USE.NAMES = TRUE)

  raw_data$data <- data

  return(raw_data)
}


#' A generalized function to start data cleaning
#
#' @param raw_data list of tabular data and metadata
#' @param cols_to_remove a vector containing the names of columns to remove
#' @param id_replacement_names a vector containing
#'
#' @return A list containing tabular data and metadata
#' @export
#'
cleanData <- function(raw_data , cols_to_remove = c("^objectid$", "CreationDate", "Creator", "EditDate", "Editor"), id_replacement_names = c("globalid", "objectid", "parentglobalid")) {
  raw_data <- setDataTypesFromMetadata(raw_data)

  # Clean up data table columns
  raw_data$data <- sapply(raw_data$data, function(tbl) {
    # Fix case in id col names
    global_id <- grepl("^globalid$", names(tbl), ignore.case = TRUE)
    object_id <- grepl("^objectid$", names(tbl), ignore.case = TRUE)
    parent_global_id <- grepl("^parentglobalid$", names(tbl), ignore.case = TRUE)
    id_col_indices <- global_id | object_id | parent_global_id
    # replacement_names <- id_replacement_names[c(any(global_id), any(object_id), any(parent_global_id))]
    #
    # names(tbl)[id_col_indices] <- replacement_names
    # cols_to_remove <- paste0("(", paste(cols_to_remove, collapse = ")|("), ")")  # Turn columns to remove into a regex
    # remove <- names(tbl)[grepl(cols_to_remove, names(tbl))]
    # if(length(remove) > 0) {
    #   tbl <- dplyr::select(tbl, -remove)
    # }

    tbl <- dplyr::mutate(tbl,
                         dplyr::across(tidyselect::where(is.character), ~trimws(.x, which = "both")),
                         dplyr::across(tidyselect::where(is.character), ~dplyr::na_if(.x, "")))
    return(tbl)
  })

  raw_data <- removeCols(raw_data, cols_to_remove = cols_to_remove)

  return(raw_data)
}

fetchHostedCSV <- function(item_id, token, root = "nps.maps.arcgis.com") {
  url <- paste0("https://", root, "/sharing/rest/content/items/", item_id, "/data")
  resp <- httr::GET(url, query = list(token = token$token))
  content <- httr::content(resp, type = "text/csv", encoding = "UTF-8")

  return(content)
}


#' A function to remove specified columns and associated metadata
#
#' @param all_data list of tabular data and metadata
#' @param cols_to_remove a vector containing the names of columns to remove (default is creator and editor columns)
#' @param exact Should the columns be matched exactly or should regular expressions be used?
#'
removeCols <- function(all_data, cols_to_remove = c("CreationDate", "Creator", "EditDate", "Editor"), exact = TRUE) {

  # Remove variables using exact matches
  if(exact){
    print("Removing columns using exact matching")
    # Remove specified attributes from data tables
    all_data$data <- lapply(all_data$data, function(table){
      table <- table[, names(table) %in% cols_to_remove == FALSE]
    })

    # Remove specified attributes from metadata info
    all_data$metadata <- lapply(all_data$metadata, function(table){
      table$fields <- table$fields[names(table$fields) %in% cols_to_remove == FALSE]
      return(table)
    })}
  # Remove variables using regular expressions
  else{
    print("Removing columns using regular expressions")
    # Remove specified attributes from data tables
    all_data$data <- lapply(all_data$data, function(table){
      table2 <- table %>%
        # Remove columns with names that are in the columns to remove vector
        dplyr::select(-grep(paste(cols_to_remove,collapse="|"), names(table), ignore.case = TRUE))
    })

    # Remove specified attributes from metadata info
    all_data$metadata <- lapply(all_data$metadata, function(table){
      table$fields <- table$fields[-grep(paste(cols_to_remove,collapse="|"), names(table$fields), ignore.case = TRUE)]
      return(table)
    })
  }

  return(all_data)
}

#' A function to help troubleshoot differences between column names in data and metadata if querying data using metadata fails
#'
#' @description
#' A short description...
#'
#'
#' @param all_data output of `fetchRawData()` containing data and metadata
#' @param returnAll Do you want to return all column names or only ones that don't have a match
#' @param allInfo Should all attribute information or only column names be returned
#'
#' @returns list of dfs containing data and metadata column names
#' @export
troubleshootMetadata <- function(all_data, returnAll = FALSE, allInfo = FALSE){

  # Vector of valid EML types
  classList <- c("string", "integer", "decimal", "datetime", "date", "time")

  results <- list()

  results <- sapply(names(all_data$data), function(table){
    print(table)
    # Create a new data frame using the extracted data column names
    dataNames <- data.frame(dataColumnNames = colnames(all_data$data[[table]]))

    # Create a new data frame using the extracted metadata column names
    metadataNames <- data.frame(metadataColumnNames = names(all_data$metadata[[table]]$fields))

    # To return all metadata info
    if(allInfo){
      # Extract definition from metadata
      metadataNames$metadataDef <- sapply(all_data$metadata[[table]]$fields, "[[", 1)

      # TODO: This seems to be a list for some tables instead of a vector???
      # Extract class and unit from metadata
      fieldList <- sapply(all_data$metadata[[table]]$fields, "[[", 2)
      metadataNames$metadataClass <- as.vector(sapply(fieldList, "[", 1))
      metadataNames$metadataUnit <- as.vector(sapply(fieldList, "[", 2))
    }

    # Join data and metadata column names together
    joined_data <- dataNames %>%
      dplyr::full_join(metadataNames, dplyr::join_by(dataColumnNames == metadataColumnNames), keep = TRUE) %>%
      # Replace any NULL values with NA
      replace(. == "NULL" | is.null(.), NA_character_)

    # To only return the column names without matches
    if(!returnAll){
      if(allInfo){
      joined_data <- joined_data %>%
        dplyr::filter(is.na(dataColumnNames) | is.na(metadataColumnNames) | is.na(metadataDef) | metadataDef == "" | is.na(metadataClass) | !(tolower(metadataClass) %in% classList) | ((tolower(metadataClass) == "integer" | tolower(metadataClass) == "decimal") & is.na(metadataUnit)))
      } else{
        joined_data <- joined_data %>%
          # Filter only for for N
          dplyr::filter(is.na(dataColumnNames) | is.na(metadataColumnNames))
      }
    }

    # # Add results to list
    # results[[i]] <- joined_data

    return(joined_data)
  }, simplify = FALSE)

  # Give tables in result the correct names
  names(results) <- names(all_data$data)
  return(results)
}
