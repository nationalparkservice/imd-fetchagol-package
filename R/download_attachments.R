#' Download AGOL attachments (JPG or PNG)
#'
#' Currently only works with jpeg or png images. Currently does not handle large (>2000 records) datasets.
#'
#' @param feature_layer_url URL of the feature layer containing attachments (including layer ID). *Do not* include a slash at the end.
#' @param agol_username AGOL username for a headless account with permissions to view the feature layer.
#' @param agol_password Do not hardcode passwords in your saved code! By default, assumes that the password was stored using [keyring::key_set()] with `service = AGOL`.
#' @param test_run If `TRUE`, returns attachment data and proposed file locations without actually downloading and saving attachments.
#' @param dest_folder Folder in which to save downloaded attachments
#' @param prefix Prefix for photo file names. Can either be a character string (to set the same prefix for all files) or an unquoted column name from the attachment table.
#' @param sep Separator character to use between prefix and photo ID in file names. Ignored if `custom_name` == FALSE
#' @param custom_name Use prefix and sep to create filenames from attachment ID's? If `FALSE`, will use the filename as stored in AGOL. If you are not customizing your filenames when saving attachments, this may cause problems due to duplicate filenames.
#' @param append_id Append attachment id to custom prefix? Ignored if `custom_name == FALSE`
#' @param join_cols Named vector in the format `c("foreign key column to data table in attachment table" = "primary key column of data table")` where the attachment table is the table that stores attachments only (this is mostly hidden from view in AGOL) and the data table is the table for which attachments have been enabled. If you aren't sure what the foreign key column is called, leave this argument empty and set `test_run = TRUE`. The function will try to guess the join columns, but if it gets it wrong, it will return the attachment table without the accompanying data table. At that point, you can check the name(s) of the foreign key column(s) and re-run your code with the join columns specified. They should be something along the lines of "parentGlobalId" or "parentObjectId".
#'
#' @return Tibble of attachment data, including paths to saved files.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic use
#' downloadPhotos("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SV_OverviewPhotos/FeatureServer/0", agol_username = "mojn_data")
#'
#' # Include data from table for which attachments are enabled
#' downloadPhotos("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SV_OverviewPhotos/FeatureServer/0",
#'                         agol_username = "mojn_data",
#'                         join_cols = c("parentGlobalId" = "globalID"))
#' }
downloadPhotos <- function(feature_layer_url, agol_username, agol_password = keyring::key_get("AGOL", agol_username), dest_folder = "photo_downloads", custom_name = TRUE, prefix = "photo", append_id = TRUE, sep = "_", join_cols = c(), test_run = FALSE) {

  # Format destination folder path properly and create it if it doesn't exist
  dest_folder <- normalizePath(dest_folder, winslash = .Platform$file.sep, mustWork = FALSE)
  if (!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE)
  }

  # AGOL authentication - get token
  token_resp <- httr::POST("https://nps.maps.arcgis.com/sharing/rest/generateToken",
                           body = list(username = agol_username,
                                       password = agol_password,
                                       referer = 'https://irma.nps.gov',
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))

  # TODO: modify this to handle datasets with more records than the API transfer limit
  # Get attachment table data
  data <- httr::GET(paste0(feature_layer_url, "/query"),
                    query = list(where="1=1",
                                 outFields="*",
                                 f="JSON",
                                 token=agol_token$token)) %>%
    httr::content(type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  global_id <- data$globalIdFieldName  # Get global ID field name
  object_id <- data$objectIdFieldName  # Get object ID field name

  data <- data$features$attributes %>%
    tibble::as_tibble()

  # Get attachment info
  attrs <- httr::GET(paste0(feature_layer_url, "/queryAttachments"),
                     query = list(f="JSON",
                                  definitionExpression = "1=1",
                                  returnUrl = "true",
                                  returnMetadata="true",
                                  token=agol_token$token)) %>%
    httr::content(type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  # Get attachment info as a dataframe and generate file paths for saving attachments
  attachments <- attrs$attachmentGroups %>%
    tibble::as_tibble() %>%
    tidyr::unnest(cols = attachmentInfos)

  # Join attachment table data to attachment info
  if (is.null(join_cols)) {
    if ("parentGlobalId" %in% names(attachments) && length(global_id) == 1) {
      join_cols <- c("parentGlobalId" = global_id)
    } else if ("parentObjectId" %in% names(attachments) && length(object_id) == 1) {
      join_cols <- c("parentObjectId" = object_id)
    }
  }

  if (length(join_cols) > 0) {
    attachments <- dplyr::left_join(attachments, data, by = join_cols)
  } else {
    warning("Could not join attachment info to parent data table")
  }

  if (!append_id) {
    sep <- ""  # Make separator blank if not appending ID to photo name
  }
  attachments <- attachments %>%
    dplyr::mutate(customFileName = custom_name,
                  appendID = append_id,
                  fileExt = paste0(".", tools::file_ext(name)),  # Get file extension
                  fileName = ifelse(customFileName, paste({{prefix}}, ifelse(appendID, id, ""), sep = sep), tools::file_path_sans_ext(name)),  # If custom_name is FALSE, just use the original filename
                  fileDest = file.path(dest_folder, paste0(fileName, fileExt))) %>%  # Full file path
    dplyr::select(-fileExt, -fileName, -customFileName)

  if (length(unique(attachments$fileDest)) != length(attachments$fileDest)) {
    stop("Cannot save photos due to duplicate filenames. Try setting `custom_name = TRUE`.")
  }

  if (!test_run) {
    apply(attachments, 1, function(att) {
      dat <- httr::GET(att$url,
                       query = list(token = agol_token$token)) %>%
        httr::content()
      if (grepl("jpe{0,1}g", att$contentType, ignore.case = TRUE)) {
        jpeg::writeJPEG(dat, att$fileDest)
      } else if (grepl("png", att$contentType, ignore.case = TRUE)) {
        png::writePNG(dat, target = att$fileDest)
      } else {
        warning(paste("Could not write", att$fileDest, "- unsupported file type"))
      }
    })
  }

  return(attachments)
}
