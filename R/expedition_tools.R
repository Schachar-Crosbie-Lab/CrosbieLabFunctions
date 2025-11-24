
#### Get Data ####
#' @name get_data
#'
#' @title Get Data
#'
#' @description
#'  `r lifecycle::badge('experimental')`
#' This function uses a REDCap API token to return the project's data and dictionary.
#' This work relies on REDCapR. See \link[REDCapR]{redcap_read}
#'
#' @export
#'
#' @inheritParams REDCapR::redcap_read
#' @inheritParams REDCapR::redcap_metadata_read
#' @inheritParams REDCapR::redcap_event_instruments
#' @inheritParams REDCapR::redcap_variables
#'
#' @importFrom REDCapR redcap_read
#' @importFrom REDCapR redcap_metadata_read
#' @importFrom REDCapR redcap_event_instruments
#' @importFrom REDCapR redcap_project_info_read
#' @importFrom REDCapR redcap_variables
#' @import dplyr
#'
#' @returns A list of dataframes
#'    * `df`: The data in the associated REDCap project
#'    * `dd`: A dataframe with the project's data dictionary
#'    * `events`: A dataframe with the project's event instrument mapping
#'    * `info`: A dataframe with the information about the REDCap project
#'
get_data <- function(token = NULL, redcap_uri = NULL, fields = NULL, export_survey_fields = TRUE) {

  #Use redcap_read to use API to return REDCap data
  df <- REDCapR::redcap_read(redcap_uri = redcap_uri,
                             token = token,
                             fields = fields,
                             export_survey_fields = TRUE,
                             verbose = FALSE,
                             raw_or_label = "raw")$data

  meta_data <- REDCapR::redcap_metadata_read(redcap_uri = redcap_uri,
                                             token = token)$data

  events <- REDCapR::redcap_event_instruments(redcap_uri = redcap_uri,
                                              token = token)$data

  info <- REDCapR::redcap_project_info_read(redcap_uri = redcap_uri,
                                            token = token)$data

  vars <-  REDCapR::redcap_variables(redcap_uri = redcap_uri,
                                     token = token)$data

  dd_out <- full_join(meta_data,vars, by = c('field_name' = 'original_field_name')) |>
    mutate(field_type = case_when(stringr::str_detect(field_name,"_complete") & is.na(field_type) ~ "checkbox",
                                  T ~ field_type),
           form_name = case_when(stringr::str_detect(field_name,"_complete") & is.na(form_name) ~ stringr::str_extract(field_name, ".*(?=_complete)"),
                                 T ~ form_name),
           export_field_name = case_when(is.na(export_field_name) ~ field_name,
                                         T ~ export_field_name)) |>
    rename(field_name_base = field_name,
           field_name = export_field_name) |>
    select(field_name, field_name_base, everything())

  return(list(df = df, dd = dd_out, events = events, info = info))



}


#### Clean Data
#' @name clean_data
#'
#' @title Clean Data
#'
#' @description
#'  `r lifecycle::badge('experimental')`
#' This function "cleans" REDCap data from the [get_data()] function. To clean the data, the function uses the [clean_checkboxes()] function and the project's data dictionary to expand the checkboxes
#' and the form statuses to change 0's to NA when an instrument is empty.
#'
#' @export
#'
#' @param list a list that must contain the following
#'    * `df`: The data in the associated REDCap project
#'    * `dd`: A dataframe with the project's data dictionary
#'    * `events`: A dataframe with the project's event mapping
#'    * `info`: A dataframe with the project's info API call
#'
#' @returns A list of dataframes
#'    * `df`: The data in the associated REDCap project
#'    * `dd`: A dataframe with the project's data dictionary
#'    * `events`: A dataframe with the project's event instrument mapping
#'    * `info`: A dataframe with the information about the REDCap project
#'
#'  @seealso [clean_checkboxes()]
#'
clean_data <- function(list = NULL) {


  df <- list$df
  dd <- list$dd
  events <- list$events
  info <- list$info

  # Get ID col from data dictionary
  id_col <- list$dd$field_name[1]

  # Updated the get_data function to add 'complete' forms to the data dictionary to cleaned as checkboxes
  df_out <- CrosbieLabFunctions::clean_checkboxes(df, dd) |>
    select(any_of(id_col), contains('redcap_'), everything())


  return(list(df = df_out, dd = dd, events = events, info = info))


}

#### Check for Changes
#' @name reconcile
#'
#' @title Reconcile
#'
#' @description
#'  `r lifecycle::badge('experimental')`
#' We now have a cleaned version of the data in [clean_data()]. We need to reconcile for changes in the out project. This function 1) compares the metadata and 2) looks for changes in the
#' input to output projects using the [track_changes()] function. If data are being overwritten, the process is stopped and a record of the differences is saved in the error_data.csv file. In order for the function to work you pass a list with
#' the input df, dd and events, and pass the output token and redcap_uri. The function gets the most up to date data from the output project and compares it with the input data
#'
#' @export
#'
#' @inheritParams REDCapR::redcap_read
#' @inheritParams REDCapR::redcap_metadata_read
#' @param list The list returned from [clean_data()]
#' @param transfer_to_braincode T/F - True gives additional warnings
#'
#' @importFrom here here
#' @import dplyr
#' @importFrom rio export
#'
#' @returns A List
#'    * `df`: The data in the associated REDCap project
#'    * `dd`: A dataframe with the project's data dictionary
#'    * `events`: A dataframe with the project's event instrument mapping
#'    * `info`: A dataframe with the information about the REDCap project
#'    * `reconciled`: A message whether the reconciliation was successful - success or no changes
#'    * `changes`: A dataframe with the changes to be pushed
#'    * `downstream_name`: The name of the downstream project
#'    * `downstream_pid`: The project id of the downstream project
#'
#' @seealso [track_changes()]

reconcile_data <- function(list = NULL,
                           token = NULL, redcap_uri = NULL,
                           transfer_to_braincode = TRUE){

  cat("---------------------------------------------------------------------------------\n")
  cat("Reconciling data\n---------------------------------------------------------------------------------\n\n")

  # Get downstream data
  downstream <- get_data(token, redcap_uri) |>
    clean_data()

  # Project titles of two projects
  list_name <- paste0(list$info$project_title, " (PID ",list$info$project_id,")")
  downstream_name <- paste0(downstream$info$project_title, " (PID ",downstream$info$project_id,")")

  # Check that id_cols match
  match <- list$dd$field_name[1] == downstream$dd$field_name[1]
  if(match){
    id_col <- list$dd$field_name[1]
  } else {
    stop(paste0("Reconciling data error: The ID columns between ",list_name," and ",downstream_name," do not match."))
  }

  # Most projects don't need this level of sophistication. We are just trying to make sure that our staging project always matches Braincode
  if(transfer_to_braincode){
    # Look for data dictionary changes
    dd_changes <- CrosbieLabFunctions::track_changes(df_in = list$dd,
                                                     df_out = downstream$dd,
                                                     identifier_column = 'field_name',
                                                     names = c(list_name, downstream_name)) |>
      # Got okay to ignore the SQL fields
      filter(get(downstream_name) != 'sql') |>
      filter(!field_name %in% c('consent_vrsn', 'consent_version_genomics', 'consent_vrsn_cgvr', 'consent_vrsn_cgvr_2'))

    # Look for instrument mapping changes
    event_changes <- full_join(list$events, downstream$events,
                               by = c('form','arm_num','unique_event_name'),
                               suffix = c('_in','_out')) |>
      filter(if_any(everything(), ~is.na(.x)))

    changes <- bind_rows(dd_changes, event_changes)

    warning_file <- here::here('warning_data.csv')
    rio::export(changes, file = warning_file)

  }

  id_cols <- downstream$df |>
    select(any_of(c(id_col, 'redcap_event_name', 'redcap_repeat_instrument', 'redcap_repeat_instance')))


  # Look for data differences
  df_changes <- CrosbieLabFunctions::track_changes(df_in = downstream$df,
                                                   df_out = list$df,
                                                   identifier_columns = colnames(id_cols),
                                                   names = c( downstream_name, list_name))

  error_file <- here::here('error_data.csv')
  rio::export(df_changes, file = error_file)

  # We are taking a cautious approach to what data is imported
  # Data that are added are okay
  # Data that are removed or changed need to be manually reconciled
  if(any(grepl('removed|changed', df_changes$change, ignore.case = T)) & nrow(df_changes) > 0){

    stop(paste0('In this push from ',list_name,' to ',downstream_name,
                ' some data were being removed or deleted. Please review the changes and fix manually.'))

  } else if(all(grepl('Added', df_changes$change, ignore.case = T)) & nrow(df_changes) > 0) {
    message <- 'success'

  } else if(nrow(df_changes) == 0){
    message <- 'no changes'
  }

  message(paste0("Reconciling status: ",message))

  list_out <- list
  list_out$reconciled <- message
  list_out$changes <- df_changes
  list_out$downstream_name <- downstream_name
  list_out$downstream_pid <- downstream$info$project_id
  return(list_out)
}


#### Write Data to a project ####
#' @name write
#'
#' @title Write Data
#'
#' @description
#'  `r lifecycle::badge('experimental')`
#' This function takes the list from reconcile data and writes data to a REDCap project
#'
#' @export
#'
#' @inheritParams REDCapR::redcap_write
#' @param list The list returned from [reconcile_data()]
#' @param overwrite_blanks T/F - allows overwriting blanks if changing data from a value to a blank
#'
#' @importFrom REDCapR redcap_write
#' @importFrom lubridate now
#'
#'
#' @returns A List
#'    * `status`: The status of the data transfer
#'    * `datetime`: Datetime transfer occured
#'    * `project`: The name of the downstream project
#'    * `pid`: The pid of the downstream project
#'    * `server`: The redcap_uri of the downstream project
#'    * `changes`: A dataframe with the changes pushed
#'    * `df`: The data transferred
#'
#' @seealso REDCapR::[redcap_write()]

write_data <- function(list = NULL,
                       token = NULL, redcap_uri = NULL,
                       overwrite_blanks = FALSE) {

  cat("---------------------------------------------------------------------------------\n")
  cat("Writing data\n---------------------------------------------------------------------------------\n\n")

  # Project titles of two projects
  list_name <- paste0(list$info$project_title, " (PID ",list$info$project_id,")")
  downstream_name <- list$downstream_name

  if(list$reconciled == 'success'){

    status <- REDCapR::redcap_write(ds_to_write = list$df,
                                    token = token,
                                    redcap_uri = redcap_uri,
                                    overwrite_with_blanks = overwrite_blanks)

  } else if(list$reconciled == 'no changes'){
    status <- list(success = NA)
    message(paste0("Write Data: There are no differences between ",list_name," and ",downstream_name,". No data will be pushed"))
  } else {
    status <- list(success = FALSE)
    message(paste0("Write Data: Something unexpected happened when reconciling",list_name," and ",downstream_name,". Please investigate manually."))
  }

  datetime <- lubridate::now()

  list_out <- list(status = status,
                   datetime = datetime,
                   project = downstream_name,
                   pid = list$downstream_pid ,
                   server = redcap_uri,
                   df = list$df,
                   changes = list$changes)
  return(list_out)
}

#### Backup Data if data is written ####
#' @name backup_data
#'
#' @title Backup Data
#'
#' @description
#'  `r lifecycle::badge('experimental')`
#' This function backs up the data that has just been written from [write_data()]
#'
#' @export
#'
#' @param list The list returned from [write_data()]
#' @param file_path The file path to the backup folder
#'
#' @importFrom lubridate ymd_hms
#' @importFrom tibble rownames_to_column
#' @importFrom here here
#' @importFrom rio export
#' @import dplyr
#' @importFrom utils head
#'
#' @returns backup The status of the backup.
#'
#'
backup_data <- function(list = NULL,
                        file_path = NULL) {

  cat("---------------------------------------------------------------------------------\n")
  cat("Backing up data\n---------------------------------------------------------------------------------\n\n")

  backup <- FALSE

  # recognize server for backup
  recognize_server <- if(grepl('braincode',list$server)) {
    host <- 'braincode'
  } else {
    host <- 'sickkids'
  }

  project_backup_dir <- file.path(file_path,paste0(host,"_",list$pid))

  # Check if dir exists
  exists <- dir.exists(project_backup_dir)
  if(!exists){
    dir.create(project_backup_dir)
  }

  # Date when the data are loaded
  date <- format(lubridate::ymd_hms(list$datetime),"%Y-%m-%d %H%M")

  # Backup file info
  files <- file.info(list.files(project_backup_dir, full.names = T)) |>
    tibble::rownames_to_column('file')

  # Backup with a spreadsheet
  if(list$status$success == TRUE & !is.na(list$status$success)){

    backup_file <- here::here(project_backup_dir,paste0(date,".csv"))

    message(paste0('Saving new backup file - ',backup_file))

    # Export backup
    rio::export(list$df, file = backup_file)

    backup <- TRUE
  } else {
    message(paste0('No backup file created'))
  }

  # Delete if more than 90 have accumulated
  if(nrow(files) > 90){
    oldest_file <- files |>
      arrange(.data$ctime) |>
      utils::head(1) |>
      pull(file)

    message(paste0('Deleting old backup file - ',oldest_file))

    delete_status <- file.remove(oldest_file)
  }

  return(backup)

}

