
#' @name describe_df
#'
#' @title Return descriptive statistics about all variables in a data.frame
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' This function returns data type, count, number missing, mean, median, min, max, and standard deviation for all numberic columns in a dataframe.
#' It returns data type, count, and missing for any non-numeric columns in a dataframe.
#'
#'
#' @param df a data.frame object
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom purrr negate
#'
#' @returns a data.frame with descriptive statistics
#'
#' @export
#'
#'
#'
#df <- data.frame(num = c(1:20))

describe_df <- function(df = NULL){

  #### Format Input Data Frame ####
  # Return order of variables for rearranging
  df_order <- data.frame(variable = colnames(df),
                            order = 1:length(colnames(df)))

  has_nums <- any(sapply(df, class) %in% c('numeric','integer'))
  has_other <- any(sapply(df, class) != 'numeric' & sapply(df, class) != 'integer')

  if(has_nums){
    # Get summary of numeric data
    df_num <- df |>
      dplyr::summarize(dplyr::across(dplyr::where(is.numeric), .fns =
                                       list(zzdata_type = ~dplyr::first(class(.)),
                                            zzcount = ~sum(!is.na(.)),
                                            zzmissing = ~sum(is.na(.)),
                                            zzmean = ~mean(., na.rm = T),
                                            zzmedian = ~median(., na.rm = T),
                                            zzmax = ~max(., na.rm = T),
                                            zzmin = ~min(., na.rm = T),
                                            zzstdev = ~sd(., na.rm = T)))) |>
      tidyr::pivot_longer(dplyr::everything(), names_sep = "_zz", names_to=c('variable', '.value'))
  }

  if(has_other){

    # Get summary of non-numeric data
    df_nonnum <- df |>
      dplyr::summarize(dplyr::across(dplyr::where(purrr::negate(is.numeric)),
                                     .fns = list(zzdata_type = ~ dplyr::first(class(.)),
                                                 zzcount = ~sum(!is.na(.)),
                                                 zzmissing = ~sum(is.na(.))))) |>
      tidyr::pivot_longer(dplyr::everything(), names_sep = "_zz", names_to=c('variable', '.value'))
  }

  if(has_nums & has_other){
    # Join into single data.frame
    df_sum <- dplyr::bind_rows(df_nonnum, df_num) |>
      dplyr::left_join(df_order, by = c('variable')) |>
      dplyr::arrange(order)
  } else if(has_nums){
    df_sum <- df_num
  } else {
    df_sum <- df_nonnum
  }


  return(df_sum)
}

#' @name compare_dfs
#'
#' @title Compare the the differences between two dataframes
#'
#' @description
#' `r lifecycle::badge('experimental')` \cr
#' Commonly in the Crosbie lab, we need to be visually inspecting the changes code makes to a dataframe. This function takes an input and an output dataframe and returns the differences between the two
#'
#' @param input_df data.frame object before changes
#' @param output_df data.frame object after changes
#' @param names suffix to attach to the output data.frame. Defaults to 'input' and 'output'
#'
#' @import dplyr
#' @importFrom methods is
#'
#' @returns table with t-scores attached to raw swan values
#'
#' @export
#'
#'
#'
#### Developing Functions ####
compare_dfs <- function(input_df = NULL, output_df = NULL, names = c('input','output')) {

  # Check that inputs are dataframes
  if(!(methods::is(input_df, 'data.frame') & methods::is(output_df, 'data.frame'))){
    stop("The parameters must be data.frame objects.")
  }

  # Get descriptives of input df
  input_df_sum <- CrosbieLabFunctions::describe_df(input_df) |>
    dplyr::rename_with(
      ~ paste0(.x,"_",names[1])
    ) |>
    dplyr::rename(variable = paste0('variable_',names[1]))


  # Get descriptives of output df
  output_df_sum <- CrosbieLabFunctions::describe_df(output_df) |>
    dplyr::rename_with(
      ~ paste0(.x,"_",names[2])
    ) |>
    dplyr::rename(variable = paste0('variable_',names[2]))


  # Create summary df of differences
  compare_df <- dplyr::full_join(input_df_sum, output_df_sum, by = c('variable')) |>
    dplyr::mutate(dif_in_mean = round(get(paste0('mean_',names[2])) - get(paste0('mean_',names[1])), digits = 2),
                  dif_in_count = round(get(paste0('count_',names[2])) - get(paste0('count_',names[1])), digits = 2)) |>
    dplyr::select(variable, dplyr::contains('data_type'),
                  dplyr::contains('count'), dplyr::contains('missing'), dplyr::contains('mean'),
                  dplyr::contains('median'), dplyr::contains('max'), dplyr::contains('min'), dplyr::contains('stdev')) |>
    dplyr::mutate(different =
                    is.na(get(paste0('count_',names[2]))) | is.na(get(paste0('count_',names[1])))
                  | dif_in_count != 0 | dif_in_mean != 0 |
                    get(paste0('data_type_',names[1])) != get(paste0('data_type_',names[2])))


  return(compare_df)
}

#' @name compare_df
#'
#' @title Compare the the differences between two dataframes
#'
#' @description
#' `r lifecycle::badge("deprecated")` \cr
#' Commonly in the Crosbie lab, we need to be visually inspecting the changes code makes to a dataframe. This function takes an input and an output dataframe and returns the differences between the two
#'
#' @param input_df data.frame object before changes
#' @param output_df data.frame object after changes
#' @param names suffix to attach to the output data.frame. Defaults to 'input' and 'output'
#'
#' @import dplyr
#' @importFrom methods is
#' @importFrom lifecycle deprecate_warn
#'
#' @returns table with t-scores attached to raw swan values
#'
#' @export
#'
#'
#'
#### Developing Functions ####
compare_df <- function(input_df = NULL, output_df = NULL, names = c('input','output')) {

  lifecycle::deprecate_warn(
    when = '0.6.3',
    what = 'compare_df()',
    with = 'compare_dfs()',
    details = "Please use `compare_dfs()` instead.")

  # Check that inputs are dataframes
  if(!(methods::is(input_df, 'data.frame') & methods::is(output_df, 'data.frame'))){
    stop("The parameters must be data.frame objects.")
  }

  # Get descriptives of input df
  input_df_sum <- CrosbieLabFunctions::describe_df(input_df) |>
    dplyr::rename_with(
      ~ paste0(.x,"_",names[1])
    ) |>
    dplyr::rename(variable = paste0('variable_',names[1]))


  # Get descriptives of output df
  output_df_sum <- CrosbieLabFunctions::describe_df(output_df) |>
    dplyr::rename_with(
      ~ paste0(.x,"_",names[2])
    ) |>
    dplyr::rename(variable = paste0('variable_',names[2]))


  # Create summary df of differences
  compare_df <- dplyr::full_join(input_df_sum, output_df_sum, by = c('variable')) |>
    dplyr::mutate(dif_in_mean = round(get(paste0('mean_',names[2])) - get(paste0('mean_',names[1])), digits = 2),
                  dif_in_count = round(get(paste0('count_',names[2])) - get(paste0('count_',names[1])), digits = 2)) |>
    dplyr::select(variable, dplyr::contains('data_type'),
                  dplyr::contains('count'), dplyr::contains('missing'), dplyr::contains('mean'),
                  dplyr::contains('median'), dplyr::contains('max'), dplyr::contains('min'), dplyr::contains('stdev')) |>
    dplyr::mutate(different =
                    is.na(get(paste0('count_',names[2]))) | is.na(get(paste0('count_',names[1])))
                  | dif_in_count != 0 | dif_in_mean != 0 |
                    get(paste0('data_type_',names[1])) != get(paste0('data_type_',names[2])))


  return(compare_df)
}

#### Track Changes ####
#' @name track_changes
#'
#' @title Track Changes
#'
#' @export
#'
#'
#' @description \cr
#' `r lifecycle::badge('experimental')`
#' This function can be used to see changes between two data frames. It's intended to be used to see the changes code introduces.
#'
#' @section Development Notes:
#' 2025-09-12: Added naming functionality
#' 2025-09-24: Added multiple identifier columns functionality
#'
#' @param df_in The dataframe before the changes
#' @param df_out The dataframe after the changes
#' @param identifier_columns The identifier columns in the data.frame
#' @param names A length of two vector for renaming the 'values' columns. Pass c('df1', 'df2') to names, where df1 = df_in and df_out = df2 to rename the output columns to match the comparison dataframes
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @returns A data frame with changes
#'

track_changes <- function(df_in = NULL, df_out = NULL, identifier_columns = NULL, names = NULL){

  if(is.null(identifier_columns)){
    stop("Unfortunately this function is designed to work with an identifier column. Please specify which column in your dataframes are the identifier column. This identifier column must match between the two projects.")
  }

  not_present_in <- identifier_columns[!identifier_columns %in% colnames(df_in)]
  not_present_out <- identifier_columns[!identifier_columns %in% colnames(df_out)]

  # Error messages if identifier columns are unusable
  if(length(not_present_in) > 0 | length(not_present_out) > 0){

    if(any((length(not_present_in) > 0 & length(not_present_out) == 0),
           (length(not_present_out) > 0 & length(not_present_in) == 0),
           (not_present_in != not_present_out))){
      stop(paste0("The identifier columns do not match between df_in and df_out.\n\n",
                  "Columns not present in df_in: ",not_present_in,"\n",
                  "Columns not present in df_out: ",not_present_out))
    }

    warning(paste0("Changes will still be tracked, but the following identifier columns were not found in the data. Ideally, all identifier columns are available in the data.\n\n",
                   "Identifier columns not present: ",not_present_in))
  }

  df_in_long <- df_in |>
    dplyr::mutate(dplyr::across(dplyr::all_of(dplyr::everything()), ~as.character(.x))) |>
    tidyr::pivot_longer(cols = -c(any_of(identifier_columns)))

  df_out_long <- df_out |>
    dplyr::mutate(dplyr::across(dplyr::all_of(dplyr::everything()), ~as.character(.x))) |>
    tidyr::pivot_longer(cols = -c(any_of(identifier_columns)))

  join <- dplyr::full_join(df_in_long, df_out_long,
                           by = c(identifier_columns,'name'),
                           suffix = c('_in','_out'))  |>
    dplyr::mutate(equal = (.data$value_in == .data$value_out) | (is.na(.data$value_in) & is.na(.data$value_out))) |>
    dplyr::filter(!.data$equal | is.na(.data$equal)) |>
    dplyr::mutate(change = dplyr::case_when(.data$equal == FALSE ~ "Changed",
                                            is.na(.data$value_in) & !is.na(.data$value_out) ~ "Added",
                                            !is.na(.data$value_in) & is.na(.data$value_out) ~ "Removed"))

  if(!is.null(names)){

    join_out <- join |>
      dplyr::rename_with(~c(names[1], names[2]), c(.data$value_in, .data$value_out))
  } else {
    join_out <- join
  }

  return(join_out)
}

#### UAT ####
#' @name uat_redcap_data
#'
#' @title User Acceptance Testing for REDCap Data
#'
#' @export
#'
#' @description
#' `r lifecycle::badge('experimental')` \cr
#' Data being uploaded into Expedition go through User Acceptance Testing. This involves ensuring that the data within Expedition are as expected.
#' This function is specifically designed for User Acceptance Testing \strong{REDCap} data. It allows you to compare data exported from REDCap with data exported from Expedition.\cr \cr
#' \emph{\strong{Instructions}}\cr
#' \enumerate{
#'  \item Export raw data from REDCap
#'  \item Export data from Expedition
#'  \item Use this function to compare the two tables
#' }
#'
#' @param redcap_file_path The pathway to the downloaded REDCap data, as a .csv or .xlsx file
#' @param expedition_file_path The pathway to the downloaded Expedition data, as a .csv file. \cr
#' \itemize{
#'  \item Note: Expedition limits downloads to 100,000 rows}
#'
#' @param redcap_df A dataframe with REDCap data. Use redcap_df instead of redcap_file_path if you are importing REDCap data in a different way, i.e. API.
#' @param expedition_df A dataframe with Expedition data. Use expedition_df instead of expedition_file_path if you are importing Expedition data in a different way, i.e. API.
#'
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom utils read.csv
#' @import cli


uat_redcap_data <- function(redcap_file_path = NULL, expedition_file_path = NULL,
                            redcap_df = NULL, expedition_df = NULL){

  cli::cli_progress_bar("User Acceptance Testing", total = 5)

  # Read and clean data
  if(!is.null(redcap_file_path)){
    redcap_df <- utils::read.csv(redcap_file_path) |>
      clean_export()
  } else if(!is.null(redcap_df)){
    redcap_df <- clean_export(redcap_df)
  } else {
    stop("No REDCap data provided. Please provide a redcap_file_path or a redcap_df.")
  }

  cli::cli_progress_update()
  if(!is.null(expedition_file_path)){
    expedition_df <- utils::read.csv(expedition_file_path) |>
      clean_export()
  } else if(!is.null(expedition_df)){
    expedition_df <- clean_export(expedition_df)
  } else {
    stop("No Expedition data provided. Please provide an expedition_file_path or expedition_df")
  }



  cli::cli_progress_update()
  # Get ID col
  id_col = colnames(redcap_df)[1]
  if(!id_col %in% colnames(expedition_df)){
    stop(paste0('Error: The id column in REDCap (',id_col,') is not in the Expedition data.'))
  }

  potential_id_cols <- c(id_col,'redcap_repeat_instance','redcap_repeat_instrument','redcap_event_name')
  actual_id_cols <- potential_id_cols[which(potential_id_cols %in% colnames(redcap_df))]

  # Compare
  cli::cli_progress_update()
  compare <- compare_df(input_df = redcap_df, output_df = expedition_df,
                         names = c('redcap','expedition'))

  broad_difs <- compare |>
    filter(.data$different)

  # Track Changes
  cli::cli_progress_update()
  changes <- track_changes(df_in = redcap_df, df_out = expedition_df,
                           identifier_columns = actual_id_cols,
                           names = c('redcap','expedition'))

  cli::cli_progress_done()

  #### Output ####
  cli::cli_h1("Results")
  cli::cli_h2("Column Changes")
  cli::cli_ul(broad_difs |>
                group_by(.data$change) |>
                count() |>
                mutate(join = paste0(.data$change, ": ",.data$n)) |>
                pull(.data$join))
  cli::cli_h2('Individual Data Changes')
  cli::cli_ul(changes |>
                group_by(.data$change) |>
                count() |>
                mutate(join = paste0("Values ",.data$change, ": ",.data$n)) |>
                pull(.data$join))
  cli::cli_h1("Recommendations")
  if(nrow(broad_difs) ==  0 & nrow(changes) == 0){
    cli::cli_alert_success('No differences between data. User acceptance testing successful.')
  } else {
    cli::cli_alert_danger('User acceptance testing found differences between REDCap and Expedition.')
    cli::cli_verbatim("\n")
    cli::cli_text('Use the returned list to manually determine if the differences are expected. Broad column-based differences are found in the column_changes dataframe and individual data changes are found in the individual_changes data frame.')
  }

  return(list(column_changes = broad_difs, individual_changes = changes))

}

#### Clean Expedition Export ####
#' @name clean_export
#'
#' @title Clean Export
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Data exported from Expedition have standardized SQL notations. Data from REDCap also have standaradized structures.
#' This function cleans for comparison with other data.
#'
#' @param df A dataframe to be cleaned
#'
#' @export
#'
clean_export <- function(df = NULL){

  values_to_clean <- c('NULL','')

  df_out <- df |>
    mutate(across(where(is.character), ~case_when(.x %in% values_to_clean ~ NA,
                                                  T ~ .x))) |>
    mutate(across(everything(), ~trimws(.x))) |>
    mutate(across(everything(), ~type.convert(.x, as.is = TRUE, numerals = 'no.loss')))

  return(df_out)

}

