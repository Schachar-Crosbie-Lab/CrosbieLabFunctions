#### Load language ####
#### Declares language upload loading the package.
.onAttach <- function(libname, pkgname) {
  if(utils::packageVersion('CrosbieLabFunctions') != "0.6.1"){

    cli::cli_ul(paste0("This version of CrosbieLabFunctions is out of date. Please follow these instructions to re-install CrosbieLabFunctions. \n\n",
                       "1. Restart R - Go to the session tab and select Restart R\n\n",
                       "2. Use pak or devtools to reinstall the package \n\n",
                       "   devtools - {.code devtools::install_github('Schachar-Crosbie-Lab/CrosbieLabFunctions')}\n\n",
                       "   pak - {.code pak::pkg_install('Schachar-Crosbie-Lab/CrosbieLabFunctions')}"))

  } else {
    packageStartupMessage("CrosbieLabFunctions has been loaded and is up-to-date")
  }

}

#### Global Variables ####
globalVariables(c(
  # Variables created in \link(describe_df) and \link(compare_df) but are not variables needed in input data.frame
  'variable','data_type_output','dif_in_count','dif_in_mean',
  'equal','value_in','value_out',
  'gender','youth','p_respondent','cis_study_tscores',
  # send_email
  'attachment',
  # redcap_data_dictionary_functions.R global variables
  'field_type', 'form_name', 'field_name', 'value', 'variable_field_name', 'choices_calculations_or_slider_labels',
  'branching_logic_show_field_only_if', 'question_number_surveys_only', 'field_name', 'field_type', 'field_name_base',
  'select_choices_or_calculations', 'row_num', 'choice', 'comma', 'raw_value','form'))


#' @name describe_df
#'
#' @title Return descriptive statistics about all variables in a data.frame
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
#' @description Commonly in the Crosbie lab, we need to be visually inspecting the changes code makes to a dataframe. This function takes an input and an output dataframe and returns the differences between the two
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
compare_df <- function(input_df = NULL, output_df = NULL, names = c('input','output')) {

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
#' @description This function can be used to see changes between two data frames. It's intended to be used to see the changes code introduces.
#'
#' @param df_in The dataframe before the changes
#' @param df_out The dataframe after the changes
#' @param identifier_column The identifier column in the data.frame
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#'
#' @returns A data frame with changes
#'
#'

track_changes <- function(df_in = NULL, df_out = NULL, identifier_column = NULL){

  if(is.null(identifier_column)){
    stop("Unfortunately this function is designed to work with an identifier column. Please specify which column in your dataframes are the identifier column. This identifier column must match between the two projects.")
  }

  df_in_long <- df_in |>
    dplyr::mutate(dplyr::across(tidyr::all_of(dplyr::everything()), ~as.character(.x))) |>
    tidyr::pivot_longer(cols = -c(identifier_column))

  df_out_long <- df_out |>
    dplyr::mutate(dplyr::across(tidyr::all_of(dplyr::everything()), ~as.character(.x))) |>
    tidyr::pivot_longer(cols = -c(identifier_column))

  join <- dplyr::full_join(df_in_long, df_out_long,
                           by = c(identifier_column,'name'),
                           suffix = c('_in','_out')) |>
    dplyr::mutate(equal = (value_in == value_out) | (is.na(value_in) & is.na(value_out))) |>
    dplyr::filter(!equal | is.na(equal)) |>
    dplyr::mutate(change = dplyr::case_when(equal == FALSE ~ "Changed",
                                            is.na(value_in) & !is.na(value_out) ~ "Added",
                                            !is.na(value_in) & is.na(value_out) ~ "Removed"))

  return(join)
}


