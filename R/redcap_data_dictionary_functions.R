#### Check DD Source ####
#' @name clean_redcap_dd_names
#'
#' @title Clean REDCap DD Column Names
#'
#' @description REDCap data dictionaries exported via the API return different column names compared to
#' data dictionaries manually exported from a file. This function checks which source was used to export a
#' data dictionary and harmonizes the column names.
#'
#' @importFrom janitor clean_names
#' @import dplyr
#'
#' @param dd a REDCap data dictionary
#'
#' @returns  REDCap data dictionary with corrected names
#'
#' @export

clean_redcap_dd_names <- function(dd = NULL){

  names <- colnames(janitor::clean_names(dd))

  if(any(c('variable_field_name','choices_calculations_or_slider_labels') %in% names)){
    dd_out <- dd |>
      janitor::clean_names() |>
      dplyr::rename(field_name = variable_field_name,
                    select_choices_or_calculations = choices_calculations_or_slider_labels,
                    branching_logic = branching_logic_show_field_only_if,
                    question_number = question_number_surveys_only)
  } else {
    dd_out <- dd
  }

  return(dd_out)
}

#### Expand Checkboxes ####
#' @name expand_checkboxes
#'
#' @title Expand Checkboxes
#'
#' @export
#'
#' @description REDCap data dictionaries don't match the REDCap exports specifically for checkbox questions. Checkbox questions
#' append a ___[raw_value] to each question for each answer choice.
#' This function takes a data dictionary and adds a row for each checkbox answer choice so that the dictionary matches the data frame
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#'
#' @param dd a REDCap data dictionary file
#'
#' @returns a data dictionary dataframe with a row for each checkbox option. Two additional columns are added to the data dictionary - 'field_name_base' and 'row_num'
#' The column to match the column names in the dataframe is 'field_name'
#' The column with the base name (as data dictionaries are usually exported) is 'field_name_base'
#'


expand_checkboxes <- function(dd = NULL){

  dd_in <- dd |>
    clean_redcap_dd_names() |>
    dplyr::mutate(row_num = dplyr::row_number()) |>
    dplyr::rename(field_name_base = field_name)

  check <- dd_in |>
    dplyr::filter(field_type == "checkbox")


  if(nrow(check) > 0){
    checkboxes <- check |>
      dplyr::select(field_name_base, select_choices_or_calculations, row_num) |>
      tidyr::separate_wider_delim(cols = c('select_choices_or_calculations'),
                                  delim = "|",
                                  names_sep = "",
                                  too_few = "align_start") |>
      tidyr::pivot_longer(cols = -c('field_name_base','row_num'),
                          names_to = "number",
                          values_to = "choice") |>
      dplyr::filter(!is.na(choice)) |>
      dplyr::mutate(comma = stringr::str_locate(choice, ",")[,"start"]) |>
      dplyr::mutate(raw_value = trimws(stringr::str_sub(choice, 1, comma-1))) |>
      dplyr::mutate(field_name = paste0(field_name_base,"___",raw_value)) |>
      dplyr::select(field_name, field_name_base)  |>
      dplyr::left_join(dd_in, by = "field_name_base")


    #' REDCap export data dictionary joins the raw data dictionary with all of the checkbox possibilities. This data dictionary
    #' should have one row for every possible column in the REDCap data export
    redcap_export_data_dictionary <- dplyr::anti_join(dd_in, check, by = 'field_name_base') |>
      dplyr::mutate(field_name = field_name_base) |>
      dplyr::bind_rows(checkboxes) |>
      dplyr::select(field_name, field_name_base, dplyr::everything()) |>
      dplyr::arrange(row_num)

  } else {

    # If a dataframe doesn't have any checkboxes
    # Need to create the new variable names, but don't need to expand anything
    redcap_export_data_dictionary <- dd_in |>
      dplyr::mutate(field_name = field_name_base) |>
      dplyr::select(field_name, field_name_base, dplyr::everything()) |>
      dplyr::arrange(row_num)

  }


  return(redcap_export_data_dictionary)

}


#### Clean Checkboxes ####
#' @name clean_checkboxes
#'
#' @title Clean Checkboxes
#'
#' @export
#'
#' @description
#' When data are exported from REDCap, checkboxes always export as 1 or 0 regardless of whether the question was completed.
#' This function corrects the check box data that should be missing by reading in the dataframe of interest and the REDCap data dictionary.
#' It checks for whether any questions were completed on a form. If they were, it leaves the question as is. If no questions were completed, it changes to NA.
#'
#' @param df The redcap export to clean
#' @param dd The data dictionary that corresponds to the redcap export to clean
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#'
#' @returns A redcap export with cleaned checkboxes
#'

clean_checkboxes <- function(df = NULL, dd = NULL){

  dd_in <- CrosbieLabFunctions::expand_checkboxes(dd) |>
    dplyr::filter(field_type != 'descriptive')

  # Need unique identifying column to pivot longer
  id_col = dd_in$field_name[1]

  #### Clean Checkboxes ####
  #' Checkbox data always export from REDCap as either 0 or 1, regardless of if it's missing or not
  #' A lot of the 0s should actually be NA, as the participant didn't complete the form
  #' They are assigned a reference variable from the REDCap instrument that contained the checkbox
  #' If the reference variable is missing, the checkbox is also set to missing
  checkboxes_to_fix <- dd_in |>
    dplyr::filter(field_type == "checkbox")

  unique_forms <- unique(checkboxes_to_fix$form_name)

  # Create data frame of values to fix
  fix_checkboxes <- df |>
    dplyr::select(id_col, dplyr::all_of(checkboxes_to_fix$field_name))

  fix_checkboxes_out <- fix_checkboxes

  # evaluate each row in the checkboxes to fix and make NA when the reference variable is NA
  for(i in 1:length(unique_forms)){

    var_form <- unique_forms[i]

    var_names <- checkboxes_to_fix |>
      dplyr::filter(form_name == var_form) |>
      pull(field_name)

    form_vars <- dd_in |>
      dplyr::filter(form_name == var_form) |>
      dplyr::filter(field_type %in% c('text','radio','yesno','dropdown','notes','checkbox')) |>
      dplyr::select(field_name, field_type)

    form_long <- df |>
      dplyr::select(id_col, dplyr::all_of(form_vars$field_name)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.x))) |>
      tidyr::pivot_longer(cols = -c(id_col),
                          names_to = 'field_name')  |>
      dplyr::left_join(form_vars, by = 'field_name') |>
      dplyr::mutate(value = dplyr::case_when(field_type == 'checkbox' & value == 0 ~ NA_character_,
                                             T ~ value)) |>
      dplyr::mutate(value = dplyr::na_if(trimws(value),"")) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::rename(id_col = !!id_col)

    fix_checkboxes_out <- fix_checkboxes_out |>
      #' Sometimes questions have been moved to inactive instruments
      #' When this occurs, this method for cleaning checkboxes doesn't work, so I make sure no 1's are overwritted.
      dplyr::mutate(dplyr::across(dplyr::all_of(var_names), ~dplyr::case_when(.x == 1 ~ .x,
                                                                              !id_col %in% form_long$id_col ~ NA,
                                                                              T ~ .x)))

  }

  df_out <- df |>
    dplyr::select(-dplyr::all_of(checkboxes_to_fix$field_name)) |>
    dplyr::bind_cols(fix_checkboxes_out |> dplyr::select(-id_col)) |>
    dplyr::select(dd_in$field_name, everything())

  return(df_out)
}
