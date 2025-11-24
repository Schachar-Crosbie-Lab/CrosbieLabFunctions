#### Load language ####
#### Declares language upload loading the package.
.onAttach <- function(libname, pkgname) {
  if(utils::packageVersion('CrosbieLabFunctions') != "0.7.1"){

    cli::cli_ul(paste0("This version of CrosbieLabFunctions is out of date. Please follow these instructions to re-install CrosbieLabFunctions. \n\n",
                       "1. Restart R - Go to the session tab and select Restart R\n\n",
                       "2. Use pak or devtools to reinstall the package \n\n",
                       "   devtools - {.code devtools::install_github('Schachar-Crosbie-Lab/CrosbieLabFunctions')}\n\n",
                       "   pak - {.code pak::pkg_install('Schachar-Crosbie-Lab/CrosbieLabFunctions')}"))

  } else {
    packageStartupMessage("CrosbieLabFunctions has been loaded and is up-to-date")
  }

}
#
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
  'select_choices_or_calculations', 'row_num', 'choice', 'comma', 'raw_value','form','redcap_repeat_instance','redcap_event_name',
  #expedition_tools
  'export_field_name'
  ))
