#' @name compare_dfs
#'
#' @title Compare the the differences between two dataframes
#'
#' @description Commonly in the Crosbie lab, we need to be visually inspecting the changes code makes to a dataframe. This function takes an input and an output dataframe and returns the differences between the two
#'
#' @param input_df data.frame object before changes
#' @param output_df data.frame object after changes
#'
#'
#' @returns table with t-scores attached to raw swan values
#'
#' @export
#'
#'

input_df <-

get_swan_tscores <- function(input_df = NULL, output_df = NULL) {

  # Check that inputs are dataframes
  if(class(input_df) != 'data.frame' | class(output_df) != 'data.frame'){
    stop("The parameters must be data.frame objects.")
  }

  old_s2_sum_num <- old_s2 %>%
    summarize(across(where(is.numeric), .fns = list(zzdata_type = ~first(class(.)),
                                                    zzcount = ~sum(!is.na(.)),
                                                    zzmissing = ~sum(is.na(.)),
                                                    zzmean = ~mean(., na.rm = T),
                                                    zzmedian = ~median(., na.rm = T),
                                                    zzmax = ~max(., na.rm = T),
                                                    zzmin = ~min(., na.rm = T),
                                                    zzstdev = ~sd(., na.rm = T)))) %>%
    pivot_longer(everything(), names_sep = "_zz", names_to=c('variable', '.value'))

  old_s2_sum_else <- old_s2 %>%
    summarize(across(where(negate(is.numeric)), .fns = list(zzdata_type = ~first(class(.)),
                                                            zzcount = ~sum(!is.na(.)),
                                                            zzmissing = ~sum(is.na(.))))) %>%
    pivot_longer(everything(), names_sep = "_zz", names_to=c('variable', '.value'))

  old_s2_sum <- bind_rows(old_s2_sum_else, old_s2_sum_num)

  colnames(old_s2_sum) <- paste0(colnames(old_s2_sum),"_old")


  new_s2_sum_num <- S2_nodup %>%
    summarize(across(where(is.numeric), .fns = list(zzcount = ~sum(!is.na(.)),
                                                    zzmissing = ~sum(is.na(.)),
                                                    zzmean = ~mean(., na.rm = T),
                                                    zzmedian = ~median(., na.rm = T),
                                                    zzmax = ~max(., na.rm = T),
                                                    zzmin = ~min(., na.rm = T),
                                                    zzstdev = ~sd(., na.rm = T)))) %>%
    pivot_longer(everything(), names_sep = "_zz", names_to=c('variable', '.value'))

  new_s2_sum_else <- S2_nodup %>%
    summarize(across(where(negate(is.numeric)), .fns = list(zzdata_type = ~first(class(.)),
                                                            zzcount = ~sum(!is.na(.)),
                                                            zzmissing = ~sum(is.na(.))))) %>%
    pivot_longer(everything(), names_sep = "_zz", names_to=c('variable', '.value'))

  new_s2_sum <- bind_rows(new_s2_sum_else, new_s2_sum_num)

  colnames(new_s2_sum) <- paste0(colnames(new_s2_sum),"_new")

  compare_df_temp <- full_join(old_s2_sum, new_s2_sum, by = c('variable_old' = 'variable_new')) %>%
    mutate(dif_in_mean = round(mean_new - mean_old, digits = 2),
           dif_in_count = round(count_new - count_old, digits = 2)) %>%
    select(variable_old, contains('count'), contains('missing'), contains('mean'), contains('median'), contains('max'), contains('min'), contains('stdev'))

  compare_df <- compare_df_temp |>
    filter(is.na(count_new) | is.na(count_old) | dif_in_count != 0 | dif_in_mean != 0)

  dif_count <- compare_df %>%
    filter(dif_in_count != 0)

  mean_dif <- compare_df %>%
    #filter(mean_old != mean_new) %>%
    mutate(dif_in_mean = round(mean_new - mean_old)) %>%
    select(variable_old, contains('mean')) |>
    filter(dif_in_mean != 0)

  new_cols <- compare_df |>
    filter(is.na(count_old))

  old_cols <- compare_df |>
    filter(is.na(count_new))
}
