#' Populate workplanr database with data
#'
#' @param staff Names of staff members
#' @param projects Names of projects
#' @param project_phases List of phases in any project in order of execution
#' @param project_roles List of project roles
#' @param out_of_office Names of staff that are going to be out of the office
#' @param public_holidays A data frame of dates of public holidays
#' @param roles_responsibilities Data frame for responsibilites of roles across project phases
#' @param time_estimates Time estimates of how long each phase will take in relation
#' @param staff_name_for_unassigned_work Dummy staff member to assign work to
#' @export
#'
add_staff <- function(df, by, board, token){
  tmp  <- get_board_members(board, token) %>%
    dplyr::select(id, fullName)
  names(tmp) = c(by, "staff_name")
  df <- df %>% dplyr::left_join(tmp)
  df <- df %>% select(-by)
  return(df)
}
