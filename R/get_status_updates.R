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
get_status_updates = function(idb, token){
  actions = get_board_actions(idb, token)
  if("data.card.closed" %in% names(actions)){
    closed.tasks = actions %>%
      filter(data.card.closed == "TRUE")
    actions = actions %>%
      filter(!(data.card.id %in% closed.tasks$data.card.id))
  }


  base = actions %>%
    filter(type == "createCard") %>%
    select(data.card.id, date, data.list.id)
  updates = actions %>%
    filter(!is.na(data.listAfter.id)) %>%
    select(data.card.id, date, data.listAfter.id) %>%
    rename(data.list.id = data.listAfter.id)
  tmp <- rbind(base, updates)
  card.name = actions %>%
    select(data.card.id, data.card.name, date) %>%
    group_by(data.card.id) %>%
    filter(!is.na(data.card.name), date == max(date)) %>%
    select(-date)
  tmp <- left_join(tmp, card.name)
  list.name = actions %>%
    select(data.list.id, data.list.name, date) %>%
    group_by(data.list.id) %>%
    filter(!is.na(data.list.name), date == max(date)) %>%
    select(-date)
  tmp <- left_join(tmp, list.name) %>%
    filter(data.list.name %in% c("To Do", "Doing", "Done"))
  tmp <- tmp %>%
    select(data.card.name, data.list.name, date) %>%
    rename(name = data.card.name, status = data.list.name,
           status_date = date)
  tmp$status_date = gsub("T", " ", tmp$status_date)
  tmp$status_date = gsub("Z", " ", tmp$status_date)
  tmp$status_date = as.POSIXct(tmp$status_date, tz = "UTC")
  return(tmp)
}
