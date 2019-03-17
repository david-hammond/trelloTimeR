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
get_timesheet <- function(my_boards, token){
  cal <- bizdays::create.calendar("normal", weekdays = c("saturday", "sunday"))
  current = list()
  for(i in 1:nrow(my_boards)){
    idb <- get_id_board(url = my_boards$url[i], token = token)
    cards = get_board_cards(idb, token) %>%
      select( name, due, idMembers) %>%
      tidyr::unnest()
    if(nrow(cards) > 0){
      cards = add_staff(cards, "idMembers", idb, token)

      cards$due = as.Date(cards$due)

      actions = get_status_updates(idb, token)

      tmp = left_join(cards, actions)
      timespent = tmp %>%
        group_by(name) %>%
        arrange(desc(status_date)) %>%
        mutate(end = lag(status_date)) %>%
        rename(start = status_date) %>%
        ungroup() %>%
        filter(!status %in% c("To Do", "Done"))
      pos = is.na(timespent$end)
      timespent$end[pos] = as.POSIXct(lubridate::now(tzone = "UTC"),
                                      "%Y-%m-%d %H:%M:%OS")

      current[[my_boards$name[i]]] = timespent %>% filter(!is.na(status))
    }

  }
  current = bind_rows(current, .id = "project")

  current$days_spent = bizdays::bizdays(current$start, current$end,
                                        'normal')
  pos = current$days_spent <= 0
  current$days_spent[pos] = round((current$end[pos] - current$start[pos])/(24*3600),2)

  current = current %>%
    tidyr::gather(type, date, c(start, end)) %>%
    select(-c(type, days_spent, status)) %>% mutate(date = as.Date(date))

  current = padr::pad(current, by = "date", group = c("project", "name", "staff_name", "due"), interval = "day")

  current = current %>% group_by(date, staff_name) %>%
    mutate(n = length(unique(name))) %>%
    ungroup() %>%
    group_by(staff_name, project, date, name, due) %>%
    mutate(time_spent = 1/n) %>%
    distinct() %>%
    ungroup()

  return(current)
}
