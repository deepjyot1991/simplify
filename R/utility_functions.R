#' Create/update log file with the logged data
#'
#' Log file is created or updated with the latest message along with the log timestamp.
#' Enhanced for multi-threading if multiple threads are trying to log messages at the same time.
#'
#' @param message A character string representing the message to log.
#' @param file_path A character string representing the path of the log file. Do not include file extension.
#'
#' @return A logical value TRUE if the operation is successful.
#' @export
#'
#' @examples
log_messages <- function(message, file_path) {

  message_df <- tibble::tibble(Timestamp = as.character(Sys.time()), Message = message)
  # Lock the log file
  logs_lock <- filelock::lock(path = paste0(file_path, ".lock"), exclusive = TRUE, timeout = 5000)

  # Check if the log feather file exists
  if(file.exists(paste0(file_path, ".feather"))) {
    # Check if the log feather file can be read
    if("try-error" %in% class(try(expr = {existing_file <- feather::read_feather(path = paste0(file_path, ".feather"))}, silent = TRUE))) {
      file.rename(from = paste0(file_path, ".feather"), to = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S-corrupt"), ".feather"))
      new_file <- message_df
    } else {
      new_file <- rbind(message_df, existing_file)
    }
  } else {
    new_file <- message_df
  }
  # Check if the log feather file can be written
  if("try-error" %in% class(try(expr = feather::write_feather(x = new_file, path = paste0(file_path, ".feather")), silent = TRUE))) {
    feather::write_feather(x = new_file, path = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S-locked"), ".feather"))
  }
  # Check if the log csv file can be written
  if("try-error" %in% class(try(expr = data.table::fwrite(x = new_file, file = paste0(file_path, ".csv")), silent = TRUE))) {
    data.table::fwrite(x = new_file, file = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S-locked"), ".csv"))
  }

  # Release the lock on the log file
  filelock::unlock(lock = logs_lock)
  TRUE
}

