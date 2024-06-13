#' Fast md5 check
#'
#' md5 values are computed based on combination of file path, file size and file modified time.
#'
#' @param path_files a character vector containing absolute file paths.
#'
#' @return a character vector containing md5 values.
#' @export
#'
#' @examples
#' custom_md5_hash(path_files = list.files(full.names = TRUE))
custom_md5_hash <- function(path_files) {

  path_files <- gsub(pattern = "\\\\", replacement = "/", x = path_files)

  size_files <- sapply(path_files, file.size)
  mtime_files <- sapply(path_files, file.mtime)

  info_files <- paste0(gsub(pattern = ".*/(.*)$", replacement = "\\1", x = path_files), size_files, mtime_files)

  as.character(sapply(info_files, digest::digest, algo="md5"))
}

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

#' Update/create a file with the given data at a given path
#'
#' Updates data in a file based on the given path. Backs up the old file if the file with the same name is already present.
#' Appends the file name with the timestamp for the back up files.
#'
#' @param data A data.frame, tibble or data.table.
#' @param file_path A character string representing the path of the output file. Do not include file extension.
#' @param max_limit A numeric value representing the number of most recent back up files to keep. Default is 1.
#' @param csv_backup A logical value to denote whether you want additional csv backup.
#'
#' @return A logical value TRUE if the operation is successful.
#' @export
#'
#' @examples
update_output_file <- function(data, file_path, max_limit = 0, csv_backup = FALSE) {
  file_path <- gsub(pattern = "(\\/)*$", replacement = "", x = file_path)
  dir.create(path = gsub(pattern = "([^/]*)$", replacement = "", x = file_path), recursive = TRUE, showWarnings = FALSE)

  if(file.exists(paste0(file_path, ".feather"))) {
    if(file.rename(from = paste0(file_path, ".feather"), to = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S"), ".feather"))) {
      feather::write_feather(x = data, path = paste0(file_path, ".feather"))
    } else {
      feather::write_feather(x = data, path = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S-locked"), ".feather"))
    }
  } else {
    feather::write_feather(x = data, path = paste0(file_path, ".feather"))
  }

  if(csv_backup) {
    if(file.exists(paste0(file_path, ".csv"))) {
      if(file.rename(from = paste0(file_path, ".csv"), to = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S"), ".csv"))) {
        data.table::fwrite(x = data, file = paste0(file_path, ".csv"), row.names = FALSE)
      } else {
        data.table::fwrite(x = data, file = paste0(file_path, format(x = Sys.time(), format = "-%Y-%m-%d-%H%M%S-locked"), ".csv"), row.names = FALSE)
      }
    } else {
      data.table::fwrite(x = data, file = paste0(file_path, ".csv"), row.names = FALSE)
    }
  }


  folder_path <- gsub(pattern = "^(.*)/.+$", replacement = "\\1", x = file_path)

  file_name <- gsub(pattern = ".*/(.*)$", replacement = "\\1", x = file_path)


  if(!csv_backup) {
    csv_file_path <- sort(list.files(path = folder_path, pattern = paste0("^", file_name, "\\.csv"), full.names = TRUE))

    if(length(csv_file_path) > 0) {
      file.remove(csv_file_path)
    }
  }

  all_extra_csv_files <- sort(list.files(path = folder_path, pattern = paste0("^", file_name, "-.+\\.csv"), full.names = TRUE))

  if(length(all_extra_csv_files) > 0) {
    files_to_remove <- all_extra_csv_files[1:(length(all_extra_csv_files)-0)]
    file.remove(files_to_remove)
  }


  all_extra_feather_files <- sort(list.files(path = folder_path, pattern = paste0("^", file_name, "-.+\\.feather"), full.names = TRUE))

  if(length(all_extra_feather_files) > max_limit) {
    files_to_remove <- all_extra_feather_files[1:(length(all_extra_feather_files)-max_limit)]
    file.remove(files_to_remove)
  }

  TRUE
}

#' Append multiple files in a given folder into one
#'
#' Appends all the files in a folder. Files have to be either csv, xlsx or txt. Note that certain file types are ignored such as those end with .trc, .xlsm, .log and those containing ~$.
#' custom_md5_hash from package "simplify" is used to generate md5 hash.
#'
#' @param folder_path A character string representing an absolute folder path.
#' @param sheet A character string representing name of the excel sheet.
#' @param mode A character string with either "strict" or "lenient". strict mode reprocesses all the files when there is a file change while lenient mode only reprocesses the modified files.
#' @param key_cols A charater vector representing columns identifying distinctive feature value between data files.
#'
#' @return A data.table representing the combined data of all the files within that given folder.
#' @export
#'
#' @examples
file_append <- function(folder_path, sheet = NULL, mode = "strict", key_cols = NULL) {

  if(FALSE %in% mode %in% c("strict", "lenient") | !methods::is(mode, "character") | length(mode) != 1) {
    stop("mode argument needs to be either strict or lenient")
  }

  if(!is.null(key_cols)) {
    mode <- "lenient"
  }

  output_file <- gsub(pattern = "^(.*)/$", replacement = "\\1", x = folder_path)

  # file_list <- grep(pattern = "[^\\.log]$", x = grep(pattern = "^[^\\~\\$].*", x = list.files(path = folder_path), value = TRUE), value = TRUE)
  all_files <- list.files(path = folder_path)
  file_list <- all_files[!all_files %in% grep(pattern = "\\.trc$|\\.xlsm$|\\.log$|\\~\\$.*", x = all_files, value = TRUE)]

  files_md5 <- simplify::custom_md5_hash(path_files = paste0(output_file, "/", file_list))

  if(file.exists(paste0(output_file, "_md5.csv"))) {
    read_md5 <- data.table::fread(file = paste0(output_file, "_md5.csv"))

    if(mode == "strict") {
      if(FALSE %in% (read_md5$x %in% files_md5)) {
        reset_flag <- TRUE
      } else {
        reset_flag <- FALSE
      }
    } else if(mode == "lenient") {
      reset_flag <- FALSE
    }

    if(FALSE %in% (files_md5 %in% read_md5$x)) {
      update_flag <- TRUE
    } else {
      update_flag <- FALSE
    }
  } else {
    read_md5 <- data.frame(x = character())
    update_flag <- FALSE
    reset_flag <- TRUE
  }

  data_exists <- FALSE

  if(!reset_flag) {
    if(file.exists(paste0(output_file, "_merged.feather"))) {

      # Read the csv file in case reading feather results in error such as duplicate column names are present
      if("try-error" %in% class(try(expr = {
        existing_data <- feather::read_feather(path = paste0(output_file, "_merged.feather"))
        data.table::setDT(existing_data)
        data_exists <- TRUE
      }, silent = TRUE))) {
        reset_flag <- TRUE
      }
    } else {
      reset_flag <- TRUE
    }
  }


  if(reset_flag | update_flag) {
    first_loop <- TRUE
    for(i in 1:length(file_list)) {
      if((!files_md5[i] %in% read_md5$x) | reset_flag) {
        if(grepl(pattern = "\\.xlsx$", x = file_list[i])) {
          read_file <- tibble::as_tibble(readxl::read_excel(path = paste0(folder_path, "/", file_list[i]), sheet = sheet, col_types = "text", .name_repair = "minimal"), .name_repair = "minimal")

          #If the file is in the SAP BO raw format
          if(!is.na(read_file[1,1])) {
            if(read_file[1,1] == "Business Reporting and Analytics") {
              read_file <- tibble::as_tibble(readxl::read_excel(path = paste0(folder_path, "/", file_list[i]), sheet = sheet, col_types = "text", .name_repair = "minimal", skip = 14), .name_repair = "minimal")

              read_file <- read_file[,-c(1:3)]
            }
          }

          if(colnames(read_file)[1] == "" | is.na(colnames(read_file)[1])) {
            read_file[1,][is.na(read_file[1,])] <- ""
            colnames(read_file) <- gsub(pattern = "^Snapshot Calender.*", replacement = "", x = colnames(read_file), ignore.case = TRUE)

            col_names <- paste(colnames(read_file), read_file[1,])
            for(j in 1:length(col_names)) {
              read_file[1,j] <- col_names[j]
            }
            names(read_file) <- gsub(pattern = "^\\s*(.*)", replacement = "\\1", x = read_file[1,])
            read_file <- read_file[-1,]
          }
        } else if(grepl(pattern = "\\.csv$", x = file_list[i])) {
          read_file <- tibble::as_tibble(readr::read_csv(file = paste0(folder_path, "/", file_list[i]), col_types = readr::cols(.default = readr::col_character())), .name_repair = "minimal")
        } else if(grepl(pattern = "\\.txt$", x = file_list[i])) {
          read_file <- tibble::as_tibble(readLines(con = paste0(folder_path, "/", file_list[i]), n = 1), stringsAsFactors = FALSE, .name_repair = "minimal")
          if(grepl(pattern = "\t", x = read_file[1,1])) {
            read_file <- tibble::as_tibble(utils::read.delim(file = paste0(folder_path, "/", file_list[i]), stringsAsFactors = FALSE), .name_repair = "minimal")
          } else {
            read_file <- tibble::as_tibble(readLines(con = paste0(folder_path, "/", file_list[i]), n = -1, warn = FALSE), stringsAsFactors = FALSE, .name_repair = "minimal")
            colnames(read_file) <- "Lines"
          }
        } else if(grepl(pattern = "\\.json$", x = file_list[i])) {
          read_file <- jsonlite::fromJSON(txt = paste0(folder_path, "/", file_list[i]))
        } else if(grepl(pattern = "\\.feather$", x = file_list[i])) {
          read_file <- feather::read_feather(path = paste0(folder_path, "/", file_list[i]))
        }

        if(nrow(read_file) == 0) {
          next
        }

        #Set as data.table
        data.table::setDT(read_file)

        #Rename blank column names
        col_rename <- which(colnames(read_file) == "")
        if(length(col_rename)) {
          colnames(read_file)[col_rename] <- paste0(colnames(read_file)[col_rename-1], " name")
        }

        if(first_loop) {
          data_final <- read_file
          first_loop <- FALSE
        } else {
          cols_intersect <- intersect(colnames(data_final), colnames(read_file))

          if(length(colnames(data_final)) == length(cols_intersect) & length(colnames(read_file)) == length(cols_intersect)) {
            data_final <- data_final[, cols_intersect, with = FALSE]
            read_file <- read_file[, cols_intersect, with = FALSE]
            data_final <- rbind(read_file, data_final)
          } else {
            max_rows <- nrow(data_final) + nrow(read_file)
            data_final <- merge(x = data_final, y = read_file, by = cols_intersect, all = TRUE)
            if(nrow(data_final) > max_rows) {
              stop("Rows in merged data are more than sum of rows in individual files")
            }
          }
        }
      }
    }

    if(!reset_flag & data_exists & !is.null(key_cols)) {
      new_data <- unique(data_final[, key_cols, with = FALSE])[, dummy_col := T]
      existing_data <- merge(x = existing_data, y = new_data, all.x = T)[is.na(`dummy_col`)][, `dummy_col` := NULL]
    }

    if(data_exists) {
      cols_intersect <- intersect(colnames(data_final), colnames(existing_data))

      if(length(colnames(data_final)) == length(cols_intersect) & length(colnames(existing_data)) == length(cols_intersect)) {
        data_final <- data_final[, cols_intersect, with = FALSE]
        existing_data <- existing_data[, cols_intersect, with = FALSE]
        data_final <- rbind(existing_data, data_final)
      } else {
        max_rows <- nrow(data_final) + nrow(existing_data)
        data_final <- merge(x = data_final, y = existing_data, by = cols_intersect, all = TRUE)
        if(nrow(data_final) > max_rows) {
          stop("Rows in merged data are more than sum of rows in individual files")
        }
      }
    }

    feather::write_feather(x = data_final, path = paste0(output_file, "_merged.feather"))
  } else {
    data_final <- existing_data
  }

  data_final
}

#' Compare given md5 values with the one present in _md5.csv file
#'
#' Return TRUE if provided md5 values matches with the one in the file.
#'
#' @param md5_data A character vector representing md5 values.
#' @param file_path A character vector representing md5 file path.
#'
#' @return FALSE if md5 values match, TRUE otherwise.
#' @export
#'
#' @examples
compare_md5_change <- function(md5_data, file_path) {

  output_file <- gsub(pattern = "^(.*)/$", replacement = "\\1", x = file_path)

  if(file.exists(paste0(output_file, "_md5.csv"))) {
    read_md5 <- data.table::fread(file = paste0(output_file, "_md5.csv"))

    if(identical(sort(md5_data), sort(read_md5$x))) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Generate md5 hash of the files in a given folder
#'
#' Generates the md5 hash of the files in a given folder. Note that certain file types are ignored such as those end with .trc, .xlsm, .log and those containing ~$.
#' custom_md5_hash from package "simplify" is used to generate md5 hash.
#'
#' @param folder_path A character string representing an absolute folder path.
#'
#' @return A character vector with md5 hash values of all the files within a given folder.
#' @export
#'
#' @examples
#' read_md5_folder(folder_path = getwd())
read_md5_folder <- function(folder_path) {

  output_file <- gsub(pattern = "^(.*)/$", replacement = "\\1", x = folder_path)

  all_files <- list.files(path = folder_path)
  file_list <- all_files[!all_files %in% grep(pattern = "\\.trc$|\\.xlsm$|\\.log$|\\~\\$.*", x = all_files, value = TRUE)]

  files_md5 <- simplify::custom_md5_hash(path_files = paste0(output_file, "/", file_list))

  files_md5
}

#' Update the md5 hash once all the files in a given folder have processed
#'
#' A csv file with the same name as the folder name is created/updated with new md5 values. This file with md5 hash values and the given folder has same parent directory.
#'
#' @param md5_data A character vector representing md5 hash values of the files within the given folder.
#' @param folder_path A character string representing an absolute folder path.
#'
#' @return A logical value TRUE if the operation is successful.
#' @export
#'
#' @examples
update_md5_file <- function(md5_data, folder_path) {

  dir.create(path = gsub(pattern = "([^/]*)$", replacement = "", x = folder_path), recursive = TRUE, showWarnings = FALSE)

  output_file <- gsub(pattern = "^(.*)/$", replacement = "\\1", x = folder_path)

  data.table::fwrite(data.table::data.table(x = md5_data), paste0(output_file, "_md5.csv"))

  TRUE
}

#' Update the folder md5 hash once all the files in a given folder have processed
#'
#' A csv file with the same name as the folder name is created/updated with new md5 values. This file with md5 hash values and the given folder has same parent directory.
#'
#' @param md5_data A character vector representing md5 hash values of the folder.
#' @param folder_path A character string representing an absolute folder path.
#'
#' @return A logical value TRUE if the operation is successful.
#' @export
#'
#' @examples
update_md5_folder <- function(md5_data, folder_path) {

  dir.create(path = gsub(pattern = "([^/]*)$", replacement = "", x = folder_path), recursive = TRUE, showWarnings = FALSE)

  output_file <- gsub(pattern = "^(.*)/$", replacement = "\\1", x = folder_path)

  data.table::fwrite(data.table::data.table(x = md5_data), paste0(output_file, "_fldr_md5.csv"))

  TRUE
}

#' Fill missing values in a data.frame or a data.table by reference
#'
#' Intelligently fill missing values in a data.frame or a data.table in memory.
#' If column names are not provided, missing values will be filled in for all the columns where data type of the column matches with the val argument's data type.
#' Use with caution. Since data is modified in memory, no data is returned. Hence no need to specify assignment operator '<-'.
#'
#' @param DT A data.table or a data.frame. A data.frame will be converted to a data.table.
#' @param column_names A character vector of column names to fill missing values. If NULL which is default, missing values will be filled in for all the columns where data type of the column matches with the val argument's data type.
#' @param val Value to replace the missing values. Default is 0.
#'
#' @return A data.table with missing values replace with val.
#' @export
#' @importFrom data.table :=
#' @importFrom data.table .SD
#'
#' @examples
#' dt_sample <- data.table::data.table(Col_1 = c("a", "b", NA), Col_2 = c(2, NA, 5))
#' dt_fill_NAs(DT = dt_sample, val = 0)
dt_fill_NAs <- function(DT, column_names = NULL, val = 0) {

  if(!"data.table" %in% class(DT)) {
    data.table::setDT(DT)
  }

  if(methods::is(val, "numeric")) {
    integer_cols <- colnames(DT)[sapply(DT, class) %in% "integer"]
    if(length(integer_cols)) {
      DT[ , (integer_cols) := lapply(.SD, as.numeric), .SDcols = integer_cols]
    }
  }

  if(is.null(column_names)) {
    colnums <- which(x = as.vector(sapply(DT, class)) == class(val))

    # or by number (slightly faster than by name) :
    for (j in colnums)
      data.table::set(DT, which(is.na(DT[[j]])), j, val)
  } else {
    colnums <- colnames(DT)[colnames(DT) %in% column_names]

    class_mismatch <- colnames(DT[,colnums, with = FALSE])[as.vector(sapply(DT[,colnums, with = FALSE], class)) != class(val)]

    if(length(class_mismatch)) {
      if(methods::is(val, "numeric")) {
        DT[ , (class_mismatch) := lapply(.SD, as.character), .SDcols = class_mismatch]
        DT[ , (class_mismatch) := lapply(.SD, as.numeric), .SDcols = class_mismatch]
      }
      if(methods::is(val, "character")) {
        DT[ , (class_mismatch) := lapply(.SD, as.character), .SDcols = class_mismatch]
      }
      if(methods::is(val, "Date")) {
        DT[ , (class_mismatch) := lapply(.SD, as.Date), .SDcols = class_mismatch]
      }
    }

    for (j in colnums)
      data.table::set(DT, which(is.na(DT[[j]])), j, val)
  }

}

#' Calculate DCF Value
#'
#' @param fcf A numeric vector containing Free Cash Flow.
#' @param growth_rate A numeric vector containing Growth Rate.
#' @param growth_years A numeric vector containing Years of Growth. Default 10.
#' @param terminal_rate A numeric vector containing Terminal Rate. Default 0.03.
#' @param discount_rate A numeric vector containing Discount Rate. Default 0.15.
#' @param shares A numeric vector containing number of Outstanding Shares. Default 1.
#' @param excess_cash A numeric vector containing Excess Cash.
#'
#' @return A numeric vector with DCF Value.
#' @export
#'
#' @examples
#' dcf_valuation(fcf = 1000, growth_rate = 0.10)
dcf_valuation <- function(fcf, growth_rate, growth_years = 10, terminal_rate = 0.03, discount_rate = 0.15, shares = 1, excess_cash = 0) {

  data_all <- data.table::data.table(`Current FCF` = fcf, `Growth Rate` = growth_rate, `Growth Years` = growth_years,
                                     `Terminal Rate` = terminal_rate, `Discount Rate` = discount_rate,
                                     `Share Count` = shares, `Excess Cash` = excess_cash)

  data_all[`Growth Rate` == `Discount Rate`,
           `DCF Valuation` := (((`Current FCF`*`Growth Years`) +
                                 ((`Current FCF`*((1+`Growth Rate`)^`Growth Years`)*(1+`Terminal Rate`)/(`Discount Rate`-`Terminal Rate`))/((1+`Discount Rate`)^`Growth Years`))) + `Excess Cash`)/`Share Count`]

  data_all[`Growth Rate` != `Discount Rate`,
           `DCF Valuation` := (((`Current FCF`*(1+`Growth Rate`)*(1-((1+`Growth Rate`)^`Growth Years`)*(1+`Discount Rate`)^-`Growth Years`)/(`Discount Rate`-`Growth Rate`)) +
                                 ((`Current FCF`*((1+`Growth Rate`)^`Growth Years`)*(1+`Terminal Rate`)/(`Discount Rate`-`Terminal Rate`))/((1+`Discount Rate`)^`Growth Years`))) + `Excess Cash`)/`Share Count`]
  return(data_all$`DCF Valuation`)
}

#' Calculate Expected Return from investment
#'
#' @param fcf A numeric vector containing Free Cash Flow.
#' @param growth_rate A numeric vector containing Growth Rate.
#' @param growth_years A numeric vector containing Years of Growth. Default 10.
#' @param terminal_rate A numeric vector containing Terminal Rate. Default 0.03.
#' @param discount_rate A numeric vector containing Discount Rate. Default 0.15.
#' @param shares A numeric vector containing number of Outstanding Shares.
#' @param price A numeric vector containing current share price.
#' @param excess_cash A numeric vector containing Excess Cash.
#'
#' @return A numeric vector with Expected Return.
#' @export
#'
#' @examples
#' expected_return(fcf = 100000000, growth_rate = 0.10, shares = 100000, price = 100)
expected_return <- function(fcf, growth_rate, growth_years = 10, terminal_rate = 0.03, discount_rate = 0.15, shares, price, excess_cash = 0) {

  data_all <- data.table::data.table(`Current FCF` = fcf, `Growth Rate` = growth_rate, `Growth Years` = growth_years,
                                     `Terminal Rate` = terminal_rate, `Discount Rate` = discount_rate, `Share Count` = shares,
                                     `Current Price` = price, `Excess Cash` = excess_cash)

  for (i in 1:nrow(data_all)) {

    dr <- data_all$`Discount Rate`[i]
    stock_price <- round(data_all$`Current Price`[i], digits = 2)
    dcf_value <- stock_price

    if(dcf_value > 0 & stock_price > 0 & data_all$`Current FCF`[i] > 0) {
      prev_dr <- round(dr/2, digits = 3)
      first_time <- TRUE
      while ((dcf_value != stock_price & dr != prev_dr) | first_time) {

        dcf_value <- simplify::dcf_valuation(fcf = data_all$`Current FCF`[i], growth_rate = data_all$`Growth Rate`[i],
                                             growth_years = data_all$`Growth Years`[i], terminal_rate = data_all$`Terminal Rate`[i],
                                             discount_rate = dr, shares = data_all$`Share Count`[i],
                                             excess_cash = data_all$`Excess Cash`[i])

        dcf_value <- round(dcf_value, digits = 2)
        prev_dr <- dr

        if(first_time) {
          data_history <- data.table::data.table(DCF = c(dcf_value, stock_price), discount_rate = c(dr, 0))
          first_time <- FALSE
        } else {
          data_history <- rbind(data_history, data.table::data.table(DCF = dcf_value, discount_rate = dr))
        }
        data.table::setorder(data_history, DCF)
        if(stock_price > min(data_history$DCF) & stock_price < max(data_history$DCF)) {
          price_index <- which(data_history$DCF == stock_price)[1]
          dr <- round((data_history$discount_rate[price_index-1] + data_history$discount_rate[price_index+1])/2, digits = 3)
        } else if(stock_price >= max(data_history$DCF)) {
          dr <- round(dr*0.85, digits = 3)
        } else if(stock_price <= min(data_history$DCF)) {
          dr <- round(dr*1.15, digits = 3)
        } else {}

      }
      data_all[i, `Expected Return` := prev_dr]
    } else {
      data_all[i, `Expected Return` := NA]
    }
  }
  return(data_all$`Expected Return`)
}

#' Calculate Expected Growth Rate from investment
#'
#' @param fcf A numeric vector containing Free Cash Flow.
#' @param growth_rate A numeric vector containing Growth Rate.
#' @param growth_years A numeric vector containing Years of Growth. Default 10.
#' @param terminal_rate A numeric vector containing Terminal Rate. Default 0.03.
#' @param discount_rate A numeric vector containing Discount Rate. Default 0.15.
#' @param shares A numeric vector containing number of Outstanding Shares.
#' @param price A numeric vector containing current share price.
#' @param excess_cash A numeric vector containing Excess Cash.
#'
#' @return A numeric vector with Expected Growth Rate.
#' @export
#'
#' @examples
#' expected_growth_rate(fcf = 100000000, shares = 100000, price = 100)
expected_growth_rate <- function(fcf, growth_rate = 0.15, growth_years = 10, terminal_rate = 0.03, discount_rate = 0.15, shares, price, excess_cash = 0) {

  data_all <- data.table::data.table(`Current FCF` = fcf, `Growth Rate` = growth_rate, `Growth Years` = growth_years,
                                     `Terminal Rate` = terminal_rate, `Discount Rate` = discount_rate, `Share Count` = shares,
                                     `Current Price` = price, `Excess Cash` = excess_cash)

  for (i in 1:nrow(data_all)) {

    gr <- data_all$`Growth Rate`[i]
    stock_price <- round(data_all$`Current Price`[i], digits = 2)
    dcf_value <- stock_price

    if(dcf_value > 0 & stock_price > 0 & data_all$`Current FCF`[i] > 0) {
      prev_gr <- round(gr/2, digits = 3)
      first_time <- TRUE
      while ((dcf_value != stock_price & gr != prev_gr) | first_time) {

        dcf_value <- simplify::dcf_valuation(fcf = data_all$`Current FCF`[i], growth_rate = gr,
                                             growth_years = data_all$`Growth Years`[i], terminal_rate = data_all$`Terminal Rate`[i],
                                             discount_rate = data_all$`Discount Rate`[i], shares = data_all$`Share Count`[i],
                                             excess_cash = data_all$`Excess Cash`[i])

        dcf_value <- round(dcf_value, digits = 2)
        prev_gr <- gr

        if(first_time) {
          data_history <- data.table::data.table(DCF = c(dcf_value, stock_price), growth_rate = c(gr, 0))
          first_time <- FALSE
        } else {
          data_history <- rbind(data_history, data.table::data.table(DCF = dcf_value, growth_rate = gr))
        }
        data.table::setorder(data_history, DCF)
        if(stock_price > min(data_history$DCF) & stock_price < max(data_history$DCF)) {
          price_index <- which(data_history$DCF == stock_price)[1]
          gr <- round((data_history$growth_rate[price_index-1] + data_history$growth_rate[price_index+1])/2, digits = 3)
        } else if(stock_price >= max(data_history$DCF)) {
          gr <- round(gr*1.15, digits = 3)
        } else if(stock_price <= min(data_history$DCF)) {
          gr <- round(gr*0.85, digits = 3)
        } else {}

      }
      data_all[i, `Expected Growth Rate` := prev_gr]
    } else {
      data_all[i, `Expected Growth Rate` := NA]
    }
  }
  return(data_all$`Expected Growth Rate`)
}


#' Read a feather file and return a data.table object
#'
#' @param path  A character vector containing an absolute path to a feather file.
#' @param columns Columns to read (names or indexes). Default: Read all columns.
#'
#' @return A data.table.
#' @export
#'
#' @examples
feather_to_DT <- function(path, columns = NULL) {
  data.table::setDT(feather::read_feather(path = path, columns = columns))
}

#' Format a numeric vector into K, M, B, T
#'
#' @param num_col A numeric vector.
#' @param digits A numeric value representing number of decimal digits.
#'
#' @return A numeric vector with KMBT format.
#' @export
#'
#' @examples
#' format_number(c(10, 42385, 8921478432, NA))
format_number <- function(num_col, digits = 0) {
  num_dt <- data.table::data.table(num_col = num_col)

  num_dt[, new_col := as.character(num_col)]
  num_dt[num_col < 1000, new_col := round(num_col, digits = digits)]
  num_dt[num_col >= 1000 & num_col < 1000000, new_col := paste0(round(num_col/1000, digits = digits), " K")]
  num_dt[num_col >= 1000000 & num_col < 1000000000, new_col := paste0(round(num_col/1000000, digits = digits), " M")]
  num_dt[num_col >= 1000000000 & num_col < 1000000000000, new_col := paste0(round(num_col/1000000000, digits = digits), " B")]
  num_dt[num_col >= 1000000000000, new_col := paste0(round(num_col/1000000000000, digits = digits), " T")]
  return(num_dt$new_col)
}

#' Capitalize the first letter of all words
#'
#' Make first letter of every word uppercase. It is advised to use a string with all lower case as input.
#'
#' @param x A character value (string) preferably lowercase string.
#'
#' @return A character value (string) with first letter of each word in capital.
#' @export
#'
#' @examples
#' simpleCap("watch the magic")
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' Convert a vector of customer names into a vector of customer page links.
#'
#' Add html elements to a character vector to convert it into clickable links.
#'
#' @param to_menu A character value specifying the menu name to jump to.
#' @param to_tab A character value specifying the tab name to jump to (optional).
#' @param modify_vector A character vector of customer names to create links.
#' @param from_tab A character value specifying the current page.
#' @param display_vector A character vector of customer names for display.
#'
#' @return A character vector of customer page links.
#' @export
#'
#' @examples
#' add_openTab(to_menu = "customer", modify_vector = c("Customer A", "Customer B"),
#' from_tab = "search")
add_openTab <- function(to_menu, to_tab = "", modify_vector, display_vector = "", from_tab) {

  if(length(display_vector) == 1) {
    if(display_vector == "") {
      display_vector <- modify_vector
    }
  }

  with_double_quote <- grep(pattern = '"', x = modify_vector)

  if(length(with_double_quote))
  {
    if(length(modify_vector[-with_double_quote]))
    {
      modify_vector[-with_double_quote] <- paste("<a onclick='openTab(\"", to_menu, "\", \"", to_tab, "\", \"", gsub(pattern = '"', replacement = '&quot;', x = gsub(pattern = "'", replacement = "&#39;", x = modify_vector[-with_double_quote])) ,"\", \"", from_tab, "\")'>", display_vector[-with_double_quote], "</a>", sep = "")
    }

    if(length(modify_vector[with_double_quote]))
    {
      modify_vector[with_double_quote] <- paste("<a onclick=\"openTab('", to_menu, "', '", to_tab, "', '", gsub(pattern = '"', replacement = '&quot;', x = gsub(pattern = "'", replacement = "&#39;", x = modify_vector[with_double_quote])) ,"', '", from_tab, "')\">", display_vector[with_double_quote], "</a>", sep = "")
    }
  }
  else
  {
    modify_vector <- paste("<a onclick='openTab(\"", to_menu, "\", \"", to_tab, "\", \"", gsub(pattern = '"', replacement = '&quot;', x = gsub(pattern = "'", replacement = "&#39;", x = modify_vector)) ,"\", \"", from_tab, "\")'>", display_vector, "</a>", sep = "")
  }

  modify_vector
}

#' Unescape html characters
#'
#' @param str A character string containing HTML characers
#'
#' @return An unescaped character string
#' @export
#'
#' @examples
#' unescape_html("&euro; 2.99")
unescape_html <- function(str) {
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# analyze_project <- function(path) {
#
#   all_files <- list.files(path = path, pattern = "\\.r$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
# }
