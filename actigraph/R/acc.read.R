# function of reading ACC files
acc.read <- function(fileX, epoch = 30, DataTable = FALSE, timezone = "America/Los_Angeles", ...) {
    # convert time to numeric seconds
    toSec <- function(time) {
        if (!is.character(time)) stop("x must be a character string of the form H:M:S")
        if (length(time) <= 0) return(x)
        sepX <- as.numeric(strsplit(time, ":")[[1]])
        return(ifelse(length(sepX) == 3, sepX[1] * 3600 + sepX[2] * 60 + sepX[3], ifelse(length(sepX) == 2, sepX[1] * 60 + sepX[2], sepX[1])))
    }
    # reading header
    header <- read.csv(fileX, nrow = 10, header = FALSE, stringsAsFactors = FALSE)
    serial <- strsplit(header[2, 1], " ")[[1]][3]
    start_time <- strsplit(header[3, 1], " ")[[1]][3]
    start_date <- strsplit(header[4, 1], " ")[[1]][3]
    epo <- toSec(strsplit(header[5, 1], " ")[[1]][4])
    voltage <- strsplit(header[9, 1], " ")[[1]][4]
    # reading observations
    col.num <- max(count.fields(fileX, skip = 10, sep = ","))
    if (DataTable) {
        if (col.num > 5) {
          data <- read.csv(fileX, skip = 10, header = TRUE, colClasses = c("NULL", "NULL", rep("integer", 4), rep("NULL", col.num - 6)))[c("Axis1", "Steps")]
        } else {
          data <- read.csv(fileX, skip = 10, header = TRUE, colClasses = c("NULL", "NULL", rep("integer", 3)))[c("Axis1", "Steps")]
        }
        colnames(data) <- c("y", "Steps")
    } else {
        if (col.num > 3) {
          data <- read.csv(fileX, skip = 10, header = FALSE, colClasses = c(rep("integer", 4), rep("NULL", col.num - 4)))
          colnames(data) <- c("y", "x", "z", "Steps")
        } else {
          data <- read.csv(fileX, skip = 10, header = FALSE, colClasses = c(rep("integer", 3)))
          colnames(data) <- c("y", "axis2", "Steps")
        }
    }
    
    epo.int <- epoch / epo
    min.total <- nrow(data) %/% epo.int
    data <- head(data, min.total * epo.int)
    
    records <- data.frame(
        y = as.integer(tapply(data$y, (seq_along(data$y) - 1) %/% epo.int, sum, na.rm = TRUE)), 
        steps = as.integer(tapply(data$Steps, (seq_along(data$Steps) - 1) %/% epo.int, sum, na.rm = TRUE)), 
        stamp = NA, 
        stringsAsFactors = FALSE)
    
    # time stamp
    stamp <- strptime(paste(start_date, start_time, sep = " "), format = "%m/%d/%Y %H:%M:%S", tz = timezone)
    stamp_epoch <- as.difftime(epoch, units = "secs") # convert epoch to time
    records$stamp <- seq(from = stamp, to = stamp + (nrow(records) - 1) * stamp_epoch, by = stamp_epoch) # time seq
    # returning output
    return(list("serial" = serial, "start_time" = start_time, "start_date" = start_date, "epoch" = epoch, "voltage" = voltage, "data" = records))
}

