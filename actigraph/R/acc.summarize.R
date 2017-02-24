acc.summarize <- function(accX, age, id = "temp", VldDay_cutoff = 10, exclude = list(), MeterPlus = TRUE, timezone = "America/Los_Angeles", ...) {
    troiano <- data.frame(
        type = c("sedentary", "light", "moderate", "vigorous"),
        age6 = c(0, 100, 1400, 3758),
        age7 = c(0, 100, 1515, 3947),
        age8 = c(0, 100, 1638, 4147),
        age9 = c(0, 100, 1770, 4360),
        age10 = c(0, 100, 1910, 4588),
        age11 = c(0, 100, 2059, 4832),
        age12 = c(0, 100, 2220, 5094),
        age13 = c(0, 100, 2393, 5375),
        age14 = c(0, 100, 2580, 5679),
        age15 = c(0, 100, 2781, 6007),
        age16 = c(0, 100, 3000, 6363),
        age17 = c(0, 100, 3239, 6751),
        adult = c(0, 100, 2020, 5999),
        stringsAsFactors = FALSE
    )
    
    chunk <- data.frame(values = rle(accX$data$y)$values, lengths = rle(accX$data$y)$lengths)
    chunk$nonvalid <- ifelse(chunk$lengths >= 3600/accX$epoch & chunk$values == 0, 1, 0)
    accX$data$nonvalid <- rep(chunk$nonvalid, chunk$lengths)
    # filter for extreme values (errors)
    accX$data$nonvalid <- ifelse(accX$data$y > 16000/(60/accX$epoch) | accX$data$steps > 100/(30/accX$epoch), 1, accX$data$nonvalid)
    # exclude specifized time period (label as nonvalid)
    if (length(exclude) > 0) {
        for (i in 1:length(exclude)) {
            accX$data$nonvalid[accX$data$stamp > min(exclude[[i]]) & accX$data$stamp < max(exclude[[i]])] <- 1
        }
    }
    
    accX$data$date <- as.Date(accX$data$stamp, tz = timezone)
    # using troiano cut points for MVPA
    type <- c("nonvalid", troiano$type)
    if (age < 6) {age <- 6} # for children under 6, use age6 criteria for MVPA
    age_group <- ifelse(age >= 18, "adult", paste0('age', age))
    accX$data$MVPA <- ifelse(accX$data$nonvalid == 1, type[1], as.character(cut(accX$data$y, c(troiano[,age_group]/(60/accX$epoch), Inf), labels = type[2:5], right = FALSE)))
    accX$data$min <- accX$epoch/60
    # METs
    met <- function(cpm, age) {
      if (age >= 18 ) {
          return(1.439008 + (0.000795*cpm))
        } else {
          return(2.757 + (0.0015*cpm)-(0.08957*age)-(0.000038*cpm*age))
        }
    }
    accX$data$met <- met(accX$data$y/(accX$epoch/60), age)
    
    # function to calculate nonwear in mins
    duration <- function(x) {difftime(max(x) + accX$epoch, min(x), units = "min")} # add 1 epoch for the difference
    details <- aggregate(list(duration = accX$data$stamp), by = list(accX$data$date), duration)
    
    specs <- reshape(aggregate(accX$data$min, by = list(accX$data$date, accX$data$MVPA), sum), idvar = "Group.1", timevar = "Group.2", direction = "wide")
    details <- merge(details, specs, by = "Group.1", all.x = TRUE, sort = FALSE)
    colnames(details) <- gsub("x.", "", names(details))
    missing <- type[!type %in% names(details)]
    if (length(missing > 0)) details[missing] <- NA
    details[is.na(details)] <- 0
    details$mvpa <- details$moderate + details$vigorous
    details$valid <- details$duration - details$nonvalid
    
    details$VldHrs <- details$valid/60
    details$VldDay <- ifelse(details$VldHrs >= VldDay_cutoff, 1, 0)
    names(details)[1] <- "date"
    abstract <- data.frame(SN = as.character(), ID = as.character(), Age = as.character(), Date = as.character(), TotDays = as.numeric(), VldDays = as.numeric(), stringsAsFactors = FALSE)
    abstract[1,] <- c(accX$serial, id, ifelse(age>=18, "adult", age), as.character(details$date[1]), nrow(details), sum(details$VldDay))
    return(list("abstract" = abstract, "details" = details[c("date", "VldDay", "VldHrs", "duration", "nonvalid", "valid", "sedentary", "light", "moderate", "vigorous", "mvpa")], "data" = accX$data))
}
