acc.describe <- function(sumX) {
  require("plyr")
  local.envir <- environment() # local environment
  unit <- unique(sumX$data$min)
  # only look at valid ware time
  valid <- sumX$data[sumX$data$nonvalid == 0,]
  valid$date <- as.character(valid$date)
  date.list <- unique(valid$date)
  
  ### STANDARD ###
  # MET-hrs
  try(standard.met.hrs <- setNames(aggregate(met ~ date, data = valid, FUN = function(x) {return(sum(x)*unit/60)}), c("date", "met.hrs")), silent = TRUE)
  # Total sedentary time
  try(standard.total.sed.time <- setNames(aggregate(MVPA ~ date, data = valid, FUN = function(x) {return(sum(x == "sedentary")*unit)}), c("date", "total.sedentary")), silent = TRUE)
  # Percentage sedentary time
  try(standard.percentage.sedentary <- setNames(aggregate(MVPA ~ date, data = valid, FUN = function(x) {return(sum(x == "sedentary")/length(x)*100)}), c("date", "percentage.sedentary")), silent = TRUE)
  # Percentage standing time
  # Percentage Stepping time
  # steps
  try(standard.step <- aggregate(steps ~ date, data = valid, sum), silent = TRUE)
  # light intensity
  try(standard.light.intensity <- setNames(aggregate(MVPA ~ date, data = valid, FUN = function(x) {sum(x == "light")*unit}), c("date", "light.intensity")), silent = TRUE)
  # MVPA
  try(standard.mvpa <- setNames(aggregate(MVPA ~ date, data = valid, FUN = function(x) {sum(x %in% c("moderate", "vigorous"))*unit}), c("date", "mvpa")), silent = TRUE)
  # MVPA-guideline
  mvpa <- data.frame(mvpa = ifelse(valid$MVPA %in% c("moderate", "vigorous"), 1, 0), date = valid$date)
  bout.mvpa <- function(daily.mvpa, date) {
    chunk <- data.frame(values = rle(daily.mvpa$mvpa)$values, lengths = rle(daily.mvpa$mvpa)$lengths)
    chunk$period <- ifelse(chunk$values == 0 & chunk$lengths > 2/unit, 0, 1)
    chunk$bout <- ifelse(chunk$period == 0, 0, NA)
    i = 1
    while (i <= nrow(chunk)-1) {
      if(!is.na(chunk$bout[i])) {
        i <- i+1
        next
      } else if (!is.na(chunk$bout[i+1])){
        chunk$bout[i] <- ifelse(chunk$lengths[i] >= 10/unit, 1, 0)
        i <- i+1
        next
      } else {
        mvpa.length <- 0
        non.length <- 0
        k <- 0
        repeat {
          if(is.na(chunk$values[i+k])) {
            i <- i+k
            break
          }
          if (chunk$values[i+k] == 1) {
            mvpa.length <- mvpa.length + chunk$lengths[i+k]
            k <- k+1
          } else {
            non.length <- non.length + chunk$lengths[i+k]
            k <- k+1
          }
          if (non.length >= 2/unit) {
            if (mvpa.length >= 8/unit) {
              chunk$bout[i:(i+k-2)] <- 1
              while (chunk$period[i+k-1] == 1) {
                chunk$bout[i+k-1] <-1
                i <- i+1
                if (i+k-1 > length(chunk$period)) break
              }
              break
            } else {
              chunk$bout[i:(i+k-1)] <- 0
              i <- i+k
              break
            }
          }
        }
      }
    }
    chunk$bout[nrow(chunk)] <- ifelse(chunk$values[nrow(chunk)] == 1 & chunk$lengths[nrow(chunk)] >= 10/unit, 1, 0)
    chunk$date <- date
    return(chunk)
  }
  mvpa.bout <- c()
  for (i in date.list) {
    mvpa.bout <- rbind(mvpa.bout, bout.mvpa(mvpa[mvpa$date == i,], i))
  }
  mvpa.bout$event <- ifelse(mvpa.bout$values == 1 | mvpa.bout$bout == 1, 1, 0)
  mvpa.bout$seq <- rep(seq_along(rle(mvpa.bout$event)$lengths), rle(mvpa.bout$event)$lengths)
  try(mvpa.bout.length <- aggregate(lengths ~ date + seq, data = mvpa.bout[mvpa.bout$event == 1,], sum), silent = TRUE)
  try(standard.mvpa.guideline <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x) {return(sum(x[x>=10])*unit)}), c("date", "mvpa.guideline")), silent = TRUE)
  
  # standard summary
  summary.standard.list <- c("standard.met.hrs", "standard.total.sed.time", "standard.percentage.sedentary", "standard.step", "standard.light.intensity", "standard.mvpa", "standard.mvpa.guideline")
  summary.standard <- join_all(lapply(summary.standard.list[sapply(summary.standard.list, exists, where = local.envir)], get, envir = local.envir), by = "date", type = "full")
  
  condense <- function(sedX, var, unit) {
    output <- c()
    date.list <- unique(sedX$date)
    for (i in date.list) {
      logX <- rle(sedX[sedX$date == i, var])
      daily.log <- data.frame(date = i, value = logX$values, length = logX$length*unit)
      output <- rbind(output, daily.log)
      rm(logX)
      rm(daily.log)
    }
    return(output)
  }
  ### SEDENTARY FEATURES ###
  sedentary <- data.frame(sedentary = ifelse(valid$MVPA == "sedentary", 1, 0), date = valid$date)
  sedentary.bouts <- condense(sedentary, "sedentary", unit)
  # sedentary breaks
  sed.breaks <- setNames(aggregate(value ~ date, data = sedentary.bouts, FUN = function(x) {return(length(x[x == 1]))}), c("date", "sedentary.breaks"))
  # break rate
  sed.break.rate <- merge(standard.total.sed.time, sed.breaks, by = "date", all = TRUE, sort = FALSE)
  sed.break.rate$sedentary.break.rate <- sed.break.rate$sedentary.breaks/sed.break.rate$total.sedentary
  # mean sedentary event length
  sed.mean.sedentary.length <- aggregate(length ~ date + value, data = sedentary.bouts[sedentary.bouts$value == 1,], mean)
  # duration of sedentary time above 20/60/120 minutes
  try(sed.duration.20 <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$length >= 20 & sedentary.bouts$value == 1,], sum), c("date", "sedentary.duration.20")), silent = TRUE)
  try(sed.duration.60 <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$length >= 60 & sedentary.bouts$value == 1,], sum), c("date", "sedentary.duration.60")), silent = TRUE)
  try(sed.duration.120 <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$length >= 120 & sedentary.bouts$value == 1,], sum), c("date", "sedentary.duration.120")), silent = TRUE)
  # proportion of sedentary time above 20/60/120 min
  sed.proportion.20 <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$value == 1,], FUN = function(x) {return(sum(x >= 20)/length(x)*100)}), c("date", "sedentary.proportion.20"))
  sed.proportion.60 <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$value == 1,], FUN = function(x) {return(sum(x >= 60)/length(x)*100)}), c("date", "sedentary.proportion.60"))
  sed.proportion.120 <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$value == 1,], FUN = function(x) {return(sum(x >= 120)/length(x)*100)}), c("date", "sedentary.proportion.120"))
  # Xth percentile of sedentary time (X = 5, 25, 50, 75, 95)
  sed.percentile <- aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$value == 1,], FUN = function(x) {return(quantile(x, c(0.05, 0.25, 0.50, 0.75, 0.95)))})
  sed.percentile <- setNames(data.frame(cbind(as.character(sed.percentile$date), sed.percentile$length)), c("date", paste0("sed.percentile.", c(5, 25, 50, 75, 95))))
  # alpha
  sed.alpha <- setNames(aggregate(length ~ date, data = sedentary.bouts[sedentary.bouts$value == 1,], FUN = function(x) {return(1+1/mean(log(x/min(x))))}), c("date", "sedentary.alpha"))
  # Gini
  # minutes in sedentary
  try(sed.min <- setNames(aggregate(length ~ date,  data = sedentary.bouts[sedentary.bouts$value == 1 & sedentary.bouts$length >= 30 & sedentary.bouts$length <60,], sum), c("date", "sedentary.minutes.3060")), silent = TRUE)
  try(sed.num <- setNames(aggregate(length ~ date,  data = sedentary.bouts[sedentary.bouts$value == 1 & sedentary.bouts$length >= 30 & sedentary.bouts$length <60,], length), c("date", "sedentary.number.3060")), silent = TRUE)
  
  # sedentary summary
  summary.sedentary.list <- c("sed.break.rate", "sed.mean.sedentary.length", paste0("sed.duration.", c(20, 60, 120)), paste0("sed.proportion.", c(20, 60, 120)), "sed.percentile", "sed.alpha", "sed.min", "sed.num")
  summary.sedentary <- join_all(sapply(summary.sedentary.list[sapply(summary.sedentary.list, exists, where = local.envir)], get, envir = local.envir), by = "date", type = "full")
  
  ### STANDING/LIGHT INTENSITY ###
  light <- data.frame(light = ifelse(valid$MVPA == "light", 1, 0), date = valid$date)
  light.bouts <- condense(light, "light", unit)
  light.bouts <- light.bouts[light.bouts$value == 1,]
  # number of active events
  light.num <- setNames(aggregate(length ~ date, data = light.bouts, length), c("date", "num.active.events"))
  # mean active event length
  light.mean <- setNames(aggregate(length ~ date, data = light.bouts, mean), c("date", "mean.active.event.length"))
  # proportion of active time above 5, 10, 30 min
  light.proportion.5 <- setNames(aggregate(length ~ date, data = light.bouts, FUN = function(x) {return(sum(x >= 5)/length(x)*100)}), c("date", "light.proportion.5"))
  light.proportion.10 <- setNames(aggregate(length ~ date, data = light.bouts, FUN = function(x) {return(sum(x >= 10)/length(x)*100)}), c("date", "light.proportion.10"))
  light.proportion.30 <- setNames(aggregate(length ~ date, data = light.bouts, FUN = function(x) {return(sum(x >= 30)/length(x)*100)}), c("date", "light.proportion.30"))
  # duration of active time above 5, 10, 30 min
  try(light.duration.5 <- setNames(aggregate(length ~ date, data = light.bouts[light.bouts$length >= 5,], sum), c("date", "light.duration.5")), silent = TRUE)
  try(light.duration.10 <- setNames(aggregate(length ~ date, data = light.bouts[light.bouts$length >= 10,], sum), c("date", "light.duration.10")), silent = TRUE)
  try(light.duration.30 <- setNames(aggregate(length ~ date, data = light.bouts[light.bouts$length >= 30,], sum), c("date", "light.duration.30")), silent = TRUE)
  # Xth percentile of active time (X = 5, 25, 50, 75, 95)
  light.percentile <- aggregate(length ~ date, data = light.bouts, FUN = function(x) {return(quantile(x, c(0.05, 0.25, 0.50, 0.75, 0.95)))})
  light.percentile <- setNames(data.frame(cbind(as.character(light.percentile$date), light.percentile$length)), c("date", paste0("light.percentile.", c(5, 25, 50, 75, 95))))
  # alpha
  light.alpha <- setNames(aggregate(length ~ date, data = light.bouts, FUN = function(x) {return(1+1/mean(x/min(x)))}), c("date", "light.alpha"))
  
  # light summary
  summary.light.list <- c("light.num", "light.mean", paste0("light.proportion.", c(5, 10, 30)), paste0("light.duration.", c(5, 10, 30)), "light.percentile", "light.alpha")
  summary.light <- join_all(sapply(summary.light.list[sapply(summary.light.list, exists, where = local.envir)], get, envir = local.envir), by = "date", type = "full")
 
  ### MODERATE-TO-VIGOROUS ACTIVITY ###
  
  # sporadic MVPA minutes
  try(mvpa.sporadic.mvpa.min <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(sum(x)*unit)}), c("date", "mvpa.sporadic.mvpa.min")), silent = TRUE)
  # number of sporadic MVPA events
  try(mvpa.sporadic.mvpa.event <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, length), c("date", "mvpa.sporadic.mvpa.num")), silent = TRUE)
  # proportion of MVPA time above (2, 5, 10) min
  try(mvpa.proportion.2 <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(sum(x>=2)/length(x)*100)}), c("date", "mvpa.proportion.2")), silent = TRUE)
  try(mvpa.proportion.5 <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(sum(x>=5)/length(x)*100)}), c("date", "mvpa.proportion.5")), silent = TRUE)
  try(mvpa.proportion.10 <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(sum(x>=10)/length(x)*100)}), c("date", "mvpa.proportion.10")), silent = TRUE)
  # proportion of MVPA greater than 10/20 min
  try(mvpa.proportion.20 <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(sum(x>=20)/length(x)*100)}), c("date", "mvpa.proportion.20")), silent = TRUE)
  # highest MET-value
  try(mvpa.highest.met <- setNames(aggregate(met ~ date, data = valid, max), c("date", "mvpa.highest.met")), silent = TRUE)
  # met-hrs in MVPA-guideline
  # number of guideline bouts
  try(mvpa.guideline.num <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(sum(x >= 10))}), c("date", "mvpa.guideline.bout.num")), silent = TRUE)
  # mean of MVPA-guideline
  try(mvpa.guideline.mean <- setNames(aggregate(lengths ~ date, data = mvpa.bout.length, FUN = function(x){return(mean(x[x>=10/unit])*unit)}), c("date", "mvpa.guideline.bout.mean")), silent = TRUE)
  
  # MVPA summary
  summary.mvpa.list <- c("mvpa.sporadic.mvpa.min", "mvpa.sporadic.mvpa.event", paste0("mvpa.proportion.", c(2, 5, 10, 20)), "mvpa.highest.met", "mvpa.guideline.num", "mvpa.guideline.mean")
  summary.mvpa <- join_all(lapply(summary.mvpa.list[sapply(summary.mvpa.list, exists, where = local.envir)], get, envir = local.envir), by = "date", type = "full")
 
  # summary of all
  summary.all <- join_all(list(summary.standard, summary.sedentary, summary.light, summary.mvpa), by = "date", type = "full") 
  return(list(abstract = sumX$abstract, details = sumX$details, standard = summary.standard, sedentary = summary.sedentary, light = summary.light, mvpa = summary.mvpa, full = summary.all))
}
