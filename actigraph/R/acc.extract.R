acc.extract = function(accX, start, end) {
    skeleton = data.frame(MVPA = c("light", "moderate", "nonvalid", "sedentary", "vigorous"))
    accX.seg = accX$data[accX$data$stamp > start & accX$data$stamp <= end, ]
    mvpa = aggregate(min ~ MVPA, data = accX.seg, sum, na.rm = TRUE)
    result = merge(skeleton, mvpa, by = "MVPA", all.x = TRUE, sort = FALSE)
    result[is.na(result)] = 0
    return(result)
}
