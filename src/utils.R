library(data.table); library(modelsummary);

ds <- function(data, output = "markdown") {
    message(paste(nrow(data), "obs", ncol(data), "cols"))
    return(datasummary(All(data) ~ NUnique + PercentMissing + Mean + SD + Min + Median + Max + Histogram, data = data, output = output))
}
