library(data.table); library(modelsummary);

ds <- function(data, output = "markdown") {
    message(paste(nrow(data), "obs", ncol(data), "cols"))
    return(datasummary(All(data) ~ NUnique + PercentMissing + Mean + SD + Min + Median + Max + Histogram, data = data, output = output))
}



get_samples <- function(model, parameter = NULL) {
    samps <- data.table::as.data.table(model$fit)
    param_names <- names(samps)
    message(paste0(ncol(samps), " parameters. ", dim(samps)[1], " samples each."))
    samps <- data.table::melt(samps, measure.vars = param_names, variable.name = "param", value.name = "samp")
    samps[, idx := 1:.N, param]
    if (!is.null(parameter)) {
        message(paste0("Returning only samples for ", parameter))
        return(samps[param == parameter])
    }
    return(samps)
}
