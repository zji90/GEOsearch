readURL <- function(URL, n=-1L) {
    tries <- 0L
    msg <- character()
    while (tries < 3L & !exists("URLdata")) {
        URLdata <- tryCatch({
            readLines(URL, n)
        }, error = function(e) {
            msg <<- conditionMessage(e)
            tries <<- tries + 1L
        })
    }
    if (tries == 3L)
        stop("failed to get URL after 3 tries:",
             "\n  url: ", URL,
             "\n  error: ", msg)
    URLdata
}

mygetURL <- function(URL) {
    tries <- 0L
    msg <- character()
    while (tries < 3L & !exists("URLdata")) {
        URLdata <- tryCatch({
            getURL(URL,dirlistonly = TRUE)
        }, error = function(e) {
            msg <<- conditionMessage(e)
            tries <<- tries + 1L
        })
    }
    if (tries == 3L)
        stop("failed to get URL after 3 tries:",
             "\n  url: ", URL,
             "\n  error: ", msg)
    URLdata
}
