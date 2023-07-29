#' @export
log_error <- function(e, context = "N/A") {
    box::use(
        tibble[tribble],
        jsonlite[stream_out]
    )
    msg <- tribble(
        ~Time, ~Call, ~Message, ~Context,
        Sys.time(), deparse(e[["call"]]), e[["message"]], context
    )
    con <- file("_targets/objects/log", open = "a+")
    stream_out(msg, con, verbose = FALSE)
    close(con)
    invisible(NULL)
}

#' @export
clear_logs <- function() {
    if (file.exists("_targets/objects/log.old")) {
        file.remove("_targets/objects/log.old")
    }
    if (file.exists("_targets/objects/log")) {
        file.rename("_targets/objects/log", "_targets/objects/log.old")
    }
}
