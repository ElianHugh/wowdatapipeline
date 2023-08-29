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
    con <- file("_targets/objects/log.ndjson", open = "a+")
    stream_out(msg, con, verbose = FALSE)
    close(con)
    invisible(NULL)
}

#' @export
clear_logs <- function() {
    if (file.exists("_targets/objects/log.old.ndjson")) {
        file.remove("_targets/objects/log.old.ndjson")
    }
    if (file.exists("_targets/objects/log.ndjson")) {
        file.rename("_targets/objects/log.ndjson", "_targets/objects/log.old.ndjson")
    }
}
