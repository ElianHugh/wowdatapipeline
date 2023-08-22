box::use(. / log[log_error])

#' @export
safely_reduce <- function(init, ...) {
    tryCatch(
        expr = {
            Reduce(`[[`, x = list(...), init = init)
        },
        error = function(e) {
            log_error(e, context = "Error during reduce call")
            list(NULL)
        }
    )
}


#' @export
safe_request <- function(req_call) {
    box::use(httr2[...])
    tryCatch(
        expr = {
            req <- req_perform(req_call)
            if (resp_is_error(req)) {
                NULL
            } else {
                resp_body_json(req)
            }
        },
        error = function(e) {
            log_error(e, context = "Error during an httr2 request")
        }
    )
}
