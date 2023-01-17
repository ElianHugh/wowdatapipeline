box::use(./log[log_error])

#' @export
safely_reduce <- function(init, ...) {
    tryCatch(
        {
            Reduce(`[[`, x = list(...), init = init)
        },
        error = function(e) {
            log_error(e)
            list(NULL)
        }
    )
}


#' @export
safe_request <- function(req_call) {
    box::use(httr2[...])
    tryCatch(
        {
            req <- req_perform(req_call)
            if (resp_is_error(req)) {
                NULL
            } else {
                resp_body_json(req)
            }
        },
        error = function(e) {
            log_error(e)
        }
    )
}