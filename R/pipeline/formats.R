box::use(
    targets[tar_format]
)

#' @export
tar_json <- tar_format(
    read = function(path) {
        box::use(jsonlite[read_json])
        read_json(path)
    },
    write = function(object, path) {
        box::use(jsonlite[write_json])
        write_json(
            object,
            path
        )
    },
    marshal = function(object) {
        identity(object)
    },
    unmarshal = function(object) {
        identity(object)
    },
    convert = function(object) {
        identity(object)
    },
    repository = NULL
)
