box::use(
    targets[tar_format]
)

# #' @export
# tar_json <- tar_format(
#     read = function(path) {
#         box::use(jsonlite[read_json])
#         read_json(path)
#     },
#     write = function(object, path) {
#         box::use(jsonlite[write_json])
#         write_json(
#             object,
#             path
#         )
#     },
#     marshal = function(object) {
#         identity(object)
#     },
#     unmarshal = function(object) {
#         identity(object)
#     },
#     convert = function(object) {
#         identity(object)
#     },
#     repository = NULL
# )


#' @export
tar_json <- tar_format(
    read = function(path) {
        box::use(yyjsonr[read_json_file])
        read_json_file(path)
    },
    write = function(object, path) {
        box::use(yyjsonr[write_json_file])
        write_json_file(
            object,
            path,
            auto_unbox = TRUE
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
