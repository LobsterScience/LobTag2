# startup message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to LobTag2! ",
                        " To get started, your User Guide can be found in: ",
                        system.file("data", "user_guide.pdf", package = "LobTag2"),
                        " And your data entry templates in: ",
                        system.file("extdata", package = "LobTag2"),
                        "       ",
                        "Good luck with your tagging project!")
}
