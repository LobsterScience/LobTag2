# startup message

.onAttach <- function(libname, pkgname) {
  dir.create("C:/LOBTAG",showWarnings = F)
  file.copy(system.file("data", package = "LobTag2"),"C:/LOBTAG", recursive = T, overwrite = F)
  file.copy(system.file("extdata", package = "LobTag2"), "C:/LOBTAG", recursive = T, overwrite = F)
  packageStartupMessage("Welcome to LobTag2! ",
                        " To get started, your User Guide can be found in: C:/LOBTAG/data",
                        " And your data entry templates in: C:/LOBTAG/extdata",
                        "       ",
                        "Good luck with your tagging project!")
}
