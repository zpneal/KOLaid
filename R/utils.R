.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("KOLaid")
  packageStartupMessage("KOLaid v",local_version)
  packageStartupMessage("Cite: Neal, Z. P. and Dockerty, M. (2025). KOLaid: An R package to identify key opinion")
  packageStartupMessage("      leaders from a network given constraints GitHub. https://github.com/zpneal/KOLaid/")
  packageStartupMessage("      ")
  packageStartupMessage("Help: type vignette(\"KOLaid\"); email zpneal@msu.edu; github zpneal/KOLaid")
  packageStartupMessage("Beta: type devtools::install_github(\"zpneal/KOLaid\", ref = \"devel\")")
}

