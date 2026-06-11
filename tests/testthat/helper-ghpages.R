ghpages_tests_should_skip <- function() {
  if (!nzchar(Sys.getenv("ANIMINT2_TEST_GHPAGES", unset = ""))) return(TRUE)
  pat <- Sys.getenv("GITHUB_PAT", unset = "")
  pat_com <- Sys.getenv("GITHUB_PAT_GITHUB_COM", unset = "")
  !nzchar(pat) && !nzchar(pat_com)
}

skip_ghpages_integration <- function() {
  skip_if(ghpages_tests_should_skip(), "ghpages CI setup not available")
}
