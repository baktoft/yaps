#' Internal function checking installed version vs current github version
#'
#' Adapted from https://github.com/hugomflavio/actel
#' @noRd
newPkgVersion <- function(){
  rep.ver <- tryCatch(unlist(strsplit(readLines('https://raw.githubusercontent.com/baktoft/yaps/master/DESCRIPTION')[3], " "))[2], error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(rep.ver)) {
	rep.ver.short <- substr(rep.ver, start = 1, stop = nchar(rep.ver) - 5)
	rep.ver.num <- unlist(strsplit(rep.ver.short, ".", fixed = TRUE))
	inst.ver <- utils::packageVersion("yaps")
	inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
	inst.ver.num <- unlist(strsplit(inst.ver.short, ".", fixed = TRUE))
	if (any(rep.ver.short > inst.ver.short)){
	    return(TRUE)
	} else {
		return(FALSE)
	}
  }
}
