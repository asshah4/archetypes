# Parameter Level Estimates ----------------------------------------------------

#' Holding a list of term_list
#' @keywords internal
#' @noRd
new_term_list <- function(x = list()) {
	new_list_of(x, ptype = term_archetype(), class = "term_list")
}

#' @export
term_list <- function(x = unspecified(), ...) {

	if (length(x) == 0) {
		return(new_term_list())
	}

	# Return list_of
	new_term_list(x)
}

#' @export
vec_ptype_full.term_list <- function(x, ...) "term_list"

#' @export
vec_ptype_abbr.term_list <- function(x, ...) "tmls"