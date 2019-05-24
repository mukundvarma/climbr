#' Utilities for cleaning and formatting data
#' 
#' 
#'
NULL

#' Get Routes by Location
#' 
#' The Mountain Project API requires API calls to be accompanied by a private key. 
#' Additionally, since the API for the other "Projects" (Hiking Project, 
#' MTB Project etc.) is identical, this package can be used to query those APIs as
#' well with a small modification to the base_url. You can either store your
#' private key in a json file and read it in with this function or construct a
#' named list (see examples)
#' 
#' @param
#' @param
#' 
#' @return
#' 
#' @examples
#' 
#' @seealso The Mountain Project API webpage: <https://www.mountainproject.com/data/> 
filter_routes <- function(routes, 
	min_difficulty, 
	max_difficulty,
	type,
	min_stars,
	max_stars,
	min_votes,
	min_pitches,
	max_pitches) {

	routes$pitches = as.numeric(routes$pitches)
	routes = routes %>% mutate(replace(routes, pitches==NA, 0)) %>% data.frame
	routes$min_stars = as.numeric(routes$min_stars)

}
