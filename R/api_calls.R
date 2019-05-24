#' Functions for making calls to the Mountain Project API
#' 
#' Imports
#' @importFrom magittr %>%
#' @importFrom httr GET 
#' @importFrom jsonlite parse_json read_json
NULL

PROJECT_URLS = c("https://www.mountainproject.com/data/",
	"https://www.hikingproject.com/data/",
	"https://www.powderproject.com/data/",
	"https://www.trailrunproject.com/data/",
	"https://www.mtbproject.com/data/")

ROCK_GRADES = c("3rd", "4th", "Easy 5th",
	paste("5.", 1:9, sep=""),
	paste(rep(c("5.10", "5.11", "5.12", "5.13", "5.14", "5.15"), each=4),
		  c("a", "b", "c", "d"), sep="")
)

ROCK_GRADES_DICT = 1:length(ROCK_GRADES)
names(ROCK_GRADES_DICT) = ROCK_GRADES

#' Validate config. 
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
#' @export

validate_config(config) <- {

	if (config[["private_key"]] == NULL || config[["private_key"]] == ""){
		stop("Config doesn't contain private key. Visit ", PROJECT_URLS[0])
	}

	if !(config[["base_url"]] %in% PROJECT_URLS){
		stop("Invalid base url specified. URL should be of form ", PROJECT_URLS[0])
	}

	test_url <- paste(base_url, 
		"get_routes?routeIds=105748391&key=", 
		config[["private_key"]], 
		sep="")
	test_response <- GET(test_url)

	if (http_status(test_response)$category!="Success") {
		stop("API Error. Your private key is likely wrong. Server message: ",
			http_status(test_response)$message)
	}
	return(TRUE)
}


#' Read in Mountain Project config file.
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
#' @export

read_api_config(filename) <- {

	mp_config <- read_json(mp_config)

	if (validate_config(mp_config)) {
		return(mp_config)
	}
	return(FALSE)
}


#' Make GET query.
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
#' @export

make_query <- function(config=NULL,
	private_key=NULL,
	base_url=PROJECT_URLS[0], 
	query_name="get-routes", 
	query_args=list(routeIds="105748391,105750454,105749956")) {

	if is.null(config) {
		if is.null(private_key) {
			stop("Private key must be specified as part of config or separately.")
		}
	} else {
		info("Using private key from config.")
		if (validate_config(config) == F){
			stop("Invalid Config.")
		}
		private_key = config[["private_key"]]
		base_url = config[["base_url"]]
	}

	query_url = paste(base_url, query_name, "?", sep="")
	response <- GET(query_url, key=private_key, query=query_args))

	if (http_status(response)[["category"]] != "Success") {
		stop("Request failed. Message: ", http_status(response)[["message"]])
	}

	return(content(response))
}


#' Get User.
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

get_user <- function(config=NULL, user_id=NULL, email=NULL) {

	if (user_id == NULL && email == NULL) {
		stop("Either user ID or email must be provided.")
	}

	if (email != NULL)
		query_args = list(user_id=user_id)
	else
		query_args = list(email=email)

	user <- make_query(config, query_name="get-user", query_args=query_args)
	return(user)
}

#' Get Ticks
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

get_ticks <- function(config=NULL,
	user_id=NULL,
	email=NULL,
	start_pos=0) {

	if (user_id == NULL && email == NULL) {
		stop("Either user ID or email must be provided.")
	}

	if (email != NULL)
		query_args = list(user_id=user_id, startPos=start_pos)
	else
		query_args = list(email=email, startPos=start_pos)

	routes <- make_query(config, query_name="get-ticks", query_args=query_args)
	return(routes)
}

#' Get To-dos
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

get_todos <- function(config=NULL, user_id=NULL, email=NULL) {

	if (user_id == NULL && email == NULL) {
		stop("Either user ID or email must be provided.")
	}

	if (email != NULL)
		query_args = list(user_id=user_id, startPos=start_pos)
	else
		query_args = list(email=email, startPos=start_posx)

	routes <- make_query(config, query_name="get-todos", query_args=query_args)
	return(routes)
}

#' Get Routes By ID
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

get_routes_by_id <- function(config=NULL, route_ids=NULL) {

	if (length(route_ids) == 0) {
		stop("No routes provided.")
	}

	route_ids_query= paste(route_ids, collapse=",")
	query_args = list(routeIds=route_ids_query)
	routes <- make_query(config, query_name="get-routes", query_args=query_args)
	return(routes)
}



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

get_routes_by_location <- function(config=NULL, 
	latitude=40.03,
	longitude=105.25,
	max_distance=30,
	max_results=500,
	min_difficulty=NULL,
	max_difficulty=NULL,
	type="all",
	min_stars=0,
	max_stars=5,
	min_votes=1,
	min_pitches=0,
	max_pitches=1) {

	if (max_distance > 200) {
		warn("Max distance provided is greater than 200. Defaulting to 200")
	}

	if (max_distance > 200) {
		warn("Max distance provided is greater than 200. Defaulting to 200")
	}

	route_ids_query= paste(route_ids, collapse=",")
	
	query_args = list(lat=latitude,
		long=longitude,
		maxDistance=max_distance)
	
	if (!is.null(min_difficulty))
		query_args[["minDiff"]] <- min_difficulty
	if (!is.null(max_difficulty))
		query_args[["maxDiff"]] <- max_difficulty
	
	routes <- make_query(config, query_name="get-routes", query_args=query_args)

	routes <- filter_routes(routes,
		type=type,
		min_stars=min_stars,
		max_stars=max_stars,
		min_votes=min_votes,
		min_pitches=min_pitches,
		max_pitches=max_pitches)
	
	return(routes)
}

