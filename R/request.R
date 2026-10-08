# Requests to the Census Bureau (the Census API and file downloads), built on
# httr2. Callers keep their own status checks and error messages; these helpers
# handle building and sending requests, retries, connection failures, keeping
# the API key out of anything displayed, and show_call.

# HTTP statuses worth retrying: rate limiting and temporary server problems
census_retry_statuses <- c(429, 500, 502, 503, 504)
census_max_tries <- 3

tidycensus_user_agent <- function() {
  sprintf(
    "tidycensus/%s (https://walker-data.com/tidycensus/)",
    utils::packageVersion("tidycensus")
  )
}

# Build a request to a Census URL. NULL query values are dropped, as they were
# with httr. There is deliberately no overall timeout: large PUMS and flat-file
# downloads can take a long time on slow connections.
census_request <- function(url, query = list(), progress = FALSE) {
  req <- httr2::request(url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent(tidycensus_user_agent()) |>
    httr2::req_retry(
      max_tries = census_max_tries,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% census_retry_statuses
      },
      retry_on_failure = TRUE
    ) |>
    # Callers check the status themselves to give tidycensus error messages
    httr2::req_error(is_error = function(resp) FALSE)

  if (progress) {
    req <- httr2::req_progress(req)
  }

  req
}

# Perform a request. If the connection fails on every attempt, say so with the
# underlying cause; the full original error is kept on the condition as
# `original_error` rather than printed.
census_perform <- function(req, path = NULL) {
  tryCatch(
    httr2::req_perform(req, path = path),
    httr2_failure = function(e) {
      keys <- url_api_keys(req$url)

      # A missing FTP file is reported by curl as a failure, not a status code;
      # it isn't a connection problem, so don't describe it as one
      if (inherits(e$parent, "curl_error_remote_file_not_found")) {
        rlang::abort(
          redact_api_key(sprintf("File not found on the Census Bureau's server: %s", req$url), keys),
          class = "tidycensus_file_not_found",
          original_error = e,
          call = NULL
        )
      }

      rlang::abort(
        c(
          sprintf(
            "Unable to connect to %s after %s attempts.",
            httr2::url_parse(req$url)$hostname,
            census_max_tries
          ),
          i = redact_api_key(root_cause_message(e), keys)
        ),
        class = "tidycensus_connection_error",
        original_error = e,
        call = NULL
      )
    }
  )
}

# Send a Census API request and return the httr2 response. Prints the call
# (without the API key) when show_call = TRUE, and errors clearly if the API is
# still unavailable after retrying.
census_api_get <- function(url, query = list(), show_call = FALSE,
                           decode_call = FALSE, progress = FALSE) {
  resp <- census_perform(census_request(url, query, progress = progress))

  if (show_call) {
    call_url <- remove_api_key(httr2::resp_url(resp))
    if (decode_call) {
      call_url <- utils::URLdecode(call_url)
    }
    message(paste("Census API call:", redact_api_key(call_url, response_api_keys(resp))))
  }

  if (httr2::resp_status(resp) %in% census_retry_statuses) {
    rlang::abort(
      sprintf(
        "The Census API returned %s %s after %s attempts; it may be temporarily unavailable. Please try again later.",
        httr2::resp_status(resp),
        httr2::resp_status_desc(resp),
        census_max_tries
      ),
      class = "tidycensus_api_unavailable",
      call = NULL
    )
  }

  resp
}

# Standard checks for Census API data requests: a non-200 status or an invalid
# API key. Returns the response body as text.
census_api_content <- function(resp) {
  if (httr2::resp_status(resp) != 200) {
    # The API's error text can echo the request, so keep the key out of it
    msg <- redact_api_key(resp_text(resp), response_api_keys(resp))

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }
  }

  content <- resp_text(resp)

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  content
}

# Response body as text; an empty body (e.g. a 204) is ""
resp_text <- function(resp) {
  if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else ""
}

# Status descriptions where httr's wording differs from httr2's, generated from
# httr's status table so existing error messages read exactly as before
httr_status_descriptions <- c(
  "102" = "Processing (WebDAV; RFC 2518)",
  "207" = "Multi-Status (WebDAV; RFC 4918)",
  "208" = "Already Reported (WebDAV; RFC 5842)",
  "226" = "IM Used (RFC 3229)",
  "300" = "Multiple Choices",
  "306" = "Switch Proxy",
  "308" = "Permanent Redirect (experimental Internet-Draft)",
  "413" = "Request Entity Too Large",
  "414" = "Request-URI Too Long",
  "416" = "Requested Range Not Satisfiable",
  "418" = "I'm a teapot (RFC 2324)",
  "420" = "Enhance Your Calm (Twitter)",
  "422" = "Unprocessable Entity (WebDAV; RFC 4918)",
  "423" = "Locked (WebDAV; RFC 4918)",
  "424" = "Failed Dependency (WebDAV; RFC 4918)",
  "425" = "Unordered Collection (Internet draft)",
  "426" = "Upgrade Required (RFC 2817)",
  "428" = "Precondition Required (RFC 6585)",
  "429" = "Too Many Requests (RFC 6585)",
  "431" = "Request Header Fields Too Large (RFC 6585)",
  "444" = "No Response (Nginx)",
  "449" = "Retry With (Microsoft)",
  "450" = "Blocked by Windows Parental Controls (Microsoft)",
  "451" = "Unavailable For Legal Reasons (Internet draft)",
  "499" = "Client Closed Request (Nginx)",
  "506" = "Variant Also Negotiates (RFC 2295)",
  "507" = "Insufficient Storage (WebDAV; RFC 4918)",
  "508" = "Loop Detected (WebDAV; RFC 5842)",
  "509" = "Bandwidth Limit Exceeded (Apache bw/limited extension)",
  "510" = "Not Extended (RFC 2774)",
  "511" = "Network Authentication Required (RFC 6585)",
  "598" = "Network read timeout error (Unknown)",
  "599" = "Network connect timeout error (Unknown)"
)

# httr-style status description, e.g. "Client error: (400) Bad Request"
http_status_message <- function(resp) {
  status <- httr2::resp_status(resp)
  category <- if (status < 200) {
    "Information"
  } else if (status < 300) {
    "Success"
  } else if (status < 400) {
    "Redirection"
  } else if (status < 500) {
    "Client error"
  } else {
    "Server error"
  }
  desc <- httr_status_descriptions[as.character(status)]
  if (is.na(desc)) {
    desc <- httr2::resp_status_desc(resp)
  }
  sprintf("%s: (%s) %s", category, status, desc)
}

# API key values in a URL's query string (there may be more than one)
url_api_keys <- function(url) {
  keys <- regmatches(url, gregexpr("(?<=[?&]key=)[^&#]*", url, perl = TRUE))[[1]]
  unique(utils::URLdecode(keys[nzchar(keys)]))
}

# API keys from both the original request and the final (redirected) URL
response_api_keys <- function(resp) {
  request_url <- if (is.null(resp$request)) "" else resp$request$url
  unique(c(url_api_keys(request_url), url_api_keys(httr2::resp_url(resp))))
}

# Replace every occurrence of the API key(s) in displayed text
redact_api_key <- function(text, keys) {
  for (k in keys) {
    text <- gsub(k, "<REDACTED>", text, fixed = TRUE)
  }
  text
}

# Remove every `key` parameter from a URL, wherever it appears
remove_api_key <- function(url) {
  repeat {
    stripped <- sub("([?&])key=[^&#]*&?", "\\1", url)
    if (identical(stripped, url)) break
    url <- stripped
  }
  sub("[?&]$", "", url)
}

# The innermost error message in a chain of errors (e.g. curl's, under httr2's)
root_cause_message <- function(e) {
  while (!is.null(e$parent)) {
    e <- e$parent
  }
  trimws(gsub("\\s+", " ", conditionMessage(e)))
}

# Download a Census flat file and read it with readr, trying HTTPS first and
# FTP second (as the Census website offers both). Replaces readr::read_csv()
# on a URL so downloads get retries and clear connection errors.
census_read_csv <- function(https_url, ftp_url, required_col) {
  raw <- suppressWarnings(tryCatch(read_census_file(https_url), error = function(e) NULL))

  if (is.null(raw) || !required_col %in% names(raw)) {
    raw <- read_census_file(ftp_url)
  }

  raw
}

read_census_file <- function(url) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(url)))
  on.exit(unlink(tmp), add = TRUE)

  resp <- census_perform(census_request(url), path = tmp)
  status <- httr2::resp_status(resp)

  # FTP transfers report curl's FTP codes rather than HTTP statuses
  if (startsWith(url, "http") && status != 200) {
    stop(sprintf("Unable to download %s (%s).", url, http_status_message(resp)), call. = FALSE)
  }

  suppressMessages(readr::read_csv(tmp, lazy = FALSE))
}
