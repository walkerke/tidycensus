# Cleaned variable labels and label-based names (the `labels` argument of
# get_acs() and get_decennial(), and `clean_labels` in load_variables())

# "Estimate!!Total:!!Male:!!Under 5 years" -> "Total > Male > Under 5 years"
clean_variable_label <- function(x, sep = " > ") {
  x <- trimws(x)
  x <- sub("^!!", "", x)                          # 2020 decennial: " !!Total:"
  x <- sub("^Estimate!!", "", x)                  # ACS estimates
  x <- sub("^([0-9]{4}) Estimate!!", "\\1: ", x) # comparison profiles
  x <- sub("^Percent!!", "Percent: ", x)          # Data Profile percentages
  parts <- strsplit(x, "!!", fixed = TRUE)
  vapply(parts, function(p) paste(sub(":$", "", trimws(p)), collapse = sep), character(1))
}

# "SEX BY AGE [79]" -> "SEX BY AGE" (2000 concepts include cell counts)
clean_concept <- function(x) {
  trimws(sub("\\s*\\[[0-9]+\\]$", "", x))
}

# "Total > Male > Under 5 years" -> "total_male_under_5_years"
snake_label <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(x))
  x <- gsub("^_+|_+$", "", x)
  # names can't start with a digit without backticks (comparison profiles)
  ifelse(grepl("^[0-9]", x), paste0("y", x), x)
}

# Words of `x` (snake case) not already in `label`
words_not_in <- function(x, label) {
  mapply(function(x, label) {
    w <- strsplit(snake_label(x), "_", fixed = TRUE)[[1]]
    lw <- strsplit(snake_label(label), "_", fixed = TRUE)[[1]]
    paste(w[nzchar(w) & !w %in% lw], collapse = "_")
  }, x, label, USE.NAMES = FALSE)
}

# "Sex by Age (White Alone)" -> qualifier "White Alone", base "Sex by Age"
concept_qualifier <- function(concept) {
  has <- grepl("\\(([^()]*)\\)\\s*$", concept)
  ifelse(has, sub(".*\\(([^()]*)\\)\\s*$", "\\1", concept), "")
}

concept_base <- function(concept) {
  trimws(sub("\\s*\\([^()]*\\)\\s*$", "", concept))
}

append_part <- function(name, part) {
  ifelse(nzchar(part), paste0(name, "_", part), name)
}

# Names for variables from their cleaned labels and concepts.
#   "label":   the label alone; variables whose names would collide get concept
#              text added (just the qualifier, e.g. "white_alone", when their
#              concepts differ only by it; otherwise the concept).
#   "concept": the label plus the concept's qualifier, or the concept itself
#              when it has none, always.
# Words already in the label aren't repeated.
label_names <- function(code, label, concept, mode = c("label", "concept")) {
  mode <- rlang::arg_match(mode)
  name <- snake_label(label)

  qualifier <- words_not_in(concept_qualifier(concept), label)
  whole <- words_not_in(concept, label)

  if (mode == "concept") {
    name <- append_part(name, ifelse(nzchar(concept_qualifier(concept)), qualifier, whole))
  } else {
    collided <- character()

    for (n in unique(name[duplicated(name)])) {
      i <- which(name == n)
      bases <- unique(tolower(concept_base(concept[i])))
      part <- if (length(bases) == 1) qualifier[i] else whole[i]
      name[i] <- append_part(name[i], part)
      collided <- c(collided, code[i])
    }

    if (length(collided)) {
      rlang::inform(c(
        "Some variables share a label, so their concept was added to their names:",
        stats::setNames(paste0(collided, " -> ", name[match(collided, code)]), rep("*", length(collided)))
      ))
    }
  }

  dup <- name[duplicated(name)]

  if (length(dup)) {
    rlang::abort(c(
      "Unable to give these variables unique names from their labels and concepts:",
      stats::setNames(paste(code[name %in% dup], collapse = ", "), "x"),
      "i" = "Name them yourself, e.g. `variables = c(my_name = \"B01001_001\")`."
    ))
  }

  name
}

# Session cache of variable metadata, so repeated requests don't reload it
.label_cache <- new.env(parent = emptyenv())

# Cleaned labels and concepts for `codes` in a dataset (as in load_variables()),
# trying `fallback` datasets for codes not found (e.g. 2000 SF3 after SF1)
variable_labels <- function(codes, year, dataset, key, fallback = character()) {
  out <- data.frame(code = character(), label = character(), concept = character())

  for (d in c(dataset, fallback)) {
    missing <- setdiff(codes, out$code)
    if (!length(missing)) break

    cache_key <- paste(year, d, sep = "/")
    if (is.null(.label_cache[[cache_key]])) {
      v <- suppressMessages(load_variables(year, d, key = key))
      .label_cache[[cache_key]] <- data.frame(
        code = v$name,
        label = clean_variable_label(v$label),
        concept = clean_concept(v$concept)
      )
    }

    v <- .label_cache[[cache_key]]
    out <- rbind(out, v[v$code %in% missing, ])
  }

  out[match(codes, out$code), ]
}

# The dataset (as in load_variables()) for an ACS variable code
acs_label_dataset <- function(code, survey) {
  ifelse(grepl("^DP", code), paste0(survey, "/profile"),
    ifelse(grepl("^S[0-9]", code), paste0(survey, "/subject"),
      ifelse(grepl("^CP", code), paste0(survey, "/cprofile"), survey)))
}

# In partly named `variables`, unnamed elements keep their IDs as names (rather
# than becoming "")
fill_variable_names <- function(variables) {
  if (is.null(variables) || is.null(names(variables))) {
    return(variables)
  }
  blank <- is.na(names(variables)) | !nzchar(names(variables))
  names(variables)[blank] <- variables[blank]
  variables
}

# Upper-case variable IDs (as get_acs() and get_decennial() do), keeping names
toupper_values <- function(variables) {
  if (is.null(variables)) return(NULL)
  stats::setNames(toupper(variables), names(variables))
}

validate_labels_arg <- function(labels) {
  if (!(isTRUE(labels) || isFALSE(labels) || identical(labels, "concept") || identical(labels, "attributes"))) {
    rlang::abort("`labels` must be FALSE, TRUE, \"concept\", or \"attributes\".")
  }
  labels
}

# Add labels to, or rename with labels, a finished get_acs() / get_decennial()
# result.
#   result:    the data as returned with labels = FALSE
#   variables: the `variables` argument (possibly named by the user)
#   lookup:    function(codes) -> data frame of code, label, concept
#   suffix:    wide-column suffixes (ACS: c("E", "M"); decennial: "")
apply_variable_labels <- function(result, labels, variables, output, suffix, lookup) {
  # Attributes only apply to the columns of wide output
  if (identical(labels, "attributes") && output != "wide") {
    rlang::inform("`labels = \"attributes\"` only applies to wide output; use `output = \"wide\"` or `labels = TRUE`.")
    return(result)
  }

  # Variables the user named keep their names; others are identified by code
  user_names <- if (!is.null(names(variables))) names(variables)[nzchar(names(variables))] else character()
  user_codes <- if (length(user_names)) unname(variables[nzchar(names(variables))]) else character()

  if (output == "wide") {
    cols <- setdiff(names(result), c("GEOID", "NAME", "geometry", "summary_est", "summary_moe", "summary_value"))
    stems <- cols
    for (s in suffix[nzchar(suffix)]) {
      ends <- endsWith(cols, s)
      stems[ends] <- substr(cols[ends], 1, nchar(cols[ends]) - nchar(s))
    }
  } else {
    stems <- unique(result$variable)
  }

  # Map each stem in the output back to its variable code
  codes <- ifelse(stems %in% user_names, user_codes[match(stems, user_names)], stems)
  info <- lookup(unique(codes))
  info <- info[match(codes, info$code), ]

  if (identical(labels, "attributes")) {
    for (i in seq_along(cols)) {
      attr(result[[cols[i]]], "label") <- info$label[i]
      attr(result[[cols[i]]], "concept") <- info$concept[i]
    }
    return(result)
  }

  # labels = TRUE / "concept": rename variables the user didn't name
  auto <- !stems %in% user_names
  ustems <- unique(stems[auto])
  uinfo <- info[auto, ][match(ustems, stems[auto]), ]

  if (anyNA(uinfo$label)) {
    rlang::abort(c(
      "No label was found for some variables, so they can't be named from labels:",
      stats::setNames(paste(ustems[is.na(uinfo$label)], collapse = ", "), "x")
    ))
  }

  new <- label_names(ustems, uinfo$label, uinfo$concept, mode = if (isTRUE(labels)) "label" else "concept")

  if (any(new %in% user_names)) {
    rlang::abort(c(
      "Label-based names would duplicate names you supplied:",
      stats::setNames(paste(new[new %in% user_names], collapse = ", "), "x")
    ))
  }

  map <- stats::setNames(new, ustems)

  if (output == "wide") {
    renamed <- ifelse(auto, paste0(map[stems], substring(cols, nchar(stems) + 1)), cols)
    names(result)[match(cols, names(result))] <- renamed
  } else {
    hit <- result$variable %in% ustems
    result$variable[hit] <- map[result$variable[hit]]
  }

  result
}
