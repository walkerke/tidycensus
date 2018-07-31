library(stringr)

x <- load_variables(2016, "acs5", cache = TRUE)

# Make sure that all variables start with B, C, DP, S, P, or H followed by a number

x1 <- x[grepl("^B[0-9]|C[0-9]|DP[0-9]|S[0-9]|P[0-9]|H[0-9]", x$name), ]

y1 <- y[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P[0-9]|^H[0-9]", y$name), ]

# Remove the E or M at the end if it is there now


x1 <- x[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P[0-9]|^H[0-9]", x$name), ]

x1$name <- str_replace(x1$name, "E$|M$", "")


x <- load_variables(2014, "acs5")

x1 <- x[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P[0-9]|^H[0-9]", x$name), ]

x1$name <- str_replace(x1$name, "E$|M$", "")

x2 <- x1[!grepl("Margin Of Error|Margin of Error", x1$label), ]




ly <- get_acs(geography = "block group", table = "B99011", state = "VA", county = "Lynchburg city",
        geometry = TRUE, keep_geo_vars = TRUE)


ga <- get_acs(geography = "state", state = "GA", table = "B99011")

ga <- get_acs(geography = "state", state = "GA", table = "B99011")


nom <- c('B99011','B99012','B99021','B99031','B99051','B99052','B99053','B99061','B99071','B99072','B99080', 'B99081','B99082','B99083','B99084','B99092','B99102','B99103','B99104','B99121','B99141', 'B99142', 'B99151','B99152','B99161','B99162','B99163','B99171','B99172','B99191','B99192','B99193','B99194', 'B99201','B99211','B99212','B99231','B99232','B99233','B99234','B99241','B99242','B99243', 'B992510', 'B992511','B992512','B992513','B992514','B992515','B992516','B992518', 'B992519', 'B99252', 'B992520','B992521','B992522','B99253','B99254','B99255','B99256','B99257','B99258','B99259')

x <- load_variables(2016, "acs5", cache = TRUE)

library(tidyverse)

vars_we_need <- map_df(nom, function(g) {
  filter(x, grepl(paste0("^", g), name))
})


get_acs('tract', year = 2016, variable = 'S1701_C01_057', state = '06', output = 'wide')

get_acs('tract', year = 2016, variable = 'S1701_C01_076', state = '06', output = 'wide')

EducationVariables <- c(EduTotal = "B16010_001", Edu1 = "B16010_002", Edu2 = "B16010_015", Edu3 = "B16010_028", Edu4 = "B16010_041")


EducationKeepGeoVars <- get_acs(geography = "zcta",
                                variables = EducationVariables,
                                year = 2011, survey = "acs5",
                                output = "wide", geometry = TRUE,
                                keep_geo_vars=TRUE)

