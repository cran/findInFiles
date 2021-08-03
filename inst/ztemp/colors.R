library(vctrs)

data <- read.table(text="file line code
'folder/f.R' 1 'f <- function(x){'
'folder/subfolder/g.R' 1 'g <- function(y){'
'folder/subfolder/subsubfolder/h.R' 1 'h <- function(z){'
",header=T)

function_red <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = "vctrs_function_red")
}

format.vctrs_function_red <- function(x,...) {
  gsub("function",crayon::red("function"),vec_data(x))
}

data$code <- function_red(data$code)
( tbl <- tibble::tibble(data) )

vctrs::vec_data(tbl$code)

ansi

fff <- function(x) { # nice!
  new_vctr(gsub("function", crayon::red("function"), x))
}

redify <- function(x, pattern, perl = FALSE, wholeWord = FALSE){
  if(wholeWord){
    pattern <- sprintf("\\b%s\\b", pattern)
  }
  starts <- gregexpr(pattern, x, perl = perl)[[1L]]
  if(starts[1L] == -1L){
    return(x)
  }
  ends <- starts + attr(starts, "match.length") - 1L
  n <- length(starts)
  replacements <- character(n)
  for(i in seq_len(n)){
    subString <- substr(x, starts[i], ends[i])
    replacements[i] <- crayon::red(subString)
  }
  new_vctr(stringi::stri_sub_replace_all(
    x, starts, ends, replacement = replacements
  ))
}
