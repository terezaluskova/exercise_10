### Task 1
# * In R, implement a function `SuffixArray()` to create a suffix array from a string.
# 
# * Input:
#   * A `DNAString` object .
# 
# * Output:
#   * A vector of integers.
library(Biostrings)
DNAString <- ("CTAATAATG")

SuffixArray <- function(DNAString){
  s <- as.character(DNAString)
  n <- nchar(DNAString)
  suffix <- character(n)
  for (i in 1:n){
    suffix[i] <- substr(s, i, n)
  }
  order(suffix)
  
}

SuffixArray(DNAString)

### Task 2
# * In R, implement a function `InverseSuffixArray()` to create an inverse suffix array from a suffix array.
# 
# * Input:
# * A vector of integers representing suffix array.
# 
# * Output:
# * A vector of integers.

InverseSuffixArray <- function(suffix){
  len_sa <- length(suffix)
  isa <- integer(len_sa)
  for (i in 1:len_sa){
    isa[suffix[i]] <- i 
  }
  return(isa)
}
InverseSuffixArray(SuffixArray(DNAString))


### Task 3
# * In R, implement a function `LCPArray()` according to pseudocode.
# 
# * Input:
#   * `text` A `DNAString` representing analyzed string.
# * `SA` A vector of integers representing a suffix array.
# * `ISA` A vector of integers representing an inverse suffix array.
# 
# * Output:
#   * `LCP` A vector of integers.
# 
# **Hint:** 
#   The text will be indexed at *m* + 1 position, that does not exist. Add one character at the end of the text
# (in general use `$`, for `DNAString` in R use `+`).

LCPArray <- function(text, SA, ISA){
  m <- nchar(text)
  LCP <- integer(m)
  l <- 0
  for (i in 2:m){
    j <- ISA[i]
    if (j > 1){
      k <- SA[j - 1]
      while  (substring(text, k + l, k + l) == substring(text, i + l, i + l)){
        l <- l + 1}}
    LCP[j] <- l
    l <- max(l - 1, 0)
  }
  LCP[1] <- -1
  LCP[m+1] <- -1
  
  return(LCP)
}

LCPArray(DNAString, SuffixArray(DNAString), InverseSuffixArray(SuffixArray(DNAString)))

