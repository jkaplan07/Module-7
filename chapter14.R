###Julia Kaplan
###Chapter 14: Strings

####14.1 Introduction ----
#install.packages("stringr")
library(tidyverse)
library(stringr)

###14.2 String basics----
#you can create strings with either single quotes or double quotes - doesn't matter.  Best practice is to use double quotes ("") instead of single quotes ('')

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

#if you forget to close a string you will see a + in the console.  Press escape and try again.

#to include a literal single or double quote in a string you can use / to "escape" it:
  double_quote <- "\"" # or '"'
  single_quote <- '\'' # or "'"
#thus if you want to include a literal backslash you need to double it up "\\"

#printed representation fo a string is not the same thing as a string itself - printed respresentation shows the escepes.  To see raw contents of string, use: writeLines()
  
  x <- c("\"", "\\")
  x
  #> [1] "\"" "\\"
  writeLines(x)
  #> "
  #> \
#there are a handful of other special characters.  "\" "\t" for example.  
#multiple strings are often stored in a character vector, which you ccan create with c()

#string length
  #str_length() tells you the number of characters in a string
str_length(c("a", "R for data science", NA)) 
  
#to combine strings, use str_c()
str_c("x", "y")
#> [1] "xy"
str_c("x", "y", "z")
#> [1] "xyz"

#sep argument controls how they are separated.
str_c("x", "y", sep = ", ")

#missing values are contiguous.  If you want them to print as "NA" you need to use str_replace_NA()
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

#str_c() is vectorized and automatically recycles shorter vectors to the same length as the longest.
str_c("prefix-", c("a", "b", "c"), "-suffix")

#objects of legnth 0 are silently dropped.  particulary useful in conjunction with if:
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

#collapse: collapse a vector of 2 strings into a single string.
str_c(c("x", "y", "z"), collapse = ", ")

#subsetting strings
#you can extract parts of a string using str_sub().  Str_sub() also takes tart and end arguments giveing the (inclusive) position of the substring.  Negative substrings work from the end. str_sub() won't fail if string is too short.  it will just return as much as it can.
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)

#can use an assignment form of str_sub() to modify strings.  
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

#str_to_lower() to change the text to lower case. You can also use str_to_upper() or str_to_title().  Different languages have different rules for changing case.  YOu can pick which set of rules to use by specifying a locale.

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en") # English
#> [1] "apple" "banana" "eggplant"
str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple" "eggplant" "banana"

# Turkish has two i's: with and without a dot, and it
# has a different rule for capitalising them:
str_to_upper(c("i", "ı"))
#> [1] "I" "I"
str_to_upper(c("i", "ı"), locale = "tr")
#> [1] "İ" "I"

#sorting: If you want robust behaviour across different computers, you may want to use str_sort() and str_order() which take an additional locale argument.

####14.2.5 Exercises ----
#1. In code that doesn’t use stringr, you’ll often see paste() and paste0() . What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA ?
paste("foo", "bar")
#> [1] "foo bar"
paste0("foo", "bar")
#> [1] "foobar"
str_c("foo", "bar")
#> [1] "foobar"

str_c("foo", NA)
#> [1] NA
paste("foo", NA)
#> [1] "foo NA"
paste0("foo", NA)
#> [1] "fooNA"

#2. In your own words, describe the difference between the sep and collapse arguments to str_c().
#The sep argument is the string inserted between arguments to str_c(), while collapse is the string used to separate any elements of the character vector into a character vector of length one.

#3. Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?
x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
m <- ceiling(L / 2)
str_sub(x, m, m)
#> [1] "a" "b" "b" "c" "c"
#4. What does str_wrap() do? When might you want to use it?
 #The function str_wrap() wraps text so that it fits within a certain width. This is useful for wrapping long strings of text to be typeset.
#5. What does str_trim() do? What’s the opposite of str_trim() ?
str_trim(" abc ")
#> [1] "abc"
str_trim(" abc ", side = "left")
#> [1] "abc "
str_trim(" abc ", side = "right")
#> [1] " abc"

str_pad("abc", 5, side = "both")
#> [1] " abc "
str_pad("abc", 4, side = "right")
#> [1] "abc "
str_pad("abc", 4, side = "left")
#> [1] " abc"

#6. Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c . Think carefully about what it should do if given a vector of length 0, 1, or 2.
str_commasep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    # no comma before and when n == 2
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    # commas after all n - 1 elements
    not_last <- str_c(x[seq_len(n - 1)], delim)
    # prepend "and" to the last element
    last <- str_c("and", x[[n]], sep = " ")
    # combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}
str_commasep("")
#> [1] ""
str_commasep("a")
#> [1] "a"
str_commasep(c("a", "b"))
#> [1] "a and b"
str_commasep(c("a", "b", "c"))
#> [1] "a, b, and c"
str_commasep(c("a", "b", "c", "d"))
#> [1] "a, b, c, and d"