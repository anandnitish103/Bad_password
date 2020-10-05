# Importing the tidyverse library

library(tidyverse)
# Loading users.csv 
users <- read_csv("users.csv")

# Counting how many users we've got

dim(users)
# Taking a look at the 12 first users

head(users,12)

#Verifiers SHALL require subscriber-chosen memorized secrets to be
#at least 8 characters in length.
# Calculating the lengths of users' passwords

users$length <- str_length(users$password)

# Flagging the users with too short passwords

users$too_short <- ifelse(users$length < 8 , TRUE , FALSE)

# Counting the number of users with too short passwords

users %>% filter(too_short == TRUE) %>% count()

# Taking a look at the 12 first rows

head(users,12)

#verifiers SHALL compare the prospective secrets against a list that
#contains values known to be commonly-used, expected, or compromised.

# -> Passwords obtained from previous breach corpuses.
# -> Dictionary words.
# -> Repetitive or sequential characters (e.g. ‘aaaaaa’, ‘1234abcd’).
# -> Context-specific words, such as the name of the service, the username, and derivatives thereof.

# Reading in the top 10000 passwords

common_passwords <- read_lines("10_million_password_list_top_10000.txt")

# Taking a look at the top 100

head(common_passwords,100)



# Flagging the users with passwords that are common passwords

users$common_password <- users$password %in% common_passwords

# Counting the number of users using common passwords

sum(users$common_password)

# Taking a look at the 12 first rows

head(users,12)

# It is easy for hackers to check users' passwords against common English
#words and therefore common English words make bad passwords. 
#Let's check our users' passwords against the top 10,000 English words

# Reading in a list of the 10000 most common words

words <- read_lines("google-10000-english.txt")

# Flagging the users with passwords that are common words

users$common_word <- users$password %in% words

# Counting the number of users using common words as passwords

sum(users$common_word)

# Taking a look at the 12 first rows

head(users,12)

# Extracting first and last names into their own columns

users$first_name <- str_extract(users$user_name, "^\\w+")
users$last_name <- str_extract(users$user_name, "\\w+$")

# Flagging the users with passwords that matches their names

users$uses_name <- str_to_lower(users$password) == users$first_name |
  str_to_lower(users$password) == users$last_name

# Counting the number of users using names as passwords

sum(users$uses_name)

# Taking a look at the 12 first rows

head(users, 12)

#verifiers SHALL compare the prospective secrets [so that they don't contain]
#repetitive or sequential characters (e.g. ‘aaaaaa’, ‘1234abcd’).

# Splitting the passwords into vectors of single characters

split_passwords <- str_split(users$password, "")

# Picking out the max number of repeat characters for each password

users$max_repeats <- sapply(split_passwords, function(split_password) {
  rle_password <- rle(split_password)
  max(rle_password$lengths)
})

# Flagging the users with passwords with >= 4 repeats

users$too_many_repeats <- users$max_repeats >= 4

# Taking a look at the users with too many repeats

users[users$too_many_repeats,]



# Flagging all passwords that are bad

users$bad_password <- users$too_short | 
  users$common_password |
  users$common_word |
  users$uses_name |
  users$too_many_repeats

# Counting the number of bad passwords

sum(users$bad_password)

# Looking at the first 100 bad passwords

head(users$password[users$bad_password], 100)

#Verifiers SHOULD NOT impose other composition rules
#(e.g., requiring mixtures of different character types or prohibiting 
#consecutively repeated characters) for memorized secrets. 
#Verifiers SHOULD NOT require memorized secrets to be changed arbitrarily (e.g., periodically).  


# Enter a password that passes the NIST requirements

new_password <- "Nitish@103"


