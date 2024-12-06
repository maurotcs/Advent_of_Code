

# https://adventofcode.com/2020/day/4

# --- Day 4: Passport Processing ---
#   Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?

input = readLines('Input/input_04.txt')
head(input)
str(input)
tail(input)
length(input)

fields = data.frame(key = c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid', 'cid'),
                    value = c('Birth Year', 'Issue Year', 'Expiration Year', 'Height', 'Hair Color', 'Eye Color', 'Passport ID', 'Country ID'),
                    stringsAsFactors = FALSE)
fields

get_keys = function(char, blanks = ' ', sep = ':'){
  char = strsplit(char, split = blanks)[[1]]
  char = sapply(char, function(x) strsplit(x, split = sep)[[1]][1])
  return(char)
}


index = which(input == '')
index = data.frame(start = c(1, index + 1),
                   end = c(index - 1, length(input)) )

ok = apply(index, MARGIN = 1, FUN = function(vec){
  char = paste(input[vec[1]:vec[2]], collapse = ' ')
  keys = get_keys(char)
  return(all(fields$key[-nrow(fields)] %in% keys))
})


sum(ok)
# [1] 202


#-------------------------------------------------------
# --- Part Two ---
#   Count the number of valid passports - those that have all required fields and valid values. Continue to treat cid as optional. In your batch file, how many passports are valid?

library(stringr)

check_byr = function(char){ # Birth year
  # 4 digits
  if(grepl(x = char, pattern = '^[0-9]{4}$')){
    year = as.numeric(char)
    # correct range
    if(year >= 1920 & year <= 2002){
      return(TRUE)
    }
  }
  
  return(FALSE)
}
check_iyr = function(char){ # Issue Year
  # 4 digits
  if(grepl(x = char, pattern = '^[0-9]{4}$')){
    year = as.numeric(char)
    # correct range
    if(year >= 2010 & year <= 2020){
      return(TRUE)
    }
  }
  
  return(FALSE)
}
check_eyr = function(char){ # Expiration Year
  # 4 digits
  if(grepl(x = char, pattern = '^[0-9]{4}$')){
    year = as.numeric(char)
    # correct range
    if(year >= 2020 & year <= 2030){
      return(TRUE)
    }
  }
  
  return(FALSE)
}
check_hgt = function(char){ # Height
  # a number followed by either cm or in
  if(!grepl(pattern = '^[0-9]+[A-z]{2}$', x = char)){
    return(FALSE)
  }
  
  # extract parts
  num = as.numeric(str_extract(string = char, pattern = '[0-9]+'))
  unit = tolower(str_extract(string = char, pattern = '[A-z]{2}'))
  
  if(unit == 'cm'){
    # If cm, the number must be at least 150 and at most 193.
    if(num >= 150 & num <= 193){
      return(TRUE)
    }
  } else if(unit == 'in'){
    # If in, the number must be at least 59 and at most 76.
    if(num >= 59 & num <= 76){
      return(TRUE)
    }
  }
  
  return(FALSE)
}
check_hcl = function(char){ # Hair Color
  # a # followed by exactly six characters 0-9 or a-f.
  if(grepl(pattern = '^#{1}[0-9a-z]{6}$', x = char)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
eye_color = c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
check_ecl = function(char){ # Eye Color
  # exactly one of: amb blu brn gry grn hzl oth.
  if(length(char) == 1){
    if(char %in% eye_color){
      return(TRUE)
    } 
  }
  return(FALSE)
}
check_pid = function(char){ # Passport ID
  # a nine-digit number, including leading zeroes.
  if(grepl(pattern = '^[0-9]{9}$', x = char)){
    return(TRUE)
  }
  return(FALSE)
}
check_cid = function(char){TRUE}

pre_check = cbind(index, ok)
valid = apply(pre_check, MARGIN = 1, FUN = function(vec){
  if(vec[3]){
    char = paste(input[vec[1]:vec[2]], collapse = ' ')
    char = strsplit(char, split = ' ')[[1]]
    char = sapply(char, FUN = strsplit, split = ':')
    
    checks = sapply(char, FUN = function(char){
      fun = paste0('check_', char[1])
      get(fun)(char[2])
    })
    
    if(all(checks)){
      return(TRUE)
    }
  }
  
  return(FALSE)
})


sum(valid)
# [1] 137






