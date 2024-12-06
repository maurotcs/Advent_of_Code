

# https://adventofcode.com/2020/day/2


# --- Day 2: Password Philosophy ---
#   How many passwords are valid according to their policies?


input = readLines('Input/input_02.txt')
head(input)
str(input)


process = function(char){
  char = strsplit(char, split = ' ')[[1]]
  
  reps = as.numeric(strsplit(char[1], split = '-')[[1]])
  fixed = strsplit(char[2], split = '')[[1]][1]
  char = char[3]
  
  return(list(range = reps[1]:reps[2],
              fixed = fixed,
              string = char))
}

valid = function(ll){
  chars = strsplit(ll$string, split = '')[[1]]
  count = sum(chars == ll$fixed)
  
  if(count %in% ll$range){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

proc = lapply(input, FUN = process)
validate = sapply(proc, FUN = valid)

sum(validate)
# [1] 603


#--------------------------
# --- Part Two ---
#   How many passwords are valid according to the new interpretation of the policies?

proc2 = function(char){
  char = strsplit(char, split = ' ')[[1]]
  
  reps = as.numeric(strsplit(char[1], split = '-')[[1]])
  fixed = strsplit(char[2], split = '')[[1]][1]
  char = char[3]
  
  return(list(position = reps,
              fixed = fixed,
              string = char))
}

valid2 = function(ll){
  chars = strsplit(ll$string, split = '')[[1]]
  
  one = (chars[ll$position[1]] == ll$fixed)
  two = (chars[ll$position[2]] == ll$fixed)
  
  if(sum(one, two) == 1){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

proc2 = lapply(input, FUN = proc2)
validate2 = sapply(proc2, FUN = valid2)

sum(validate2)
# [1] 404
