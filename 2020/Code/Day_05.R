

# https://adventofcode.com/2020/day/5


# --- Day 5: Binary Boarding ---
#   As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass?

input = readLines('Input/input_05.txt')
head(input)
str(input)
tail(input)

plane_divide = function(ll){
  midpoint = (ll$first+ll$last) / 2
  half = ll$half[1]
  
  # stop condition
  if(length(ll$half) == 1){
    if(half == 'F'){
      out = floor(midpoint)
    } else if(half == 'B'){
      out = ceiling(midpoint)
    }else{
      stop(paste0("Something's worng with the last 'half' input - ", half))
    }
    
    return(out)
  } else if(length(ll$half) > 1){
    if(half == 'F'){
      ll$last = floor(midpoint)
    } else if(half == 'B'){
      ll$first = ceiling(midpoint)
    } else{
      stop(paste0("Something's worng with the last 'half' - ", half))
    }
    
    ll$half = ll$half[-1]
    out = plane_divide(ll)
    
    return(out)
  } else{
    stop(paste0("Something's worng with the 'll$half' - ", ll$half))
  }
  
  return(out)
}
find_seat = function(char, max.row = 127, max.col = 7){
  char = strsplit(char, split = '')[[1]]
  
  row = plane_divide(ll = list(half = char[1:7], first = 0, last = max.row))
  aux = sapply(char[8:10], FUN = function(x) ifelse(x == 'R', 'B', 'F') )
  col = plane_divide(ll = list(half = aux, first = 0, last = max.col))
  
  out = structure(c(row, col), names = c("row", 'column'))
  
  return(out)
}
seat_ID = function(vec){
  (vec[['row']] * 8) + vec[['column']]
}

# test
(seat = find_seat('BFFFBBFRRR'))
seat_ID(seat)

# apply it
all_seats = lapply(input, find_seat)
IDs = sapply(all_seats, seat_ID)
max(IDs)
# [1] 858


#--------------------------------------------
# --- Part Two ---
#   What is the ID of your seat?
(missing = which(!(1:max(IDs) %in% IDs)))
diff(missing)







