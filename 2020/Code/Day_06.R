

# https://adventofcode.com/2020/day/6


# --- Day 6: Custom Customs ---
#   For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?


input = readLines('Input/input_06.txt')
head(input)
tail(input)


break_groups = function(vec){
  index = which(vec == '')
  index = data.frame(start = c(1, index+1),
                     end = c(index-1, length(vec)))
  out = apply(index, MARGIN = 1, FUN = function(line){
    vec[line[1]:line[2]]
  })
  return(out)
}

groups = break_groups(input)
merged = lapply(groups, FUN = function(x) paste0(x, collapse = ''))
yes = sapply(merged, FUN = function(x){
  x = strsplit(x, split = '')[[1]]
  return(unique(x))
  })
Nyes = sapply(yes, length)

sum(Nyes)
# [1] 6170



#-----------------------------------------------------
# --- Part Two ---
#   For each group, count the number of questions to which everyone answered "yes". What is the sum of those counts?

check_group = function(group, answer, sum = TRUE){
  out = sapply(answer, FUN = function(x){
    aux = sapply(group, FUN = function(g) grepl(pattern = x, x = g))
    all(aux)
  })
  
  if(sum){
    return(sum(out))
  } else{
    return(out)
  }
}

groups = break_groups(input)
corrected = mapply(FUN = function(gg, ans){
  check_group(group = gg, answer = ans, sum = TRUE)
}, gg = groups, ans = yes)

sum(corrected)
# [1] 2947



