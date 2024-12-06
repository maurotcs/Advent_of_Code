

# https://adventofcode.com/2020/day/9


#--- Day 9: Encoding Error ---
#   The first step of attacking the weakness in the XMAS data is to find the first number in the list (after the preamble) which is not the sum of two of the 25 numbers before it. What is the first number that does not have this property?

input = as.numeric(readLines('Input/input_09.txt'))
head(input)
summary(input)

valid = function(vec, x){
  pairs = combn(vec, 2)
  sums = unique(apply(pairs, MARGIN = 2, sum))
  out = x %in% sums
  
  return(out)
}
loop = function(num, preamble = 25){
  ll = length(num)
  indexes = seq(preamble+1, ll-1, by = 1)
  sapply(indexes, FUN = function(i){
    valid(vec = num[(i-preamble):i], x = num[i])
  })
}

res = loop(num = input)
index = which(!res)[1]+25
input[index]
# [1] 731031916



#-----------------------------------------------------------
# --- Part Two ---
#   What is the encryption weakness in your XMAS-encrypted list of numbers?

target = input[index]

run_lines = function(ind, vec, targ){
  vec = vec[ind:length(vec)]
  summed = cumsum(vec)
  if(targ %in% summed){
    aux = which(summed == targ)
    out = vec[1:aux]
  } else{
    out = NA_real_
  }
  return(out)
}
find_target = function(ind, target, num){
  start = seq(ind, length(num)-3, by = 1)
  listOFlist = lapply(start, FUN = run_lines, vec = num, targ = target)
  len = sapply(listOFlist, length)
  if(any(len > 1)){
    out = listOFlist[[which(len > 1)]]
  } else{
    out = NA_real_
  }
  return(out)
}

res = parallel::mclapply(1:(length(input)-4), FUN = find_target,
                         target = target, num = input, 
                         mc.cores = 3)
found = res[which(sapply(res, length) > 1)[1]][[1]]
found
sum(found)
target
sum(range(found))
# [1] 93396727