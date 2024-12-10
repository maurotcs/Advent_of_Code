


# --- Day 7: Bridge Repair ---
# Determine which equations could possibly be true. What is their total calibration result?

input = sapply(readLines('../Input/input_07.txt'), strsplit, split = ' ')

results = as.numeric(sapply(input, function(x) gsub(x = x[1], pattern = ':', replacement = '')))
numbers = sapply(input, function(x) x[-1])

check_validity = function(res, numb){
  nn = length(numb)
  possibilities = e1071::bincombinations(nn-1)+1
  operators = c('+', '*')
  out = c()
  for(pp in 1:nrow(possibilities)){
    foo = numb[1]
    for(i in 2:nn){
      foo = paste0(foo, operators[possibilities[pp, i-1]], numb[i])
      foo = eval(parse(text=foo))
    }
    out[pp] = foo
  }
  
  res %in% out
}

valid = sapply(seq_along(results), function(x) check_validity(results[x], numbers[[x]]))
sum(results[valid]) # 5837374519342




#--------------------------------------------------------------------
#--- Part Two ---
# Using your new knowledge of elephant hiding spots, determine which equations could possibly be true. What is their total calibration result?











