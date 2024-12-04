

# --- Day 3: Mull It Over ---
# "Our computers are having issues, so I have no idea if we have any Chief Historians in stock! You're welcome to check the warehouse, though," says the mildly flustered shopkeeper at the North Pole Toboggan Rental Shop. The Historians head out to take a look.
# The shopkeeper turns to you. "Any chance you can see why our computers are having issues again?"
# The computer appears to be trying to run a program, but its memory (your puzzle input) is corrupted. All of the instructions have been jumbled up!
# It seems like the goal of the program is just to multiply some numbers. It does that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. Similarly, mul(123,4) would multiply 123 by 4.
# However, because the program's memory has been corrupted, there are also many invalid characters that should be ignored, even if they look like part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.
# For example, consider the following section of corrupted memory:
# xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

# Only the four highlighted sections are real mul instructions. Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).
# Scan the corrupted memory for uncorrupted mul instructions. What do you get if you add up all of the results of the multiplications?

input = readLines('../Input/input_03.txt')
str(input)
input = paste0(input, collapse='')
str(input)

# find the valid entries
aux = gregexpr("mul\\([0-9]{0,3},[0-9]{0,3}\\)", text = input)
# extract the length of the strings
len = attr(aux[[1]], 'match.length')-1
# extract the indice of the start of the strings
start = aux[[1]]
# initialize a list to store the results
expr = list()
# initialize a list to store ONLY numbers
numb = list()
# break apart the input to be sub-divided
lista = strsplit(x = input, split = '')[[1]]
# use a loop to extract all the valid entries
for(i in 1:length(start)){
  expr[[i]] = lista[start[i]:(start[i]+len[i])]
  aux = paste0(expr[[i]][5:(length(expr[[i]])-1)], collapse = '')
  numb[[i]] = as.numeric(strsplit(aux, split=',')[[1]])
}

sum(sapply(numb, prod)) # 161085926



#-----------------------------------------------------------------
# --- Part Two ---
# As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.
# There are two new instructions you'll need to handle:
# The do() instruction enables future mul instructions.
# The don't() instruction disables future mul instructions.
# Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.

# For example:
# xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))

# This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.
# This time, the sum of the results is 48 (2*4 + 8*5).

# Handle the new instructions; what do you get if you add up all of the results of just the enabled multiplications?


# find the "do()"s
dos = gregexpr("do\\(\\)", text = input)
# find the "don't()"s
donts = gregexpr("don't\\(\\)", text = input)

find_enabled_sections = function(d, dn, full.stop){
  d = append(d, 1, 0)
  ind.d = 1
  start = end = c()
  while(1){
    ind.dn = which(dn > d[ind.d])[1]
    if(is.na(ind.dn)){
      end = append(end, full.stop)
      start = append(start, d[ind.d])
      break
    } 
    end = append(end, dn[ind.dn])
    start = append(start, d[ind.d])
    ind.d = which(d > dn[ind.dn])[1]
    if(is.na(ind.d)){
      break
    }
  }
  
  enabled = data.frame(start, end)
  
  return(enabled)
}

# extract the enabled sections
enabled = find_enabled_sections(dos[[1]], donts[[1]], length(lista))
# expand the data.frame
enabled.vector = unlist(apply(enabled, MARGIN = 1, function(x) x[1]:x[2]))
# find the enabled commands
enabled.boolean = sapply(start, function(x) x %in% enabled.vector)
# and extract them
numb.enabled = numb[enabled.boolean]

sum(sapply(numb.enabled, prod)) # 82045421
