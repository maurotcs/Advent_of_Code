

# --- Day 3: Mull It Over ---
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
