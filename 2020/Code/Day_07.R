

# https://adventofcode.com/2020/day/7


#--- Day 7: Handy Haversacks ---
#   How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)


input = readLines('Input/input_07.txt')
head(input)
summary(input)

get_inside_bags = function(char){
  char = strsplit(char, split = ' ')[[1]]
  out = list(N = as.numeric(char[1]),
             color = paste(char[2], char[3], sep = '_'))
  
  return(out)
}
get_rule = function(char, sep = ',', word = 'contain'){
  char = strsplit(char, split = word)[[1]]
  # outter
  outter = strsplit(x = char[1], split = ' ')[[1]]
  outter = paste(outter[1], outter[2], sep = '_')
  # contains
  inside = strsplit(x = char[2], split = sep)[[1]]
  inside = gsub(pattern = "^ ", replacement = '', x = inside)
  if(length(inside) == 1){
    if(grepl(pattern = 'no other', x = inside)){
      col = 'nothing'
      n = -999
    } else{
      aux = get_inside_bags(inside)
      col = aux$color
      n = aux$N
    }
  } else{
    aux = lapply(inside, FUN = get_inside_bags)
    col = sapply(aux, '[[', 'color')
    n = sapply(aux, '[[', 'N')
  }
  
  out = data.frame(outter = outter, 
                   color = col,
                   N = n,
                   stringsAsFactors = FALSE)
  
  return(out)
}
find_bag = function(bag, guide){
  index = which(guide$color == bag)
  if(length(index) > 0){
    col1 = guide$outter[index]
    col2 = lapply(col1, find_bag, guide = guide)
    out = c(col1, do.call('c', col2))
  } else{
    out = character(length = 0)
  }
  
  return(out)
}

rulesLL = lapply(input, FUN = get_rule)
rules = do.call('rbind', rulesLL)
head(rules)
summary(rules)
str(rules)

all_colors = find_bag(bag = 'shiny_gold', guide = rules[which(rules$N > 0), ])
colors = unique(all_colors)
length(colors)
# [1] 278



#---------------------------------------------
#--- Part Two ---
#   How many individual bags are required inside your single shiny gold bag?

pack_bag = function(color, guide, mult = 1){
  index = which(guide$outter == color)
  N = guide$N[index]
  inside = guide$color[index]
  
  if(length(index) == 1){
    if(N > 0){
      out = pack_bag(color = inside, guide = guide, mult = N)
    } else{
      out = 0
    }
  } else if(length(index) > 1){
    out = mapply(FUN = function(col, m){
      pack_bag(color = col, guide = guide, mult = m)
      }, col = inside, m = N,
      SIMPLIFY = TRUE)
  }
  out = mult + sum(out*mult)
  
  return(out)
}

all_bags = sort(unique(rules$outter))
contain = sapply(all_bags, pack_bag, guide = rules, mult = 1)
full_count = data.frame(color = all_bags,
                        Nbags = contain - 1,
                        row.names = NULL,
                        stringsAsFactors = FALSE)
full_count[which(full_count$color == 'shiny_gold'), ]
