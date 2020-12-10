

# https://adventofcode.com/2020/day/7


#--- Day 7: Handy Haversacks ---
#   You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.
#   Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
#   For example, consider the following rules:
#     - light red bags contain 1 bright white bag, 2 muted yellow bags.
#     - dark orange bags contain 3 bright white bags, 4 muted yellow bags.
#     - bright white bags contain 1 shiny gold bag.
#     - muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
#     - shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
#     - dark olive bags contain 3 faded blue bags, 4 dotted black bags.
#     - vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
#     - faded blue bags contain no other bags.
#     - dotted black bags contain no other bags.

#   These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.
#   You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
#   In the above rules, the following options would be available to you:
#     - A bright white bag, which can hold your shiny gold bag directly.
#     - A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
#     - A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
#     - A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.

#   So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.
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
#   It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!
#   Consider again your shiny gold bag and the rules from the above example:
#     - faded blue bags contain 0 other bags.
#     - dotted black bags contain 0 other bags.
#     - vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
#     - dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.

#   So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
#   Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
#   Here's another example:
#     - shiny gold bags contain 2 dark red bags.
#     - dark red bags contain 2 dark orange bags.
#     - dark orange bags contain 2 dark yellow bags.
#     - dark yellow bags contain 2 dark green bags.
#     - dark green bags contain 2 dark blue bags.
#     - dark blue bags contain 2 dark violet bags.
#     - dark violet bags contain no other bags.

#   In this example, a single shiny gold bag must contain 126 other bags.
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
