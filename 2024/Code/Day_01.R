

# --- Day 1: Historian Hysteria ---
# Your actual left and right lists contain many location IDs. What is the total distance between your lists?

input = readLines('../Input/input_01.txt')
head(input)
tail(input)

# processing
listID = strsplit(input, split = '   ', fixed = TRUE)
matrixID = as.matrix(sapply(listID, as.numeric))
dfID = data.frame('lista.1' = sort(matrixID[1, ]),
                  'lista.2' = sort(matrixID[2, ]))

# total distance calculation
diffs = abs(dfID[['lista.1']] - dfID[['lista.2']])
sum(diffs) # 1765812



#---------------------------------------------------------
# --- Part Two ---
# Once again consider your left and right lists. What is their similarity score?

#similarity score calculation
countID = sapply(dfID[['lista.1']], function(x) sum(x == dfID[['lista.2']]))
sum(dfID[['lista.1']]*countID) # 20520794
