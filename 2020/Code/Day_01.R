

# https://adventofcode.com/2020/day/1


# --- Day 1: Report Repair ---
#   Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?


input = as.numeric(readLines('Input/input_01.txt'))
head(input)
str(input)


pairs = combn(input, 2)
str(pairs)
class(pairs)

sums = apply(pairs, MARGIN = 2, sum)
(selected = which(sums == 2020))
sums[selected]
pairs[1:2 , selected]
prod(pairs[1:2 , selected])
# [1] 485739


#-----------------------------
#   --- Part Two ---
#   In your expense report, what is the product of the three entries that sum to 2020?

pairs3 = combn(input, 3)
str(pairs3)
class(pairs3)

sums3 = apply(pairs3, MARGIN = 2, sum)
(sel3 = which(sums3 == 2020))
sums[sel3]
pairs3[1:3 , sel3]
prod(pairs3[1:3 , sel3])
# [1] 161109702




