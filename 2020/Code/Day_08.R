

# https://adventofcode.com/2020/day/8



#--- Day 8: Handheld Halting ---
#   Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?

input = readLines('Input/input_08.txt')
head(input)

instructions = vapply(X = input, FUN = function(x) strsplit(x, split = ' ')[[1]],
                      FUN.VALUE = character(2))
instr = data.frame(operation = instructions[1, ],
                   argument = as.numeric(instructions[2, ]),
                   order = NA_real_,
                   accumulator = NA_real_,
                   stringsAsFactors = FALSE)
head(instr)

count = step = 1
acc = 0
while(is.na(instr[step, 'order'])){
  oprt = instr[step, 'operation']
  if(oprt == 'acc'){
    acc = acc + instr[step, 'argument']
    instr[step, 'accumulator'] = acc
  }
  instr[step, 'order'] = count
  step = step + ifelse(oprt == 'jmp', instr[step, 'argument'], 1)
  count = count + 1
}
count
head(instr)
summary(instr)
acc
# [1] 1317


#--------------------------------------------------
#--- Part Two ---
#   Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?

instr2 = data.frame(operation = instructions[1, ],
                   argument = as.numeric(instructions[2, ]),
                   order = NA_real_,
                   accumulator = NA_real_,
                   stringsAsFactors = FALSE)
nrow(instr2)
sum(instr2$operation == 'jmp')
sum(instr2$operation == 'nop')
# too many to do by hand!

run_instr = function(dat, ind, ll){
  dat[ind, 'operation'] = ifelse(dat[ind, 'operation'] == 'jmp', 'nop', 'jmp')
  
  count = step = 1
  acc = 0
  while(is.na(dat[step, 'order'])){
    oprt = dat[step, 'operation']
    if(oprt == 'acc'){
      acc = acc + dat[step, 'argument']
    }
    dat[step, 'accumulator'] = acc
    dat[step, 'order'] = count
    step = step + ifelse(oprt == 'jmp', dat[step, 'argument'], 1)
    if(step > ll){
      break()
    }
    count = count + 1
  }
  return(dat)
}

nr = nrow(instr2)
indexes = which(instr2$operation != 'acc')
all_instr = lapply(indexes, run_instr, dat = instr2, ll = nr)
correct = which(sapply(all_instr, function(x) !is.na(x[nr, 'order'])))
correct
all_instr[[correct]][nr, 'accumulator']
# [1] 1033