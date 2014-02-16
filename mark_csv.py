import csv
import sys

print 'Marking {} against {}'.format(sys.argv[1], sys.argv[2])

# Open the files
fA = open(sys.argv[1], 'rb')
fB = open(sys.argv[2], 'rb')

error, total = 0, 0
# Put the lines of the two files next to each other
lines = zip(enumerate(csv.reader(fA)), enumerate(csv.reader(fB)))
for (i, rowA), (j, rowB) in lines:
  if i == 0:
    continue
  if i % 10000 == 0:
    print error / float(total)
  boardA = rowA[2:]
  boardB = rowB[2:]
  error += sum(1 if x != y else 0 for x, y in zip(boardA, boardB))
  total += len(rowA)

print 'Final error:'
print error / float(total)