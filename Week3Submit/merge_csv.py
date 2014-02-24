import csv
import sys

sys.stderr.write('Ensemble merging {} with {}\n'.format(sys.argv[1], sys.argv[2]))

# Open the files
fA = open(sys.argv[1], 'rb')
fB = open(sys.argv[2], 'rb')

lines = zip(enumerate(csv.reader(fA)), enumerate(csv.reader(fB)))
for (i, rowA), (j, rowB) in lines:
  if i == 0:
    print ','.join('"%s"' % x for x in rowA)
    continue
  _id = i
  rowA = map(lambda x: 0 if x == 'NA' else float(x), rowA[-400:])
  rowB = map(lambda x: 0 if x == 'NA' else float(x), rowB[-400:])
  board = ','.join('1' if (0.6 * x + 0.6 * y) > 0.5 else '0' for x, y in zip(rowA, rowB))
  print '{},{}'.format(_id, board)
