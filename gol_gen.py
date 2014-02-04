from collections import defaultdict
from itertools import product

# We don't care about the outer X's in our result
# All we care about is
# 1) The previous value of the center X
# 2) What the center 3x3 looks like as "output"
# X X X X X
# X O O O X
# X O X O X
# X O O O X
# X X X X X
BOARD_SIZE = 5


def to_board(l):
  board = defaultdict(int)
  for i, v in enumerate(l):
    x = i % 5
    y = i / 5
    board[(x, y)] = v
  return board


def print_board(b):
  print '=-=-=-='
  for sy in xrange(BOARD_SIZE):
    for sx in xrange(BOARD_SIZE):
      print b[(sx, sy)],
    print
  print '=-=-=-='


def neighbours(board, sx, sy):
  return sum(board[(x, y)] for x in xrange(sx - 1, sx + 2) for y in xrange(sy - 1, sy + 2))


# All ways to live: (CENTER, # OF NEIGHBOURS)
to_live = defaultdict(int, {(1, 2): 1, (1, 3): 1, (0, 3): 1})

print 'Processing {} boards'.format(2 ** (5 * 5))
for i, l in enumerate(product([0, 1], repeat=5 * 5)):
  if i % 10000 == 0:
    print '... at board {} ...'.format(i)
  board = to_board(l)
  new_board = defaultdict(int)
  for sy in xrange(1, BOARD_SIZE - 1):
    for sx in xrange(1, BOARD_SIZE - 1):
      new_board[(sx, sy)] = to_live[(board[(sx, sy)], neighbours(board, sx, sy))]
  if False:
    print_board(board)
    print '...TO...'
    print_board(new_board)
    print
