from collections import defaultdict
from itertools import product
import random

# We don't care about the outer X's in our result
# All we care about is
# 1) The previous value of the center X
# 2) What the center 3x3 looks like as "output"
# X X X X X
# X O O O X
# X O X O X
# X O O O X
# X X X X X
BOARD_SIZE = 20


def to_board(l):
  board = defaultdict(int)
  for i, v in enumerate(l):
    x = i % BOARD_SIZE
    y = i / BOARD_SIZE
    board[(x, y)] = v
  return board


def random_board(fill=None):
  if not fill:
    fill = random.random()
  board = defaultdict(int)
  for sy in xrange(BOARD_SIZE):
    for sx in xrange(BOARD_SIZE):
      board[(sx, sy)] = 1 if random.random() > fill else 0
  return board


def print_board(b):
  print '=-=-=-='
  for sy in xrange(BOARD_SIZE):
    for sx in xrange(BOARD_SIZE):
      print 'X' if b[(sx, sy)] else '-',
    print
  print '=-=-=-='


def compute_board(board):
  new_board = defaultdict(int)
  for sy in xrange(BOARD_SIZE):
    for sx in xrange(BOARD_SIZE):
      new_board[(sx, sy)] = to_live[(board[(sx, sy)], neighbours(board, sx, sy))]
  return new_board


def living_board(board):
  return sum(board.values()) > 0


def mark_board(guess, gold):
  return sum(guess[k] != gold[k] for k in gold), BOARD_SIZE ** 2


def forward_board_error():
  pass


def neighbours(board, sx, sy):
  return sum(board[(x, y)] for x in xrange(sx - 1, sx + 2) for y in xrange(sy - 1, sy + 2)) - board[(sx, sy)]


def get_block(board, sx, sy, jump=1):
  # Note: jump = 1 is equivalent to a 3x3
  return ''.join(str(board[(x, y)]) for x in xrange(sx - jump, sx + 1 + jump) for y in xrange(sy - jump, sy + 1 + jump))


def apply_block(board, sx, sy, pattern, jump=1, additive=True):
  i = 0
  for y in xrange(sy - jump, sy + 1 + jump):
    for x in xrange(sx - jump, sx + 1 + jump):
      if additive and int(pattern[i]):
        board[(x, y)] = 1
      else:
        board[(x, y)] = int(pattern[i])
      i += 1

# All ways to live: (CENTER, # OF NEIGHBOURS)
to_live = defaultdict(int, {(1, 2): 1, (1, 3): 1, (0, 3): 1})

if __name__ == '__main__':
  results = defaultdict(lambda: defaultdict(int))
  print 'Processing {} boards'.format(2 ** (BOARD_SIZE * BOARD_SIZE))
  for i, l in enumerate(product([0, 1], repeat=BOARD_SIZE * BOARD_SIZE)):
    if i and i % 10000 == 0:
      print '... at board {} ...'.format(i)
      #print results
      #print '... at board {} ...'.format(i)

    # "Warm-up" the board
    boards = [random_board()]
    for i in xrange(5):
      boards.append(compute_board(boards[-1]))
    # Generate a next step
    boards.append(compute_board(boards[-1]))
    # Skip if the board is dead
    if not living_board(boards[-1]):
      print 'All dead...'
      continue

    """
    if board[(2, 2)]:
      results[''.join(r)]['on'] += 1
    else:
      results[''.join(r)]['off'] += 1
    """
    if True:
      print_board(boards[-1])
      print '...TO...'
      print_board(boards[-2])
      print

  fn = 'reverse_mapping.csv'
  f = open(fn, 'w')
  f.write('key, on, off\n')
  for key in sorted(results):
    f.write('"{}", {}, {}\n'.format(key, results[key]['on'], results[key]['off']))
