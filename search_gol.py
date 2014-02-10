import csv
from collections import defaultdict
from itertools import combinations

from gol_gen import BOARD_SIZE, apply_block, compute_board, mark_board, neighbours, print_board, to_board
BOARD_SIZE, apply_block, compute_board, print_board, to_board


def list_neighbours(sx, sy):
  return [(x, y) for x in xrange(sx - 1, sx + 2) for y in xrange(sy - 1, sy + 2)]


def _recurse_board(guess, end, unhappy_pixels, possible):
  # Take a pixel and make it happy by adding past neighbours (highest 'shared' value first)
  # If it is impossible to make it happy, return with fail case
  # Ensure it doesn't activate any non-alive pixels?
  # Remove all the happy pixel's other possible past neighbour locations from the possible list
  ###
  # FOR pixels IN [ALL COMBINATIONS OF TWO AND THREE NEIGHBOUR PIXELS]:
  #   ENSURE EXPECTED RESULT
  #   REMOVE ALL OTHER POTENTIAL NEIGHBOURS
  #   TRY TO FIX NEXT HAPPY PIXEL
  #   IF SUCCESS, RETURN HAPPY
  #   IF FAIL, CONTINUE LOOP
  ###
  if not unhappy_pixels:
    if compute_board(guess) == end:
      global found
      key = tuple((k) for k in guess if guess[k])
      if key not in found:
        found.add(key)
        if len(found) % 10 == 0:
          print 'Happiness!'
          print 'Guess'
          print_board(guess)
          print 'to'
          print_board(compute_board(guess))
          print 'which should be'
          print_board(end)
      return guess
    return
  ###
  pix = unhappy_pixels.pop()
  dead_neighbours = [p for p in list_neighbours(*pix) if guess[p] == 0 and p in possible]
  living = neighbours(guess, *pix)
  tries = []
  # If the pixel is already happy, it needs nothing -- very zen
  if living == 2 or living == 3:
    tries.append(None)
  # Add one
  if living == 1 or living == 2:
    for p in dead_neighbours:
      tries.append([p])
  # Add two
  if living == 0 or living == 1:
    for p in combinations(dead_neighbours, 2):
      tries.append(p)
  # Add three
  if living == 0:
    for p in combinations(dead_neighbours, 3):
      tries.append(p)
  # Flick on and off the combinations and try them
  global lowest
  global found
  lowest = min(lowest, len(unhappy_pixels))
  print 'Lowest / Current (FOUND):', lowest, len(unhappy_pixels), '(', len(found), ')'
  for ps in tries:
    if ps:
      for p in ps:
        guess[p] = 1
        possible.remove(p)
    ###
    pretend_end = compute_board(guess)
    if pretend_end[pix] == 1 and not [k for k in end if pretend_end[k] == 1 and end[k] == 0]:
      res = _recurse_board(guess, end, unhappy_pixels, possible)
      if res:
        return res
    ###
    guess[pix] = 1
    pretend_end = compute_board(guess)
    if pretend_end[pix] == 1 and not [k for k in end if pretend_end[k] == 1 and end[k] == 0]:
      res = _recurse_board(guess, end, unhappy_pixels, possible)
      if res:
        return res
    guess[pix] = 0
    ###
    if ps:
      for p in ps:
        guess[p] = 0
        possible.add(p)
  unhappy_pixels.append(pix)
lowest = 1000
found = set()


def recurse_board(end):
  global lowest
  global found
  print 'Recursing...'
  lowest = 1000
  found = set()
  guess = defaultdict(int)
  # Each pixel must be satisfied by the new board
  # List of pixels to satisfy
  unhappy_pixels = []
  for sy in xrange(BOARD_SIZE):
    for sx in xrange(BOARD_SIZE):
      if end[(sx, sy)]:
        unhappy_pixels.append((sx, sy))
  unhappy_pixels = unhappy_pixels[::-1]
  # List of possible locations to place pixels
  possible = set()
  for pixel in unhappy_pixels:
    [possible.add(x) for x in list_neighbours(*pixel)]
  results = _recurse_board(guess, end, unhappy_pixels, possible)
  return results


err = []
same = []
all_off = []
mapping = defaultdict(list)
with open('data/train.csv', 'rb') as f:
  for i, row in enumerate(csv.reader(f)):
    if i == 0:
      continue
    elif i % 100 == 0:
      print i
      if err:
        print 'Guess error:', sum(err) / len(err)
        print 'Start=End benchmark error:', sum(same) / len(same)
        print 'All Off benchmark error:', sum(all_off) / len(all_off)

    row = map(int, row)
    delta = row[1]
    start = to_board(row[2:400 + 2])
    end = to_board(row[402:8002])

    if delta == 1:
      if sum(end.values()) < 28:
        guess = recurse_board(end)

        b, t = mark_board(guess, start)
        err.append(b / float(t))
        print 'Err:', b / float(t)
        b, t = mark_board(end, start)
        same.append(b / float(t))
        print 'Same:', b / float(t)
        b, t = mark_board(defaultdict(int), start)
        all_off.append(b / float(t))
        print 'All off:', b / float(t)
