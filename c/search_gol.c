#include "stdio.h"
#include "stdlib.h"
#include "gol_utils.h"

int _recurse_board(int *guess, int *end, int *unhappy_pixels, int unhappy_index, int *possible, int *found) {
  // If all pixels are happy, make sure it maps to a real board, then add to found
  if (!unhappy_pixels[unhappy_index]) {
    // Ensure the guess board is equal to the end board
    // If so, add all on pixels to the found
  }
  // Work out all possible ons and offs
  // For each possibility
  for (int p = 0; p < 0; ++p) {
    // Turn on the pixels and set them to 'not possible'
    // TODO(Optimization): If pixel has exactly three neighbours, none of the neighbours are possible
    // Evolve the board and if the target pixel is on and no 'bad' ones are turned on, recurse
    // Set the target pixel on, evolve the board, if no 'bad' ones are turned on, recurse
    // Turn off all the pixels that were turned on
  }
  return 0;
}

int *recurse_board(int *end) {
  int *possible = calloc(BOARDX * BOARDY, sizeof(int));
  int *guess = calloc(BOARDX * BOARDY, sizeof(int));
  int *found = calloc(BOARDX * BOARDY, sizeof(int));
  int *unhappy_pixels = calloc(BOARDX * BOARDY, sizeof(int));
  // Work out which pixels are unhappy (i.e. need to be turned on)
  int unhappy_index = 0;
  for (int i = 0; i < BOARDY; i++) {
    for (int j = 0; j < BOARDX; j++) {
      unhappy_pixels[unhappy_index] = end[i * BOARDX + j];
      ++unhappy_index;
    }
  }
  // Turn on all the neighbours of unhappy pixels in possible
  for (int i = 0; i < BOARDY; i++) {
    for (int j = 0; j < BOARDX; j++) {
      if (end[i * BOARDX + j]) {
        for (int k = (i == 0 ? 0 : i-1); k <= (i == BOARDY-1 ? BOARDY-1 : i+1); k++) {
          for (int l = (j == 0 ? 0 : j-1); l <= (j == BOARDX-1 ? BOARDX-1 : j+1); l++) {
            // Ensure we're not turning on the pixel itself
            if (!(k == i && l == j))
              possible[k * BOARDX + l] = 1;
          }
        }
      }
    }
  }
  print_board(end);
  print_board(possible);
  //
  int total_found = _recurse_board(guess, end, unhappy_pixels, unhappy_index, possible, found);
  free(possible);
  return found;
}

int main(int argc, const char *argv[]) {
  int *board = calloc(BOARDX * BOARDY, sizeof(int));
  board[9 * BOARDX + 9] = 1;
  board[8 * BOARDX + 9] = 1;
  board[7 * BOARDX + 9] = 1;

  //print_board(board);
  recurse_board(board);
  //print_board(board);

  free(board);
  return 0;
}
