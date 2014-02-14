#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "gol_utils.h"

#define DEBUG (1)

// Toggle a flip in the given board and the possible board set
void toggle_board(int a, int b, int c, int *board, int *possible) {
  board[a] = !board[a];
  //possible[a] = !possible[a];
  if (b != -1) {
    board[b] = !board[b];
    //possible[b] = !possible[b];
  }
  if (c != -1) {
    board[c] = !board[c];
    //possible[c] = !possible[c];
  }
}

int check_pixel(int *board, int x, int y) {
  int surrounding = neighbours(board, x, y);
  return (surrounding == 3) || (surrounding == 2 && board[y * BOARDX + x]);
}

// Check if the two boards are exactly equivalent after evolving forward the guess
int exact_board(int *guess, int *end) {
  int evolved[400];
  for (int i = 0; i < 400; ++i) evolved[i] = guess[i];
  evolve(evolved);
  for (int i = 0; i < BOARDY; i++) {
    for (int j = 0; j < BOARDX; j++) {
      if (evolved[i * BOARDX + j] != end[i * BOARDX + j]) {
        return 0;
      }
    }
  }
  return 1;
}

int _recurse_board(int *guess, int *end, int *unhappy_pixels, int unhappy_index, int *possible, int *found) {
  static unsigned int calls = 0;
  if (unhappy_index == 0) calls = 0;
  ++calls;
  if (calls % 500000 == 0) {
    printf("%u\n", calls);
  }
  // If all pixels are happy, make sure it maps to a real board, then add to found
  if (unhappy_pixels[unhappy_index] == -1) {
    if (!exact_board(guess, end)) {
      return 0;
    }
    if (DEBUG) {
      print_board(guess);
    }
    // Ensure the guess board is equal to the end board
    // If so, add all on pixels to the found
    for (int i = 0; i < BOARDY; i++) {
      for (int j = 0; j < BOARDX; j++) {
        found[i * BOARDX + j] += guess[i * BOARDX + j];
      }
    }
    return 1;
  }
  // Collect the target pixel we're going to "fix"
  int i, j;
  i = unhappy_pixels[unhappy_index] / BOARDX;
  j = unhappy_pixels[unhappy_index] % BOARDX;
  // Get the nine neighbours of this pixel
  int pixel_ind = 0;
  int pixels[9];
  for (int k = (i == 0 ? 0 : i-1); k <= (i == BOARDY-1 ? BOARDY-1 : i+1); k++) {
    for (int l = (j == 0 ? 0 : j-1); l <= (j == BOARDX-1 ? BOARDX-1 : j+1); l++) {
      if (possible[k * BOARDX + l]) {
        pixels[pixel_ind++] = k * BOARDX + l;
        possible[k * BOARDX + l] = 0;
      }
    }
  }
  // Count how many neighbours are alive
  int n = neighbours(guess, i, j);
  // Work out all possible ons and offs
  int total_found = 0;
  for (int m = 0; m < 2; ++m) {
    // Try with and without the target pixel on
    if (m == 1) toggle_board(i * BOARDX + j, -1, -1, guess, possible);
    //
    for (int a = 0; a < pixel_ind; ++a) {
      // Only add a pixel if starting live neighbours is 0 or 1 or 2
      if (n <= 2) {
        for (int b = a + 1; b < pixel_ind; ++b) {
          // Only add 2 pixels if starting live neighbours is 0 or 1
          if (n <= 1) {
            for (int c = b + 1; c < pixel_ind; ++c) {
              // Add 3 pixels when no neighbours -- valid in all cases
              if (n == 0) {
                toggle_board(pixels[a], pixels[b], pixels[c], guess, possible);
                if (check_pixel(guess, i, j)) total_found += _recurse_board(guess, end, unhappy_pixels, unhappy_index + 1, possible, found);
                toggle_board(pixels[a], pixels[b], pixels[c], guess, possible);
              }
            }
            // Add two pixels if: (n == 1) [results in n == 3] || (target is on && n == 0) [results in n == 2 and alive]
            if (n == 1 || (m == 1 && n == 0)) {
              toggle_board(pixels[a], pixels[b], -1, guess, possible);
              if (check_pixel(guess, i, j)) total_found += _recurse_board(guess, end, unhappy_pixels, unhappy_index + 1, possible, found);
              toggle_board(pixels[a], pixels[b], -1, guess, possible);
            }
          }
        }
        // Only add a single pixel if neighbours is (n == 1 and target is on) [results in n == 2 and alive] or (n == 2) [results in n == 3]
        if ((n == 1 && m == 1) || (n == 2)) {
          toggle_board(pixels[a], -1, -1, guess, possible);
          if (check_pixel(guess, i, j)) total_found += _recurse_board(guess, end, unhappy_pixels, unhappy_index + 1, possible, found);
          toggle_board(pixels[a], -1, -1, guess, possible);
        }
      }
    }
    if (check_pixel(guess, i, j)) total_found += _recurse_board(guess, end, unhappy_pixels, unhappy_index + 1, possible, found);
  }
  // Undo the target pixel toggle on
  toggle_board(i * BOARDX + j, -1, -1, guess, possible);
  // Restore the possible states of the neighbours
  for (int i = 0; i < pixel_ind; ++i) {
    possible[pixels[i]] = 1;
  }
  //
  if (unhappy_index == 0) printf("Total found: %d\n", total_found);
  return total_found;
}

int *recurse_board(int *end) {
  int *possible = calloc(BOARDX * BOARDY, sizeof(int));
  int *guess = calloc(BOARDX * BOARDY, sizeof(int));
  int *found = calloc(BOARDX * BOARDY, sizeof(int));
  int *unhappy_pixels = calloc(BOARDX * BOARDY, sizeof(int));
  for (int i = 0; i < 400; ++i) {
    unhappy_pixels[i] = -1;
  }
  // Work out which pixels are unhappy (i.e. need to be turned on)
  int unhappy_index = 0;
  for (int i = 0; i < BOARDY; i++) {
    for (int j = 0; j < BOARDX; j++) {
      if (end[i * BOARDX + j]) {
        unhappy_pixels[unhappy_index] = i * BOARDX + j;
        ++unhappy_index;
      }
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
  int total_found = _recurse_board(guess, end, unhappy_pixels, 0, possible, found);
  print_board(found);
  free(possible);
  free(guess);
  free(unhappy_pixels);
  return found;
}

int main(int argc, const char *argv[]) {
  int *board = calloc(BOARDX * BOARDY, sizeof(int));

  // Box
  board[13 * BOARDX + 5] = 1;
  board[13 * BOARDX + 6] = 1;
  board[14 * BOARDX + 5] = 1;
  board[14 * BOARDX + 6] = 1;
  // Double box
  board[16 * BOARDX + 5] = 1;
  board[16 * BOARDX + 6] = 1;
  board[17 * BOARDX + 5] = 1;
  board[17 * BOARDX + 6] = 1;
  // Blinker
/*  board[5 * BOARDX + 5] = 1;*/
/*  board[4 * BOARDX + 5] = 1;*/
/*  board[3 * BOARDX + 5] = 1;*/
  // R-pentomino
/*  board[9 * BOARDX + 9] = 1;*/
/*  board[9 * BOARDX + 10] = 1;*/
/*  board[8 * BOARDX + 9] = 1;*/
/*  board[8 * BOARDX + 7] = 1;*/
/*  board[7 * BOARDX + 9] = 1;*/

  //print_board(board);
  int *found = recurse_board(board);
  //print_board(board);

  free(found);
  free(board);
  return 0;
}
