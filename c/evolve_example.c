#include "stdio.h"
#include "stdlib.h"
#include "gol_utils.h"

// To compile:
// gcc -std=c99 gol_utils.c evolve_example.c && ./a.out

int main(int argc, const char *argv[]) {
  int *board = calloc(BOARDX * BOARDY, sizeof(int));
  // Blinker
  board[5 * BOARDX + 5] = 1;
  board[4 * BOARDX + 5] = 1;
  board[3 * BOARDX + 5] = 1;
  // R-pentomino
  board[9 * BOARDX + 9] = 1;
  board[9 * BOARDX + 10] = 1;
  board[8 * BOARDX + 9] = 1;
  board[8 * BOARDX + 7] = 1;
  board[7 * BOARDX + 9] = 1;

  print_board(board);
  evolve(board);
  print_board(board);

  free(board);
  return 0;
}
