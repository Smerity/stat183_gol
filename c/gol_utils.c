#include "stdio.h"
//
#include "gol_utils.h"

int has_neighbour(int *board, int i, int j) {
  int surrounding = 0;
  for (int k = (i == 0 ? 0 : i-1); k <= (i == 19 ? 19 : i+1); k++)
    for (int l = (j == 0 ? 0 : j-1); l <= (j == 19 ? 19 : j+1); l++)
      if (!(k == i && l == j))
        return 1;
  return 0;
}

int neighbours(int *board, int i, int j) {
  int surrounding = 0;
  for (int k = (i == 0 ? 0 : i-1); k <= (i == 19 ? 19 : i+1); k++)
    for (int l = (j == 0 ? 0 : j-1); l <= (j == 19 ? 19 : j+1); l++)
      if (!(k == i && l == j))
        surrounding += board[k * BOARDX + l];
  return surrounding;
}

void print_board(int *board) {
  printf("=-=-=-=-=-=\n");
  for(int y = 0; y < BOARDY; ++y) {
    for(int x = 0; x < BOARDX; ++x) {
      if (board[y * BOARDX + x]) {
        //printf("X");
        printf("%d ", board[y * BOARDX + x]);
      }
      else {
        printf("- ");
      }
    }
    printf("\n");
  }
  printf("=-=-=-=-=-=\n");
}

// Modified from Aaron, Daniel, Christopher
// TODO: Remove all hard coded variables with BOARDX / BOARDY
void evolve(int *board) {
  // copy the board so we can overwrite it
  int copy[20 * 20];
  // TODO(Smerity): Replace with memcpy for improved speed
  for (int i = 0; i < 20; i++)
    for (int j = 0; j < 20; j++)
      copy[i * BOARDX + j] = board[i * BOARDX + j];

  // calculate the next step
  for (int i = 0; i < 20; i++) {
    for (int j = 0; j < 20; j++) {
      int surrounding = neighbours(copy, i, j);
      board[i * BOARDX + j] = (surrounding == 3) || (surrounding == 2 && copy[i * BOARDX + j]);
    }
  }
}
