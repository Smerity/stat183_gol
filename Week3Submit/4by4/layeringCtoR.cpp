#include <Rcpp.h>
#include <time.h>
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <assert.h>
#include <fstream>
#include <cstring>
#include <iomanip>
#include <math.h>
using namespace Rcpp;

#define T (1 << 25)
#define FILE_LENGTH 20000000
#define NTHREADS (1)

int get_region(int board[24][24], int x, int y) {
  int val = 0;
  for (int k = x; k <= x + 4; k++) {
    for (int l = y; l <= y + 4; l++) {
      val = 2*val + board[k][l];
    }
  }
  return val;
}

// internal helper function to calculate the evolution of
// a 20-by-20 board in the Game of Life
void evolve(int **board)
{
  // copy the board so we can overwrite it
  int copy[20][20];
  for (int i = 0; i < 20; i++) {
    for (int j = 0; j < 20; j++) {
      copy[i][j] = board[i][j];
    }
  }

  // calculate the next step
  for (int i = 0; i < 20; i++) {
    for (int j = 0; j < 20; j++) {
      int surrounding = 0;
      for (int k = (i == 0 ? 0 : i-1); k <= (i == 19 ? 19 : i+1); k++) {
        for (int l = (j == 0 ? 0 : j-1); l <= (j == 19 ? 19 : j+1); l++) {
          if (!(k == i && l == j)) {
            surrounding += copy[k][l];
          }
        }
      }
      board[i][j] = (surrounding == 3) || (surrounding == 2 && copy[i][j]);
    }
  }
  return;
}

// The two C++ functions below are exported to R. You can
// source these functions into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar


// Get the raw predictions of the alive probability in each cell
// INPUT:
//  - guesses: the empirical frequencies for each pattern and each delta
//  - boardData: the final boards
//  - deltas: the delta values of the final boards
//  - i_offset: the column offset of the pattern
//  - j_offset: the row offset of the pattern
// OUTPUT:
//  - predictions: the alive probability of each cell in the beginning boards
// NOTE:
//  - `i_offset` and `j_offset` must match the corresponding values that
//    were used to obtain guesses
// - This function can be exported to R

// [[Rcpp::export]]
NumericVector getPredictionCpp(NumericMatrix guesses, NumericMatrix boardData, NumericVector deltas,
                                  int i_offset, int j_offset) {
  int Nboard = boardData.nrow();
  NumericVector predictions(boardData.nrow() * (boardData.ncol()));
  int finish[24][24] = {{0}};
  int count = 0;
  for (int n= 0; n < Nboard; n++) {
    int delta = deltas(n);
    int posCnt = 0;
    for (int i = i_offset; i < 20+i_offset; i++) {
      for (int j = j_offset; j < 20+j_offset; j++) {
        finish[i][j] = boardData(n, posCnt);
        posCnt++;
      }
    }
    for (int i = i_offset; i < 20+i_offset; i++) {
      for (int j = j_offset; j < 20+j_offset; j++) {
        predictions[count] = guesses(get_region(finish, i - i_offset, j - j_offset), delta-1);
        count++;
      }
    }
  }
  return predictions;
}

// Get the empirical alive probabilities corresponding to each pattern
// INPUT:
//  - N_BOARD: the number of randomly generated training boards
//  - i_offset: the column offset of the pattern
//  - j_offset: the row offset of the pattern
// OUTPUT:
//  - guesses: the empirical alive probabilities
// NOTE:
// - This function can be exported to R

// [[Rcpp::export]]
NumericMatrix getFrequencyCpp(int N_BOARD, int i_offset, int j_offset) {
  //
  srand(time(NULL));
  int dist = abs(i_offset-2) + abs(j_offset-2);
  // initialize some stuff
  printf("-----------------------------------\n");
  printf("Offset = %d %d\n",i_offset,j_offset);
  int **board = new int*[20];
  for (int i = 0; i < 20; i++) {
    board[i] = new int[20];
  }
  int start[24][24] = {{0}}, finish[24][24] = {{0}};
  int **counts = new int*[5];
  int **alive = new int*[5];
  NumericMatrix guesses(T,5);

  for (int i = 0; i < 5; i++) {
    counts[i] = new int[T];
    alive[i] = new int[T];
//    guesses[i] = new float[T];
  }
  // Create N_BOARD number of boards
  for (int t = 0; t <= N_BOARD; t++) {
    if (t % (N_BOARD / 10)  == 0) {printf("Number of training boards done: %d\n", t); fflush(stdout);}
    // generate number of ones and clean board
    int ones = (rand() % 393) + 4;
    for (int i = 0; i < 20; i++)
      for (int j = 0; j < 20; j++)
        board[i][j] = 0;

    // fill randomly with ones
    while (ones > 0) {
      int a = rand() % 20, b = rand() % 20;
      if (!board[a][b]) {
        board[a][b] = 1;
        ones--;
      }
    }

    // evolve 5 steps and write down start board
    for (int i = 0; i < 5; i++)
      evolve(board);
    int boardIsAlive = 0;
    for (int i = 0; i < 20; i++) {
      for (int j = 0; j < 20; j++) {
        boardIsAlive += board[i][j];
        start[i+i_offset][j+j_offset] = board[i][j];
      }
    }
    if (!boardIsAlive) continue;

    // evolve delta steps and write down finish board
    int delta = (rand() % 5) + 1;
    for (int i = 0; i < delta; i++)
      evolve(board);
    delta--;

    for (int i = 0; i < 20; i++) {
      for (int j = 0; j < 20; j++) {
              finish[i+i_offset][j+j_offset] = board[i][j];
      }
    }

    // machine learn on the 5x5 subgrids
    for (int i = i_offset; i < 20+i_offset; i++) {
      for (int j = j_offset; j < 20+j_offset; j++) {
        int val = get_region(finish, i - i_offset, j - j_offset);
        counts[delta][val]++;
        alive[delta][val] += start[i][j];
      }
    }
  }

  printf("Calculating raw guesses...\n");
  // calculate raw guesses
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < T; j++) {
      if (counts[i][j] == 0) counts[i][j] = 1;
      guesses(j,i) = ((float) (alive[i][j]))/(counts[i][j] + 0.001);
    }
  }

  // free allocated memory
  for (int i = 0; i < 20; i++) {
    delete[] board[i];
  }
  delete[] board;

  for (int i = 0; i < 5; i++) {
    delete[] counts[i];
    delete[] alive[i];
  }

  delete[] counts;
  delete[] alive;
  return guesses;
}

// Test function for C++-to-R integration
// [[Rcpp::export]]
int timesTwo(int x) {
  std::cout << "HELLO WORLD!" << std::endl;
  return x * 2;
}
