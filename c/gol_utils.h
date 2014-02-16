#define BOARDX (20)
#define BOARDY (20)

int has_neighbour(int *board, int x, int y);

int neighbours(int *board, int x, int y);

void print_board(int *board);

void evolve(int *board);
