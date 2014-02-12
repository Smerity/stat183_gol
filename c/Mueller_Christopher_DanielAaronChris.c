#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#define T 33554432

void evolve(int **board)
{
	// copy the board so we can overwrite it
	int copy[20][20];
	for (int i = 0; i < 20; i++)
		for (int j = 0; j < 20; j++)
			copy[i][j] = board[i][j];

	// calculate the next step
	for (int i = 0; i < 20; i++)
		for (int j = 0; j < 20; j++)
		{
			int surrounding = 0;
			for (int k = (i == 0 ? 0 : i-1); k <= (i == 19 ? 19 : i+1); k++)
				for (int l = (j == 0 ? 0 : j-1); l <= (j == 19 ? 19 : j+1); l++)
					if (!(k == i && l == j))
						surrounding += copy[k][l];

			board[i][j] = (surrounding == 3) || (surrounding == 2 && copy[i][j]);
		}

	return;
}

int main(int argc, char const *argv[])
{
	srand(time(NULL));

	// initialize some stuff
	int **board = malloc(20 * sizeof(int*));
	for (int i = 0; i < 20; i++)
		board[i] = malloc(20 * sizeof(int));

	int start[24][24] = {{0}}, finish[24][24] = {{0}};
	int **counts = malloc(5 * sizeof(int*));
	int **alive = malloc(5 * sizeof(int*));
	int **guesses = malloc(5 * sizeof(int*));

	for (int i = 0; i < 5; i++)
	{
		counts[i] = malloc(T * sizeof(int));
		alive[i] = malloc(T * sizeof(int));
		guesses[i] = malloc(T * sizeof(int));
	}

	// 1 million boards
	for (int b = 1; b <= 1000000; b++)
	{
		if (b % 100000 == 0) {printf("%d\n", b); fflush(stdout);}
		// generate number of ones and clean board
		int ones = (rand() % 393) + 4;
		for (int i = 0; i < 20; i++)
			for (int j = 0; j < 20; j++)
				board[i][j] = 0;

		// fill randomly with ones
		while (ones > 0)
		{
			int a = rand() % 20, b = rand() % 20;
			if (!board[a][b])
			{
				board[a][b] = 1;
				ones--;
			}
		}

		// evolve 5 steps and write down start board
		for (int i = 0; i < 5; i++)
			evolve(board);

		for (int i = 0; i < 20; i++)
			for (int j = 0; j < 20; j++)
				start[i+2][j+2] = board[i][j];

		// evolve delta steps and write down finish board
		int delta = (rand() % 5) + 1;
		for (int i = 0; i < delta; i++)
			evolve(board);
		delta--;

		for (int i = 0; i < 20; i++)
			for (int j = 0; j < 20; j++)
				finish[i+2][j+2] = board[i][j];

		// machine learn on the 5x5 subgrids
		for (int i = 2; i < 22; i++)
			for (int j = 2; j < 22; j++)
			{
				int val = 0;
				for (int k = i-2; k <= i+2; k++)
					for (int l = j-2; l <= j+2; l++)
						val = 2*val + finish[k][l];

				counts[delta][val]++;
				alive[delta][val] += start[i][j];
			}
	}

	// calculate guesses
	for (int i = 0; i < 5; i++)
	{
		for (int j = 0; j < T; j++)
		{
			if (counts[i][j] == 0) counts[i][j] = 1;
			guesses[i][j] = ((float) (alive[i][j]))/counts[i][j] > .5;
		}
	}

	FILE *fp = fopen("train.csv", "r");
	FILE *op = fopen("prediction.csv", "w");
	char *str = malloc(3900);
	fread(str, 1, 3900, fp);
	fwrite(str, 3900, 1, op);
	fputc('\n', op);
	fclose(fp);
	fp = fopen("test.csv", "r");
	fseek(fp,3500,SEEK_SET);

	int game, steps;
	for (int n = 0; n < 50000; ++n)
	{
		fscanf(fp,"%d,%d,", &game, &steps);
		fprintf(op,"%d,", game);
		steps--;
		for (int i = 2; i < 22; i++)
			for (int j = 2; j < 22; j++)
				finish[i][j] = fgetc(fp) - '0' - 0*fgetc(fp);

		for (int i = 2; i < 22; i++)
			for (int j = 2; j < 22; j++)
			{
				int val = 0;
				for (int k = i-2; k <= i+2; k++)
					for (int l = j-2; l <= j+2; l++)
						val = 2*val + finish[k][l];
				fputc(guesses[steps][val] + '0', op);
				(j == 21 && i == 21) ? fputc('\n', op) : fputc(',', op);
			}
	}

	fclose(fp);

	// test our guesses on the training set for fun
	long long correct[5] = {0}, total[5] = {0};
	fp = fopen("train.csv", "r");
	fseek(fp,7393,SEEK_SET);
	for (int n = 0; n < 50000; ++n)
	{
		fscanf(fp,"%d,%d,", &game, &steps);
		steps--;
		for (int i = 2; i < 22; i++)
			for (int j = 2; j < 22; j++)
				start[i][j] = fgetc(fp) - '0' - 0*fgetc(fp);
		for (int i = 2; i < 22; i++)
			for (int j = 2; j < 22; j++)
				finish[i][j] = fgetc(fp) - '0' - 0*fgetc(fp);

		for (int i = 2; i < 22; i++)
			for (int j = 2; j < 22; j++)
			{
				int val = 0;
				for (int k = i-2; k <= i+2; k++)
					for (int l = j-2; l <= j+2; l++)
						val = 2*val + finish[k][l];
				total[steps]++;
				correct[steps] += guesses[steps][val] == start[i][j];
			}
	}
	// print results
	printf("Accuracies:");
	for (int i = 0; i < 5; i++)
		printf(" %.6f", ((float) correct[i])/total[i]);
	printf("\n");

	return 0;
}
