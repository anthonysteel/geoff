#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct sudoku_board {
	int** rows;
	int** cols;
	int** squares;
} typedef sudoku_board;

int** get_rows (int board[9][9]) {
	int** rows = (int**) malloc(9 * sizeof(int*));
	for (int i = 0; i < 9; ++i) {
		rows[i] = (int *) malloc (9 * sizeof(int));
		for (int j = 0; j < 9; ++j) {
			rows[i][j] = board[i][j];
		}
	}
	return rows;
}

int** get_cols (int board[9][9]) {
	int** cols = (int**) malloc(9 * sizeof(int*)); 
	for (int i = 0; i < 9; ++i) {
		cols[i] = (int *) malloc (9 * sizeof(int));
		for (int j = 0; j < 9; ++j) {
			cols[i][j] = board[j][i];
		}
	}
	return cols;
}

int** get_squares (int board[9][9]) {
	int** squares = (int**) malloc(9 * sizeof(int*));
	for (int i = 0; i < 9; ++i) {
		squares[i] = (int *) malloc(9 * sizeof(int*));
	}
	for (int l = 0; l < 3; ++l) {
		for (int k = 0; k < 3; ++k) {
			for (int j = 0; j < 3; ++j) {
				for (int i = 0; i < 3; ++i) {
					squares[k + (l * 3)][i + (j * 3)] = board[j + (l * 3)][i + (k * 3)];
				}
			}
		}
	}
	return squares;
}

void update_squares (int** rows, int** squares) {
	for (int l = 0; l < 3; ++l) {
		for (int k = 0; k < 3; ++k) {
			for (int j = 0; j < 3; ++j) {
				for (int i = 0; i < 3; ++i) {
					squares[k + (l * 3)][i + (j * 3)] = rows[j + (l * 3)][i + (k * 3)];
				}
			}
		}
	}
}

void update(int i, int j, int new_elem, sudoku_board* board) {
	board->rows[i][j] = new_elem;
	board->cols[j][i] = new_elem;
	update_squares(board->rows, board->squares);
}

void copy(sudoku_board* new_board, sudoku_board* old_board) {
	for (int i = 0; i < 9; ++i) {
		new_board->rows[i] = (int *) malloc(9 * sizeof(int*));
		new_board->cols[i] = (int *) malloc(9 * sizeof(int*));
		new_board->squares[i] = (int *) malloc(9 * sizeof(int*));
		for (int j = 0; j < 9; ++j) {
			new_board->rows[i][j] = old_board->rows[i][j];
			new_board->cols[i][j] = old_board->cols[i][j];
			new_board->squares[i][j] = old_board->squares[i][j];
		}
	}
}

void print_board (sudoku_board* board) {
	for (int i = 0; i < 9; ++i) {
		for (int j = 0; j < 9; ++j) {
			printf("%d ", board->rows[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}

sudoku_board* init_board (int board[9][9]){
	sudoku_board* new_board = (sudoku_board*) malloc(sizeof(sudoku_board));
	new_board->rows = get_rows(board);
	new_board->cols = get_cols(board);
	new_board->squares = get_squares(board);
	return new_board;
}

void print_rows (sudoku_board* board) {
	for (int i = 0; i < 9; ++i) {
		for (int j = 0; j < 9; ++j) {
			printf("%d ", board->rows[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}

void print (int** matrix) {
	for (int i = 0; i < 9; ++i) {
		for (int j = 0; j < 9; ++j) {
			printf("%d ", matrix[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}

bool is_duplicate (int* array) {
	for (int i = 0; i < 8; ++i) {
		for (int j = i + 1; j < 9; j++) {
			if (array[i] == array[j] && array[i] != 0) {
				return true;
			}
		}
	}
	return false;
}

bool contains0to9 (int* array) {
	int hash[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
	for (int i = 0; i < 9; ++i) {
		hash[array[i] - 1] += 1;
	}
	for (int i = 0; i < 9; ++ i) {
		if (hash[i] == 0) return false;
	}
	return true;
}

bool is_full (sudoku_board* board) {
	bool full = true;
	for (int i = 0; i < 9; ++i) {
		full = full && contains0to9(board->rows[i]) &&
			       contains0to9(board->cols[i]) &&
		               contains0to9(board->squares[i]);
	}
	return full;
}

bool is_valid (sudoku_board* board) {
	bool valid = true;
	for (int i = 0; i < 9; ++i) {
		valid = valid && !is_duplicate(board->rows[i]) &&
				 !is_duplicate(board->cols[i]) &&
				 !is_duplicate(board->squares[i]);
	}
	return valid;
}


bool is_solved (sudoku_board* board) {
	return is_valid(board) && is_full(board);
}

void solve (sudoku_board* board) {
	print_board(board);
	if (is_solved(board)) print_board(board);
	if (is_valid(board)) {
		sudoku_board* new_board = (sudoku_board *) malloc(sizeof(sudoku_board));
		copy(new_board, board);
		for (int i = 0; i < 9; ++i) {
			for (int j = 0; j < 9; ++j) {
				if (new_board->rows[i][j] == 0) {
					for (int k = 1; k <= 9; ++k) {
						update(i, j, k, new_board);
						solve(new_board);
					}
				}
			}
		}
	}	
	return;
}

int main () {
	int elems[9][9] = {
		{5, 3, 0, 0, 7, 0, 0, 0, 0},
		{6, 0, 0, 1, 9, 5, 0, 0, 0},
		{0, 9, 8, 0, 0, 0, 0, 6, 0},
		{8, 0, 0, 0, 6, 0, 0, 0, 3},
		{4, 0, 0, 8, 0, 3, 0, 0, 1},
		{7, 0, 0, 0, 2, 0, 0, 0, 6},
		{0, 6, 0, 0, 0, 0, 2, 8, 0},
		{0, 0, 0, 4, 1, 9, 0, 0, 5},
		{0, 0, 0, 0, 8, 0, 0, 7, 9}
	};
	sudoku_board* board = init_board(elems);
	solve(board);
}
