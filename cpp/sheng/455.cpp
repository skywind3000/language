#include <stdio.h>

int N, R;
int path[30];

int dfs(int depth, int remain) {
	if (depth > N || remain <= 0) {
		if (remain <= 0) {
			for (int i = 1; i < depth; i++) {
				if (path[i]) printf("  %d", i);
			}
			printf("\n");
			return 1;
		}
		return 0;
	}
	path[depth] = 1;
	dfs(depth + 1, remain - 1);
	path[depth] = 0;
	dfs(depth + 1, remain);
	return 0;
}

int main() {
	scanf("%d %d", &N, &R);
	dfs(1, R);
	return 0;
}

/*
@input:
5 3
@output:
  1  2  3
  1  2  4
  1  2  5
  1  3  4
  1  3  5
  1  4  5
  2  3  4
  2  3  5
  2  4  5
  3  4  5
*/

