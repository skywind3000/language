#include <stdio.h>
#include <iostream>

int N = 0, M = 0;
int w[200], c[200];
int dp[200][200];
int rr[200][200];

int recurrence()
{
	for (int i = 0; i <= N; i++) {
		if (i == 0) {
			dp[i][0] = 0;
			for (int j = 0; j < 200; j++) {
				if (j >= w[0]) {
					dp[0][j] = c[0];
					rr[0][j] = 1;
				} else {
					dp[0][j] = 0;
					rr[0][j] = 0;
				}
			}
		}
		else {
			for (int j = 0; j <= M; j++) {
				int a = dp[i - 1][j];
				int b = 0;
				if (j >= w[i]) {
					b = c[i] + dp[i - 1][j - w[i]];
				}
				if (a > b) {
					dp[i][j] = a;
					rr[i][j] = 0;
				} else {
					dp[i][j] = b;
					rr[i][j] = 1;
				}
			}
		}
		// printf("dp[%d]: ", i);
	}
	return dp[N-1][M];
}

void print_path()
{
	char path[200];
	int j = M;
	for (int i = N - 1; i >= 0; i--) {
		if (rr[i][j] == 1) {
			path[i] = '1';
			j = j - w[i];
		}
		else {
			path[i] = '0';
		}
	}
	path[N] = '\0';
	printf("%s\n", path);
}

int main()
{
	scanf("%d %d", &M, &N);
	for (int i = 0; i < N; i++) {
		scanf("%d %d", &w[i], &c[i]);
		// printf("%d %d\n", w[i], c[i]);
	}
	int ans = recurrence();
	printf("%d\n", ans);
	print_path();
	return 0;
}

/*
@input:
10 4
2 1
3 3
4 5
7 9
@output:
12
0101
*/


