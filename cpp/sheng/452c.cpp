#include <stdio.h>
#include <iostream>

int N = 0, M = 0;
int w[200], c[200];
int dp[200][200];

int recurrence()
{
	for (int i = 0; i <= N; i++) {
		if (i == 0) {
			dp[i][0] = 0;
			for (int j = 0; j < 200; j++) {
				dp[i][j] = (j >= w[0])? c[0] : 0;
			}
		}
		else {
			for (int j = 0; j <= M; j++) {
				int a = dp[i - 1][j];
				int b = 0;
				if (j >= w[i]) {
					b = c[i] + dp[i - 1][j - w[i]];
				}
				dp[i][j] = a > b ? a : b;
			}
		}
		// printf("dp[%d]: ", i);
	}
	return dp[N-1][M];
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
*/


