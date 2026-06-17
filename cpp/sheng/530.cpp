#include <stdio.h>
#include <iostream>

int N;
int B[202];
int dp[202][202];
int dm = 0;

void search()
{
	for (int j = 0; j < N; j++) {
		dp[0][j] = 1;
	}
	for (int i = 1; i < N; i++) {
		for (int j = 0; j < N; j++) {
			dp[i][j] = 1;
		}
		for (int j = 0; j < N; j++) {
			for (int k = 0; k < j; k++) {
				if (B[k] <= B[j]) {
					int d = dp[i - 1][k] + 1;
					if (d > dp[i][j]) {
						dp[i][j] = d;
					}
					if (d > dm) dm = d;
				}
			}
		}
	}
}

int main()
{
	std::cin >> N;
	for (int i = 0; i < N; i++) {
		std::cin >> B[i];
	}
	search();
#if 0
	for (int i = 0; i < N; i++) {
		for (int j = 0; j < N; j++) {
			std::cout << dp[i][j] << " " ;
		}
		std::cout << std::endl;
	}
#else
	printf("Max=%d", dm);
#endif
	return 0;
}


/*
@input:
14
13 7 9 16 38 24 37 18 44 19 21 22 63 15
@output:
Max=8
*/


