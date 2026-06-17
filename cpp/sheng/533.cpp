#include <stdio.h>
#include <iostream>

int N;
int B[2002];
int dp[2002];
int dm = 0;

void search()
{
	for (int i = 0; i < N; i++) {
		if (i == 0) {
			dp[0] = B[0];
			continue;
		}
		dp[i] = B[i];
		for (int j = 0; j < i; j++) {
			if (B[j] <= B[i]) {
				int d = dp[j] + B[i];
				if (d > dp[i]) {
					dp[i] = d;
				}
				if (d > dm) dm = d;
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
		std::cout << dp[i] << " " ;
	}
	std::cout << std::endl;
#else
	printf("%d\n", dm);
#endif
	return 0;
}


/*
@input:
7
1 7 3 5 9 4 8
@output:
18
*/



