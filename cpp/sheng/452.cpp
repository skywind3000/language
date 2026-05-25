#include <stdio.h>
#include <iostream>

int N = 0, M = 0;
int w[200], c[200];

int dfs(int index, int remain)
{
	if (index >= N) return 0;
	if (remain <= 0) return 0;
	int a = dfs(index + 1, remain);
	int b = 0;
	if (remain >= w[index]) {
		b = c[index] + dfs(index + 1, remain - w[index]);
	}
	return a > b ? a : b;
}

int main()
{
	scanf("%d %d", &M, &N);
	for (int i = 0; i < N; i++) {
		scanf("%d %d", &w[i], &c[i]);
		// printf("%d %d\n", w[i], c[i]);
	}
	int ans = dfs(0, M);
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

