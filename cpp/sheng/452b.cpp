#include <stdio.h>
#include <iostream>

int N = 0, M = 0, best = -1;
int w[200], c[200], p[200];
int path[200];

int dfs(int index, int remain, int sum)
{
	if (index >= N) {
		if (sum > best) {
			best = sum;
			for (int i = 0; i < N; i++) {
				path[i] = p[i];
			}
		}
		return 0;
	}
	if (remain <= 0) return 0;
	p[index] = 0;
	int a = dfs(index + 1, remain, sum);
	int b = 0;
	if (remain >= w[index]) {
		int value = c[index];
		p[index] = 1;
		b = value + dfs(index + 1, remain - w[index], sum + value);
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
	int ans = dfs(0, M, 0);
	printf("%d\n", ans);
	for (int i = 0; i < N; i++) {
		printf("%d", path[i]);
	}
	printf("\n");
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

