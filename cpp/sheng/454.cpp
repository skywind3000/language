#include <stdio.h>

int N, K;
int nums[200];
int count = 0;

int dfs(int depth, int sum) {
	if (depth >= N) {
		if (sum > K) count++;
		return 0;
	}
	dfs(depth + 1, sum * 10 + nums[depth]);
	dfs(depth + 1, sum);
	return 0;
}

int main() {
	char text[100];
	scanf("%s %d", text, &K);
	N = 0;
	for (int i = 0; text[i]; i++) {
		if (text[i] >= '0' && text[i] <= '9') {
			nums[N++] = text[i] - '0';
		}
	}
	dfs(0, 0);
	printf("%d\n", count);
	return 0;
}

/*
@input:
1234
21
@output:
8
*/

