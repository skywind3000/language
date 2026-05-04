#include <stdio.h>

int main()
{
	int s[10000];
	int k = 0;
	while (1) {
		int c = getchar();
		if (c < 0) break;
		if (c == '\n' || c == '\r') {
			break;
		}
		if (c == '#') {
			if (k > 0) {
				k--;
			}
		}
		else {
			s[++k] = c;
		}
	}
	for (int i = 1; i <= k; i++) {
		putchar(s[i]);
	}
	putchar('\n');
	return 0;
}


