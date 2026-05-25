#include <iostream>
int n, total_count = 0;
int path[10000];
int p = 0;

void chai(int sum, int last_num)
{
	if (sum == n) {
		std::cout << n << "=";
		for (int i = 1; i <= p; i++) {
			if (i > 1) std::cout << "+";
			std::cout << path[i];
		}
		std::cout << std::endl;
		total_count++;
		return;
	}
	// 确定下一个数上限
	int max_num = n - sum;
	// 第一个数不能是 n
	if (p == 0) max_num = n - 1;
	// 枚举下一个需要加入的数字
	for (int i = last_num; i <= max_num; i++) {
		path[++p] = i;
		chai(sum + i, i);
		p--;
	}
}

int main()
{
	n = 6;
	chai(0, 1);
	std::cout << "total=" << total_count << std::endl;
}
