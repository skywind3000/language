#include <bits/stdc++.h>
using namespace std;
int main() {
    int n;
    cin >> n;
    for(int i = 1; i <= n; i++) {
		int space = n - i;
		for(int j = 1; j <= space; j++) cout << " ";
        for(int j = 1; j <= i; j++) cout << j;
        for(int j = i - 1; j >= 1; j--) cout << j;
        cout << endl;
    }
    return 0;
}

/*
@input:
5
@output:
    1
   121
  12321
 1234321
123454321
*/
