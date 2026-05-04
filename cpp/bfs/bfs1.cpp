#include <iostream>
#include <queue>
#include <cstring>
#include <algorithm>

int m, n;
char maze[105][105];
bool visited[105][105];

int dirx[] = {-1, 1, 0, 0};
int diry[] = {0, 0, -1, 1};

struct Point {
	int x, y;
	int step;
};

Point pre[105][105];

bool bfs(int sx, int sy, int dx, int dy) 
{
	std::queue<Point> q;
	q.push({sx, sy, 0});
	visited[0][0] = true;
	pre[0][0] = {-1, -1};

	while (!q.empty()) {
		Point c = q.front();
		q.pop();
		if (c.x == dx && c.y == dy) {
			std::cout << "Minimum steps: " << c.step << std::endl;
			return true;
		}
		for (int i = 0; i < n; i++) {
			int nx = c.x + dirx[i];
			int ny = c.y + diry[i];
			if (nx >= 0 && nx < m && ny >= 0 && ny < n) {
				if (maze[nx][ny] == '.' && visited[nx][ny] == false) {
					q.push({nx, ny, c.step + 1});
					visited[nx][ny] = true;
					pre[nx][ny] = {c.x, c.y};
				}
			}
		}
	}
	return false;
}

void printPath(int sx, int sy, int dx, int dy)
{
	std::string path = "";
	int x = dx, y = dy;
	while (pre[x][y].x != -1) {
		int px = pre[x][y].x;
		int py = pre[x][y].y;
		if (x == px + 1) path += 'D';
		else if (x == px - 1) path += 'U';
		else if (y == py + 1) path += 'R';
		else if (y == py - 1) path += 'L';
		x = px;
		y = py;
	}
	std::reverse(path.begin(), path.end());
	std::cout << "Path: " << path << std::endl;
}

int main()  
{
	std::cin >> m >> n;
	for (int i = 0; i < m; i++) {
		std::cin >> maze[i];
	}
	memset(visited, false, sizeof(visited));
	if (bfs(0, 0, m - 1, n - 1)) {
		printPath(0, 0, m - 1, n - 1);
	} else {
		std::cout << "No path found" << std::endl;
	}
	return 0;
}

/*

4 4
....
.##.
....
###.

*/
