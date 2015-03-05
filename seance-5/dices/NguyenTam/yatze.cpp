#include <iostream>

using namespace std;

bool isFull(int d[], int n) {
	for (int i = 0; i < n; i++) {
		for (int j = i; j < n; j++) {
			if (d[i] != d[j])
				return false;
		}
	}
	for (int i = 0; i < n; i++) {
		cout << d[i];
	}
	return true;
}

bool isSuite(int d[], int n) {
	for (int i = 0; i < n; i++) {
		if (d[i] != d[i+1] - 1)
			return false;
	}
	for (int i = 0; i < n; i++) {
		cout << d[i];
	}
	return true;
}

void main(){
	for (int t = 0; t < 100; t++) {
		int n = 5;
		int d[n];
		for (int i = 0; i < n; i++) {
			d[i] = rand() % 7;
		}
		if (isFull(d,n))
			cout << "d is full";
		else if (isSuite(d,n))
			cout << "d is suite";
	}
}

