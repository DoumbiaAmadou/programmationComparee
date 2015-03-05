include <istream>

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
	for (in i = 0; i < n; i++) {
		cout << d[i];
	}
	return true;
}

void main(){
	int d = new int[5];
	for (int i = 0; i < 5; i++) {
		d[i] = math.rand() % 7;
	}
	if (isFull(d))
		cout << "d is full";
	else if (isSuite(d))
		cout << "d is suite";
}

