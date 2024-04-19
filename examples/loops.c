#include <stdio.h>

int main() {
	for (int i = 0; i < 10; i++) {
		printf("%d", i);
	}
	int i = 0;
	while (i < 10) {
		printf("%d", i);
		i++;
	}
	i = 0;
	do {
		printf("%d", i);
		i++;
	} while (i < 10);
	return 0;
}
