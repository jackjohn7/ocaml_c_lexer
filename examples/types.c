#include <stdio.h>

enum DayOfWeek {
	SUNDAY,
	MONDAY,
	TUESDAY,
	WEDNESDAY,
	THURSDAY,
	FRIDAY,
	SATURDAY
};

struct Project {
	int id;
	char* name;
};

typedef int port;

int main() {
	enum DayOfWeek today = THURSDAY;

	struct Project p1;
	p1.id = 1203;
	p1.name = "SML Lexer in C";

	printf("Today's enumerated id is %d\n", today);
	printf("Project with id %d named \"%s\"\n", p1.id, p1.name);

	port host_port = 5173;
	printf("Server hosted at port %d\n", host_port);
	return 0;
}
