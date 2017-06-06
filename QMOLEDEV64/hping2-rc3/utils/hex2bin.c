/*
 * hex2bin - simple hex to bin filter
 * antirez@invece.org - under GPL version 2
 */

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

int hex2bin(void)
{
	char hex[3];
	int d = 0;
	unsigned char c;
	int stdin_fd = fileno(stdin);
	int n_read;

	while((n_read = read(stdin_fd, hex, 2)) > 0)
	{
		if (n_read == 1)
		{
			if (hex[0] != '\n')
			{
				fprintf(stderr,
				"input parse error, odd digits in hex file\n");
				exit(1);
			}
			else
				exit(1);
		}
		hex[2] = '\0';
		sscanf(hex, "%x", &d);
		c = (unsigned char) d;
		printf("%c", c);
	}
	return 0;
}

int bin2hex(void)
{
	int stdin_fd = fileno(stdin);
	int n_read;
	unsigned char c;

	while((n_read = read(stdin_fd, &c, 1)) > 0)
	{
		printf("%.2x", c);
	}
	return 0;
}

int main(int argc, char **argv)
{
	if (argc >= 2 && strstr(argv[1], "-r"))
		bin2hex();
	else
		hex2bin();

	return 0;
}
