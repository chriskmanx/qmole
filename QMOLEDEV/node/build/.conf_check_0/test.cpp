
    #include <unistd.h>
    int main(void)
    {
       int fd = 0;
       fdatasync (fd);
       return 0;
    }
  