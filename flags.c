#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int main(){
  printf("RDONLY = %i\n", O_RDONLY);
  printf("WRONLY = %i\n", O_WRONLY);
  printf("RDWR = %i\n", O_RDWR);
  printf("CREAT = %i\n", O_CREAT);
}
