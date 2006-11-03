#define _GNU_SOURCE
#define __USE_BSD
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <dlfcn.h>
#include <sys/stat.h>
#include <unistd.h>

#define prefix "##[MAKEOMATIC]## "

#define O_ACCMODE          0003
#define O_RDONLY             00
#define O_WRONLY             01
#define O_RDWR               02
#define O_CREAT            0100 /* not fcntl */
#define O_EXCL             0200 /* not fcntl */
#define O_NOCTTY           0400 /* not fcntl */
#define O_TRUNC           01000 /* not fcntl */
#define O_APPEND          02000
#define O_NONBLOCK        04000
#define O_NDELAY        O_NONBLOCK
#define O_SYNC           010000
#define O_FSYNC          O_SYNC
#define O_ASYNC          020000
# define O_DIRECT        040000 /* Direct disk access.  */
# define O_DIRECTORY    0200000 /* Must be a directory.  */
# define O_NOFOLLOW     0400000 /* Do not follow links.  */
# define O_NOATIME     01000000 /* Do not set atime.  */
# define O_LARGEFILE    0100000

void print_open_flags(int flags) {
  /*
  if (flags == 2050) {
  if (flags & O_RDONLY) fprintf(stdout,"RDONLY ");
  if (flags & O_WRONLY) fprintf(stdout,"WRONLY ");
  if (flags & O_RDWR) fprintf(stdout,"RDWR ");
  if (flags & O_CREAT) fprintf(stdout,"CREAT ");
  if (flags & O_EXCL) fprintf(stdout,"EXCL ");
  if (flags & O_NOCTTY) fprintf(stdout,"NOCTTY ");
  if (flags & O_TRUNC) fprintf(stdout,"TRUNC ");
  if (flags & O_APPEND) fprintf(stdout,"APPEND ");
  if (flags & O_ASYNC) fprintf(stdout,"ASYNC ");
  if (flags & O_DIRECT) fprintf(stdout,"DIRECT ");
  if (flags & O_LARGEFILE) fprintf(stdout,"LARGEFILE ");
  if (flags & O_NOFOLLOW) fprintf(stdout,"NOFOLLOW ");
  if (flags & O_NONBLOCK) fprintf(stdout,"NONBLOCK ");
  if (flags & O_SYNC) fprintf(stdout,"SYNC ");
  fprintf(stdout,"\n");
  }
*/
}

void print_access_mode(int mode) {
  /*  if (mode & R_OK) fprintf(stdout,"R_OK ");
  if (mode & W_OK) fprintf(stdout,"W_OK ");
  if (mode & X_OK) fprintf(stdout,"X_OK "); */
}

static FILE *fifo_cmd;
static FILE *fifo_ping;
static int in_init = 0;

static void init() {
  if (fifo_cmd) return;
  in_init = 1;
  char *fn;

  fn = getenv("MAKEOMATIC_FIFO_CMD");
  if (!fn) {
    fprintf(stderr,"MAKEOMATIC_FIFO_CMD undefined\n");
    _exit(2);
  }
  fifo_cmd = fopen(fn, "r+");
  if (!fifo_cmd) {
    fprintf(stderr,"%s cannot be opened\n",fn);
    _exit(2);
  }


  fn = getenv("MAKEOMATIC_FIFO_PING");
  if (!fn) {
    fprintf(stderr,"MAKEOMATIC_FIFO_PING undefined\n");
    _exit(2);
  }
  fifo_ping = fopen(fn, "r");
  if (!fifo_ping) {
    fprintf(stderr,"%s cannot be opened\n",fn);
    _exit(2);
  }
  in_init = 0;
}

void makeomatic_wait() {
  fflush(fifo_cmd);
  fgetc(fifo_ping);
}

int unlink(const char *pathname)
{
  static int (*func)(const char*);
  init();
  fprintf(fifo_cmd,"%sunlink %s\n", prefix, pathname);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "unlink");
  return func(pathname);
}

int access(const char *pathname, int mode)
{
  static int (*func)(const char*, int);
  init();
  fprintf(fifo_cmd,"%saccess %d %s\n", prefix, mode, pathname);
  print_access_mode(mode);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "access");
  return func(pathname,mode);
}

int __lxstat64(int ver, const char *path, struct stat64 *buf)
{
  static int (*func)(int, const char *, struct stat64 *);
  init();
  fprintf(fifo_cmd,"%s__lxstat64 %s\n", prefix,path);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "__lxstat64");
  return func(ver,path,buf);
}

FILE *fopen(const char *path, const char *mode)
{
  static FILE *(*func)(const char*, const char*);
  if (!in_init) {
    init();
    fprintf(fifo_cmd,"%sfopen %s %s\n", prefix,mode,path);
    makeomatic_wait();
  }
  if(!func) func = dlsym(RTLD_NEXT, "fopen");
  return func(path,mode);
}

FILE *fopen64(const char *path, const char *mode)
{
  static FILE *(*func)(const char*, const char*);
  if (!in_init) {
    init();
    fprintf(fifo_cmd,"%sfopen %s %s\n", prefix,mode,path);
    makeomatic_wait();
  }
  if(!func) func = dlsym(RTLD_NEXT, "fopen64");
  return func(path,mode);
}

int open(const char *path, int flags, mode_t mode)
{
  static int (*func)(const char*, int, mode_t);
  init();
  fprintf(fifo_cmd,"%sopen %d %s\n", prefix, flags, path); 
  print_open_flags(flags);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "open");
  return(func(path,flags,mode));
}

int open64(const char *path, int flags, mode_t mode)
{
  static int (*func)(const char*, int, mode_t);
  init();
  fprintf(fifo_cmd,"%sopen64 %d %s\n", prefix, flags, path);
  print_open_flags(flags);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "open64");
  return func(path,flags, mode);
}

int __xstat64(int ver, const char *path, struct stat64 *buf)
{
  static int (*func)(int, const char *, struct stat64 *);
  init();
  fprintf(fifo_cmd,"%s__xstat64 %s\n", prefix, path);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "__xstat64");
  return func(ver,path,buf);
}

int __fxstat64(int ver, int fd, struct stat64 *buf)
{
  static int (*func)(int, int, struct stat64 *);
  init();
  fprintf(fifo_cmd,"%s__fxstat64 %i\n", prefix, fd);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "__fxstat64");
  return func(ver,fd,buf);
}

int fstat64(int fd, struct stat64 *buf)
{
  static int (*func)(int, struct stat64 *);
  init();
  fprintf(fifo_cmd,"%sfstat64 %d\n", prefix, fd);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "fstat64");
  return func(fd,buf);
}

int __fstat(int fd, struct stat *buf)
{
  static int (*func)(int, struct stat *);
  init();
  fprintf(fifo_cmd,"%s__fxstat %d\n", prefix,fd);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "__fxstat");
  return(func(fd,buf));
}

int stat(const char *path, struct stat *buf)
{
  static int (*func)(const char *, struct stat *);
  init();
  fprintf(fifo_cmd,"%sstat %s\n", prefix,path);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "stat");
  return(func(path,buf));
}

FILE *fdopen(int fd, const char *mode)
{
  static FILE *(*func)(int fd, const char*);
  init();
  fprintf(fifo_cmd,"%sfdopen %s %d\n",prefix,mode,fd);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "fdopen");
  return func(fd,mode);
}

/*
int close(int fd)
{
  static int (*func)(int);
  init();
  fprintf(stderr,"%sclose %d\n", prefix,fd);
  if (fd == 0 || fd == 1 || fd == 2) return 0;
  if(!func) func = dlsym(RTLD_NEXT, "close");
  return func(fd);
}

int dup2(int fd1,int fd2)
{
  static int (*func)(int,int);
  init();
  fprintf(stderr,"%sdup2 %d %d\n", prefix,fd1,fd2);
  if(!func) func = dlsym(RTLD_NEXT, "dup2");
  if (fd2 == 1 || fd2 == 2) return 0;
  return func(fd1,fd2);
}
*/

/*
ssize_t read(int fd, void *buf, size_t count)
{
  static ssize_t (*func)();
  fprintf(fifo_cmd,"%sread %i\n", prefix,fd);
  makeomatic_wait();
  if(!func) func = dlsym(RTLD_NEXT, "read");
  return(func(fd, buf, count));
}



int execve(const char *filename, char *const argv[], char *const envp[]){
  static int (*func)(const char*, char *const [], char *const []);
  fprintf(stderr,"execve(%s)\n",filename);
  if(!func) func = dlsym(RTLD_NEXT, "execve");
  return(func(filename,argv,envp));
}

int fexecve(int fd, char *const argv[], char *const envp[]){
  static int (*func)(int fd, char *const [], char *const []);
  fprintf(stderr,"fexecve(%d)\n",fd);
  if(!func) func = dlsym(RTLD_NEXT, "fexecve");
  return(func(fd,argv,envp));
}

int execv(const char *path, char *const argv[]){
  static int (*func)(const char *, char *const[]);
  fprintf(fifo_cmd,"[%d] execv(%s)\n",getpid(),path);
  if(!func) func = dlsym(RTLD_NEXT, "execv");
  return(func(path,argv));
}


int execle(const char *path, const char *arg, ...){
  fprintf(fifo_cmd,"execle!!\n");
  _exit(2);
}

int execvp(const char *path, char *const argv[]){
  static int (*func)(const char *, char *const[]);
  fprintf(fifo_cmd,"[%d] execvp(%s)\n",getpid(),path);
  {
    int i = 0;
    while (argv[i]) {
      fprintf(fifo_cmd," arg:%s\n",argv[i]);
      i++;
    }
  }

  if(!func) {
    func = dlsym(RTLD_NEXT, "execvp");
  }
  return(func(path,argv));
}


pid_t vfork(){
  static pid_t (*func)();
  fprintf(fifo_cmd,"vfork()\n");
  if(!func) {
    func = dlsym(RTLD_NEXT, "fork");
  }
  pid_t ret = func();
  return(ret);
}


*/
