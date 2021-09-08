#define _GNU_SOURCE

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <sys/time.h>

#include <mpi.h>

/* IMPORTANT: MESSAGE_LEN must match module_wrf_error.F */
#define MESSAGE_LEN 120
#define SLEEP_TIME 1000
#define STACK_SIZE (1048576*32)

#if defined(_POSIX_TIMERS)
#if ( _POSIX_TIMERS > 0 )
/* According to the POSIX standard, we only get here if the system
   supports clock_gettime. */
#define USE_CLOCK_GETTIME 1
#endif
#endif

struct log_entry {
  char message[MESSAGE_LEN];
  struct log_entry * volatile next;
};

static int g_started_thread=0;
static pthread_t g_log_thread=-1;
static char *g_log_filename=NULL;
static struct log_entry *g_last=(struct log_entry*)NULL;
static struct log_entry *g_terminate=(struct log_entry*)NULL;
static int g_log_fd=-1;
static double g_start_time=0; /* must be initialized to 0 */

double c_quilt_log_time(void);
int c_quilt_log_open(const char *filename);
int c_quilt_log_message(char message[MESSAGE_LEN]);
int c_quilt_log_close(void);
int c_usleep(double d);

static int c_internal_log_open(const char *filename);
static int c_internal_log_terminate();
static int c_internal_log_close(int fd);
static int c_internal_log_open_fd(const char *filename);
static int c_internal_log_write(int fd,struct log_entry *message);
static void* logging_thread_main(void*first);

/**********************************************************************/

/* API called from Fortran */

int c_usleep(double d) {
  useconds_t u=(useconds_t)(1000000*d);
  return usleep(u);
}

double c_quilt_log_time(void) {
  double result;

  if(USE_CLOCK_GETTIME) {
    struct timespec tp;
    if(clock_gettime(CLOCK_MONOTONIC,&tp))
      return -1.0;
    result=tp.tv_sec;
    result+=(double)tp.tv_nsec / 1000000000.0;
  } else {
    struct timeval tv;
    if(gettimeofday(&tv,NULL))
      return -1.0;
    result= (double)tv.tv_sec;
    result += (double)tv.tv_usec/1000000.0 ;
  }

  return result-g_start_time;
}

int c_quilt_log_open(const char *filename) {
  return c_internal_log_open(filename);
}


int c_quilt_log_message(char message[MESSAGE_LEN]) {
  struct log_entry *next;

  if(!g_log_thread) {
    return -1; /* no way to log */
  }

  if( ! (next=(struct log_entry*)malloc(sizeof(struct log_entry))) ) {
    return -1;
  }

  memcpy(&(next->message[0]),message,MESSAGE_LEN);
  next->next=NULL;
  g_last->next=next;
  g_last=next;
  return 0;
}

int c_quilt_log_close(void) {
  return c_internal_log_terminate();
}

/**********************************************************************/

/* Internal implementation of logging */

static int c_internal_log_open(const char *filename) {
  pthread_attr_t attr;

  if(g_started_thread)
    c_internal_log_close(g_log_fd);

  /* A barrier ensures the g_start_time is the nearly the same time
     for every process. */
  if(MPI_Barrier(MPI_COMM_WORLD)!=MPI_SUCCESS) {
    fprintf(stderr,"c_quilt_log_open: cannot do a barrier on MPI_COMM_WORLD\n");
    return -1;
  }
  g_start_time=c_quilt_log_time();
  if(! (g_log_filename=strdup(filename)) ) {
    fprintf(stderr,"c_quilt_log_open: cannot strdup filename: %s\n",
            strerror(errno));
    return -1;
  }

  if( 0 > (g_log_fd=c_internal_log_open_fd(g_log_filename)) ) {
    fprintf(stderr,"c_quilt_log_open: %s: cannot open: %s\n",
            g_log_filename,strerror(errno));
    return -1;
  }

  if(! (g_last=(struct log_entry*)malloc(sizeof(struct log_entry))) ) {
    fprintf(stderr,"c_quilt_log_open: cannot allocate %lld bytes: %s\n",
            (long long)sizeof(struct log_entry),strerror(errno));
    return -1;
  }
  if(! (g_terminate=(struct log_entry*)malloc(sizeof(struct log_entry))) ) {
    fprintf(stderr,"c_quilt_log_open: cannot allocate %lld bytes: %s\n",
            (long long)sizeof(struct log_entry),strerror(errno));
    return -1;
  }

  g_last->message[0]=0; /* empty messages are not printed */
  g_last->next=NULL; /* null indicates next log entry is not available yet */

  g_terminate->message[0]=0; /* empty messages are not printed */
  g_terminate->next=g_terminate; /* self-referential means terminate */
  if(pthread_attr_init(&attr)) {
    fprintf(stderr,"c_quilt_log_open: pthread_attr_init: %s\n",
            strerror(errno));
    return -1;
  }
  if(pthread_attr_setstacksize(&attr,STACK_SIZE)) {
    fprintf(stderr,"c_quilt_log_open: pthread_attr_setstacksize to %lld: %s\n",
            (long long)STACK_SIZE,strerror(errno));
    return -1;
  }
  if(pthread_create(&g_log_thread,&attr,&logging_thread_main,(void*)g_last)) {
    fprintf(stderr,"c_quilt_log_open: pthread_create: %s\n",
            strerror(errno));
    return -1;
  } else {
    fprintf(stderr,"c_quilt_log_open: created thread!\n");
  }
  g_started_thread=1;
  if(pthread_attr_destroy(&attr)) {
    fprintf(stderr,"c_quilt_log_open: warning: pthread_attr_destroy: %s\n",
            strerror(errno));
    /* don't consider this a fatal error because logging has started */
  }
  fprintf(stderr,"Initialized quilt log.\n");
  return 0;
}

static int c_internal_log_terminate() {
  if(g_started_thread && g_last->next!=g_last) {
    g_last->next=g_terminate;
    g_last=g_terminate;
    if(pthread_join(g_log_thread,NULL)) {
      fprintf(stderr,"c_quilt_log_close: pthread_join: %s\n",
              strerror(errno));
      return -1;
    }
  } else {
    fprintf(stderr,"c_quilt_log_close: attempted to close twice\n");
    return -1;
  }
  g_started_thread=0;
  g_log_thread=-1;
  if(g_log_filename) free(g_log_filename);
  g_log_filename=NULL;
  g_last=NULL;
  if(g_terminate) free(g_terminate);
  g_terminate=NULL;
  g_log_fd=-1;
  g_start_time=0;

  return 0;
}

static int c_internal_log_close(int fd) {
  if(fd<=0) return 0; /* was never opened */
  return close(fd);
}

static int c_internal_log_open_fd(const char *filename) {
  int fd;
  int options=O_APPEND|O_WRONLY;
  if( 0 > (fd=open(filename,options|O_CREAT,0644)) )
    fd=open(filename,options);
  return fd;
}

static int c_internal_log_write(int fd,struct log_entry *message) {
  int written=-1,ferr=-1,len=0,bad=0;
  if(!message || !message->message[0]) {
    return 0; /* no message to write */
  }
  if(fd<0) {
    return -1; /* file was never opened */
  }
  for(len=0;len<MESSAGE_LEN && message->message[len];len++);
  if( (written=write(fd,message->message,len)) != len) {
    fprintf(stderr,"io_quilt.log: cannot write: %s\n",strerror(errno));
    bad=-1;
  }
  if( ferr=fdatasync(fd) ) {
    fprintf(stderr,"io_quilt.log: cannot fdatasync: %s\n",strerror(errno));
    bad=-1;
  }
  return bad;
}

/**********************************************************************/

/* Background logging thread */

static void* logging_thread_main(void*first) {
  struct log_entry *current=(struct log_entry*)first,*prior;

  while(current && current->next!=current) {
    if(c_internal_log_write(g_log_fd,current))
      fprintf(stderr,"BAD: could not write: %s\n",strerror(errno));
    while(!current->next)
      usleep(SLEEP_TIME);
    prior=current;
    current=current->next;
    if(prior && prior!=current) { /* never free termination request node */
      free(prior);
      prior=NULL;
    }
  }
  return NULL;
}
