#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

// Very simple test of how Kvasir handles threading
// The goal of this test is to record an entry for 1,
// record an entry for 2, then exit 1, then exit 2,
// hoping Fjalar handles the interleaving fine.

void *thread1(void *arg)
{
  printf("Thread 1 entered\n");
  printf("Thread 1 Sleeping for 2 seconds\n");
  sleep(2);
  printf("Thread 1 woken up and exiting\n");
}


void *thread2(void *arg)
{
  printf("Thread 2 entered\n");
  printf("Thread 2 Sleeping for 4 seconds\n");
  sleep(4);
  printf("Thread 2 woken up and exiting\n");
}


int main (int argc, char *argv[])
{
   pthread_t threads[2];
   int rc;
   long t;
   rc = pthread_create(&threads[0], NULL, thread1, (void*)0x1111);
   if (rc){
     printf("ERROR; return code from pthread_create() is %d\n", rc);
     exit(-1);
   }

   rc = pthread_create(&threads[1], NULL, thread2, (void*)0x3333);
   if (rc){
     printf("ERROR; return code from pthread_create() is %d\n", rc);
     exit(-1);
   }

   rc = pthread_join(threads[0], NULL);
   rc = pthread_join(threads[1], NULL);
   return 0;
}
