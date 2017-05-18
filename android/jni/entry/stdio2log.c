// Normally, all stdout/stderr activity is dumped on the ground on
// Android. This make it very difficult to debug! Create a native
// thread that dup that moves stuff from stdout/err to logcat.
//
// Shamelessly stolen from
// https://codelab.wordpress.com/2014/11/03/how-to-use-standard-output-streams-for-logging-in-android-apps/

#include <android/log.h>
#include <pthread.h>

static int pfd[2];
static pthread_t thr;
static const char tag[] = "CHICKEN";

void *thread_func( void *ptr );

int start_logger()
{
    __android_log_write(ANDROID_LOG_DEBUG, tag, "====== testing from stdio2log.c");
    /* make stdout and stderr line-buffered */
    setvbuf(stdout, 0, _IOLBF, 0);
    setvbuf(stderr, 0, _IOLBF, 0);

    /* create the pipe and redirect stdout and stderr */
    pipe(pfd);
    dup2(pfd[1], 1);
    dup2(pfd[1], 2);

    /* spawn the logging thread */
    if(pthread_create(&thr, 0, thread_func, 0) == -1)
        return -1;
    pthread_detach(thr);

    // sleep a little to let the logging thread bootstrap and get
    // ready. TODO: does this actually help?
    usleep(100000);
    return 0;
}


void *thread_func( void *ptr)
{
    ssize_t rdsz;
    char buf[1024];
    while((rdsz = read(pfd[0], buf, sizeof buf - 1)) > 0) {
        if(buf[rdsz - 1] == '\n') --rdsz;
        buf[rdsz] = 0;  /* add null-terminator */
        __android_log_write(ANDROID_LOG_DEBUG, tag, buf);
    }
    return 0;
}
