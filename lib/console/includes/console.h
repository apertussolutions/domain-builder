// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#ifndef CONSOLE_H
#define CONSOLE_H

#include <smallOS/xen-version.h>
#include <smallOS/mem.h>

typedef enum { black, blue, green, cyan, red, magenta, yellow, white } color_t;

typedef struct {
  uint8_t character;
  struct {
    uint8_t fg_color  :3;
    uint8_t bright    :1;
    uint8_t bg_color  :3;
    uint8_t flash     :1;
  };
} screen_char_t;


#define ROWS 25
#define COLS 80

typedef struct {
  screen_char_t buffer[ROWS][COLS];
  uint8_t cur_x;
  uint8_t cur_y;
  screen_char_t cur;
  uint8_t padding[92];
} console_t;


void con_scroll(console_t *screen, unsigned long n);
void con_new_line(console_t *screen);
void con_home(console_t *screen);
void con_next(console_t *screen);
void con_cls(console_t *screen);
void con_put_char(console_t *screen, char c);
void con_put_string(console_t *screen, char *s);
void con_set_bg(console_t *screen, color_t c);
void con_set_fg(console_t *screen, color_t c);
int con_init (console_t *screen);
void con_print(console_t *screen, const char *fmt, ...);
#endif
