// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#include <smallOS/xen-version.h>
#include <smallOS/mem.h>
#include <stdio.h>
#include <console.h>

void con_scroll(console_t *screen, unsigned long n) {
  unsigned long i, j;
  screen->cur.character = ' ';

  for (i = 0; i < ROWS - n; ++i)
    memcpy( &screen->buffer[i]
          , &screen->buffer[n + i]
          , sizeof(screen_char_t[COLS])
          );

  for (i = ROWS - n; i < ROWS; ++i)
    for(j = 0; j < COLS; ++j)
      screen->buffer[i][j] = screen->cur;
}

void con_new_line(console_t *screen) {
  screen->cur_x = 0;
  if (screen->cur_y < ROWS - 1) ++screen->cur_y; else con_scroll(screen,1);
}

void con_home(console_t *screen) {
  screen->cur_x = 0;
  screen->cur_y = 0;
}

void con_next(console_t *screen) {
  if (screen->cur_x < COLS - 1) ++screen->cur_x;
  else con_new_line(screen);
}

void con_cls(console_t *screen) {
  con_scroll(screen,ROWS);
  con_home(screen);
}


void con_put_char(console_t *screen, char c) {
  if (c == '\n') {
    con_new_line(screen);
    return;
  } else {
    screen->cur.character = c;
    screen->buffer[screen->cur_y%ROWS][screen->cur_x%COLS] = screen->cur;
    con_next(screen);
  }
}


void con_put_string(console_t *screen, char *s) {
  for (;  *s != 0; ++s) {
    con_put_char(screen,*s);
  }
}


void con_set_bg(console_t *screen, color_t c) {
  screen->cur.bg_color = c;
}

void con_set_fg(console_t *screen, color_t c) {
  screen->cur.fg_color = c;
}


int con_init (console_t *screen) {
  int err;
  mfn_t screen_mfn = 0xB8; // { .num = 0xB8 };

  if (((unsigned long)screen % PAGE_SIZE) != 0) return -1;

  err = map_foreign_page(DOMID_SELF, screen, screen_mfn,
                                                _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) return err;

  con_home(screen);
  con_set_bg(screen, black);
  con_set_fg(screen, white);

  return 0;
}

void con_print(console_t *screen, const char *fmt, ...)
{
    char   buf[1024];
    va_list       args;
    va_start(args, fmt);
    (void)vsnprintf(buf, sizeof(buf), fmt, args);
    buf[1023] = 0;
    con_put_string(screen, buf);
    va_end(args);
}

