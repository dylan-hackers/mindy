/**********************************************************************\
*
*  Copyright (c) 2015 Mindy contributors.
*  All rights reserved.
*
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University, and the Gwydion Dylan Maintainers.
*
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation.
*
\**********************************************************************/

#include <stdbool.h>
#include <stdio.h>

#ifdef _WIN32
// Implement...
#else
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "color_output.h"

static bool WantColor = false;

static bool check_env_value(const char *key, const char *desired)
{
    const char *value = getenv(key);
    if (value && (strcmp(value, desired) == 0)) {
        return true;
    }
    return false;
}

void init_color(bool force_color)
{
    if (force_color) {
        WantColor = true;
    } else {
        WantColor = false;
#ifdef _WIN32
        // Implement...
#else
        if (isatty(fileno(stdout)) &&
            !check_env_value("TERM", "dumb") &&
            !check_env_value("EMACS", "t"))
        {
            WantColor = true;
        }
#endif
        if (check_env_value("MINDY_COLOR", "1"))
            WantColor = true;
        if (check_env_value("MINDY_COLOR", "0"))
            WantColor = false;
    }
}

static const char *NormalColors[] = {
    "\x1b[30m",
    "\x1b[31m",
    "\x1b[32m",
    "\x1b[33m",
    "\x1b[34m",
    "\x1b[35m",
    "\x1b[36m",
    "\x1b[37m",
    "\x1b[38m",
    "\x1b[39m"
};

static const char *IntenseColors[] = {
    "\x1b[30;1m",
    "\x1b[31;1m",
    "\x1b[32;1m",
    "\x1b[33;1m",
    "\x1b[34;1m",
    "\x1b[35;1m",
    "\x1b[36;1m",
    "\x1b[37;1m",
    "\x1b[38;1m",
    "\x1b[39;1m"
};

void mindy_color(enum MindyColor color, bool bright)
{
    if (WantColor) {
        if (bright) {
            fputs(IntenseColors[color], stdout);
        } else {
            fputs(NormalColors[color], stdout);
        }
    }
}

void mindy_reset_color(void)
{
    if (WantColor) {
        fputs("\x1b[0m", stdout);
    }
}

void mindy_color_err(enum MindyColor color, bool bright)
{
    if (WantColor) {
        if (bright) {
            fputs(IntenseColors[color], stderr);
        } else {
            fputs(NormalColors[color], stderr);
        }
    }
}

void mindy_reset_color_err(void)
{
    if (WantColor) {
        fputs("\x1b[0m", stderr);
    }
}
