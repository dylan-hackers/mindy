/* Unix support routines for the Dylan operating-system library
   Written by Tom Emerson, tree@tiac.net
   
   Copyright (C) 1999 Thomas R. Emerson

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

   Bug reports, questions, comments, and suggestions should be sent by
   E-mail to the Internet address "gwydion-bugs@randomhacks.com".
*/

/* $Header: /scm/cvs/src/common/system/Attic/posix-system.h,v 1.1 1999/04/09 16:19:23 tree Exp $ */

#ifndef _POSIX_SYSTEM_H_
#define _POSIX_SYSTEM_H_

#ifdef __cplusplus
extern "C" {
#endif

extern int primary_group_name(unsigned char *outBuf, long bufLen);
extern int safe_putenv(const char *nvp);
extern int safe_unsetenv(const char *name);

#ifdef __cplusplus
}
#endif

#endif /* _POSIX_SYSTEM_H_ */

