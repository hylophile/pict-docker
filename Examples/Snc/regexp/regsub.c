/*
 * regsub
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 *** THIS IS AN ALTERED VERSION.  It was altered by Benjamin Pierce,
 *** bcp1000@cl.cam.ac.uk, on 6 July 1996 to give regsub an extra length
 *** parameter so that storage following dest is not overwritten by
 *** mistake. 
 */
#include <stdio.h>
#include <regexp.h>
#include "regmagic.h"

#define MIN(x,y) (x<y) ? x : y
#ifndef CHARBITS
#define	UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define	UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

/*
 - regsub - perform substitutions after a regexp match
 */
void
regsub(prog, source, dest, destlen)
regexp *prog;
char *source;
char *dest;
{
	register char *src;
	register char *dst;
	register char c;
	register int no;
	register int len;
	register int l;
	extern char *strncpy();

	if (prog == NULL || source == NULL || dest == NULL) {
		regerror("NULL parm to regsub");
		return;
	}
	if (UCHARAT(prog->program) != MAGIC) {
		regerror("damaged regexp fed to regsub");
		return;
	}

	src = source;
	dst = dest;
	l = destlen;
	while (l > 0 && (c = *src++) != '\0') {
		if (c == '&')
			no = 0;
		else if (c == '\\' && '0' <= *src && *src <= '9')
			no = *src++ - '0';
		else
			no = -1;
 		if (no < 0) {	/* Ordinary character. */
 			if (c == '\\' && (*src == '\\' || *src == '&'))
 				c = *src++;
 			*dst++ = c;
                        l--;
 		} else if (prog->startp[no] != NULL && prog->endp[no] != NULL) {
			len = prog->endp[no] - prog->startp[no];
			(void) strncpy(dst, prog->startp[no], MIN(len,l));
			dst += len;
			l -= len;
			if (len != 0 && *(dst-1) == '\0') {	/* strncpy hit NUL. */
				regerror("damaged match string");
				return;
			}
		}
	}
        if (l > 0) {
          *dst++ = '\0';
        }
}
