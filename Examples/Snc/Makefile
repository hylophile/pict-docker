SRCDIR = ../..
TARGETS = snc testremote
include $(SRCDIR)/SiteSpecific
include $(SRCDIR)/PiRules
include .depend

install: snc
  ifeq ($(ARCH),defaultarch)
	cp snc $(HOME)/bin
  else
	cp snc $(HOME)/bin/$(ARCH)
  endif

try: 
	 snc -verbose \
            $(HOME)/a.tmp \
	    $(HOME)/b.tmp 

tryagain:
	time snc -verbose \
            $(HOME)/a.tmp \
	    localhost $(HOME)/b.tmp -workers 2 \
	    -rsync 0 -port $(PORT)

try1:
	@$(MAKE) tryagain -e PORT=5499

try2:
	@$(MAKE) tryagain -e PORT=5498

tryremote: install
	time snc -verbose \
            $(HOME)/a.tmp \
	    rose.al.cl.cam.ac.uk /home/bcp1000/b.tmp
