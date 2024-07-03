SRCDIR = ../..
TARGETS = pictsmap rotateppm binarizeppm doubleppm
include $(SRCDIR)/SiteSpecific
include $(SRCDIR)/PiRules
include .depend

demo:: pictsmap
	@echo
	@echo Forking pictsmap
	@./pictsmap ../Ants/bitmaps.26/food.ppm &
