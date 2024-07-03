SRCDIR = ../..
TARGETS = blockade
include $(SRCDIR)/SiteSpecific
include $(SRCDIR)/PiRules
include .depend

demo:: blockade
	@echo Forking blockade server
	@./blockade &
	@echo Forking blockade client
	@(sleep 3; ./blockade -client) &
