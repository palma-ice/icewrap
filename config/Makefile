.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# PATH options
srcdir = src
libdir = libs
objdir = lib/include
bindir = lib/bin

# Command-line options at make call
debug    ?= 0
parallel ?= 0 

## COMPILER CONFIGURATION ##
# (should be loaded from config directory)

<COMPILER_CONFIGURATION>

# Determine whether to use normal flags or debugging flags
DFLAGS   = $(DFLAGS_NODEBUG)
ifeq ($(debug), 1)
	DFLAGS   = $(DFLAGS_DEBUG)
endif

# Debugging flags with profiling output enabled
ifeq ($(debug), 2)
	DFLAGS   = $(DFLAGS_PROFILE)
endif

###############################################
##
## List of source files
##
###############################################

$(objdir)/icewrap_yelmo.o: $(srcdir)/icewrap_yelmo.f90 $(objdir)/icewrap_def.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/icewrap_stub.o: $(srcdir)/icewrap_stub.f90 $(objdir)/icewrap_def.o
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/icewrap_def.o: $(srcdir)/icewrap_def.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

$(objdir)/ncio.o: $(libdir)/ncio.f90
	$(FC) $(DFLAGS) $(FFLAGS) $(INC_NC) -c -o $@ $<

$(objdir)/nml.o: $(libdir)/nml.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

icewrap_libs = 	$(objdir)/icewrap_def.o
# icewrap_libs = 	$(objdir)/icewrap_def.o \
# 				$(objdir)/ncio.o \
# 				$(objdir)/nml.o

###############################################
##
## Compilation of complete programs
##
###############################################

test_stub : $(icewrap_libs) $(objdir)/icewrap_stub.o
		$(FC) $(DFLAGS) $(FFLAGS) $(INC_LIS) -o $(bindir)/test_stub.x tests/test_stub.f90 \
			$(LFLAGS) $^
		@echo " "
		@echo "    test_stub.x is ready."
		@echo " "

test_yelmo : $(icewrap_libs) $(objdir)/icewrap_yelmo.o
		$(FC) $(DFLAGS) $(FFLAGS) $(INC_LIS) -o $(bindir)/test_yelmo.x tests/test_yelmo.f90 \
			$(LFLAGS) $^
		@echo " "
		@echo "    test_yelmo.x is ready."
		@echo " "

clean:
	rm -f $(bindir)/*.x
	rm -f  *.x gmon.out $(objdir)/*.o $(objdir)/*.mod $(objdir)/*.a $(objdir)/*.so
	rm -rf *.x.dSYM

