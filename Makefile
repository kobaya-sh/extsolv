#FC	=	ifort
#FFLAG	=	-O3
#FFLAG	=	-g -traceback -CB -check all
#FC	=	pgf95
#FFLAG	=	-C -g -traceback
FC	=	gfortran
#FFLAG	=	-O3
FFLAG	=	-fbacktrace -fbounds-check -Wuninitialized -g  -v
#FFLAG	=	-fbounds-check -g
#FC	=	g95
#FFLAG	=	-i8 -r8 -fbounds-check -g -ftrace=full
#FFLAG	=	-O3
#INCLUDE	=	/home/$(LOGNAME)/.local/include
#LIB	=	/home/$(LOGNAME)/.local/lib
INCLUDE	=	/usr/include/
LIB	=	/usr/lib/x86_64-linux-gnu/
OBJS	=	main.o 				\
		prmtop.o	option.f90	\
		prmtop_r.o			\
		ein.o		eou.o		\
		moment.o			\
		bond.o		bend.o		\
		tors.o				\
		nonbon.o	nb14.o		\
		pme.o		fm.o		\
		function.o			\
		pbc.o				\
		clock.o
#		fft.o
		#zfft3d.o	fft235.o	\
		#kernel.o

FFTW3	=	./fftw-3.3.8/.libs/libfftw3.a -I./fftw-3.3.8/api/

.PHONY:	fftw3
.SUFFIXES: .o .f90

.f90.o:		*.inc
	#$(FC) -L$(LIB) -I$(INCLUDE) -lblas -llapack -llapacke $(FFLAG) $(FFTW3) -c $<
	$(FC) $(FFLAG) $(FFTW3) -c $<
all:		mm.x

mm.x:		$(OBJS) *.inc
	#$(FC) -L$(LIB) -I$(INCLUDE) -lblas -llapack $(FFLAG) $(OBJS) $(FFTW3) /usr/lib/liblapack.a /usr/lib/libblas.a -o $@
	#$(FC) -L$(LIB) -L/usr/lib/ -I$(INCLUDE) -lblas -llapack -llapacke $(FFLAG) $(OBJS) $(FFTW3) -o $@
	#$(FC) $(FFLAG) /usr/lib/x86_64-linux-gnu/libblas.a /usr/lib/x86_64-linux-gnu/liblapack.a $(OBJS) $(FFTW3) -o $@
	#$(FC) $(FFLAG) $(OBJS) $(FFTW3) -L/usr/lib/x86_64-linux-gnu/ -lblas -L/usr/lib/x86_64-linux-gnu/ -llapack -o $@
	$(FC) $(FFLAG) $(OBJS) -L/usr/lib/x86_64-linux-gnu/ -lblas -L/usr/lib/x86_64-linux-gnu/ -llapack -L/usr/lib/x86_64-linux-gnu/ -lfftw3f -o $@

clean:
	rm -f *.o *.x
	rm -f prmtop_r.f90
	./moduleclean

#zfft3d.o : ffte-5.0/zfft3d.f ffte-5.0/param.h
#	$(F77) $(FFLAG) -c ffte-5.0/zfft3d.f -o zfft3d.o
#
#fft235.o : ffte-5.0/fft235.f
#	$(F77) $(FFLAG) -c ffte-5.0/fft235.f -o fft235.o
#
#kernel.o : ffte-5.0/kernel.f
#	$(F77) $(FFLAG) -c ffte-5.0/kernel.f -o kernel.o

prmtop_r.f90:		prmtop_r.sh
	sh prmtop_r.sh > prmtop_r.f90

mkprm.x:		mkprm.f90 prmtop_r.f90 modprm.f90 prmtop_w.f90
	$(FC) $(FFLAG) mkprm.f90 prmtop_r.f90 modprm.f90 prmtop_w.f90 -o $@

debug_function.x:	debug_function.f90 function.f90
	$(FC) debug_function.f90 function.f90 -o $@

debug_pbc.x:		debug_pbc.f90 pbc.f90 mmpara.inc
	$(FC) debug_pbc.f90 pbc.f90 -o $@

debug_ein.x:		debug_ein.f90 ein.f90 *.inc
	$(FC) $(FFLAG) debug_ein.f90 ein.f90 -o $@

#debug_ewald.x:		debug_ewald.o ewald_e.o ewald_f.o function.o pbc.o *.inc
#	$(FC) $(FFLAG) debug_ewald.o ewald_e.o ewald_f.o function.o pbc.o -o $@

#debug_ewald.x:		debug_ewald.o ewald_e.o ewald_f.o pme.o function.o pbc.o *.inc zfft3d.o fft235.o kernel.o ffte-5.0/param.h fm.o
#	$(FC) $(FFLAG) debug_ewald.o ewald_e.o ewald_f.o pme.o function.o pbc.o zfft3d.o fft235.o kernel.o fm.o -o $@

debug_ewald.x:		debug_ewald.o ewald_e.o ewald_f.o pme.o function.o pbc.o *.inc fft.o fm.o
	$(FC) $(FFLAG) debug_ewald.o ewald_e.o ewald_f.o pme.o function.o pbc.o fft.o fm.o -o $@

debug_option.x:		debug_option.f90 option.f90 *.inc
	$(FC) $(FFLAG) debug_option.f90 option.f90 -o $@

debug_pme.x:		debug_pme.f90 cds_e.f90 pme_e.f90 ewald_e.f90 cds_f.f90 pme_f.f90 ewald_f.f90 \
			function.f90 pbc.f90 zfft3d.o fft235.o kernel.o *.inc ffte-5.0/param.h
	$(FC) $(FFLAG) debug_pme.f90 cds_e.f90 pme_e.f90 ewald_e.f90 cds_f.f90 pme_f.f90 ewald_f.f90  \
			function.f90 pbc.f90 zfft3d.o fft235.o kernel.o -o $@

fftw3:	./fftw-3.3.8/.libs/libfftw3.a

./fftw-3.3.8/.libs/libfftw3.a:
	# to be moved to configure in future
	tar zxvf fftw-3.3.8.tar.gz
	(cd fftw-3.3.8; ./configure --prefix=$(PWD))
	make -C fftw-3.3.8


