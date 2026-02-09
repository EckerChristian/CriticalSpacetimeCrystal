CF = gfortran
#CF = pgf77
FLAGS = -O -Wall
.f.o:
	$(CF) -c $(FLAGS) -o $*.o $*.f 

.KEEP_STATE:

FOUR = fourier/four1.o fourier/fourdiff2.o fourier/inhom.o fourier/lamint.o \
	fourier/halve.o fourier/double.o fourier/mik.o fourier/fourinterpol.o \
	fourier/four_to_three.o fourier/three_to_four.o fourier/fourdiff1.o \
	fourier/fourdiff1smoothe.o

OLD = old/conv_relax_old.o \
	old/fieldsfromy.o old/derivs.o \
	old/leftcond.o old/rightcond.o old/yfromfields.o \
	solve/solvde_nb.o solve/difeq_nb.o \
	solve/red.o solve/pinvs.o solve/bksub.o \
	$(FOUR)

RELAXINNER = inner/conv_relax_inner.o \
	inner/fieldsfromy.o inner/derivs.o \
	inner/leftcond.o inner/rightcond.o inner/yfromfields.o \
	solve/solvde_nb.o solve/difeq_nb.o \
	solve/red.o solve/pinvs.o solve/bksub.o \
	$(FOUR)

EVOLVEINNER= inner/evolvemain_inner.o shoot/implicit_step.o \
	$(FOUR) \
	inner/derivs.o inner/yfromfields.o inner/fieldsfromy.o \
	inner/leftdata.o inner/rightdata.o 

RELAXOUTER = outer/conv_relax_outer.o \
	outer/fieldsfromy.o outer/derivs.o \
	outer/leftcond.o outer/rightcond.o outer/yfromfields.o \
	solve/solvde_nb.o solve/difeq_nb.o \
	solve/red.o solve/pinvs.o solve/bksub.o \
	$(FOUR)

EVOLVEOUTER= outer/evolvemain_outer.o shoot/implicit_step.o \
	$(FOUR) \
	outer/derivs.o outer/yfromfields.o outer/fieldsfromy.o \
	outer/leftdata.o outer/rightdata.o 

SHOOTINNER = inner/shootmain_inner.o inner/shoot1new.o \
	shoot/implicit_step.o shoot/explicit_step.o \
	$(FOUR) solve/lubksb.o solve/ludcmp.o solve/mprove.o \
	inner/derivs.o inner/yfromfields.o inner/fieldsfromy.o \
	inner/leftdata.o inner/rightdata.o inner/mismatch.o\
	shoot/pack2.o shoot/unpack2.o

SHOOTOUTER = outer/shootmain_outer.o outer/shoot1.o \
	shoot/implicit_step.o shoot/explicit_step.o \
	$(FOUR) solve/lubksb.o solve/ludcmp.o solve/mprove.o \
	outer/derivs.o outer/yfromfields.o outer/fieldsfromy.o \
	outer/leftdata.o outer/rightdata.o \
	shoot/pack2.o shoot/unpack2.o 

SHOOTFUTURE = future/shootmain_future.o future/shoot1new.o \
	shoot/implicit_step.o shoot/explicit_step.o \
	$(FOUR) solve/lubksb.o solve/ludcmp.o solve/mprove.o \
	future/derivs.o future/yfromfields.o future/fieldsfromy.o \
	future/leftdata.o future/rightdata.o \
	shoot/pack.o shoot/unpack.o 

TOCRITSCALAR = inner/to_critscalar.o shoot/implicit_step.o \
	shoot/explicit_step.o $(FOUR) inner/derivs.o \
	inner/fieldsfromy.o inner/yfromfields.o \
	inner/leftdata.o inner/rightdata.o

TEST = fourier/fouriermain.o fourier/fourdiff1.o fourier/four1.o fourier/mik.o
CHANGE = fourier/changeresolution.o $(FOUR)
TAUSHIFT = fourier/taushift.o $(FOUR)

conv_relax_old.exe: $(OLD)
	$(CF) $(OLD) -o $@ $(FLAGS)
conv_relax_inner.exe: $(RELAXINNER)
	$(CF) $(RELAXINNER) -o $@ $(FLAGS)
evolve_inner.exe: $(EVOLVEINNER)
	$(CF) $(EVOLVEINNER) -o $@ $(FLAGS)
shoot_inner.exe: $(SHOOTINNER)
	$(CF) $(SHOOTINNER) -o $@ $(FLAGS)
conv_relax_outer.exe: $(RELAXOUTER)
	$(CF) $(RELAXOUTER) -o $@ $(FLAGS)
evolve_outer.exe: $(EVOLVEOUTER)
	$(CF) $(EVOLVEOUTER) -o $@ $(FLAGS)
shoot_outer.exe: $(SHOOTOUTER)
	$(CF) $(SHOOTOUTER) -o $@ $(FLAGS)
shoot_future.exe: $(SHOOTFUTURE)
	$(CF) $(SHOOTFUTURE) -o $@ $(FLAGS)
test.exe: $(TEST)
	$(CF) $(TEST) -o $@ $(FLAGS)
changeresolution.exe: $(CHANGE)
	$(CF) $(CHANGE) -o $@ $(FLAGS)
taushift.exe: $(TAUSHIFT)
	$(CF) $(TAUSHIFT) -o $@ $(FLAGS)
to_critscalar.exe: $(TOCRITSCALAR)
	$(CF) $(TOCRITSCALAR) -o $@ $(FLAGS)
clean:
	rm fort.* *~ *.exe *.out *.junk */*.o */*~ *~ *.o


