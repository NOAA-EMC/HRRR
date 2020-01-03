include conf/configure

LIB	= ./libnemsio.a
INCMOD	= ./include

OBJS    = nemsio_openclose.o nemsio_read.o nemsio_write.o nemsio_module.o nemsio_module_mpi.o

$(LIB):	$(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	mv *.mod $(INCMOD)
	rm -f *.o

clean:
	$(RM) -f *.o $(INCMOD)/*.mod $(LIB)

.SUFFIXES: .c .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

.f.o:
	$(FC) $(FFLAGS) $(FIXED) -c $*.f

nemsio_read.o: nemsio_openclose.o
nemsio_write.o: nemsio_openclose.o
nemio_module.o: nemsio_openclose.o nemsio_read.o nemsio_write.o
