# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS = 
FLAGS    = -assume byterecl -convert big_endian

INCLD= 
LIBS = 
# cio
OBJS = write_idate.o

all: write_idate

write_idate: ${OBJS}
	${FC} -o hrrr_write_idate ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_write_idate
