CUBLASDIR=/opt/cuda-toolkit/include/
CUBLASLIB=/opt/cuda-toolkit/lib64/
EXEC=Test.hs

HSCFILES= $(wildcard *.hsc StarPU/*.hsc)
HSC= $(HSCFILES:.hsc=.hs)
HSCOBJS= $(HSCFILES:.hsc=.o)

CFILES= $(wildcard *.c StarPU/*.c BLAS/*.c StarPU/Data/*.c)
OBJS= $(CFILES:.c=.o)

HSFILES= $(wildcard *.hs StarPU/*.hs BLAS/*.hs StarPU/Data/*.hs)
HSOBJS= $(HSFILES:.hs=.o)

%.hs: %.hsc
	@hsc2hs --cflag=`pkg-config libstarpu --cflags-only-I` $<

%.o: %.c
	gcc -Wall -o $@ `pkg-config libstarpu --cflags-only-I` -I$(CUBLASDIR) -c $<

all: $(HSC) $(EXEC) $(OBJS)
	ghc -O -XMultiParamTypeClasses -XFunctionalDependencies `pkg-config libstarpu --libs --cflags` -L$(CUBLASLIB) -lcublas -lcudart $(EXEC) $(OBJS)
	

clean:
	@rm -f *.hi BLAS/*.hi StarPU/*.hi StarPU/Data/*.hi Test $(HSC) $(OBJS) $(HSOBJS) $(HSCOBJS)

