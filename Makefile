CUBLASDIR=/opt/cuda-toolkit/include/
CUBLASLIB=/opt/cuda-toolkit/lib64/
EXEC=Test.hs

HSCFILES= $(wildcard *.hsc StarPU/*.hsc)
HSC= $(HSCFILES:.hsc=.hs)

CFILES= $(wildcard *.c StarPU/*.c BLAS/*.c)
OBJS= $(CFILES:.c=.o)

%.hs: %.hsc
	@hsc2hs --cflag=`pkg-config libstarpu --cflags-only-I` $<

%.o: %.c
	gcc -Wall -o $@ `pkg-config libstarpu --cflags-only-I` -I$(CUBLASDIR) -c $<

all: $(HSC) $(EXEC) $(OBJS)
	ghc -O -XMultiParamTypeClasses -XFunctionalDependencies `pkg-config libstarpu --libs --cflags` -L$(CUBLASLIB) -lcublas -lcudart $(EXEC) $(OBJS)
	

clean:
	@rm -f *.hi BLAS/*.hi StarPU/*.hi Test $(HSC) $(OBJS)

