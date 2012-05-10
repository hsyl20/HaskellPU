CUBLASDIR=/opt/cuda-toolkit/include/
CUBLASLIB=/opt/cuda-toolkit/lib64/
EXEC=Test.hs

HSCFILES= $(wildcard *.hsc)
HSC= $(HSCFILES:.hsc=.hs)

CFILES= $(wildcard *.c)
OBJS= $(CFILES:.c=.o)

%.hs: %.hsc
	@hsc2hs --cflag=`pkg-config libstarpu --cflags-only-I` $<

%.o: %.c
	gcc -Wall -o $@ `pkg-config libstarpu --cflags-only-I` -I$(CUBLASDIR) -c $<

all: $(HSC) $(EXEC) $(OBJS)
	@ghc -XMultiParamTypeClasses -XFunctionalDependencies `pkg-config libstarpu --libs --cflags` -L$(CUBLASLIB) $(EXEC) $(OBJS)
	

clean:
	@rm -f *.o *.hi Test $(HSC)

