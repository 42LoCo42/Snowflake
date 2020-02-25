SRCS := $(shell find . -name '*.hs')
OBJS := $(SRCS:.hs=.o)
INFS := $(SRCS:.hs=.hi)

Main: $(SRCS)
	ghc --make -dynamic Main.hs

clean:
	rm -f $(OBJS) $(INFS)

reset:
	cp SaveState.hs.old SaveState.hs
	make
