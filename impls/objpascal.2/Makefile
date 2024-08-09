STEPS := $(basename $(notdir $(wildcard step*.pas)))
FPMAKE = fpmake
FPMAKE_SRC = fpmake.pp
FPFLAGS = -O3 -XX -Si
FPC = fpc

all: $(FPMAKE) build $(STEPS)

$(FPMAKE): $(FPMAKE_SRC)
	$(FPC) $(FPFLAGS) $(FPMAKE_SRC)

build: $(FPMAKE)
	./$(FPMAKE) compile -T $(shell nproc)

$(STEPS): build
	@echo "Building $@"

clean:
	@echo "Cleaning up..."
	./$(FPMAKE) distclean
	rm -rf $(FPMAKE) $(STEPS) *.o *.ppu backup/ units/

.PHONY: all build clean
