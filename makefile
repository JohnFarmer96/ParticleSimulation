# Direktiven beim Linken
PRG_DIR = dist/
SRC_DIR = src/
OBJ_DIR = out/
MOD_DIR = src/
MKL_DIR = ../packages/

# Programmname
PROG = particle_simulation
# Liste der Quelldateien
SRCS = main.f90
# Liste der Objektdateien (geordnet)
MODS = module_parameters.f90 module_particle.f90 module_init.f90 numeric_integration.f90 module_force.f90 module_evaporation.f90 module_movement.f90 particle_simulation.f90
# Liste der MKL-Dateien (geordnet)
MKLS = # ? mkl_blas.f90 blas.f90
# Name des Compilers
FC = gfortran
# Compilieroptionen beim Übersetzen
FLAGS = -O2
# Bibliotheken
LIBS =  # ? ${MKLROOT}/lib/intel64/libmkl_blas95_ilp64.a ${MKLROOT}/lib/intel64/libmkl_lapack95_ilp64.a -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -ldl

# * ================== ADAPT DIRECTORY PREFIXES ============================
PRG_PATH = $(addprefix $(PRG_DIR),$(PROG))
SRC_PATH = $(addprefix $(SRC_DIR),$(SRCS))

MOD_PATH = MODS
MKL_PATH = MKLS

ifdef MOD_PATH
	MOD_PATH = $(addprefix $(MOD_DIR),$(MODS))
endif

ifdef MKL_PATH
	MKL_PATH = $(addprefix $(MKL_DIR),$(MKLS))
endif

# * ========================================================================

clean:
	rm -f $(PRG_PATH) *.mod *.o
build: 
	$(FC) -o $(PRG_PATH) $(MKL_PATH) $(MOD_PATH) $(SRC_PATH) $(LIBS)
	rm -f *.mod
run:
	./$(PRG_PATH)
echo: 
	@echo $(PRG_PATH)
	@echo $(SRC_PATH)
	@echo $(MOD_PATH)
	@echo $(MKL_PATH)