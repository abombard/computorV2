#
# Pure OCaml, package from Opam, two directories
#

NAME = computorV2
SOURCES = expr.ml main.ml

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug
	cp main.native $(NAME)

clean:
			$(OCB) -clean

fclean: clean
			rm -f $(NAME)

re: fclean all

native: 	sanity
			$(OCB) main.native

byte:		sanity
			$(OCB) main.byte

profile: 	sanity
			$(OCB) -tag profile main.native

debug: 		sanity
			$(OCB) -tag debug main.byte

sanity:
			# check that packages can be found
			# ocamlfind query menhir

test: 		native
			echo '[1, 2, "three", {"four": 4}]' | ./main.native

.PHONY: 	all clean byte native profile debug sanity test

