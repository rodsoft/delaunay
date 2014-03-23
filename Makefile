TESTS = 5 on_edge square 23 100

all: $(addprefix test.,$(TESTS))

.PRECIOUS: my.%.ps gold.%.ps

%.ps: tests.lua delaunay.lua mesh/mesh.lua
	lua tests.lua $* > $@ || ( rm -f $@ && false )

exp/%.ps: tests.lua delaunay.lua mesh/mesh.lua
	lua tests.lua $* > $@ || ( rm -f $@ && false )

test.%: %.ps
	diff $< exp/$< > test.$*.diff && rm -f test.$*.diff

gen_expected: $(addprefix exp/, $(addsuffix .ps,$(TESTS)))

bench:
	@lua tests.lua bench 10000

clean:
	rm -f *.ps test.*.diff
