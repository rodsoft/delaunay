TESTS = on_edge square 23 100

all: $(addprefix test.,$(TESTS))

.PRECIOUS: my.%.ps gold.%.ps

my.%.ps: tests.lua delaunay.lua mesh/mesh.lua
	lua tests.lua my $* > $@ || ( rm -f $@ && false )

gold.%.ps: tests.lua delaunay.lua mesh/mesh.lua
	lua tests.lua gold $* > $@ || ( rm -f $@ && false )

test.%: my.%.ps gold.%.ps
	diff $^ > test.$*.diff && rm -f test.$*.diff

bench:
	@lua tests.lua bench 10000

clean:
	rm -f my.*.ps gold.*.ps test.*.diff
