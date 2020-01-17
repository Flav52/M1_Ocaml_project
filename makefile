all:rsa KEYS indexK

rsa: primes.ml tux.ml cle.ml 
	ocamlc -o rsa nums.cma primes.ml tux.ml cle.ml 

KEYS:
	cp -R Static/KEYS .

indexK:
	cp Static/indexK .

clean:
	rm cle.cmo cle.cmi tux.cmo tux.cmi primes.cmo primes.cmi rsa indexK crypt.txt decrypt.txt;\
	rm -r KEYS

flush: 
	rm indexK;\
	rm -r KEYS;\
	make