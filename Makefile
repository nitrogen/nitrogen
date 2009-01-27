compile: 
	erl -make

clean:
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*

test: compile
	erl -noshell -pa ebin -pa test/ebin -s test_suite test -s init stop

