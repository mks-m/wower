all:
	(erl -make;cp common/src/wower.app ebin/)

clean:
	(rm -rf ebin/*.beam)
