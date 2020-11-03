xmonad:
	ghc .xmonad/xmonad.hs

clean:
	rm .xmonad/xmonad
	rm .xmonad/xmonad.hi
	rm .xmonad/xmonad.o


install:
	cp .xmobarrc ~/.xmobarrc
	cp -r .xmonad ~/.xmonad