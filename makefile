xmonad:
	ghc .xmonad/xmonad.hs

clean:
	rm .xmonad/xmonad
	rm .xmonad/xmonad.hi
	rm .xmonad/xmonad.o


install:
	cp -ru .xmonad/ ~/
	cp -ru .xmobar/ ~/
	cp -ru .config/ ~/
	cp -u .Xresources ~/