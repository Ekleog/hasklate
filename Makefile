main: src/Main.hs
	stack build
	echo '#!/bin/sh' > main
	echo 'exec stack exec -- hasklate "$$@"' >> main
	chmod +x main

clean:
	stack clean
	rm main
