all:
	jar -cmf manifest.mf Tasm6.jar -C workspace/Tasm6/bin jp

install:
	install -m 755 -o root -g wheel ./tasm6 /usr/local/bin/
	mkdir -p /usr/local/lib/tasm/
	install -m 644 -o root -g wheel ./Tasm6.jar /usr/local/lib/tasm/

