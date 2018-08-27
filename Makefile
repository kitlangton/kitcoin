setup: install-deps build

build:
	psc-package build

install-deps:
	yarn install

serve:
	parcel serve index.html
