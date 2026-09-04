.PHONY: build setup setup_force

build:
	./scripts/build-user-lisp

setup: build
	./scripts/install-git-hooks

setup_force: build
	./scripts/install-git-hooks --force
