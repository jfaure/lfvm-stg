NAME	=	lfvm

all:
	stack -j4 build &&\
	ln -sf $$(stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)
