##
## EPITECH PROJECT, 2019
## evalexpr
## File description:
## Makefile
##

PROJECT_NAME = evalexpr

BINARY_NAME = funEvalExpr

PATH = $(shell stack path --local-install-root)

$(PROJECT_NAME):
			# stack build
			stack exec $(PROJECT_NAME)-exe
			cp $(PATH)/bin/$(PROJECT_NAME)-exe .
			mv $(PROJECT_NAME)-exe $(BINARY_NAME)

setup:
		stack setup
clean:
		stack clean

fclean:	clean
		rm -rf $(BINARY_NAME)
		# stack purge

re:	fclean all

all:	$(PROJECT_NAME)

run:
		stack run
