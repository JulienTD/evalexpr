##
## EPITECH PROJECT, 2019
## makefile
## File description:
## makefile
##

NAME=	evalexpr

BINARY_NAME=	funEvalExpr

SRC=	app/Main.hs

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .
	mv $(NAME)-exe $(BINARY_NAME)

clean:
	stack clean
	rm -rf .stack-work evalexpr.cabal

fclean:	clean
	rm -rf $(BINARY_NAME)

re:	fclean all

.PHONY: all clean fclean re