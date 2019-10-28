##
## EPITECH PROJECT, 2019
## makefile
## File description:
## makefile
##

NAME	=	evalexpr

SRC		=	app/Main.hs

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .
	mv $(NAME)-exe $(NAME)

clean:
	stack clean
	rm .stack-work evalexpr.cabal -rf

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all clean fclean re