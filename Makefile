##
## EPITECH PROJECT, 2018
## DumbXML
## File description:
## Makefile
##

NAME	=	koak

RM		=	rm -f

all:	$(NAME)

$(NAME): clean
	@stack build
	cp `stack path --local-install-root`/bin/koak-exe ./$(NAME)

clean:
	$(RM) $(NAME)

fclean:	clean
	@stack clean

re:		fclean all

watch:
	stack build --file-watch

test:
	rm -f ./koak-exe.tix
	@stack test --fast --file-watch --coverage

.PHONY: all clean fclean re watch test
