NAME = wolfram
BIN_NAME = wolfram-exe
BIN_PATH = $(shell stack path --local-install-root)/bin/$(BIN_NAME)

.PHONY: all build run clean fclean re

all: build

build:
	stack build
	cp $(BIN_PATH) .
	mv $(BIN_NAME) $(NAME)

run:
	./$(NAME)

clean:
	stack clean
	rm -f *.o *.log a.out

fclean: clean
	rm -f $(NAME)

re: fclean all
