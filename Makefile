all:
	@stack build
	@stack exec free-monads-example-exe

docker:
	@docker build -t stilesb/free-monads-example .
