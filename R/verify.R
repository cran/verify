"print.verify" <-
function(x, short = FALSE, ...)
{
	cat("Passed:\n")
	print(attr(x, "passed"), ...)
	if(!short) {
		cat("Commands:\n")
		print(attr(x, "commands"), ...)
	}
	cat("Names of data:", names(attr(x, "data")), "\n")
	cat("Specifics:\nversion: ")
	specs <- attr(x, "specifics")
	print(
		paste(paste(specs$version$language, "version",
		    paste(specs$version[c("major", "minor")], collapse = ".")),
		paste(specs$version[c("year", "month","day")], collapse = "-"),
		sep=", ", collapse=" ")
	)
	cat("Machine:", specs$machine, "\nDate:", specs$date, "\n")
	invisible(x)
}

"verify" <- function(x, ...)
UseMethod("verify")

"[.verify" <-
function(x, i)
{
	xat <- attributes(x)
	xat$passed <- xat$passed[i]
	xat$commands <- xat$commands[i]
	x <- unclass(x)
	x <- x[i]
	xat$names <- names(x)
	attributes(x) <- xat
	x
}

"verify.default" <-
function(x, data = list(), ...)
{
	if(!is.character(x))
		stop("x should be a character vector of commands")
	if(!exists(".Random.seed"))
		junk <- runif(1)
	random.seed <- .Random.seed
	n <- length(x)
	ans <- vector(mode = "list", length = n)
	tnam <- xnam <- names(x)
	if(!length(tnam))
		tnam <- 1:n
	for(i in 1:n) {
		if(length(data)) {
			tmp <- eval(parse(text = x[i]), data)
			if(is.null(tmp)) {
				ans[[i]] <- "(null)"
			} else {
				ans[[i]] <- eval(parse(text = x[i]), data)
			}
		}
		else {
			tmp <- eval(parse(text = x[i]))
			if(is.null(tmp)) {
				ans[[i]] <- "(null)"
			} else {
				ans[[i]] <- eval(parse(text = x[i]))
			}
		}
		data[[paste("Test", tnam[i], sep = ".")]] <- ans[[i]]
	}
	if(all(random.seed == .Random.seed))
		random.seed <- NULL
	passed <- rep(NA, n)
	names(passed) <- xnam
	length(data) <- length(data) - n
	specifics <- list(version = version, 
			machine = system("hostname", intern=TRUE), 
			date = date())
	attributes(ans) <- list(names = xnam, data = data, commands = x, 
		passed = passed, random.seed = random.seed, 
		specifics = specifics, 
		class = "verify")
	ans
}

"verify.verify" <-
function(x, ...)
{
	myhost <- function()
	if(.Platform$OS.type == "unix") {
		system("hostname", intern=TRUE)
	} else {
		system("hostname", intern=TRUE, invisible = TRUE)
	}
	commands <- attr(x, "commands")
	random.seed <- attr(x, "random.seed")
	if(length(random.seed)) {
		old.seed <- .Random.seed
		on.exit(.Random.seed <<- old.seed)
		.Random.seed <<- random.seed
	}
	n <- length(x)
	data <- attr(x, "data")
	passed <- ans <- vector("list", n)
	tnam <- xnam <- names(x)
	if(!length(tnam))
		tnam <- 1:n
	for(i in 1:n) {
		if(length(data)) {
			tmp <- eval(parse(text = commands[i]), data)
			if(is.null(tmp)) {
				ans[[i]] <- "(null)"
			} else {
				ans[[i]] <- eval(parse(text=commands[i]), data)
			}
		}
		else { 
			tmp <- eval(parse(text = commands[i]))
			if(is.null(tmp)) {
				ans[[i]] <- "(null)"
			} else {
				ans[[i]] <- eval(parse(text = commands[i]))
			}
		}
		passed[[i]] <- all.equal(x[[i]], ans[[i]])
		data[[paste("Test", tnam[i], sep = ".")]] <- ans[[i]]
	}
	if(all(unlist(lapply(passed, mode)) == "logical"))
		passed <- unlist(passed)
	names(passed) <- xnam
	length(data) <- length(data) - n
	specifics <- list(
		version = if(exists("version")) version else NULL, 
		machine = myhost(), 
		date = date())
	attributes(ans) <- list(
		names = xnam, 
		data = data, 
		commands = commands, 
		passed = passed, 
		random.seed = random.seed,
		specifics = specifics, 
		class = "verify")
	ans
}
