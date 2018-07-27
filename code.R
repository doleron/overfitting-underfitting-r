
#set.seed(1234567)
#set.seed(101)
#set.seed(123)
set.seed(4321)
x <- runif(10, min = 0.0, max = 1.0)

y <- sin(2 * pi *x)

plot(x, y, col="brown", bg="yellow", xlab="x")

white.noise <- rnorm(10, mean = 0, sd = 0.4)

t <- y + white.noise

plot(x, y, col="brown", bg="yellow", xlab="x")

x.seq <- seq(from=0, to=1, by=0.01)

f <- sin(2 * pi * x.seq)

lines(x.seq, f, col="red")

plot(x, t, pch=21, col="brown", bg="yellow", xlab="x")

model.1 <- lm(t ~ x)

model.1.curve <- predict(model.1, list(x = x.seq))

lines(x.seq, model.1.curve, col="navy")

model.2 <- lm(t ~ x + I(x^2));

model.3 <- lm(t ~ x + I(x^2) + I(x^3));
model.3.curve <- predict(model.3, list(x = x.seq))
lines(x.seq, model.3.curve, col="red")

model.4 <- lm(t ~ x + I(x^2) + I(x^3) + I(x^4));

model.9 <- lm(t ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9));
model.9.curve <- predict(model.9, list(x = x.seq))
lines(x.seq, model.9.curve, col="navy")


rms <- function(my.model, xs, ts) {
    result <- 0.0;
    for(i in 1:length(xs)) {
        value.x <- xs[i];
        value.t <- ts[i];
        estimative.local <- predict(my.model, list(x=value.x));
        error.local <- estimative.local - value.t;

        error.local <- error.local * error.local;
        result  <- result + error.local;
    }
    result <- sqrt(result/length(xs));

    return (result );
}

rms(model.1, x, t)

rms(model.2, x, t)

rms(model.3, x, t)

rms(model.4, x, t)

rms(model.9, x, t)

x.valid <- runif(10, min = 0.0, max = 1.0)

y.valid <- sin(2 * pi * x.valid)

white.noise.valid <- rnorm(10, mean = 0, sd = 0.4)

t.valid <- y.valid + white.noise.valid

rms(model.1, x.valid, t.valid)
rms(model.2, x.valid, t.valid)
rms(model.3, x.valid, t.valid)
rms(model.4, x.valid, t.valid)
rms(model.9, x.valid, t.valid)
