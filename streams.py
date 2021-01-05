def average(x, y):
    return (x + y) / 2

def sqrt_stream(n):
    def _improve_guess(g):
        return average(g, n / g)

    yield 1
    yield from map(_improve_guess, sqrt_stream(n))

s = sqrt_stream(2)
for _ in range(10):
    print(next(s))
