
def ones():
	yield 1
	for o in ones():
		yield o
		
def integers():
	yield 1
	ii = integers()
	oo = ones()
	while True:
		yield ii.next() + oo.next()
		
def take(gen, len):
	while len > 0:
		yield gen.next()
		len -= 1
		
print list(take(integers(),7)) # ==> [1, 2, 3, 4, 5, 6, 7]