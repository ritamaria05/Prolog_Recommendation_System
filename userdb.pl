
assert(db2('Orlando',userName,'Orlando')).
assert(db2('Orlando',password,orlando)).
assert(db2('Orlando',film,tt0004972)).
assert(db2('Orlando',film,tt0012349)).
retractall(db2(_,film,tt0012349),1).
