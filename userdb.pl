
assert(db2('Orlando',userName,'Orlando')).
assert(db2('Orlando',password,orlando)).
assert(db2('Orlando',film,tt0004972)).
assert(db2('Orlando',film,tt0012349)).
retractall(db2(_,film,tt0012349),1).
assert(db2('Max',userName,'Max')).
assert(db2('Max',password,max)).
assert(db2('Test1',userName,'Test1')).
assert(db2('Test1',password,test1)).
assert(db2('Test2',userName,'Test2')).
assert(db2('Test2',password,test2)).
assert(db2(test3,userName,test3)).
assert(db2(test3,password,test3)).
assert(db2('123',userName,'123')).
assert(db2('123',password,'123')).
assert(db2('Max',film,tt0015864)).
assert(db2('Max',film,tt0004972)).
assert(db2('Max',film,tt0018773)).
assert(db2('Max',film,tt0021749)).
retractall(db2('Max',film,tt0015864),1).
assert(db2('Max',film,tt0015864)).

assert(db2('Test69',userName,'Test69')).
assert(db2('Test69',password,'69')).
assert(db2('Test69',film,tt0015864)).
retractall(db2('Test69',film,tt0015864),1).
