class A extends Object {
    A () {
        super();
    }
}

class B extends Object {
    B() {
        super();
    }
}

class Pair extends Object {
    Object fst;
    Object snd;
    Pair(Object fst, Object snd) {
        super();
        this.fst=fst;
        this.snd=snd;
    }
    Pair setfst(Object newfst) {
        return new Pair(newfst, this.snd);
    }
}

class Demo extends Object {
    Demo() {
        super();
    }
    B demoMethod() {
        // This expression evaluates to the expression new B()
        return (B)((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd;
    }
}
