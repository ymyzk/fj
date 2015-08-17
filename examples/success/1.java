class X extends Object {
    X() {
        super();
    }
    Object m(A x) {
        return x.f;
    }
}

class A extends Object {
    Object f;
    A(Object f) {
        super();
        this.f = f;
    }
}
