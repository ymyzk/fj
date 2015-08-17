class A extends Object {
    A () {
        super();
    }
}

class D extends Object {
    D () {
        super();
    }
}

class B extends A {
    A a;
    B (A aa) {
        super();
        this.a = aa;
    }
}

class C extends B {
    D d;
    C (D dd, A aa) {
        super(dd);
        this.d = dd;
    }
}
