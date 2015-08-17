class A extends Object {
    Object x;
    A(Object x) {
        super();
        this.x = x;
    }

    /* T-New */
    A methodA(Object o) {
        return new A(o);
    }

    /* T-New */
    A methodB(A o) {
        /* A <: Object なのでコンストラクタを呼べる */
        return new A(o);
    }

    /* T-Var */
    A methodC(A a) {
        return a;
    }

    /* T-Field */
    Object methodD(A a) {
        return a.x;
    }

    /* T-Invk */
    A methodE(A a, Object o) {
        return a.methodA(o);
    }

    /* T-Invk */
    A methodF(A a, A b) {
        /* A <: Object なので methodA (Object -> A) を呼べる */
        return a.methodA(b);
    }

    /* T-UCast */
    Object methodG(A a) {
        return (Object)a;
    }

    /* T-UCast */
    A methodH(A a) {
        return (A)a;
    }

    /* T-DCast */
    A methodI(Object o) {
        return (A)o;
    }

    /* T-SCast */
    A methodJ(B b) {
        return (A)b;
    }

    /* T-Method */
    Object methodK(Object o) {
        return o;
    }

    /* T-Method */
    Object methodL(A a) {
        /* A <: Object なので A を返すのは OK */
        return a;
    }
}

class B extends Object {
    B() {
        super();
    }
}

class C extends A {
    C(Object x) {
        super(x);
    }

    /* T-Method */
    /* オーバーライド */
    Object methodL(A a) {
        return (Object)a;
    }
}

/* T-Class */
class D extends A {
    B y;
    C z;
    D(Object x, B y, C z) {
        super(x);
        this.y = y;
        this.z = z;
    }
}

/* T-Class */
class E extends A {
    B y;
    C z;
    E(B y, Object x, C z) {
        super(x);
        this.y = y;
        this.z = z;
    }
}

class F extends Object {
    A a;
    B b;
    F(A a, B b) {
        super();
        this.a = a;
        this.b = b;
    }

    A first() {
        return this.a;
    }

    B second() {
        return this.b;
    }

    Object method(F f) {
        return (Object)new F((A)(C)f.first(), f.second());
    }
}
