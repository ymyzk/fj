class A extends Object {
    A() {
        super();
    }

    Object method(Object o) {
        return o;
    }
}

/* A と C の間に B を挟んでもオーバーライドのチェックができるか */
class B extends A {
    B() {
        super();
    }
}

class C extends B {
    C() {
        super();
    }

    /* 戻り値の型が違う */
    A method(Object o) {
        return (A)o;
    }
}
