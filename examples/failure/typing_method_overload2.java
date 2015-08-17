class A extends Object {
    A() {
        super();
    }

    Object method(Object o) {
        return o;
    }
}

class B extends A {
    B() {
        super();
    }

    /* 引数の型が違う */
    Object method(A a) {
        return (Object)a;
    }
}
