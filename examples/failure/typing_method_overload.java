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

    /* 戻り値の型が違う */
    A method(Object o) {
        return (A)o;
    }
}
